"""Silver-layer transformation from Bronze data to a canonical contracts table.

This module takes the flexible, lineage-rich Bronze structure and converts it
into a stable Silver contracts table that downstream metrics can rely on.
"""

from __future__ import annotations

import json
import logging
from typing import Any

import pandas as pd

from src.utils.date_helpers import coerce_reporting_period, derive_financial_year, extract_financial_year_from_text, parse_date_series


logger = logging.getLogger(__name__)


def _pick_first_non_null(frame: pd.DataFrame, candidates: list[str]) -> pd.Series:
    """Select the first populated candidate column for each row.

    Multiple source headings may map to the same canonical concept. This helper
    resolves them row-by-row in a predictable left-to-right order.
    """
    available = [candidate for candidate in candidates if candidate in frame.columns]
    if not available:
        return pd.Series(pd.NA, index=frame.index, dtype="object")
    combined = frame[available].bfill(axis=1).iloc[:, 0]
    return combined.where(~combined.astype("string").str.strip().eq(""), pd.NA)


def _build_candidate_map(schema_registry: pd.DataFrame, config: dict[str, Any]) -> dict[str, list[str]]:
    """Build a canonical-field-to-source-column lookup from schema inference.

    The schema registry may contain several possible matches per canonical
    field, so this function ranks and preserves them in score order.
    """
    canonical_fields = config.get("canonical_contract_schema", [])
    candidates: dict[str, list[str]] = {field: [] for field in canonical_fields}

    if schema_registry.empty:
        return candidates

    ranked = schema_registry.sort_values(
        ["source_file", "suggested_canonical_field", "match_score"],
        ascending=[True, True, False],
    )
    for canonical_field in canonical_fields:
        matches = ranked.loc[
            ranked["suggested_canonical_field"] == canonical_field,
            "normalised_column",
        ].dropna()
        candidates[canonical_field] = list(dict.fromkeys(matches.tolist()))
    return candidates


def standardise_bronze_to_silver(
    bronze_contracts: pd.DataFrame,
    bronze_schema_registry: pd.DataFrame,
    config: dict[str, Any],
) -> pd.DataFrame:
    """Create the canonical Silver contracts table from Bronze inputs.

    The resulting dataframe is designed to be analysis-friendly and stable:
    consistent field names, parsed dates, numeric values, and preserved lineage.
    """

    contracts = bronze_contracts.copy()
    # Translate schema-registry evidence into a simple lookup the Silver
    # standardisation logic can use for each canonical field.
    candidate_map = _build_candidate_map(bronze_schema_registry, config)
    canonical = pd.DataFrame(index=contracts.index)

    canonical["source_file"] = contracts.get("source_file", pd.Series(pd.NA, index=contracts.index))
    canonical["source_folder"] = contracts.get("source_folder", pd.Series(pd.NA, index=contracts.index))
    canonical["source_agency"] = _pick_first_non_null(contracts, candidate_map.get("source_agency", []))
    canonical["contract_id"] = _pick_first_non_null(contracts, candidate_map.get("contract_id", []))
    canonical["supplier_name"] = _pick_first_non_null(contracts, candidate_map.get("supplier_name", []))
    canonical["supplier_abn"] = _pick_first_non_null(contracts, candidate_map.get("supplier_abn", []))
    canonical["contract_title"] = _pick_first_non_null(contracts, candidate_map.get("contract_title", []))
    canonical["procurement_category"] = _pick_first_non_null(contracts, candidate_map.get("procurement_category", []))
    canonical["procurement_method"] = _pick_first_non_null(contracts, candidate_map.get("procurement_method", []))
    canonical["contract_start_date"] = parse_date_series(
        _pick_first_non_null(contracts, candidate_map.get("contract_start_date", []))
    )
    canonical["contract_end_date"] = parse_date_series(
        _pick_first_non_null(contracts, candidate_map.get("contract_end_date", []))
    )
    canonical["publish_date"] = parse_date_series(
        _pick_first_non_null(contracts, candidate_map.get("publish_date", []))
    )
    canonical["contract_value"] = pd.to_numeric(
        _pick_first_non_null(contracts, candidate_map.get("contract_value", [])).astype("string").str.replace(",", ""),
        errors="coerce",
    )
    # Reporting period is allowed to fall back to publish/start dates when the
    # source file does not explicitly provide a reporting-period field.
    canonical["reporting_period"] = coerce_reporting_period(
        _pick_first_non_null(contracts, candidate_map.get("reporting_period", [])),
        fallback_date=canonical["publish_date"].fillna(canonical["contract_start_date"]),
    )
    canonical["financial_year"] = derive_financial_year(
        canonical["contract_start_date"].fillna(canonical["publish_date"]),
        start_month=int(config.get("runtime", {}).get("financial_year_start_month", 7)),
    )
    canonical["financial_year"] = canonical["financial_year"].fillna(
        canonical["source_file"].astype("string").map(lambda value: extract_financial_year_from_text(value) or pd.NA)
    )

    # Apply consistent cleaning rules so downstream marts are not forced to
    # keep re-implementing basic standardisation logic.
    canonical["source_agency"] = canonical["source_agency"].fillna(
        config.get("runtime", {}).get("organisation_name", "Unknown Agency")
    )
    canonical["supplier_name"] = canonical["supplier_name"].astype("string").str.strip()
    canonical["contract_title"] = canonical["contract_title"].astype("string").str.strip()
    canonical["procurement_category"] = canonical["procurement_category"].astype("string").str.strip()
    canonical["procurement_method"] = canonical["procurement_method"].astype("string").str.strip()
    canonical["supplier_abn"] = canonical["supplier_abn"].astype("string").str.replace(r"\D", "", regex=True)
    canonical["data_quality_flags"] = [[] for _ in range(len(canonical))]

    # Preserve anything that did not map cleanly so analysts can inspect the
    # original context without losing information.
    mapped_source_columns = {column for columns in candidate_map.values() for column in columns}
    preserved_columns = sorted(
        column
        for column in contracts.columns.astype(str)
        if column not in mapped_source_columns and column not in {"bronze_row_id", "source_file", "source_folder", "source_path"}
    )
    canonical["raw_metadata"] = contracts.apply(
        lambda row: json.dumps(
            {
                "bronze_row_id": row.get("bronze_row_id"),
                "source_path": row.get("source_path"),
                "source_column_lineage": row.get("source_column_lineage"),
                "unmapped_fields": {
                    column: row.get(column)
                    for column in preserved_columns
                    if pd.notna(row.get(column))
                },
            },
            default=str,
            sort_keys=True,
        ),
        axis=1,
    )

    # Keep a predictable column order in the Silver table so CSV outputs are
    # easier to inspect and diff over time.
    ordered_columns = [
        "source_file",
        "source_folder",
        "source_agency",
        "contract_id",
        "supplier_name",
        "supplier_abn",
        "contract_title",
        "procurement_category",
        "procurement_method",
        "contract_start_date",
        "contract_end_date",
        "contract_value",
        "reporting_period",
        "financial_year",
        "publish_date",
        "data_quality_flags",
        "raw_metadata",
    ]

    logger.info(
        "Silver Transformation Complete | Rows=%s | Columns=%s",
        len(canonical),
        len(ordered_columns),
    )
    
    return canonical[ordered_columns]
