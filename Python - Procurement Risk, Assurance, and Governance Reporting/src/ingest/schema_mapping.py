"""Schema inference and column matching for heterogeneous procurement files.

This module is the bridge between messy source-system headings and the
canonical business schema used downstream. It does three main things:
1. Normalise raw column names into a stable comparison form.
2. Infer simple datatype hints from sampled source values.
3. Score likely matches between source fields and canonical fields.
"""

from __future__ import annotations

import math
import re
from dataclasses import dataclass
from difflib import SequenceMatcher
from typing import Any

import pandas as pd


@dataclass(slots=True)
class ColumnInference:
    """Profile information inferred from a source column.

    Each object captures enough evidence for analysts to understand why a
    source field was mapped to a canonical target or left unmapped.
    """

    source_column: str
    normalised_column: str
    inferred_dtype: str
    non_null_rate: float
    sample_values: list[str]
    suggested_canonical_field: str | None
    match_score: float
    matched_by: str


def to_snake_case(value: str) -> str:
    """Convert a source column name to a comparable snake_case key.

    A single normalisation rule prevents simple naming differences such as
    spaces, punctuation, and case from being treated as different fields.
    """

    text = re.sub(r"[^A-Za-z0-9]+", "_", value.strip())
    text = re.sub(r"([a-z0-9])([A-Z])", r"\1_\2", text)
    text = re.sub(r"_+", "_", text)
    return text.strip("_").lower()


def normalise_columns(columns: list[str]) -> dict[str, str]:
    """Return a mapping from original to snake_case column names."""

    return {column: to_snake_case(column) for column in columns}


def build_reverse_mapping(config: dict[str, Any]) -> dict[str, str]:
    """Create a reverse lookup from observed aliases to canonical field names.

    This supports exact alias matching before the pipeline falls back to more
    approximate similarity-based matching.
    """

    reverse_mapping: dict[str, str] = {}
    for canonical_field, aliases in config.get("schema_mappings", {}).items():
        reverse_mapping[to_snake_case(canonical_field)] = canonical_field
        for alias in aliases:
            reverse_mapping[to_snake_case(alias)] = canonical_field
    return reverse_mapping


def infer_series_dtype(values: pd.Series) -> str:
    """Infer a lightweight business-friendly dtype from a sampled series.

    The goal is not perfect typing. The goal is enough signal to improve
    schema matching and make registry outputs interpretable.
    """

    non_null = values.dropna()
    if non_null.empty:
        return "unknown"

    as_text = non_null.astype("string").str.strip()
    # First test whether most observed values behave like numbers after
    # removing simple thousands separators.
    numeric = pd.to_numeric(as_text.str.replace(",", ""), errors="coerce")
    if numeric.notna().mean() >= 0.95:
        return "numeric"

    # If not numeric, test whether the series mostly behaves like dates.
    dates = pd.to_datetime(as_text, errors="coerce", format="mixed")
    if dates.notna().mean() >= 0.95:
        return "date"

    # If most sampled values are exactly 11 digits, treat the column as an
    # ABN-like identifier rather than a general numeric field.
    if as_text.str.fullmatch(r"\d{11}").mean() >= 0.8:
        return "identifier"
    
    # Otherwise, treat the column as a string. This is the most permissive type.
    return "string"


def _tokenise(value: str) -> set[str]:
    """Split a field name into comparable tokens for overlap scoring."""
    return {token for token in to_snake_case(value).split("_") if token}


def _name_similarity(source_name: str, candidate_name: str) -> float:
    """Blend token overlap and sequence similarity for field-name matching."""
    source_tokens = _tokenise(source_name)
    candidate_tokens = _tokenise(candidate_name)
    overlap = len(source_tokens & candidate_tokens) / max(len(source_tokens | candidate_tokens), 1)
    sequence = SequenceMatcher(None, to_snake_case(source_name), to_snake_case(candidate_name)).ratio()
    return (overlap * 0.6) + (sequence * 0.4)


def _expected_dtypes() -> dict[str, set[str]]:
    """Define broad datatype expectations for canonical fields.

    This helps reduce obviously poor matches, for example mapping a likely
    date column to a supplier-name field.
    """
    return {
        "source_agency": {"string"},
        "contract_id": {"string", "identifier"},
        "supplier_name": {"string"},
        "supplier_abn": {"string", "identifier", "numeric"},
        "contract_title": {"string"},
        "procurement_category": {"string"},
        "procurement_method": {"string"},
        "contract_start_date": {"date", "string"},
        "contract_end_date": {"date", "string"},
        "contract_value": {"numeric", "string"},
        "reporting_period": {"date", "string"},
        "publish_date": {"date", "string"},
    }


def _dtype_compatibility_score(canonical_field: str, inferred_dtype: str) -> float:
    """Return a simple compatibility score between inferred and expected dtypes."""
    expected = _expected_dtypes().get(canonical_field, {"string"})
    return 1.0 if inferred_dtype in expected else 0.35


def infer_column_schema(
    frame: pd.DataFrame,
    config: dict[str, Any],
    sample_size: int,
) -> list[ColumnInference]:
    """Infer a source schema profile and recommend canonical column mappings.

    This is the heart of the schema inference step used by Bronze ingestion.
    It first checks known aliases, then falls back to similarity scoring.
    """

    reverse_mapping = build_reverse_mapping(config)
    aliases_by_canonical = config.get("schema_mappings", {})
    inferred: list[ColumnInference] = []
    # Work from a sample for speed while still profiling the full column for
    # null rates and other broad characteristics.
    sampled = frame.head(sample_size)

    for source_column in frame.columns.astype(str):
        normalised_column = to_snake_case(source_column)
        series = sampled[source_column] if source_column in sampled.columns else sampled.iloc[:, 0]
        inferred_dtype = infer_series_dtype(series)
        non_null_rate = float(frame[source_column].notna().mean()) if len(frame) else 0.0
        sample_values = [str(value) for value in frame[source_column].dropna().astype("string").head(3).tolist()]

        # Exact alias matches are treated as the strongest mapping signal.
        if normalised_column in reverse_mapping:
            suggested = reverse_mapping[normalised_column]
            inferred.append(
                ColumnInference(
                    source_column=source_column,
                    normalised_column=normalised_column,
                    inferred_dtype=inferred_dtype,
                    non_null_rate=non_null_rate,
                    sample_values=sample_values,
                    suggested_canonical_field=suggested,
                    match_score=1.0,
                    matched_by="alias_exact",
                )
            )
            continue

        best_field: str | None = None
        best_score = -math.inf
        matched_by = "similarity"
        # When no exact alias exists, compare the source heading to each
        # canonical field and its configured aliases.
        for canonical_field, aliases in aliases_by_canonical.items():
            candidate_names = [canonical_field, *aliases]
            name_score = max(_name_similarity(normalised_column, candidate_name) for candidate_name in candidate_names)
            score = (name_score * 0.8) + (_dtype_compatibility_score(canonical_field, inferred_dtype) * 0.2)
            if score > best_score:
                best_field = canonical_field
                best_score = score

        # Low-confidence matches are surfaced as unmapped so analysts can
        # review them rather than silently accepting weak mappings.
        if best_score < 0.55:
            best_field = None
            matched_by = "unmapped"

        inferred.append(
            ColumnInference(
                source_column=source_column,
                normalised_column=normalised_column,
                inferred_dtype=inferred_dtype,
                non_null_rate=non_null_rate,
                sample_values=sample_values,
                suggested_canonical_field=best_field,
                match_score=round(max(best_score, 0.0), 4),
                matched_by=matched_by,
            )
        )

    return inferred


def canonicalise_detected_columns(columns: list[str], config: dict[str, Any]) -> dict[str, str]:
    """Map normalised source columns to configured canonical column names where possible."""

    reverse_mapping = build_reverse_mapping(config)
    return {column: reverse_mapping.get(column, column) for column in columns}


def schema_inference_to_frame(inferred_columns: list[ColumnInference]) -> pd.DataFrame:
    """Convert inferred schema objects to a dataframe.

    A dataframe is easier to persist into the Bronze schema registry and review
    in CSV form than a list of dataclass instances.
    """

    return pd.DataFrame(
        [
            {
                "source_column": item.source_column,
                "normalised_column": item.normalised_column,
                "inferred_dtype": item.inferred_dtype,
                "non_null_rate": item.non_null_rate,
                "sample_values": " | ".join(item.sample_values),
                "suggested_canonical_field": item.suggested_canonical_field,
                "match_score": item.match_score,
                "matched_by": item.matched_by,
            }
            for item in inferred_columns
        ]
    )
