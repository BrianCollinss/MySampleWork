"""Data quality and validation checks for canonical contracts.

Validation is intentionally separated from standardisation so data cleansing
and data assurance remain distinct concerns in the pipeline design.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

import numpy as np
import pandas as pd


@dataclass(slots=True)
class ValidationOutputs:
    """Validation tables produced from the canonical contracts table."""

    contracts_with_flags: pd.DataFrame
    validation_issues: pd.DataFrame
    data_quality_summary: pd.DataFrame


def detect_possible_duplicates(contracts: pd.DataFrame) -> pd.Series:
    """Identify possible duplicates using a practical business key.

    This is a heuristic control, not a perfect deduplication engine. It is
    designed to surface review candidates for assurance follow-up.
    """

    business_key = (
        contracts["source_agency"].astype("string").fillna("")
        + "|"
        + contracts["supplier_name"].astype("string").fillna("")
        + "|"
        + contracts["contract_title"].astype("string").fillna("")
        + "|"
        + contracts["contract_value"].fillna(-1).round(2).astype("string")
        + "|"
        + contracts["contract_start_date"].astype("string").fillna("")
    )
    return business_key.duplicated(keep=False)


def _issue_rows(mask: pd.Series, contracts: pd.DataFrame, rule_name: str, severity: str, explanation: str) -> pd.DataFrame:
    """Materialise a filtered set of validation issues for one rule."""
    if not mask.any():
        return pd.DataFrame()
    issues = contracts.loc[mask, ["source_file", "source_agency", "contract_id", "supplier_name", "contract_value"]].copy()
    issues["rule_name"] = rule_name
    issues["severity"] = severity
    issues["issue_type"] = "validation"
    issues["explanation"] = explanation
    return issues


def _build_validation_rules(contracts: pd.DataFrame, config: dict[str, Any]) -> list[tuple[pd.Series, str, str, str]]:
    """Build the row-level validation masks and metadata used in Silver checks.

    Separating rule construction from output assembly keeps the validation
    logic easier to test and makes the list of active rules explicit in one
    place.
    """

    # Row-level control checks used to create both flags and issue records.
    missing_supplier = contracts["supplier_name"].isna() | contracts["supplier_name"].astype("string").str.strip().eq("")
    missing_or_zero_value = contracts["contract_value"].isna() | (contracts["contract_value"] <= 0)
    malformed_start_date = contracts["contract_start_date"].isna()
    end_before_start = (
        contracts["contract_start_date"].notna()
        & contracts["contract_end_date"].notna()
        & (contracts["contract_end_date"] < contracts["contract_start_date"])
    )
    duplicates = detect_possible_duplicates(contracts)

    # Quantile-based checks adapt to the observed data range instead of relying
    # only on fixed hard-coded value bands.
    positive_values = contracts.loc[contracts["contract_value"].fillna(0) > 0, "contract_value"]
    large_threshold = positive_values.quantile(config["risk_rules"].get("unusually_large_value_quantile", 0.95)) if not positive_values.empty else np.nan
    small_threshold = positive_values.quantile(config["risk_rules"].get("unusually_small_positive_value_quantile", 0.05)) if not positive_values.empty else np.nan

    unusually_large = contracts["contract_value"] > large_threshold if pd.notna(large_threshold) else pd.Series(False, index=contracts.index)
    unusually_small = (
        (contracts["contract_value"] > 0) & (contracts["contract_value"] < small_threshold)
        if pd.notna(small_threshold)
        else pd.Series(False, index=contracts.index)
    )

    critical_fields = config.get("critical_fields", [])
    critical_missing_rate = contracts[critical_fields].isna().mean(axis=1) if critical_fields else pd.Series(0, index=contracts.index)
    missing_critical = critical_missing_rate > config["risk_rules"].get("missing_critical_fields_threshold", 0.1)

    # Keep the rules together in one structure so the same definitions drive
    # both row-level flags and the issue register.
    return [
        (missing_supplier, "missing_supplier_name", "high", "Supplier name missing from disclosed record."),
        (missing_or_zero_value, "missing_or_zero_contract_value", "high", "Contract value missing or non-positive."),
        (malformed_start_date, "malformed_start_date", "medium", "Start date missing or could not be parsed."),
        (end_before_start, "end_date_before_start_date", "high", "Contract end date is earlier than start date."),
        (duplicates, "possible_duplicate_contract", "medium", "Potential duplicate contract detected using a business key."),
        (unusually_large, "unusually_large_contract_value", "medium", "Contract value sits in the extreme upper tail."),
        (unusually_small, "unusually_small_contract_value", "low", "Contract value is unusually small but positive."),
        (missing_critical, "missing_critical_governance_fields", "high", "Multiple critical governance fields are missing."),
    ]


def build_data_quality_outputs(
    canonical_contracts: pd.DataFrame, file_log: pd.DataFrame, config: dict[str, Any]
) -> ValidationOutputs:
    """Evaluate reusable data quality checks and summarise the results.

    This function produces three reusable outputs:
    1. The Silver contracts table with row-level flags attached.
    2. A long-form issues register for detailed review.
    3. A file-level summary used in scorecards and reporting.
    """

    contracts = canonical_contracts.copy()
    issues: list[pd.DataFrame] = []
    critical_fields = config.get("critical_fields", [])
    rules = _build_validation_rules(contracts, config)

    # Attach all triggered validation rule names back onto each row so Silver
    # outputs remain self-describing.
    flag_lists: list[list[str]] = []
    for row_index in contracts.index:
        row_flags: list[str] = []
        for mask, rule_name, _, _ in rules:
            if bool(mask.loc[row_index]):
                row_flags.append(rule_name)
        flag_lists.append(row_flags)
    contracts["data_quality_flags"] = flag_lists

    for mask, rule_name, severity, explanation in rules:
        issues.append(_issue_rows(mask, contracts, rule_name, severity, explanation))

    validation_issues = pd.concat([issue for issue in issues if not issue.empty], ignore_index=True, sort=False)
    if validation_issues.empty:
        validation_issues = pd.DataFrame(
            columns=["source_file", "source_agency", "contract_id", "supplier_name", "contract_value", "rule_name", "severity", "issue_type", "explanation"]
        )

    # Build file-level quality indicators for management reporting.
    file_null_rates = (
        contracts.groupby("source_file")[critical_fields].apply(lambda frame: frame.isna().mean().mean() if critical_fields else 0).rename("critical_field_null_rate")
    )
    issue_counts = (
        validation_issues.groupby("source_file").size().rename("validation_issue_count")
        if not validation_issues.empty
        else pd.Series(dtype="int64", name="validation_issue_count")
    )

    data_quality_summary = file_log[["source_file", "row_count", "missing_critical_fields"]].copy()
    data_quality_summary = data_quality_summary.merge(file_null_rates, on="source_file", how="left")
    data_quality_summary = data_quality_summary.merge(issue_counts, on="source_file", how="left")
    data_quality_summary["validation_issue_count"] = data_quality_summary["validation_issue_count"].fillna(0).astype(int)
    data_quality_summary["critical_field_null_rate"] = data_quality_summary["critical_field_null_rate"].fillna(0.0)
    data_quality_summary["quality_score"] = (
        100
        - (data_quality_summary["critical_field_null_rate"] * 60)
        - (data_quality_summary["validation_issue_count"] / data_quality_summary["row_count"].clip(lower=1) * 40)
    ).clip(lower=0, upper=100)

    return ValidationOutputs(
        contracts_with_flags=contracts,
        validation_issues=validation_issues,
        data_quality_summary=data_quality_summary.sort_values("quality_score", ascending=True),
    )
