"""Configurable risk rule engine for procurement governance reporting.

This module converts metrics and validation outputs into an exceptions
register that governance, procurement, and audit stakeholders can review.
"""

from __future__ import annotations

from typing import Any

import pandas as pd

from src.clean.validate_contracts import ValidationOutputs
from src.metrics.procurement_metrics import MetricPack


def _make_exception(
    exception_id: str,
    category: str,
    rule_name: str,
    severity: str,
    affected_key: str,
    explanation: str,
    recommended_follow_up: str,
) -> dict[str, str]:
    """Create a consistently structured exception record."""
    return {
        "exception_id": exception_id,
        "category": category,
        "rule_name": rule_name,
        "severity": severity,
        "affected_record_or_key": affected_key,
        "explanation": explanation,
        "recommended_follow_up": recommended_follow_up,
    }


def evaluate_risk_rules(
    contracts: pd.DataFrame,
    metric_pack: MetricPack,
    validation_outputs: ValidationOutputs,
    config: dict[str, Any],
) -> pd.DataFrame:
    """Apply configurable exception rules to contracts and aggregated metrics.

    Each rule is intentionally explicit so analysts can trace an exception back
    to a single business threshold in config.
    """

    rules = config.get("risk_rules", {})
    exceptions: list[dict[str, str]] = []

    # Aggregated concentration risk flags periods where spend is overly
    # dominated by a single supplier.
    concentration_threshold = rules.get("supplier_concentration_threshold", 0.45)
    amber_concentration = rules.get("concentration_amber_threshold", 0.35)
    top_supplier_by_period = (
        contracts.groupby(["reporting_period", "supplier_name"], dropna=False)["contract_value"]
        .sum()
        .reset_index()
        .sort_values(["reporting_period", "contract_value"], ascending=[True, False])
        .drop_duplicates(subset=["reporting_period"], keep="first")
        .set_index("reporting_period")["supplier_name"]
        .to_dict()
    )
    for _, row in metric_pack.supplier_concentration.iterrows():
        share = float(row["top_supplier_share"])
        if share >= amber_concentration:
            severity = "high" if share >= concentration_threshold else "medium"
            reporting_period = str(row["reporting_period"])
            top_supplier = str(top_supplier_by_period.get(reporting_period, "Unknown Supplier"))
            exceptions.append(
                _make_exception(
                    exception_id=f"CONC-{reporting_period}",
                    category="concentration_risk",
                    rule_name="supplier_concentration_threshold",
                    severity=severity,
                    affected_key=top_supplier,
                    explanation=f"Top supplier share reached {share:.1%} in {reporting_period}.",
                    recommended_follow_up="Review market concentration, panel usage, and competitive sourcing options.",
                )
            )

    # Near-threshold checks focus attention on awards clustered around key
    # delegated authority or process thresholds.
    threshold_low = rules.get("near_threshold_band_lower", 95000)
    threshold_high = rules.get("near_threshold_band_upper", 100000)
    near_threshold = contracts[
        contracts["contract_value"].between(threshold_low, threshold_high, inclusive="both")
    ]
    for _, row in near_threshold.iterrows():
        exceptions.append(
            _make_exception(
                exception_id=f"PROC-{row['contract_id']}",
                category="process_risk",
                rule_name="near_threshold_contract_band",
                severity="medium",
                affected_key=str(row["contract_id"]),
                explanation=f"Contract value {row['contract_value']:.0f} sits near the configured threshold band.",
                recommended_follow_up="Confirm procurement pathway and delegated approval were appropriate.",
            )
        )

    # Repeated small contracts are used as a simple fragmentation-style signal.
    small_contract_threshold = rules.get("repeated_small_contract_value_threshold", 100000)
    repeated_count_threshold = rules.get("repeated_small_contract_count_threshold", 3)
    window_days = rules.get("repeated_small_contract_window_days", 90)
    small_contracts = contracts[
        contracts["contract_value"].fillna(0).between(1, small_contract_threshold, inclusive="both")
    ].copy()
    small_contracts = small_contracts.sort_values("contract_start_date")
    for supplier_name, group in small_contracts.groupby("supplier_name", dropna=False):
        if len(group) < repeated_count_threshold:
            continue
        dates = group["contract_start_date"].dropna().sort_values()
        if len(dates) < repeated_count_threshold:
            continue
        if (dates.iloc[-1] - dates.iloc[0]).days <= window_days:
            exceptions.append(
                _make_exception(
                    exception_id=f"FIN-{str(supplier_name)[:20]}",
                    category="financial_risk",
                    rule_name="repeated_small_contracts",
                    severity="high",
                    affected_key=str(supplier_name),
                    explanation=f"{len(group)} small contracts were awarded within {window_days} days.",
                    recommended_follow_up="Assess whether purchases were intentionally fragmented or should have been aggregated.",
                )
            )

    # Period-on-period spend growth highlights sudden step changes in activity.
    spend_growth_threshold = rules.get("abrupt_spend_growth_threshold", 0.5)
    amber_growth = rules.get("spend_growth_amber_threshold", 0.25)
    for _, row in metric_pack.monthly_spend.iterrows():
        change = row.get("period_on_period_spend_change_pct")
        if pd.isna(change) or change < amber_growth:
            continue
        severity = "high" if change >= spend_growth_threshold else "medium"
        exceptions.append(
            _make_exception(
                exception_id=f"GROWTH-{row['reporting_period']}",
                category="financial_risk",
                rule_name="abrupt_spend_growth",
                severity=severity,
                affected_key=str(row["reporting_period"]),
                explanation=f"Spend increased by {change:.1%} compared with the prior period.",
                recommended_follow_up="Validate drivers of increased spend and confirm supporting approvals.",
            )
        )

    # File-level data quality rules help distinguish operational risk from
    # reporting-quality risk.
    high_null_rate_threshold = rules.get("high_null_rate_threshold", 0.2)
    for _, row in validation_outputs.data_quality_summary.iterrows():
        null_rate = float(row.get("critical_field_null_rate", 0))
        if null_rate >= high_null_rate_threshold:
            exceptions.append(
                _make_exception(
                    exception_id=f"DQ-{row['source_file']}",
                    category="data_quality_risk",
                    rule_name="high_null_rate_by_file",
                    severity="high",
                    affected_key=str(row["source_file"]),
                    explanation=f"Critical field null rate reached {null_rate:.1%} in the source file.",
                    recommended_follow_up="Engage data owners to improve disclosure completeness before relying on the file for assurance conclusions.",
                )
            )

    validation_issue_counts = validation_outputs.validation_issues.groupby("source_file").size()
    for source_file, issue_count in validation_issue_counts.items():
        if issue_count > 0:
            exceptions.append(
                _make_exception(
                    exception_id=f"TRANS-{source_file}",
                    category="transparency_risk",
                    rule_name="validation_issues_present",
                    severity="medium" if issue_count < 3 else "high",
                    affected_key=str(source_file),
                    explanation=f"{issue_count} validation issues were detected in the file.",
                    recommended_follow_up="Review data preparation controls and disclosure completeness for the affected file.",
                )
            )

    exceptions_frame = pd.DataFrame(exceptions)
    if exceptions_frame.empty:
        exceptions_frame = pd.DataFrame(
            columns=[
                "exception_id",
                "category",
                "rule_name",
                "severity",
                "affected_record_or_key",
                "explanation",
                "recommended_follow_up",
            ]
        )
    return exceptions_frame.sort_values(["severity", "category", "exception_id"], ascending=[True, True, True])
