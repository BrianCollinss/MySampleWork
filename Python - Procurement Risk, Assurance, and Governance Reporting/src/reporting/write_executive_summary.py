"""Generate a concise narrative executive summary in Markdown.

The summary turns Gold outputs into a report-style narrative that highlights
findings, limitations, and next assurance actions.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

import pandas as pd

from src.metrics.procurement_metrics import MetricPack
from src.metrics.supplier_segmentation import SupplierSegmentationResult


def _format_currency(value: float) -> str:
    """Render a numeric value in a simple whole-dollar AUD-style format."""
    return f"${value:,.0f}"


def _format_exception_scope(row: pd.Series) -> str:
    """Explain what entity, period, or file an exception applies to."""

    affected_key = str(row.get("affected_record_or_key", "")).strip()
    if not affected_key:
        return "Supplier: `Unknown`"
    return f"Supplier: `{affected_key}`"


def write_executive_summary(
    report_path: Path,
    metric_pack: MetricPack,
    exceptions: pd.DataFrame,
    data_quality_summary: pd.DataFrame,
    scorecard: pd.DataFrame,
    config: dict[str, Any],
    chart_paths: dict[str, Path],
    supplier_segmentation: SupplierSegmentationResult | None = None,
) -> None:
    """Write the executive summary markdown file.

    The output is intentionally concise and structured for management readers:
    current period, key risks, changes, data limitations, and next steps.
    """

    # Identify the current and prior periods used in the narrative comparison.
    latest_period = metric_pack.summary_metrics["reporting_period"].max()
    current_row = metric_pack.summary_metrics.loc[metric_pack.summary_metrics["reporting_period"] == latest_period].tail(1)
    prior_row = metric_pack.summary_metrics.loc[metric_pack.summary_metrics["reporting_period"] < latest_period].tail(1)

    total_value = float(current_row["total_contract_value"].iloc[0]) if not current_row.empty else 0.0
    total_count = int(current_row["total_contract_count"].iloc[0]) if not current_row.empty else 0
    change_pct = current_row["period_on_period_spend_change_pct"].iloc[0] if not current_row.empty else None
    overall_status = scorecard.loc[scorecard["indicator"] == "overall_governance_score", "status"].iloc[0]

    top_exceptions = exceptions.head(5)
    worst_quality = data_quality_summary.sort_values("quality_score").head(3)
    top_segments = (
        supplier_segmentation.cluster_summary.head(3)
        if supplier_segmentation is not None and not supplier_segmentation.cluster_summary.empty
        else pd.DataFrame()
    )
    latest_concentration_change = pd.DataFrame()
    if not metric_pack.supplier_concentration_decomposition.empty:
        latest_decomposition_period = metric_pack.supplier_concentration_decomposition["reporting_period"].astype("string").max()
        latest_concentration_change = (
            metric_pack.supplier_concentration_decomposition.loc[
                metric_pack.supplier_concentration_decomposition["reporting_period"].astype("string") == latest_decomposition_period
            ]
            .assign(abs_change=lambda frame: frame["concentration_change_contribution"].abs())
            .sort_values("abs_change", ascending=False)
            .head(3)
        )

    # Build the markdown report incrementally so the narrative sections remain
    # easy to read and easy to adjust later.
    lines = [
        "# Executive Summary",
        "",
        "## Reporting Period",
        f"- Latest reporting period: `{latest_period}`",
        f"- Overall governance status: `{overall_status}`",
        "",
        "## Headline Findings",
        f"- Reported procurement activity totalled {_format_currency(total_value)} across {total_count} contracts in the latest period.",
        f"- Period-on-period spend movement: {change_pct:.1%}." if pd.notna(change_pct) else "- Period-on-period spend movement could not be calculated for the first available period.",
        "- Supplier concentration and data quality indicators were evaluated using configurable governance thresholds from `config/analysis_config.yaml`.",
        "",
        "## Top Risk Indicators",
        "- The items below are rule-based exceptions from the current reporting run. Each item shows the risk category, severity, and the supplier most directly associated with the exception where available.",
    ]

    if top_exceptions.empty:
        lines.append("- No rule-based exceptions were triggered in the current run.")
    else:
        for _, row in top_exceptions.iterrows():
            lines.append(
                f"- `{row['category']}` | `{row['severity']}` | {_format_exception_scope(row)} | {row['explanation']}"
            )

    lines.extend(
        [
            "",
            "## Changes From Prior Period",
        ]
    )
    if not prior_row.empty:
        prior_value = float(prior_row["total_contract_value"].iloc[0])
        lines.append(f"- Prior period spend was {_format_currency(prior_value)}.")
        lines.append(f"- Latest movement versus prior period is {change_pct:.1%}." if pd.notna(change_pct) else "- Comparative movement was not available.")
    else:
        lines.append("- No prior reporting period was available for comparison.")

    lines.extend(
        [
            "",
            "## Data Quality Limitations",
        ]
    )
    if worst_quality.empty:
        lines.append("- No data quality summary was available.")
    else:
        for _, row in worst_quality.iterrows():
            lines.append(
                f"- `{row['source_file']}` scored {row['quality_score']:.1f}/100 with {row['validation_issue_count']} validation issues and a {row['critical_field_null_rate']:.1%} critical-field null rate."
            )

    lines.extend(
        [
            "",
            "## Supplier Risk Segments",
        ]
    )
    if top_segments.empty:
        lines.append("- Supplier risk segmentation was not available for this run.")
    else:
        for _, row in top_segments.iterrows():
            lines.append(
                f"- `{row['segment_label']}` | Suppliers: {int(row['supplier_count'])} | Aggregate spend: {_format_currency(float(row['cluster_total_contract_value']))} | {row['segment_description']}"
            )

    lines.extend(
        [
            "",
            "## Concentration Change Drivers",
        ]
    )
    if latest_concentration_change.empty:
        lines.append("- Concentration decomposition was not available for this run.")
    else:
        for _, row in latest_concentration_change.iterrows():
            direction = "increased" if float(row["concentration_change_contribution"]) >= 0 else "reduced"
            lines.append(
                f"- `{row['supplier_name']}` {direction} Top-N concentration by {float(row['concentration_change_contribution']):.1%} from `{row['prior_reporting_period']}` to `{row['reporting_period']}`."
            )

    lines.extend(
        [
            "",
            "## Recommended Next Assurance Actions",
            "- Review high-severity exceptions first, focusing on concentration, repeated small contracts, and abrupt spend changes.",
            "- Confirm whether near-threshold awards followed the intended procurement method and delegated approval pathway.",
            "- Work with data owners on files showing weak completeness before drawing strong assurance conclusions.",
            "- Refresh the reporting pack monthly using the same configuration to support trend-based governance monitoring.",
            "",
            "## Chart Pack",
        ]
    )
    for name, path in chart_paths.items():
        lines.append(f"- `{name}`: `{path.as_posix()}`")

    report_path.write_text("\n".join(lines), encoding="utf-8")
