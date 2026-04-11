"""Build static executive-style charts.

Chart generation is deliberately optional. The reporting pack should still be
able to run in environments where matplotlib is unavailable.
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import Any

import pandas as pd

from src.metrics.procurement_metrics import MetricPack
from src.metrics.supplier_segmentation import SupplierSegmentationResult
from src.utils.date_helpers import extract_financial_year_from_text
from src.utils.io_helpers import ensure_directories


LOGGER = logging.getLogger(__name__)


def _save_chart(fig: Any, output_path: Path) -> Path:
    """Persist a matplotlib figure to disk and return its final path."""

    fig.savefig(output_path, dpi=200, bbox_inches="tight")
    fig.canvas.draw()
    return output_path


def _add_title_block(fig: Any, ax: Any, title: str, subtitle: str) -> None:
    """Add a title block aligned to the left edge of the saved image.

    We intentionally align the text to the left-most rendered chart content
    rather than the plot area. This keeps titles visually flush with the final
    cropped image even when long axis labels extend far to the left.
    """

    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    tight_bbox = ax.get_tightbbox(renderer)
    left_edge = max(0.01, tight_bbox.x0 / fig.bbox.width)

    fig.text(left_edge, 0.97, title, ha="left", va="top", fontsize=14, fontweight="bold")
    fig.text(left_edge, 0.93, subtitle, ha="left", va="top", fontsize=10, color="#555555")
    fig.subplots_adjust(top=0.8)


def _plot_line_with_labels(ax: Any, labels: list[str], values: pd.Series) -> None:
    """Plot a line chart using stable categorical positions and text labels."""

    positions = list(range(len(labels)))
    ax.plot(positions, values, marker="o")
    ax.set_xticks(positions, labels=labels, rotation=45, ha="right")


def _plot_bar_with_labels(ax: Any, labels: list[str], values: pd.Series) -> None:
    """Plot a bar chart using stable categorical positions and text labels."""

    positions = list(range(len(labels)))
    ax.bar(positions, values)
    ax.set_xticks(positions, labels=labels, rotation=45, ha="right")


def _set_two_month_gap_labels(ax: Any, labels: list[str]) -> None:
    """Display monthly labels with a two-month gap between shown ticks."""

    positions = list(range(len(labels)))
    visible_positions = positions[::3]
    visible_labels = [labels[index] for index in visible_positions]
    ax.set_xticks(visible_positions, labels=visible_labels, rotation=45, ha="right")


def _add_mean_and_std_band(ax: Any, values: pd.Series, label_prefix: str, value_format: str = ",.0f") -> None:
    """Overlay a mean line plus one- and two-standard-deviation bands."""

    numeric_values = pd.to_numeric(values, errors="coerce").dropna()
    if numeric_values.empty:
        return

    mean_value = float(numeric_values.mean())
    std_value = float(numeric_values.std(ddof=0))
    lower_bound_1 = mean_value - std_value
    upper_bound_1 = mean_value + std_value
    lower_bound_2 = mean_value - (2 * std_value)
    upper_bound_2 = mean_value + (2 * std_value)

    # The wider two-standard-deviation band provides extra context around
    # normal variation without overpowering the main line or the tighter band.
    ax.axhspan(lower_bound_2, upper_bound_2, color="#8c8c8c", alpha=0.08, zorder=0)
    ax.axhspan(lower_bound_1, upper_bound_1, color="#8c8c8c", alpha=0.18, zorder=1)
    ax.axhline(
        mean_value,
        color="#8B0000",
        linewidth=1.8,
        linestyle="--",
    )
    ax.text(
        0.99,
        mean_value,
        f"Avg {format(mean_value, value_format)}",
        transform=ax.get_yaxis_transform(),
        ha="right",
        va="bottom",
        color="#8B0000",
        fontsize=9,
        backgroundcolor="white",
    )
    ax.text(
        0.99,
        upper_bound_1,
        f"Mean + 1SD: {format(upper_bound_1, value_format)}",
        transform=ax.get_yaxis_transform(),
        ha="right",
        va="bottom",
        color="#666666",
        fontsize=6,
        backgroundcolor="white",
    )
    ax.text(
        0.99,
        lower_bound_1,
        f"Mean - 1SD: {format(lower_bound_1, value_format)}",
        transform=ax.get_yaxis_transform(),
        ha="right",
        va="top",
        color="#666666",
        fontsize=6,
        backgroundcolor="white",
    )
    ax.text(
        0.99,
        upper_bound_2,
        f"Mean + 2SD: {format(upper_bound_2, value_format)}",
        transform=ax.get_yaxis_transform(),
        ha="right",
        va="bottom",
        color="#8c8c8c",
        fontsize=6,
        backgroundcolor="white",
    )
    ax.text(
        0.99,
        lower_bound_2,
        f"Mean - 2SD: {format(lower_bound_2, value_format)}",
        transform=ax.get_yaxis_transform(),
        ha="right",
        va="top",
        color="#8c8c8c",
        fontsize=6,
        backgroundcolor="white",
    )


def _coverage_subtitle(metric_pack: MetricPack) -> str:
    """Describe the financial-year coverage of the selected raw folder."""

    financial_years = (
        metric_pack.financial_year_summary["financial_year"]
        .dropna()
        .astype("string")
        .loc[lambda values: values.ne("Unknown")]
        .tolist()
    )
    if not financial_years:
        return "Coverage: no financial-year values available"
    return f"Coverage: FY{financial_years[0]} to FY{financial_years[-1]}"


def _latest_period_subtitle(metric_pack: MetricPack, fallback_text: str) -> str:
    """Build a subtitle focused on the latest available reporting period."""

    periods = (
        metric_pack.monthly_spend["reporting_period"]
        .dropna()
        .astype("string")
        .loc[lambda values: values.ne("Unknown")]
        .tolist()
    )
    if not periods:
        return fallback_text
    return f"Latest monthly reporting period: {periods[-1]}"


def _financial_year_labels_from_files(data_quality_summary: pd.DataFrame) -> pd.DataFrame:
    """Aggregate file-level quality issues into financial-year labels."""

    quality = data_quality_summary.copy()
    quality["financial_year"] = quality["source_file"].astype("string").map(
        lambda value: extract_financial_year_from_text(str(value)) or "Unknown"
    )
    return (
        quality.groupby("financial_year", dropna=False)["validation_issue_count"]
        .sum()
        .reset_index()
        .sort_values("financial_year")
    )


def build_all_charts(
    output_dir: Path,
    metric_pack: MetricPack,
    exceptions: pd.DataFrame,
    data_quality_summary: pd.DataFrame,
    config: dict[str, Any],
    supplier_segmentation: SupplierSegmentationResult | None = None,
) -> dict[str, Path]:
    """Generate the standard chart pack for the reporting cycle.

    The selected charts mirror a typical governance reporting pack: volume,
    spend, concentration, exceptions, and data quality.
    """

    ensure_directories([output_dir])
    try:
        import matplotlib.pyplot as plt
    except ModuleNotFoundError:
        LOGGER.warning("matplotlib is not installed; chart generation will be skipped")
        return {}

    plt.style.use(config.get("reporting", {}).get("chart_style", "tableau-colorblind10"))
    chart_paths: dict[str, Path] = {}
    coverage_subtitle = _coverage_subtitle(metric_pack)
    top_n = int(config.get("reporting", {}).get("top_n_suppliers", 10))

    fig, ax = plt.subplots(figsize=(10, 5.6))
    _plot_line_with_labels(
        ax,
        metric_pack.monthly_spend["reporting_period"].astype("string").tolist(),
        metric_pack.monthly_spend["total_contract_value"],
    )
    _add_mean_and_std_band(ax, metric_pack.monthly_spend["total_contract_value"], "Spend", ",.0f")
    _set_two_month_gap_labels(ax, metric_pack.monthly_spend["reporting_period"].astype("string").tolist())
    _add_title_block(fig, ax, "Monthly Spend Trend", f"{coverage_subtitle} | Monthly contract value based on award/start dates")
    ax.set_ylabel("Contract Value (AUD)")
    chart_paths["monthly_spend_trend"] = _save_chart(fig, output_dir / "monthly_spend_trend.png")

    fig, ax = plt.subplots(figsize=(10, 5.6))
    _plot_line_with_labels(
        ax,
        metric_pack.monthly_counts["reporting_period"].astype("string").tolist(),
        metric_pack.monthly_counts["total_contract_count"],
    )
    _add_mean_and_std_band(ax, metric_pack.monthly_counts["total_contract_count"], "Count", ",.0f")
    _set_two_month_gap_labels(ax, metric_pack.monthly_counts["reporting_period"].astype("string").tolist())
    _add_title_block(fig, ax, "Monthly Contract Count Trend", f"{coverage_subtitle} | Monthly contract counts based on award/start dates")
    ax.set_ylabel("Contract Count")
    chart_paths["monthly_contract_count_trend"] = _save_chart(fig, output_dir / "monthly_contract_count_trend.png")

    fig, ax = plt.subplots(figsize=(10, 5.6))
    ax.barh(metric_pack.top_suppliers_by_value["supplier_name"].fillna("Unknown Supplier"), metric_pack.top_suppliers_by_value["contract_value"])
    _add_title_block(
        fig,
        ax,
        f"Top-{top_n} Suppliers by Spend",
        f"{coverage_subtitle} | Highest disclosed contract value by supplier across the reporting dataset",
    )
    ax.set_xlabel("Contract Value (AUD)")
    chart_paths["top_suppliers_by_spend"] = _save_chart(fig, output_dir / "top_suppliers_by_spend.png")

    fig, ax = plt.subplots(figsize=(10, 5.6))
    _plot_line_with_labels(
        ax,
        metric_pack.supplier_concentration["reporting_period"].astype("string").tolist(),
        metric_pack.supplier_concentration["top_n_supplier_share"],
    )
    _add_mean_and_std_band(ax, metric_pack.supplier_concentration["top_n_supplier_share"], "Top-N supplier share", ".1%")
    _set_two_month_gap_labels(ax, metric_pack.supplier_concentration["reporting_period"].astype("string").tolist())
    _add_title_block(
        fig,
        ax,
        f"Supplier Concentration Over Time: Top-{top_n}",
        f"{coverage_subtitle} | Combined spend share of the Top-{top_n} suppliers by monthly reporting period",
    )
    ax.set_ylabel(f"Top-{top_n} Supplier Share")
    chart_paths["supplier_concentration_over_time"] = _save_chart(fig, output_dir / "supplier_concentration_over_time.png")

    fig, ax = plt.subplots(figsize=(10, 5.6))
    exception_counts = exceptions["category"].value_counts() if not exceptions.empty else pd.Series(dtype="int64")
    _plot_bar_with_labels(ax, exception_counts.index.astype(str).tolist(), exception_counts)
    _add_title_block(fig, ax, "Exception Counts by Category", f"{coverage_subtitle} | All exceptions generated for the selected raw folder")
    ax.set_ylabel("Exception Count")
    chart_paths["exception_counts_by_category"] = _save_chart(fig, output_dir / "exception_counts_by_category.png")

    fig, ax = plt.subplots(figsize=(10, 5.6))
    quality_by_financial_year = _financial_year_labels_from_files(data_quality_summary)
    _plot_bar_with_labels(
        ax,
        quality_by_financial_year["financial_year"].astype("string").tolist(),
        quality_by_financial_year["validation_issue_count"],
    )
    _add_title_block(fig, ax, "Data Quality Issues by Financial Year", "Validation issue counts aggregated from source files by financial year")
    ax.set_ylabel("Issue Count")
    chart_paths["data_quality_issues_by_source_file"] = _save_chart(fig, output_dir / "data_quality_issues_by_source_file.png")

    if supplier_segmentation is not None and not supplier_segmentation.cluster_summary.empty:
        fig, ax = plt.subplots(figsize=(10.5, 6.2))
        segment_summary = supplier_segmentation.cluster_summary.sort_values("cluster_total_contract_value", ascending=True)
        ax.barh(segment_summary["segment_label"], segment_summary["cluster_total_contract_value"])
        _add_title_block(
            fig,
            ax,
            "Supplier Risk Segments",
            "Clustered supplier segments by aggregate disclosed contract value",
        )
        ax.set_xlabel("Aggregate Contract Value (AUD)")
        chart_paths["supplier_risk_segments"] = _save_chart(fig, output_dir / "supplier_risk_segments.png")

    if not metric_pack.supplier_concentration_decomposition.empty:
        latest_period = metric_pack.supplier_concentration_decomposition["reporting_period"].astype("string").max()
        latest_decomposition = (
            metric_pack.supplier_concentration_decomposition.loc[
                metric_pack.supplier_concentration_decomposition["reporting_period"].astype("string") == latest_period
            ]
            .assign(abs_change=lambda frame: frame["concentration_change_contribution"].abs())
            .sort_values("abs_change", ascending=False)
            .head(top_n)
            .sort_values("concentration_change_contribution", ascending=True)
        )

        if not latest_decomposition.empty:
            fig, ax = plt.subplots(figsize=(10.8, 6.4))
            colors = latest_decomposition["concentration_change_contribution"].apply(
                lambda value: "#8B0000" if value >= 0 else "#4C78A8"
            )
            ax.barh(
                latest_decomposition["supplier_name"].fillna("Unknown Supplier"),
                latest_decomposition["concentration_change_contribution"],
                color=colors,
            )
            prior_period = latest_decomposition["prior_reporting_period"].iloc[0]
            _add_title_block(
                fig,
                ax,
                "Supplier Concentration Change Drivers",
                f"Largest supplier contributions to Top-{top_n} concentration change: {prior_period} to {latest_period}",
            )
            ax.set_xlabel("Contribution to Top-N Concentration Change")
            chart_paths["supplier_concentration_change_drivers"] = _save_chart(
                fig,
                output_dir / "supplier_concentration_change_drivers.png",
            )

    plt.close("all")
    return chart_paths
