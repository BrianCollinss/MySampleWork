"""Procurement metrics and trend calculations.

Gold metrics are centralised here so business logic for activity, trends,
concentration, and missing-data reporting lives in one place.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

import numpy as np
import pandas as pd


@dataclass(slots=True)
class MetricPack:
    """Metric outputs used by downstream reporting and rules."""

    summary_metrics: pd.DataFrame
    monthly_spend: pd.DataFrame
    monthly_counts: pd.DataFrame
    financial_year_summary: pd.DataFrame
    quarterly_spend: pd.DataFrame
    supplier_concentration: pd.DataFrame
    supplier_concentration_decomposition: pd.DataFrame
    top_suppliers_by_value: pd.DataFrame
    top_suppliers_by_count: pd.DataFrame
    agency_summary: pd.DataFrame
    category_summary: pd.DataFrame
    missing_data_rates: pd.DataFrame


def calculate_hhi(values: pd.Series) -> float:
    """Calculate the Herfindahl-Hirschman Index for supplier concentration."""
    total = values.sum()
    if total <= 0:
        return 0.0
    shares = values / total
    return float((shares.pow(2).sum()) * 10000)


def _period_summary(contracts: pd.DataFrame, period_column: str) -> pd.DataFrame:
    """Aggregate core contract activity metrics for a chosen time grain."""
    grouped = contracts.groupby(period_column, dropna=False).agg(
        total_contract_value=("contract_value", "sum"),
        total_contract_count=("contract_id", "count"),
        average_contract_value=("contract_value", "mean"),
        median_contract_value=("contract_value", "median"),
    )
    grouped = grouped.reset_index().sort_values(period_column)
    grouped["period_on_period_spend_change_pct"] = grouped["total_contract_value"].pct_change().replace([np.inf, -np.inf], np.nan)
    return grouped


def _build_supplier_concentration_decomposition(
    supplier_value: pd.DataFrame,
    top_n: int,
) -> pd.DataFrame:
    """Explain period-on-period concentration change by contributing suppliers.

    The decomposition tracks each supplier's share movement between adjacent
    reporting periods and ranks suppliers by absolute contribution to the
    change in combined Top-N concentration.
    """

    if supplier_value.empty:
        return pd.DataFrame(
            columns=[
                "reporting_period",
                "prior_reporting_period",
                "supplier_name",
                "current_supplier_spend_share",
                "prior_supplier_spend_share",
                "supplier_share_change",
                "current_rank",
                "prior_rank",
                "is_in_current_top_n",
                "is_in_prior_top_n",
                "concentration_change_contribution",
            ]
        )

    ranked_supplier_value = supplier_value.copy()
    ranked_supplier_value["current_rank"] = ranked_supplier_value.groupby("reporting_period")["contract_value"].rank(
        method="first", ascending=False
    )

    periods = sorted(ranked_supplier_value["reporting_period"].dropna().astype(str).unique().tolist())
    decomposition_frames: list[pd.DataFrame] = []

    for period_index in range(1, len(periods)):
        current_period = periods[period_index]
        prior_period = periods[period_index - 1]

        current_frame = ranked_supplier_value.loc[
            ranked_supplier_value["reporting_period"] == current_period,
            ["supplier_name", "supplier_spend_share", "current_rank"],
        ].rename(
            columns={
                "supplier_spend_share": "current_supplier_spend_share",
            }
        )
        prior_frame = ranked_supplier_value.loc[
            ranked_supplier_value["reporting_period"] == prior_period,
            ["supplier_name", "supplier_spend_share", "current_rank"],
        ].rename(
            columns={
                "supplier_spend_share": "prior_supplier_spend_share",
                "current_rank": "prior_rank",
            }
        )

        combined = current_frame.merge(prior_frame, on="supplier_name", how="outer")
        combined["reporting_period"] = current_period
        combined["prior_reporting_period"] = prior_period
        combined["current_supplier_spend_share"] = combined["current_supplier_spend_share"].fillna(0.0)
        combined["prior_supplier_spend_share"] = combined["prior_supplier_spend_share"].fillna(0.0)
        combined["supplier_share_change"] = (
            combined["current_supplier_spend_share"] - combined["prior_supplier_spend_share"]
        )
        combined["is_in_current_top_n"] = combined["current_rank"].fillna(top_n + 1) <= top_n
        combined["is_in_prior_top_n"] = combined["prior_rank"].fillna(top_n + 1) <= top_n
        combined["concentration_change_contribution"] = np.where(
            combined["is_in_current_top_n"] | combined["is_in_prior_top_n"],
            combined["supplier_share_change"],
            0.0,
        )

        decomposition_frames.append(combined)

    if not decomposition_frames:
        return pd.DataFrame(
            columns=[
                "reporting_period",
                "prior_reporting_period",
                "supplier_name",
                "current_supplier_spend_share",
                "prior_supplier_spend_share",
                "supplier_share_change",
                "current_rank",
                "prior_rank",
                "is_in_current_top_n",
                "is_in_prior_top_n",
                "concentration_change_contribution",
            ]
        )

    decomposition = pd.concat(decomposition_frames, ignore_index=True)
    return decomposition.sort_values(
        ["reporting_period", "concentration_change_contribution"],
        ascending=[True, False],
    )


def build_metric_pack(contracts: pd.DataFrame, config: dict[str, Any]) -> MetricPack:
    """Calculate procurement activity, concentration, and missing data metrics.

    The metric pack is the Gold analytical core used by:
    - risk rule evaluation
    - scorecard logic
    - CSV outputs
    - charts and executive summary generation
    """

    # Work from a defensive copy so downstream callers keep their original
    # validated dataframe untouched.
    metrics_frame = contracts.copy()
    metrics_frame["reporting_period"] = metrics_frame.get(
        "reporting_period", pd.Series("Unknown", index=metrics_frame.index)
    ).fillna("Unknown")
    metrics_frame["financial_year"] = metrics_frame.get(
        "financial_year", pd.Series("Unknown", index=metrics_frame.index)
    ).fillna("Unknown")
    metrics_frame["reporting_quarter"] = metrics_frame["contract_start_date"].dt.to_period("Q").astype("string")

    monthly_summary = _period_summary(metrics_frame, "reporting_period")
    financial_year_summary = _period_summary(metrics_frame, "financial_year")
    quarterly_summary = _period_summary(metrics_frame, "reporting_quarter")

    monthly_spend = monthly_summary[["reporting_period", "total_contract_value", "period_on_period_spend_change_pct"]].copy()
    monthly_counts = monthly_summary[["reporting_period", "total_contract_count"]].copy()

    # Supplier concentration is calculated within reporting period so trend
    # shifts can be tracked over time instead of only in aggregate.
    top_n = int(config.get("reporting", {}).get("top_n_suppliers", 10))
    supplier_value = (
        metrics_frame.groupby(["reporting_period", "supplier_name"], dropna=False)["contract_value"].sum().reset_index()
    )
    supplier_value["period_total"] = supplier_value.groupby("reporting_period")["contract_value"].transform("sum")
    supplier_value["supplier_spend_share"] = np.where(
        supplier_value["period_total"] > 0,
        supplier_value["contract_value"] / supplier_value["period_total"],
        0.0,
    )
    supplier_concentration = (
        supplier_value.groupby("reporting_period")
        .apply(
            lambda frame: pd.Series(
                {
                    "top_supplier_share": frame["supplier_spend_share"].max(),
                    "top_n_supplier_share": frame.nlargest(top_n, "contract_value")["supplier_spend_share"].sum(),
                    "spend_concentration_hhi": calculate_hhi(frame["contract_value"]),
                }
            )
        )
        .reset_index()
        .sort_values("reporting_period")
    )
    supplier_concentration_decomposition = _build_supplier_concentration_decomposition(supplier_value, top_n)

    # Ranking outputs are capped using a reporting parameter so report volume
    # stays readable without changing the calculation code.
    top_suppliers_by_value = (
        metrics_frame.groupby("supplier_name", dropna=False)["contract_value"].sum().reset_index().sort_values("contract_value", ascending=False).head(top_n)
    )
    top_suppliers_by_count = (
        metrics_frame.groupby("supplier_name", dropna=False)["contract_id"].count().reset_index().rename(columns={"contract_id": "contract_count"}).sort_values("contract_count", ascending=False).head(top_n)
    )
    agency_summary = (
        metrics_frame.groupby("source_agency").agg(contract_count=("contract_id", "count"), total_contract_value=("contract_value", "sum")).reset_index().sort_values("total_contract_value", ascending=False)
    )
    category_summary = (
        metrics_frame.groupby("procurement_category", dropna=False)
        .agg(contract_count=("contract_id", "count"), total_contract_value=("contract_value", "sum"))
        .reset_index()
        .sort_values("total_contract_value", ascending=False)
    )

    tracked_fields = ["supplier_name", "contract_value", "contract_start_date", "contract_end_date", "procurement_method", "procurement_category"]
    missing_data_rates = (
        metrics_frame.groupby(["source_file", "source_agency", "reporting_period", "financial_year"])[tracked_fields]
        .apply(lambda frame: frame.isna().mean().mean())
        .rename("missing_data_rate")
        .reset_index()
        .sort_values("missing_data_rate", ascending=False)
    )

    summary_metrics = monthly_summary.copy()
    summary_metrics["metric_scope"] = "monthly"

    return MetricPack(
        summary_metrics=summary_metrics,
        monthly_spend=monthly_spend,
        monthly_counts=monthly_counts,
        financial_year_summary=financial_year_summary,
        quarterly_spend=quarterly_summary,
        supplier_concentration=supplier_concentration,
        supplier_concentration_decomposition=supplier_concentration_decomposition,
        top_suppliers_by_value=top_suppliers_by_value,
        top_suppliers_by_count=top_suppliers_by_count,
        agency_summary=agency_summary,
        category_summary=category_summary,
        missing_data_rates=missing_data_rates,
    )
