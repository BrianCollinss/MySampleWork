"""Reusable analytical summaries for the churn project."""

from __future__ import annotations

import pandas as pd


def build_data_quality_table(datasets: dict[str, pd.DataFrame]) -> pd.DataFrame:
    """Summarise record counts, missingness, and duplicates for each dataset."""
    rows = []
    for split_name, frame in datasets.items():
        # Capture the most important quality indicators in one row per dataset
        # so the notebook can quickly show overall health.
        rows.append(
            {
                "dataset": split_name,
                "rows": len(frame),
                "columns": frame.shape[1],
                "duplicate_rows": int(frame.duplicated().sum()),
                "total_missing_values": int(frame.isna().sum().sum()),
                "churn_rate": round(frame["churn"].mean(), 2),
            }
        )
    return pd.DataFrame(rows).sort_values("dataset").reset_index(drop=True)


def build_split_comparison_table(frame: pd.DataFrame) -> pd.DataFrame:
    """Compare numeric distributions between train and test splits."""
    # These features describe customer behaviour and commercial value, so
    # comparing their means helps surface train/test drift quickly.
    numeric_columns = [
        "age",
        "tenure_months",
        "usage_frequency",
        "support_calls",
        "payment_delay_days",
        "total_spend",
        "last_interaction_days",
    ]
    summary = frame.groupby("source_split")[numeric_columns].mean().round(2).reset_index()
    return summary


def build_target_summary_table(frame: pd.DataFrame) -> pd.DataFrame:
    """Summarise churn prevalence by split."""
    # Churn rate is a primary KPI in this project, so we summarise it with
    # customer counts to avoid showing percentages without context.
    summary = (
        frame.groupby("source_split")
        .agg(customers=("customer_id", "count"), churn_rate=("churn", "mean"))
        .reset_index()
    )
    summary["churn_rate"] = summary["churn_rate"].astype(float).round(2)
    return summary


def churn_rate_by_category(frame: pd.DataFrame, category_column: str) -> pd.DataFrame:
    """Compute customer counts and churn rates for a categorical segment."""
    summary = (
        frame.groupby(category_column, as_index=False)
        .agg(
            customers=("customer_id", "count"),
            churn_rate=("churn", "mean"),
        )
        .sort_values("churn_rate", ascending=False)
    )
    summary["churn_rate"] = summary["churn_rate"].astype(float).round(2)
    return summary


def add_customer_buckets(frame: pd.DataFrame) -> pd.DataFrame:
    """Add stakeholder-friendly customer bands used in EDA and modelling."""
    enriched = frame.copy()

    enriched["tenure_band"] = pd.cut(
        enriched["tenure_months"],
        bins=[0, 12, 24, 36, 48, 60],
        labels=["0-12", "13-24", "25-36", "37-48", "49-60"],
        include_lowest=True,
    ).astype("string")
    enriched["payment_delay_band"] = pd.cut(
        enriched["payment_delay_days"],
        bins=[-1, 5, 10, 20, 30],
        labels=["0-5", "6-10", "11-20", "21-30"],
        include_lowest=True,
    ).astype("string")
    enriched["usage_band"] = pd.cut(
        enriched["usage_frequency"],
        bins=[0, 5, 10, 20, 30],
        labels=["1-5", "6-10", "11-20", "21-30"],
        include_lowest=True,
    ).astype("string")
    enriched["support_call_band"] = pd.cut(
        enriched["support_calls"],
        bins=[-1, 0, 2, 5, 10],
        labels=["0", "1-2", "3-5", "6-10"],
        include_lowest=True,
    ).astype("string")

    spend_quantiles = enriched["total_spend"].quantile([0.33, 0.66]).tolist()
    enriched["high_value_customer"] = pd.cut(
        enriched["total_spend"],
        bins=[-float("inf"), spend_quantiles[0], spend_quantiles[1], float("inf")],
        labels=["Low Value", "Mid Value", "High Value"],
        include_lowest=True,
    ).astype("string")
    return enriched


def churn_profile_table(frame: pd.DataFrame, category_column: str) -> pd.DataFrame:
    """Return both customer counts and churn outcomes for a category."""
    summary = (
        frame.groupby(category_column, dropna=False)
        .agg(
            customers=("customer_id", "count"),
            churned_customers=("churn", "sum"),
            churn_rate=("churn", "mean"),
        )
        .reset_index()
        .sort_values("churn_rate", ascending=False)
    )
    summary["churned_customers"] = summary["churned_customers"].astype(int)
    summary["churn_rate"] = summary["churn_rate"].astype(float).round(3)
    return summary


def churn_rate_by_numeric_bins(
    frame: pd.DataFrame,
    numeric_column: str,
    bins: list[float] | int,
    labels: list[str] | None = None,
) -> pd.DataFrame:
    """Group a numeric feature into bins and compute customer counts and churn rate."""
    binned = frame.copy()
    band_column = f"{numeric_column}_band"
    binned[band_column] = pd.cut(
        binned[numeric_column],
        bins=bins,
        labels=labels,
        include_lowest=True,
    ).astype("string")

    summary = (
        binned.groupby(band_column, dropna=False)
        .agg(
            customers=("customer_id", "count"),
            churn_rate=("churn", "mean"),
        )
        .reset_index()
    )
    summary["churn_rate"] = summary["churn_rate"].astype(float).round(3)
    return summary


def numeric_feature_summary(frame: pd.DataFrame) -> pd.DataFrame:
    """Compare mean numeric values for churned vs retained customers."""
    # A churn-vs-retained summary makes it easy to spot directional patterns
    # before moving into charts or predictive modelling.
    numeric_columns = [
        "age",
        "tenure_months",
        "usage_frequency",
        "support_calls",
        "payment_delay_days",
        "total_spend",
        "last_interaction_days",
    ]
    summary = frame.groupby("churn_label")[numeric_columns].mean().round(2).reset_index()
    return summary.rename(columns={"index": "feature"})


def correlation_table(frame: pd.DataFrame) -> pd.Series:
    """Return feature correlations with churn for quick ranking."""
    # Correlations are not causal, but they provide a fast way to rank which
    # variables move most strongly with the churn outcome.
    numeric_columns = [
        "age",
        "tenure_months",
        "usage_frequency",
        "support_calls",
        "payment_delay_days",
        "total_spend",
        "last_interaction_days",
        "churn",
    ]
    corr = (
        frame[numeric_columns]
        .corr(numeric_only=True)["churn"]
        .drop("churn")
        .sort_values(key=lambda x: abs(x.astype(float)), ascending=False)
    )

    return corr.round(3)
