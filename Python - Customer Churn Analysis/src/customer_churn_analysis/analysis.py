"""Reusable analytical summaries for the churn project."""

from __future__ import annotations

from typing import Dict

import pandas as pd


def build_data_quality_table(datasets: Dict[str, pd.DataFrame]) -> pd.DataFrame:
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
    summary["churn_rate"] = summary["churn_rate"].round(2)
    return summary


def churn_rate_by_category(frame: pd.DataFrame, category_column: str) -> pd.DataFrame:
    """Compute customer counts and churn rates for a categorical segment."""
    # This function is reusable for dimensions such as gender, subscription
    # type, and contract length.
    summary = (
        frame.groupby(category_column)
        .agg(customers=("customer_id", "count"), churn_rate=("churn", "mean"))
        .reset_index()
        .sort_values("churn_rate", ascending=False)
    )
    summary["churn_rate"] = summary["churn_rate"].round(2)
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
    summary = frame.groupby("churn_label")[numeric_columns].mean().round(2).T.reset_index()
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
    corr = frame[numeric_columns].corr(numeric_only=True)["churn"].drop("churn").\
                sort_values(key=lambda x: abs(x.astype(float)), ascending=False)
    
    return corr.round(3)
