"""Plotting utilities for the churn notebook and scripted reporting."""

from __future__ import annotations

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

from customer_churn_analysis.analysis import churn_rate_by_category
from customer_churn_analysis.config import FIGURES_DIR


def set_project_theme() -> None:
    """Apply a consistent plotting theme for the project."""
    # Use one styling function so every chart in the notebook looks cohesive.
    sns.set_theme(style="whitegrid", context="talk")
    plt.rcParams["figure.figsize"] = (12, 7)
    plt.rcParams["axes.titlesize"] = 18
    plt.rcParams["axes.labelsize"] = 13


def save_current_figure(filename: str) -> None:
    """Persist the active matplotlib figure to the figures directory."""
    FIGURES_DIR.mkdir(parents=True, exist_ok=True)
    plt.tight_layout()
    plt.savefig(FIGURES_DIR / filename, dpi=200, bbox_inches="tight")


def add_bar_value_labels(ax: plt.Axes, values: pd.Series) -> None:
    """Attach formatted percentage labels to a seaborn bar chart."""
    bars = [patch for patch in ax.patches if patch is not None and pd.notna(patch.get_height())]
    labels = [f"{value:.1f}%" for value in values]

    for bar, label in zip(bars, labels):
        ax.annotate(
            label,
            xy=(bar.get_x() + bar.get_width() / 2, bar.get_height()),
            xytext=(0, 3),
            textcoords="offset points",
            ha="center",
            va="bottom",
        )


def plot_churn_rate_by_split(frame: pd.DataFrame) -> None:
    """Bar chart showing churn rate by source split."""
    # Aggregate churn once so we can both plot it and label the bars using the
    # same values.
    summary = frame.groupby("source_split", as_index=False)["churn"].mean()
    summary["churn_rate_pct"] = summary["churn"] * 100

    ax = sns.barplot(
        data=summary,
        x="source_split",
        y="churn_rate_pct",
        hue="source_split",
        palette=["#1f77b4", "#ff7f0e"],
        legend=False,
    )
    ax.set_title("Churn Rate By Dataset Split")
    ax.set_xlabel("Dataset Split")
    ax.set_ylabel("Churn Rate (%)")

    # Add direct labels so the chart is presentation-ready without hover tools.
    add_bar_value_labels(ax, summary["churn_rate_pct"])
    save_current_figure("churn_rate_by_split.png")


def plot_churn_by_contract(frame: pd.DataFrame) -> None:
    """Bar chart of churn rate by contract length."""
    summary = churn_rate_by_category(frame, "contract_length")
    summary["churn_rate_pct"] = summary["churn_rate"] * 100

    ax = sns.barplot(
        data=summary,
        x="contract_length",
        y="churn_rate_pct",
        hue="contract_length",
        palette="Blues_r",
        legend=False,
    )
    ax.set_title("Churn Rate By Contract Length")
    ax.set_xlabel("Contract Length")
    ax.set_ylabel("Churn Rate (%)")
    add_bar_value_labels(ax, summary["churn_rate_pct"])
    save_current_figure("churn_rate_by_contract_length.png")


def plot_churn_by_subscription(frame: pd.DataFrame) -> None:
    """Bar chart of churn rate by subscription type."""
    summary = churn_rate_by_category(frame, "subscription_type")
    summary["churn_rate_pct"] = summary["churn_rate"] * 100

    ax = sns.barplot(
        data=summary,
        x="subscription_type",
        y="churn_rate_pct",
        hue="subscription_type",
        palette="Greens_r",
        legend=False,
    )
    ax.set_title("Churn Rate By Subscription Type")
    ax.set_xlabel("Subscription Type")
    ax.set_ylabel("Churn Rate (%)")
    add_bar_value_labels(ax, summary["churn_rate_pct"])
    save_current_figure("churn_rate_by_subscription_type.png")


def plot_numeric_distributions(frame: pd.DataFrame) -> None:
    """Compare key numeric distributions for churned vs retained customers."""
    columns = ["tenure_months", "usage_frequency", "support_calls", "payment_delay_days", "total_spend"]
    feature_labels = {
        "tenure_months": "Tenure (Months)",
        "usage_frequency": "Usage Frequency",
        "support_calls": "Support Calls",
        "payment_delay_days": "Payment Delay (Days)",
        "total_spend": "Total Spend",
    }

    # Reshape to long format so seaborn can facet one chart per feature while
    # keeping the code compact.
    melted = frame[columns + ["churn_label"]].melt(
        id_vars="churn_label",
        var_name="feature",
        value_name="value",
    ).dropna()
    melted["feature"] = melted["feature"].map(feature_labels).astype(str)
    melted["churn_label"] = melted["churn_label"].astype(str)
    melted["value"] = pd.to_numeric(melted["value"], errors="coerce")
    melted = melted.dropna(subset=["value"])

    grid = sns.displot(
        data=melted,
        x="value",
        hue="churn_label",
        col="feature",
        col_wrap=3,
        kind="kde",
        fill=True,
        common_norm=False,
        height=4,
        aspect=1.1,
        palette=["#2ca02c", "#d62728"],
        facet_kws={"sharex": False, "sharey": False},
    )
    grid.set_axis_labels("Value", "Density")
    grid.set_titles("{col_name}")
    grid.fig.subplots_adjust(top=0.9)
    grid.fig.suptitle("Feature Distributions By Churn Status")
    grid.savefig(FIGURES_DIR / "numeric_distributions_by_churn.png", dpi=200, bbox_inches="tight")


def plot_correlation_heatmap(frame: pd.DataFrame) -> None:
    """Correlation heatmap for the numeric features."""
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

    # Correlation matrices are easier to scan visually than raw tables when we
    # want a compact overview of positive and negative relationships.
    corr = frame[numeric_columns].corr(numeric_only=True)

    plt.figure(figsize=(10, 8))
    sns.heatmap(corr, annot=True, fmt=".2f", cmap="RdBu_r", center=0, square=True)
    plt.title("Correlation Heatmap For Numeric Features")
    save_current_figure("correlation_heatmap.png")
