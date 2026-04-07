"""Plotting utilities for the churn notebook and scripted reporting."""

from __future__ import annotations

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from matplotlib.axes import Axes
from matplotlib.patches import Rectangle

from customer_churn_analysis.analysis import churn_rate_by_numeric_bins
from customer_churn_analysis.config import FIGURES_DIR


def set_project_theme() -> None:
    """Apply a consistent plotting theme for the project."""
    # Use one styling function so every chart in the notebook looks cohesive.
    sns.set_theme(
        style="ticks",
        context="talk",
        palette="deep",
        rc={
            "axes.facecolor": "#ffffff",
            "figure.facecolor": "#ffffff",
            "axes.edgecolor": "#334155",
            "grid.color": "#cbd5e1",
            "grid.linestyle": "--",
            "grid.alpha": 0.5,
        },
    )

    axes_title_size = 16
    axes_label_size = axes_title_size * 0.8
    tick_label_size = axes_label_size * 0.8

    plt.rcParams["figure.figsize"] = (9, 5)
    plt.rcParams["figure.titlesize"] = 22
    plt.rcParams["axes.titlesize"] = axes_title_size
    plt.rcParams["axes.labelsize"] = axes_label_size
    plt.rcParams["xtick.labelsize"] = tick_label_size
    plt.rcParams["ytick.labelsize"] = tick_label_size
    plt.rcParams["axes.titleweight"] = "semibold"
    plt.rcParams["axes.titlepad"] = 24
    plt.rcParams["axes.labelpad"] = 12
    plt.rcParams["axes.spines.top"] = False
    plt.rcParams["axes.spines.right"] = False


def save_current_figure(filename: str) -> None:
    """Persist the active matplotlib figure to the figures directory."""
    FIGURES_DIR.mkdir(parents=True, exist_ok=True)
    plt.tight_layout()
    plt.savefig(FIGURES_DIR / filename, dpi=200, bbox_inches="tight")


def add_bar_value_labels(ax: Axes, values: pd.Series) -> None:
    """Attach formatted percentage labels to a seaborn bar chart."""
    bars = [patch for patch in ax.patches if isinstance(patch, Rectangle) and pd.notna(patch.get_height())]
    labels = [f"{value:.1f}%" for value in values]

    for bar, label in zip(bars, labels):
        ax.annotate(
            label,
            xy=(bar.get_x() + bar.get_width() / 2, bar.get_height()),
            xytext=(0, 3),
            textcoords="offset points",
            ha="center",
            va="bottom",
            fontsize=10,
        )


def _expand_with_combined_split(df: pd.DataFrame) -> pd.DataFrame:
    """Return train, test, and combined views for faceted split comparisons."""
    split_frames = [
        df.assign(plot_split=df["source_split"].astype(str)),
        df.assign(plot_split="combined"),
    ]
    expanded = pd.concat(split_frames, ignore_index=True)
    expanded["plot_split"] = pd.Categorical(
        expanded["plot_split"],
        categories=["train", "test", "combined"],
        ordered=True,
    )
    return expanded


def _plot_split_bar_columns(
    summary: pd.DataFrame,
    x_column: str,
    value_column: str,
    title: str,
    x_label: str,
    palette: str | list[str],
    filename: str | None = None,
) -> np.ndarray:
    """Render train, test, and combined bar charts as side-by-side columns."""
    fig, axes = plt.subplots(1, 3, figsize=(16, 5), sharey=True)
    split_order = ["train", "test", "combined"]

    for axis, split_name in zip(axes, split_order):
        split_summary = summary.loc[summary["plot_split"] == split_name].copy()
        sns.barplot(
            data=split_summary,
            x=x_column,
            y=value_column,
            hue=x_column,
            palette=palette,
            legend=False,
            ax=axis,
        )
        axis.set_title(split_name.title())
        axis.set_xlabel(x_label)
        axis.set_ylabel("Churn Rate (%)" if split_name == "train" else "")
        add_bar_value_labels(axis, split_summary[value_column].reset_index(drop=True))

    fig.suptitle(title)
    fig.tight_layout()
    if filename is not None:
        FIGURES_DIR.mkdir(parents=True, exist_ok=True)
        fig.savefig(FIGURES_DIR / filename, dpi=200, bbox_inches="tight")
    return axes


def plot_churn_rate_by_split(df: pd.DataFrame) -> None:
    """Bar chart showing churn rate for train, test, and combined views."""
    expanded = _expand_with_combined_split(df)
    summary = (
        expanded.groupby("plot_split", observed=False, as_index=False)
        .agg(churn_rate=("churn", "mean"))
    )
    summary["churn_rate_pct"] = summary["churn_rate"] * 100

    plt.figure()
    ax = sns.barplot(
        data=summary,
        x="plot_split",
        y="churn_rate_pct",
        color="#4c78a8",
    )
    for patch, color in zip(ax.patches, ["#1f77b4", "#ff7f0e", "#2ca02c"]):
        patch.set_facecolor(color)
    ax.set_title("Churn Rate By Dataset Split")
    ax.set_xlabel("Dataset Split")
    ax.set_ylabel("Churn Rate (%)")

    add_bar_value_labels(ax, summary["churn_rate_pct"])
    save_current_figure("churn_rate_by_split.png")


def plot_churn_by_contract(df: pd.DataFrame) -> None:
    """Bar charts of churn rate by contract length for train, test, and combined."""
    expanded = _expand_with_combined_split(df)
    summary = (
        expanded.groupby(["plot_split", "contract_length"], observed=False, as_index=False)
        .agg(customers=("customer_id", "count"), churn_rate=("churn", "mean"))
    )
    summary["churn_rate_pct"] = summary["churn_rate"] * 100
    _plot_split_bar_columns(
        summary=summary,
        x_column="contract_length",
        value_column="churn_rate_pct",
        title="Churn Rate By Contract Length",
        x_label="Contract Length",
        palette="Blues_r",
        filename="churn_rate_by_contract_length.png",
    )


def plot_churn_by_subscription(df: pd.DataFrame) -> None:
    """Bar charts of churn rate by subscription type for train, test, and combined."""
    expanded = _expand_with_combined_split(df)
    summary = (
        expanded.groupby(["plot_split", "subscription_type"], observed=False, as_index=False)
        .agg(customers=("customer_id", "count"), churn_rate=("churn", "mean"))
    )
    summary["churn_rate_pct"] = summary["churn_rate"] * 100
    _plot_split_bar_columns(
        summary=summary,
        x_column="subscription_type",
        value_column="churn_rate_pct",
        title="Churn Rate By Subscription Type",
        x_label="Subscription Type",
        palette="Greens_r",
        filename="churn_rate_by_subscription_type.png",
    )


def plot_numeric_distributions(
    df: pd.DataFrame,
    columns: list[str] | None = None,
    kind: str = "kde",
    bins: int | list[float] | list[int | list[float] | None] | None = None,
) -> None:
    """Compare numeric distributions for churned vs retained customers."""
    if columns is None:
        columns = ["age", "tenure_months", "payment_delay_days", "total_spend"]

    feature_labels = {
        "age": "Age",
        "tenure_months": "Tenure (Months)",
        "usage_frequency": "Usage Frequency",
        "support_calls": "Support Calls",
        "payment_delay_days": "Payment Delay (Days)",
        "last_interaction_days": "Last Interaction (Days)",
        "total_spend": "Total Spend",
    }

    # Reshape to long format so seaborn can facet one chart per feature while
    # keeping the code compact.
    melted = df[columns + ["churn_label"]].melt(
        id_vars="churn_label",
        var_name="feature",
        value_name="value",
    ).dropna()
    melted["feature"] = melted["feature"].map(feature_labels).astype(str)
    melted["churn_label"] = melted["churn_label"].astype(str)
    melted["value"] = pd.to_numeric(melted["value"], errors="coerce")
    melted = melted.dropna(subset=["value"])

    if kind == "hist" and isinstance(bins, list) and len(bins) == len(columns):
        fig, axes = plt.subplots(2, 2, figsize=(11, 7))
        axes_flat = axes.flatten()

        for axis, column, column_bins in zip(axes_flat, columns, bins):
            feature_name = feature_labels.get(column, column.replace("_", " ").title())
            subset = df[[column, "churn_label"]].dropna().copy()
            subset[column] = pd.to_numeric(subset[column], errors="coerce")
            subset = subset.dropna(subset=[column])

            hist_kwargs = {
                "data": subset,
                "x": column,
                "hue": "churn_label",
                "multiple": "dodge",
                "shrink": 0.8,
                "alpha": 0.85,
                "element": "bars",
                "palette": ["#d62728", "#2ca02c"],
                "ax": axis,
            }

            if column_bins is None:
                hist_kwargs["discrete"] = True
            else:
                hist_kwargs["bins"] = column_bins

            sns.histplot(**hist_kwargs)
            axis.set_title(feature_name)
            axis.set_xlabel("Value")
            axis.set_ylabel("Customer Count")

        for axis in axes_flat[len(columns):]:
            axis.set_visible(False)

        handles, labels = axes_flat[0].get_legend_handles_labels()
        for axis in axes_flat[: len(columns)]:
            legend = axis.get_legend()
            if legend is not None:
                legend.remove()

        fig.legend(handles, labels, title="churn_label", loc="center right", frameon=False)
        fig.suptitle("Discrete Feature Distributions By Churn Status")
        fig.subplots_adjust(top=0.82, right=0.84, hspace=0.65, wspace=0.40)
        FIGURES_DIR.mkdir(parents=True, exist_ok=True)
        fig.savefig(FIGURES_DIR / "discrete_distributions_by_churn.png", dpi=200, bbox_inches="tight")
        return

    displot_kwargs = {
        "data": melted,
        "x": "value",
        "hue": "churn_label",
        "col": "feature",
        "col_wrap": 2,
        "kind": kind,
        "common_norm": False,
        "height": 3.2,
        "aspect": 1.25,
        "palette": ["#d62728", "#2ca02c"],
        "facet_kws": {"sharex": False, "sharey": False},
    }

    if kind == "kde":
        displot_kwargs["fill"] = True
    elif kind == "hist":
        displot_kwargs["multiple"] = "dodge"
        displot_kwargs["discrete"] = bins is None
        displot_kwargs["shrink"] = 0.8
        displot_kwargs["alpha"] = 0.85
        displot_kwargs["element"] = "bars"
        if bins is not None:
            displot_kwargs["bins"] = bins

    grid = sns.displot(**displot_kwargs)
    grid.fig.set_size_inches(11, 7)
    grid.set_axis_labels("Value", "Density")
    grid.set_titles("{col_name}")
    grid.fig.subplots_adjust(top=0.82, right=0.84, hspace=0.65, wspace=0.45)
    if kind == "hist":
        grid.set_axis_labels("Value", "Customer Count")
        grid.fig.suptitle("Discrete Feature Distributions By Churn Status")
        output_name = "discrete_distributions_by_churn.png"
    else:
        grid.fig.suptitle("Feature Distributions By Churn Status")
        output_name = "numeric_distributions_by_churn.png"

    grid.savefig(FIGURES_DIR / output_name, dpi=200, bbox_inches="tight")


def plot_correlation_heatmap(df: pd.DataFrame) -> None:
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
    corr = df[numeric_columns].corr(numeric_only=True)

    plt.figure(figsize=(9, 7))
    sns.heatmap(
        corr,
        annot=True,
        fmt=".2f",
        cmap="RdBu_r",
        center=0,
        square=True,
        annot_kws={"size": 8},
    )
    plt.title("Correlation Heatmap For Numeric Features")
    save_current_figure("correlation_heatmap.png")


def plot_churn_rate_by_numeric_bins(
    df: pd.DataFrame,
    numeric_column: str,
    bins: list[float] | int,
    labels: list[str] | None = None,
    title: str | None = None,
) -> np.ndarray:
    """Plot train, test, and combined churn rates across grouped numeric bins."""
    expanded = _expand_with_combined_split(df)
    split_summaries: list[pd.DataFrame] = []

    for split_name in ["train", "test", "combined"]:
        split_df = expanded.loc[expanded["plot_split"] == split_name].copy()
        split_summary = churn_rate_by_numeric_bins(split_df, numeric_column, bins=bins, labels=labels)
        split_summary["plot_split"] = split_name
        split_summaries.append(split_summary)

    summary = pd.concat(split_summaries, ignore_index=True)
    summary["plot_split"] = pd.Categorical(
        summary["plot_split"],
        categories=["train", "test", "combined"],
        ordered=True,
    )
    summary["churn_rate_pct"] = summary["churn_rate"] * 100
    return _plot_split_bar_columns(
        summary=summary,
        x_column=f"{numeric_column}_band",
        value_column="churn_rate_pct",
        title=title or f"Churn Rate By {numeric_column.replace('_', ' ').title()} Band",
        x_label=numeric_column.replace("_", " ").title(),
        palette="crest",
    )


def plot_numeric_feature_by_churn(
    df: pd.DataFrame,
    numeric_column: str,
    kind: str = "box",
    title: str | None = None,
) -> Axes:
    """Plot a numeric feature split by churn status with box or violin geometry."""
    plt.figure(figsize=(8, 4.8))
    if kind == "violin":
        ax = sns.violinplot(
            data=df,
            x=numeric_column,
            y="churn_label",
            hue="churn_label",
            palette=["#2ca02c", "#d62728"],
            legend=False,
            inner="quartile",
            orient="h",
        )
    else:
        ax = sns.boxplot(
            data=df,
            x=numeric_column,
            y="churn_label",
            hue="churn_label",
            palette=["#2ca02c", "#d62728"],
            legend=False,
            orient="h",
        )
    ax.set_title(title or f"{numeric_column.replace('_', ' ').title()} By Churn Status")
    ax.set_xlabel(numeric_column.replace("_", " ").title())
    ax.set_ylabel("Churn Status")
    return ax
