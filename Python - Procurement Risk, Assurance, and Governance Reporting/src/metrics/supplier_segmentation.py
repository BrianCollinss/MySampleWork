"""Supplier risk segmentation and clustering outputs.

This module builds supplier-level analytical features and groups suppliers into
interpretable segments that support governance and assurance reporting.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

import numpy as np
import pandas as pd


@dataclass(slots=True)
class SupplierSegmentationResult:
    """Supplier-level segmentation tables used by Gold outputs and reporting."""

    supplier_segments: pd.DataFrame
    cluster_summary: pd.DataFrame


def _standardize_columns(feature_frame: pd.DataFrame) -> pd.DataFrame:
    """Z-score feature columns while handling constant-value edge cases."""

    standardised = feature_frame.copy().astype(float)
    for column in standardised.columns:
        mean_value = standardised[column].mean()
        std_value = standardised[column].std(ddof=0)
        if std_value == 0 or pd.isna(std_value):
            standardised[column] = 0.0
        else:
            standardised[column] = (standardised[column] - mean_value) / std_value
    return standardised


def _run_kmeans(features: pd.DataFrame, n_clusters: int, max_iter: int, random_state: int) -> np.ndarray:
    """Run a lightweight K-means implementation using numpy only.

    This avoids a hard dependency on scikit-learn while still providing a
    stable clustering capability for the portfolio project.
    """

    values = features.to_numpy(dtype=float)
    if len(values) == 0:
        return np.array([], dtype=int)

    cluster_count = max(1, min(n_clusters, len(values)))
    rng = np.random.default_rng(random_state)

    if cluster_count == 1:
        return np.zeros(len(values), dtype=int)

    initial_indices = rng.choice(len(values), size=cluster_count, replace=False)
    centroids = values[initial_indices].copy()
    labels = np.zeros(len(values), dtype=int)

    for _ in range(max_iter):
        distances = np.sqrt(((values[:, None, :] - centroids[None, :, :]) ** 2).sum(axis=2))
        new_labels = distances.argmin(axis=1)

        if np.array_equal(new_labels, labels):
            break
        labels = new_labels

        for cluster_id in range(cluster_count):
            cluster_points = values[labels == cluster_id]
            if len(cluster_points) == 0:
                centroids[cluster_id] = values[rng.integers(0, len(values))]
            else:
                centroids[cluster_id] = cluster_points.mean(axis=0)

    return labels


def _label_clusters(cluster_summary: pd.DataFrame) -> pd.DataFrame:
    """Assign business-readable labels to each discovered supplier cluster."""

    summary = cluster_summary.copy()

    spend_threshold = summary["mean_total_contract_value"].median()
    count_threshold = summary["mean_contract_count"].median()
    average_value_threshold = summary["mean_average_contract_value"].median()
    repeat_threshold = summary["mean_repeated_award_frequency"].median()
    concentration_threshold = summary["mean_top_period_spend_share"].median()
    data_quality_threshold = summary["mean_data_quality_issue_rate"].median()
    missing_data_threshold = summary["mean_missing_data_rate"].median()

    labels: list[str] = []
    descriptions: list[str] = []
    for _, row in summary.iterrows():
        if (
            row["mean_total_contract_value"] >= spend_threshold
            and row["mean_contract_count"] >= count_threshold
        ):
            labels.append("high-value strategic")
            descriptions.append("High-spend suppliers with sustained activity across the reporting period.")
        elif (
            row["mean_average_contract_value"] <= average_value_threshold
            and row["mean_repeated_award_frequency"] >= repeat_threshold
            and row["mean_contract_count"] >= count_threshold
        ):
            labels.append("fragmented low-value repeat")
            descriptions.append("Frequent lower-value awards that may warrant fragmentation review.")
        elif (
            row["mean_contract_count"] <= count_threshold
            and (
                row["mean_top_period_spend_share"] >= concentration_threshold
                or row["mean_data_quality_issue_rate"] >= data_quality_threshold
                or row["mean_average_contract_value"] >= average_value_threshold
            )
        ):
            labels.append("sporadic high-risk")
            descriptions.append("Less frequent suppliers with concentrated, high-value, or higher-risk patterns.")
        elif (
            row["mean_missing_data_rate"] >= missing_data_threshold
            and row["mean_data_quality_issue_rate"] >= data_quality_threshold
        ):
            labels.append("low-information vendor")
            descriptions.append("Suppliers where weak data quality limits assurance confidence.")
        else:
            labels.append("mixed profile")
            descriptions.append("Suppliers with blended operational characteristics and moderate risk signals.")

    summary["segment_label"] = labels
    summary["segment_description"] = descriptions
    return summary


def build_supplier_segmentation(contracts: pd.DataFrame, config: dict[str, Any]) -> SupplierSegmentationResult:
    """Build supplier-level features and cluster them into risk segments."""

    segmentation_config = config.get("advanced_analytics", {}).get("supplier_segmentation", {})
    if not segmentation_config.get("enabled", True):
        empty_supplier_segments = pd.DataFrame()
        empty_cluster_summary = pd.DataFrame()
        return SupplierSegmentationResult(
            supplier_segments=empty_supplier_segments,
            cluster_summary=empty_cluster_summary,
        )

    segmentation_frame = contracts.copy()
    segmentation_frame["supplier_name"] = segmentation_frame["supplier_name"].fillna("Unknown Supplier").astype("string").str.strip()
    segmentation_frame["reporting_period"] = segmentation_frame["reporting_period"].fillna("Unknown")
    segmentation_frame["procurement_method"] = segmentation_frame["procurement_method"].fillna("Unknown").astype("string")
    segmentation_frame["data_quality_issue_count"] = segmentation_frame["data_quality_flags"].apply(lambda flags: len(flags) if isinstance(flags, list) else 0)

    # Supplier activity, value, and timing features anchor the segmentation in
    # practical governance questions rather than abstract ML dimensions.
    supplier_segments = (
        segmentation_frame.groupby("supplier_name", dropna=False)
        .agg(
            total_contract_value=("contract_value", "sum"),
            contract_count=("contract_id", "count"),
            average_contract_value=("contract_value", "mean"),
            median_contract_value=("contract_value", "median"),
            distinct_reporting_periods=("reporting_period", lambda values: values.astype("string").nunique()),
            procurement_method_diversity=("procurement_method", lambda values: values.astype("string").nunique()),
            data_quality_issue_rate=("data_quality_issue_count", lambda values: (values > 0).mean()),
            missing_data_rate=(
                "contract_id",
                lambda index_values: segmentation_frame.loc[index_values.index, [
                    "supplier_name",
                    "contract_value",
                    "contract_start_date",
                    "contract_end_date",
                    "procurement_method",
                    "procurement_category",
                ]].isna().mean().mean(),
            ),
        )
        .reset_index()
    )

    monthly_supplier_spend = (
        segmentation_frame.groupby(["supplier_name", "reporting_period"], dropna=False)["contract_value"]
        .sum()
        .reset_index()
    )
    supplier_total_spend = supplier_segments.set_index("supplier_name")["total_contract_value"]
    monthly_supplier_spend["supplier_total_spend"] = monthly_supplier_spend["supplier_name"].map(supplier_total_spend).fillna(0.0)
    monthly_supplier_spend["period_share_of_supplier_spend"] = np.where(
        monthly_supplier_spend["supplier_total_spend"] > 0,
        monthly_supplier_spend["contract_value"] / monthly_supplier_spend["supplier_total_spend"],
        0.0,
    )
    top_period_share = (
        monthly_supplier_spend.groupby("supplier_name")["period_share_of_supplier_spend"]
        .max()
        .rename("top_period_spend_share")
    )

    limited_keywords = ("limited", "direct", "sole", "selective")
    limited_method_rate = (
        segmentation_frame.groupby("supplier_name")["procurement_method"]
        .apply(lambda values: values.astype("string").str.lower().str.contains("|".join(limited_keywords), regex=True).mean())
        .rename("limited_method_rate")
    )

    supplier_segments = supplier_segments.merge(top_period_share, on="supplier_name", how="left")
    supplier_segments = supplier_segments.merge(limited_method_rate, on="supplier_name", how="left")
    supplier_segments["repeated_award_frequency"] = (
        supplier_segments["contract_count"] / supplier_segments["distinct_reporting_periods"].clip(lower=1)
    )
    supplier_segments["concentration_exposure"] = supplier_segments["top_period_spend_share"].fillna(0.0)
    supplier_segments = supplier_segments.fillna(0.0)

    cluster_features = supplier_segments[
        [
            "total_contract_value",
            "contract_count",
            "average_contract_value",
            "repeated_award_frequency",
            "concentration_exposure",
            "procurement_method_diversity",
            "limited_method_rate",
            "data_quality_issue_rate",
            "missing_data_rate",
        ]
    ].copy()
    cluster_features["total_contract_value"] = np.log1p(cluster_features["total_contract_value"])
    cluster_features["average_contract_value"] = np.log1p(cluster_features["average_contract_value"].clip(lower=0))

    standardised_features = _standardize_columns(cluster_features)
    cluster_labels = _run_kmeans(
        standardised_features,
        n_clusters=int(segmentation_config.get("cluster_count", 4)),
        max_iter=int(segmentation_config.get("max_iter", 100)),
        random_state=int(segmentation_config.get("random_state", 42)),
    )

    supplier_segments["cluster_id"] = cluster_labels

    cluster_summary = (
        supplier_segments.groupby("cluster_id")
        .agg(
            supplier_count=("supplier_name", "count"),
            mean_total_contract_value=("total_contract_value", "mean"),
            mean_contract_count=("contract_count", "mean"),
            mean_average_contract_value=("average_contract_value", "mean"),
            mean_repeated_award_frequency=("repeated_award_frequency", "mean"),
            mean_top_period_spend_share=("top_period_spend_share", "mean"),
            mean_procurement_method_diversity=("procurement_method_diversity", "mean"),
            mean_limited_method_rate=("limited_method_rate", "mean"),
            mean_data_quality_issue_rate=("data_quality_issue_rate", "mean"),
            mean_missing_data_rate=("missing_data_rate", "mean"),
            cluster_total_contract_value=("total_contract_value", "sum"),
        )
        .reset_index()
        .sort_values("cluster_total_contract_value", ascending=False)
    )
    cluster_summary = _label_clusters(cluster_summary)
    supplier_segments = supplier_segments.merge(
        cluster_summary[["cluster_id", "segment_label", "segment_description"]],
        on="cluster_id",
        how="left",
    )

    supplier_segments = supplier_segments.sort_values(
        ["cluster_id", "total_contract_value"],
        ascending=[True, False],
    )

    ordered_supplier_columns = [
        "supplier_name",
        "cluster_id",
        "segment_label",
        "segment_description",
        "total_contract_value",
        "contract_count",
        "average_contract_value",
        "median_contract_value",
        "distinct_reporting_periods",
        "repeated_award_frequency",
        "top_period_spend_share",
        "concentration_exposure",
        "procurement_method_diversity",
        "limited_method_rate",
        "data_quality_issue_rate",
        "missing_data_rate",
    ]

    ordered_cluster_columns = [
        "cluster_id",
        "segment_label",
        "segment_description",
        "supplier_count",
        "cluster_total_contract_value",
        "mean_total_contract_value",
        "mean_contract_count",
        "mean_average_contract_value",
        "mean_repeated_award_frequency",
        "mean_top_period_spend_share",
        "mean_procurement_method_diversity",
        "mean_limited_method_rate",
        "mean_data_quality_issue_rate",
        "mean_missing_data_rate",
    ]

    return SupplierSegmentationResult(
        supplier_segments=supplier_segments[ordered_supplier_columns],
        cluster_summary=cluster_summary[ordered_cluster_columns],
    )
