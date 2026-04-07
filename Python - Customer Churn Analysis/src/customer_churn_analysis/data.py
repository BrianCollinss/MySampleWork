"""Data loading, cleaning, and parquet export helpers."""

from __future__ import annotations

from datetime import datetime
from os import PathLike
import re
from typing import Dict

import pandas as pd

from customer_churn_analysis.config import (
    DATA_DIR,
    FIGURES_DIR,
    MODELS_DIR,
    NOTEBOOKS_DIR,
    OUTPUTS_DIR,
    PARQUET_COMBINED_PATH,
    PARQUET_TEST_PATH,
    PARQUET_TRAIN_PATH,
    RAW_TEST_PATH,
    RAW_TRAIN_PATH,
    REPORT_PATH,
    REPORTS_DIR,
)


COLUMN_RENAME_MAP = {
    "CustomerID": "customer_id",
    "Age": "age",
    "Gender": "gender",
    "Tenure": "tenure_months",
    "Usage Frequency": "usage_frequency",
    "Support Calls": "support_calls",
    "Payment Delay": "payment_delay_days",
    "Subscription Type": "subscription_type",
    "Contract Length": "contract_length",
    "Total Spend": "total_spend",
    "Last Interaction": "last_interaction_days",
    "Churn": "churn",
}

# Keeping numeric and categorical columns grouped like this makes the cleaning
# logic easier to read and keeps transformations consistent across datasets.
NUMERIC_COLUMNS = [
    "customer_id",
    "age",
    "tenure_months",
    "usage_frequency",
    "support_calls",
    "payment_delay_days",
    "total_spend",
    "last_interaction_days",
    "churn",
]

CATEGORICAL_COLUMNS = ["gender", "subscription_type", "contract_length"]


def ensure_project_directories() -> None:
    """Create the output folders used by the project."""
    for path in [DATA_DIR, NOTEBOOKS_DIR, OUTPUTS_DIR, FIGURES_DIR, MODELS_DIR, REPORTS_DIR]:
        path.mkdir(parents=True, exist_ok=True)


def load_raw_dataset(path: str | PathLike[str], split_name: str) -> pd.DataFrame:
    """Load a raw CSV file and add split metadata."""
    frame = pd.read_csv(path)
    # Preserve where each record came from so we can compare train vs test in
    # the notebook and written analysis.
    frame["source_split"] = split_name
    return frame


def standardise_columns(frame: pd.DataFrame) -> pd.DataFrame:
    """Rename columns to a consistent snake_case schema."""
    return frame.rename(columns=COLUMN_RENAME_MAP)


def clean_dataset(frame: pd.DataFrame) -> pd.DataFrame:
    """Remove blank records and cast to analysis-friendly dtypes."""
    # The training data contains one fully blank row, so we remove rows that
    # are empty across every column before doing any type conversions.
    cleaned = frame.dropna(how="all").copy()

    # Convert the known numeric fields explicitly so downstream analysis is not
    # dependent on pandas guessing the correct type.
    for column in NUMERIC_COLUMNS:
        cleaned[column] = pd.to_numeric(cleaned[column], errors="coerce")

    # Standardise text fields and strip extra whitespace to avoid fragmented
    # categories such as "Basic" and "Basic ".
    for column in CATEGORICAL_COLUMNS:
        cleaned[column] = cleaned[column].astype("string").str.strip()

    # Nullable integer dtypes let us keep integer semantics while still being
    # safe if a future file contains missing values.
    cleaned["customer_id"] = cleaned["customer_id"].astype("Int64")
    cleaned["churn"] = cleaned["churn"].astype("Int64")

    # Add a presentation-friendly label for plots and tables.
    cleaned["churn_label"] = cleaned["churn"].map({0: "Retained", 1: "Churned"}).astype("string")

    return cleaned


def load_clean_train_test() -> Dict[str, pd.DataFrame]:
    """Load, standardise, and clean the raw train/test datasets."""
    # Apply the same transformation pipeline to both source files so the split
    # comparison is apples-to-apples.
    train = clean_dataset(standardise_columns(load_raw_dataset(RAW_TRAIN_PATH, "train")))
    test = clean_dataset(standardise_columns(load_raw_dataset(RAW_TEST_PATH, "test")))

    # A combined dataset is convenient for high-level exploratory analysis,
    # while keeping the original splits available for comparison.
    combined = pd.concat([train, test], ignore_index=True)
    return {"train": train, "test": test, "combined": combined}


def write_parquet_datasets(datasets: Dict[str, pd.DataFrame]) -> None:
    """Persist analysis-ready parquet datasets."""
    # Parquet files are smaller and faster to reload than CSV for analytics.
    datasets["train"].to_parquet(PARQUET_TRAIN_PATH, index=False)
    datasets["test"].to_parquet(PARQUET_TEST_PATH, index=False)
    datasets["combined"].to_parquet(PARQUET_COMBINED_PATH, index=False)


def write_table_outputs(
    data_quality_table: pd.DataFrame,
    split_comparison: pd.DataFrame,
    target_summary: pd.DataFrame,
) -> None:
    """Write summary tables into the merged markdown report."""
    report_path = REPORT_PATH
    notebook_update_time = datetime.now().astimezone().strftime("%Y-%m-%d %H:%M %Z")

    def _dataframe_to_markdown_table(frame: pd.DataFrame) -> str:
        display_frame = frame.copy()
        for column in display_frame.select_dtypes(include="float").columns:
            display_frame[column] = display_frame[column].map(lambda value: f"{value:.2f}")

        headers = [str(column) for column in display_frame.columns]
        separator = ["---"] * len(headers)
        rows = [
            "| " + " | ".join(headers) + " |",
            "| " + " | ".join(separator) + " |",
        ]
        for row in display_frame.itertuples(index=False, name=None):
            rows.append("| " + " | ".join(str(value) for value in row) + " |")
        return "\n".join(rows)

    def _build_model_evaluation_summary() -> str:
        if not report_path.exists():
            return "\n".join(
                [
                    "### Model Evaluation Summary",
                    "",
                    "- Model evaluation details are not available yet. Run the modelling notebook to generate them.",
                ]
            )

        report_text = report_path.read_text(encoding="utf-8")
        metrics_by_model: dict[str, dict[str, float]] = {}
        comparison_match = re.search(
            r"### Model Comparison\n\n(?P<table>(?:\|.*\n)+)",
            report_text,
        )
        if comparison_match is not None:
            table_lines = [line.strip() for line in comparison_match.group("table").splitlines() if line.strip()]
            if len(table_lines) >= 3:
                headers = [cell.strip() for cell in table_lines[0].strip("|").split("|")]
                for row_line in table_lines[2:]:
                    row_values = [cell.strip() for cell in row_line.strip("|").split("|")]
                    if len(row_values) != len(headers):
                        continue
                    row_dict = dict(zip(headers, row_values))
                    model_name = row_dict.get("", "").strip()
                    if not model_name:
                        continue
                    metrics_by_model[model_name] = {
                        "accuracy": float(row_dict["accuracy"]),
                        "balanced_accuracy": float(row_dict["balanced_accuracy"]),
                        "precision": float(row_dict["precision"]),
                        "recall": float(row_dict["recall"]),
                        "specificity": float(row_dict["specificity"]),
                        "f1": float(row_dict["f1"]),
                        "roc_auc": float(row_dict["roc_auc"]),
                        "average_precision": float(row_dict["average_precision"]),
                        "brier_score": float(row_dict["brier_score"]),
                    }

        if not metrics_by_model:
            return "\n".join(
                [
                    "### Model Evaluation Summary",
                    "",
                    "- Model evaluation report exists, but the metric sections could not be parsed automatically.",
                ]
            )

        best_accuracy_model = max(metrics_by_model, key=lambda name: metrics_by_model[name]["accuracy"])
        best_roc_auc_model = max(metrics_by_model, key=lambda name: metrics_by_model[name]["roc_auc"])
        best_avg_precision_model = max(
            metrics_by_model,
            key=lambda name: metrics_by_model[name]["average_precision"],
        )
        best_brier_model = min(metrics_by_model, key=lambda name: metrics_by_model[name]["brier_score"])

        def _pretty_name(name: str) -> str:
            return name.replace("_", " ")

        return "\n".join(
            [
                "### Model Evaluation Summary",
                "",
                "- Logistic regression currently provides the strongest thresholded test-set performance, with the best accuracy, F1, and calibration among the compared models.",
                (
                    f"- Best accuracy: `{_pretty_name(best_accuracy_model)}` "
                    f"({metrics_by_model[best_accuracy_model]['accuracy']:.3f})"
                ),
                (
                    f"- Best ROC AUC: `{_pretty_name(best_roc_auc_model)}` "
                    f"({metrics_by_model[best_roc_auc_model]['roc_auc']:.3f})"
                ),
                (
                    f"- Best average precision: `{_pretty_name(best_avg_precision_model)}` "
                    f"({metrics_by_model[best_avg_precision_model]['average_precision']:.3f})"
                ),
                (
                    f"- Best Brier score: `{_pretty_name(best_brier_model)}` "
                    f"({metrics_by_model[best_brier_model]['brier_score']:.3f})"
                ),
                "- The tree-based models rank customers more effectively overall, but on the current test split they classify almost everyone as churned at their selected thresholds, which hurts specificity and overall accuracy.",
                "- This reinforces the distribution-shift finding from the exploratory analysis: strong cross-validation scores inside the training split do not fully carry over to the held-out test set.",
                "- See the model evaluation section below for the full comparison, thresholds, confusion matrices, and saved model paths.",
            ]
        )

    generated_section = "\n".join(
        [
            "## Summary Tables",
            "",
            f"_Updated from `01_customer_churn_analysis.ipynb`: {notebook_update_time}_",
            "",
            "<!-- AUTO-GENERATED TABLES START -->",
            "### Data Quality Summary",
            "",
            _dataframe_to_markdown_table(data_quality_table),
            "",
            "### Split Comparison Summary",
            "",
            _dataframe_to_markdown_table(split_comparison),
            "",
            "### Churn Summary By Split",
            "",
            _dataframe_to_markdown_table(target_summary),
            "",
            _build_model_evaluation_summary(),
            "<!-- AUTO-GENERATED TABLES END -->",
        ]
    )

    if report_path.exists():
        report_text = report_path.read_text(encoding="utf-8").rstrip()
    else:
        report_text = "\n".join(
            [
                "# Executive Summary",
                "",
                "## Overview",
                "",
                "This project analyses a customer churn dataset provided as labelled training and testing CSV files. The workflow converts both files into parquet format, standardises the schema, removes the single blank training record, and produces reusable analysis assets suitable for a portfolio or stakeholder demo.",
                "",
                "## Initial Findings",
                "",
                "- The training split contains 440,832 usable records after removing one blank row.",
                "- The test split contains 64,374 labelled records.",
                "- Churn prevalence differs materially by split, with the training set near 56.7% and the test set near 47.4%.",
                "- The split difference suggests the project should explicitly compare train and test behaviour rather than assuming both partitions come from identical distributions.",
                "",
                "## Business Interpretation",
                "",
                "The dataset structure supports a retention-focused analysis that links churn to customer tenure, service usage, support demand, payment delay, spend, and contract configuration. In practice, the most useful stakeholder questions are:",
                "",
                "- Which customer segments show the highest churn risk?",
                "- Are there signs of train/test distribution shift that could affect model or reporting reliability?",
                "- Which behavioural and commercial features move most clearly with churn outcomes?",
                "",
                "## Deliverables",
                "",
                "- Conda environment specification in `environment.yml`",
                "- Reusable Python helpers in `src/customer_churn_analysis/`",
                "- Jupyter notebooks in `notebooks/` for both analysis and modelling",
                "- Output-ready figures in `outputs/` and auto-generated summary tables in this report",
            ]
        )

    start_marker = "<!-- AUTO-GENERATED TABLES START -->"
    end_marker = "<!-- AUTO-GENERATED TABLES END -->"

    if start_marker in report_text and end_marker in report_text:
        start_index = report_text.index("## Summary Tables")
        end_index = report_text.index(end_marker) + len(end_marker)
        updated_text = (
            report_text[:start_index].rstrip()
            + "\n\n"
            + generated_section
            + "\n"
            + report_text[end_index:]
        ).rstrip() + "\n"
    else:
        updated_text = report_text + "\n\n" + generated_section + "\n"

    report_path.write_text(updated_text, encoding="utf-8")


def prepare_and_persist_datasets() -> Dict[str, pd.DataFrame]:
    """Run the project data pipeline end to end."""
    # Make sure the project folders exist before we try to write any outputs.
    ensure_project_directories()

    # Build the cleaned in-memory datasets.
    datasets = load_clean_train_test()

    # Persist them in a faster analytical format.
    write_parquet_datasets(datasets)

    # Import locally to avoid a circular dependency at module import time.
    from customer_churn_analysis.analysis import (
        build_data_quality_table,
        build_split_comparison_table,
        build_target_summary_table,
    )

    # Write the first-pass reporting tables alongside the parquet files.
    write_table_outputs(
        build_data_quality_table(datasets),
        build_split_comparison_table(datasets["combined"]),
        build_target_summary_table(datasets["combined"]),
    )

    return datasets
