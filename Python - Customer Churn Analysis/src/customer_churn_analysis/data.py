"""Data loading, cleaning, and parquet export helpers."""

from __future__ import annotations

from os import PathLike
from typing import Dict

import pandas as pd

from customer_churn_analysis.config import (
    DATA_DIR,
    FIGURES_DIR,
    NOTEBOOKS_DIR,
    OUTPUTS_DIR,
    PARQUET_COMBINED_PATH,
    PARQUET_TEST_PATH,
    PARQUET_TRAIN_PATH,
    RAW_TEST_PATH,
    RAW_TRAIN_PATH,
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
    for path in [DATA_DIR, NOTEBOOKS_DIR, OUTPUTS_DIR, FIGURES_DIR, REPORTS_DIR]:
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
    """Write summary tables into the executive summary markdown report."""
    report_path = REPORTS_DIR / "executive_summary.md"

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

    generated_section = "\n".join(
        [
            "## Summary Tables",
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
            "<!-- AUTO-GENERATED TABLES END -->",
        ]
    )

    if report_path.exists():
        report_text = report_path.read_text(encoding="utf-8").rstrip()
    else:
        report_text = "# Executive Summary"

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
