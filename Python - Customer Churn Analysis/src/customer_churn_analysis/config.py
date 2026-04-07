"""Project configuration and canonical paths."""

from pathlib import Path


# Resolve all project paths from this file so the rest of the codebase can
# import a single source of truth for file locations.
PACKAGE_ROOT = Path(__file__).resolve().parent
PROJECT_ROOT = PACKAGE_ROOT.parent.parent
DATA_DIR = PROJECT_ROOT / "data"
NOTEBOOKS_DIR = PROJECT_ROOT / "notebooks"
OUTPUTS_DIR = PROJECT_ROOT / "outputs"
FIGURES_DIR = OUTPUTS_DIR / "figures"
TABLES_DIR = OUTPUTS_DIR / "tables"
REPORTS_DIR = PROJECT_ROOT / "reports"

RAW_TRAIN_PATH = DATA_DIR / "customer_churn_dataset-training-master.csv"
RAW_TEST_PATH = DATA_DIR / "customer_churn_dataset-testing-master.csv"

# These parquet files are the cleaned, analysis-ready equivalents of the raw
# CSV inputs.
PARQUET_TRAIN_PATH = DATA_DIR / "train.parquet"
PARQUET_TEST_PATH = DATA_DIR / "test.parquet"
PARQUET_COMBINED_PATH = DATA_DIR / "combined.parquet"

# Tables written here are small reporting artefacts that can be reused in the
# notebook, documentation, or presentation screenshots.
DATA_QUALITY_TABLE_PATH = TABLES_DIR / "data_quality_summary.csv"
SPLIT_COMPARISON_TABLE_PATH = TABLES_DIR / "split_comparison_summary.csv"
TARGET_SUMMARY_TABLE_PATH = TABLES_DIR / "churn_summary_by_split.csv"
