"""Run the customer churn data preparation workflow."""

from pathlib import Path
import sys


# Add the local `src/` directory so this script can import the project package
# without requiring an editable install.
PROJECT_ROOT = Path(__file__).resolve().parent.parent
SRC_PATH = PROJECT_ROOT / "src"
if str(SRC_PATH) not in sys.path:
    sys.path.insert(0, str(SRC_PATH))


from customer_churn_analysis.analysis import (
    build_data_quality_table,
    build_split_comparison_table,
    build_target_summary_table,
)
from customer_churn_analysis.data import prepare_and_persist_datasets


def main() -> None:
    # Step 1: read the raw CSV files, clean them, and save parquet versions.
    datasets = prepare_and_persist_datasets()

    # Step 2: build compact reporting tables that are useful in the console,
    # notebook, and written summary.
    quality_table = build_data_quality_table(datasets)
    split_comparison = build_split_comparison_table(datasets["combined"])
    target_summary = build_target_summary_table(datasets["combined"])

    # Step 3: print a lightweight execution summary so a user running the
    # script can immediately confirm what was created.
    print("Prepared parquet datasets:")
    for name, frame in datasets.items():
        print(f"  - {name}: {frame.shape}")

    print("\nData quality summary:")
    print(quality_table.to_string(index=False))

    print("\nSplit comparison summary:")
    print(split_comparison.to_string(index=False))

    print("\nChurn summary by split:")
    print(target_summary.to_string(index=False))


if __name__ == "__main__":
    # Run the full pipeline only when the file is executed as a script.
    main()
