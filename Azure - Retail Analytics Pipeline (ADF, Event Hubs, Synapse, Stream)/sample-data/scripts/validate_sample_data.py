"""Validate that required sample CSV files exist and contain data rows."""

import csv
from pathlib import Path


RAW_DIR = Path(__file__).resolve().parents[1] / "raw"
REQUIRED_FILES = [
    "customers.csv",
    "products.csv",
    "orders.csv",
    "order_items.csv",
    "campaigns.csv",
]


def main() -> None:
    """Check each required CSV file and report its data-row count."""
    for file_name in REQUIRED_FILES:
        target = RAW_DIR / file_name
        if not target.exists():
            raise FileNotFoundError(f"Missing required sample file: {target}")
        with target.open("r", encoding="utf-8") as handle:
            reader = csv.reader(handle)
            rows = list(reader)
            if len(rows) < 2:
                raise ValueError(f"{file_name} does not contain data rows.")
        print(f"Validated {file_name}: {len(rows) - 1} rows")


if __name__ == "__main__":
    main()
