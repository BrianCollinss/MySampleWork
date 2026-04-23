"""Bronze-layer build logic for minimally transformed raw source tables."""

from __future__ import annotations

from src.config import CONFIG
from src.io_files import write_dataset
from src.raw_ingestion import load_raw_tables


def build_bronze() -> dict[str, object]:
    """Load raw source files and persist Bronze copies with ingestion metadata."""

    # Read the raw CSV extracts into memory with source tracking columns attached.
    tables = load_raw_tables()

    # Persist each raw table into the Bronze layer as both Parquet and CSV.
    for name, df in tables.items():
        write_dataset(df, CONFIG.bronze_dir, f"bronze_{name}")

    return tables
