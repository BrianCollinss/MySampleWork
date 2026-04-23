"""Raw data ingestion utilities for the CRM source extracts."""

from __future__ import annotations

from datetime import UTC, datetime

import pandas as pd

from src.config import CONFIG


def load_raw_tables() -> dict[str, pd.DataFrame]:
    """Load the source CSV files and attach ingestion metadata columns."""

    # Stamp all ingested tables with one consistent pipeline execution timestamp.
    ingestion_ts = datetime.now(UTC).isoformat()
    tables: dict[str, pd.DataFrame] = {}

    # Read each known source table from the raw landing zone.
    for name in ["accounts", "products", "sales_pipeline", "sales_teams"]:
        df = pd.read_csv(CONFIG.raw_dir / f"{name}.csv")
        df["source_file"] = f"{name}.csv"
        df["ingested_at_utc"] = ingestion_ts
        tables[name] = df

    return tables
