"""Local filesystem storage for NEMWeb ingestion notebooks and tests."""

from __future__ import annotations

import csv
import os
import re
from pathlib import Path
from typing import Any, Hashable

import pandas as pd

from nem_fabric.common_ingestion import INGESTION_LOG_TABLE, MANIFEST_TABLE


class LocalCsvIngestionStore:
    """Local implementation using ZIP files and CSV control tables."""

    def __init__(self, root: str | Path = "data") -> None:
        self.root = Path(root)

    @property
    def manifest_path(self) -> Path:
        """Local CSV manifest path."""

        return self.root / "tables" / f"{MANIFEST_TABLE}.csv"

    @property
    def log_path(self) -> Path:
        """Local CSV ingestion log path."""

        return self.root / "tables" / f"{INGESTION_LOG_TABLE}.csv"

    def read_existing_manifest_urls(self) -> set[str]:
        """Read successfully processed ZIP URLs from the local CSV manifest."""

        if not self.manifest_path.exists():
            return set()
        with self.manifest_path.open("r", encoding="utf-8", newline="") as file:
            return {
                row["source_url"]
                for row in csv.DictReader(file)
                if row.get("source_url")
                and row.get("status") in {"downloaded", "dry_run"}
            }

    def write_binary(self, relative_path: str, content: bytes) -> None:
        """Write ZIP bytes below the local landing root."""

        target = self.root / relative_path
        writable_target = _windows_long_path(target)
        writable_target.parent.mkdir(parents=True, exist_ok=True)
        writable_target.write_bytes(content)

    def append_control_rows(
        self,
        manifest_rows: list[dict[str, Any]],
        log_rows: list[dict[str, Any]],
    ) -> None:
        """Append local CSV control rows."""

        append_csv_rows(self.manifest_path, manifest_rows)
        append_csv_rows(self.log_path, log_rows)


def append_csv_rows(
    path: Path,
    rows: list[dict[str, Any]] | list[dict[Hashable, Any]],
) -> None:
    """Append dictionaries to a CSV file, expanding headers for schema drift."""

    if not rows:
        return
    normalised_rows = [
        {str(key): value for key, value in row.items()}
        for row in rows
    ]
    path.parent.mkdir(parents=True, exist_ok=True)
    write_header = not path.exists() or path.stat().st_size == 0
    fieldnames = list(normalised_rows[0].keys())
    if path.exists() and path.stat().st_size > 0:
        fieldnames = _merge_existing_csv_header(path, normalised_rows)
    with path.open("a", encoding="utf-8", newline="") as file:
        writer = csv.DictWriter(file, fieldnames=fieldnames, extrasaction="ignore")
        if write_header:
            writer.writeheader()
        writer.writerows(normalised_rows)


def _merge_existing_csv_header(
    path: Path,
    rows: list[dict[str, Any]],
) -> list[str]:
    """Return a stable CSV header, rewriting existing rows when new columns arrive."""

    with path.open("r", encoding="utf-8", newline="") as file:
        reader = csv.DictReader(file)
        existing_fieldnames = list(reader.fieldnames or [])
        existing_rows = list(reader)

    new_columns = [
        column
        for row in rows
        for column in row
        if column not in existing_fieldnames
    ]
    if not new_columns:
        return existing_fieldnames

    fieldnames = existing_fieldnames + list(dict.fromkeys(new_columns))
    with path.open("w", encoding="utf-8", newline="") as file:
        writer = csv.DictWriter(file, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(existing_rows)
    return fieldnames


def append_parquet_rows(
    path: Path,
    rows: list[dict[str, Any]] | list[dict[Hashable, Any]],
) -> None:
    """Append dictionaries to a local Parquet table file, preserving data types."""

    if not rows:
        return
    parquet_path = _windows_long_path(path)
    parquet_path.parent.mkdir(parents=True, exist_ok=True)
    new_df = pd.DataFrame(rows)
    if parquet_path.exists() and parquet_path.stat().st_size > 0:
        existing_df = pd.read_parquet(parquet_path)
        new_df = pd.concat([existing_df, new_df], ignore_index=True, sort=False)
    new_df.to_parquet(parquet_path, index=False)


def write_parquet_part(path: Path, df: pd.DataFrame) -> None:
    """Write one deterministic Parquet part file."""

    parquet_part_path = _windows_long_path(path)
    parquet_part_path.parent.mkdir(parents=True, exist_ok=True)
    df.to_parquet(parquet_part_path, index=False)


def safe_parquet_part_name(*parts: str) -> str:
    """Build a filesystem-safe deterministic Parquet part filename."""

    cleaned_parts = [
        re.sub(r"[^A-Za-z0-9._-]+", "_", _shorten_source_stem(str(part))).strip("._-")
        for part in parts
        if str(part).strip()
    ]
    return "__".join(cleaned_parts) + ".parquet"


def _shorten_source_stem(value: str) -> str:
    """Drop verbose public source prefixes from deterministic part names."""

    shortened = value
    for prefix in [
        "PUBLIC_DISPATCHIS_",
        "PUBLIC_PREDISPATCHIS_",
        "PUBLIC_TRADING_CUMULATIVE_PRICE_",
        "PUBLIC_SEVENDAYOUTLOOK_FULL_",
        "PUBLIC_DISPATCHSCADA_",
        "PUBLIC_ROOFTOP_PV_ACTUAL_MEASUREMENT_",
        "PUBLIC_ROOFTOP_PV_ACTUAL_SATELLITE_",
    ]:
        if shortened.upper().startswith(prefix):
            return shortened[len(prefix) :]
    return shortened


def read_parquet_table(path: Path) -> pd.DataFrame:
    """Read a local Parquet table file or folder."""

    parquet_path = _windows_long_path(path)
    if not parquet_path.exists():
        return pd.DataFrame()
    if parquet_path.is_dir():
        parquet_part_paths = sorted(parquet_path.glob("*.parquet"))
        if not parquet_part_paths:
            return pd.DataFrame()
        return pd.concat(
            [pd.read_parquet(part_path) for part_path in parquet_part_paths],
            ignore_index=True,
            sort=False,
        )
    return pd.read_parquet(parquet_path)


def write_parquet_table(path: Path, df: pd.DataFrame) -> None:
    """Overwrite a local Parquet table file."""

    parquet_path = _windows_long_path(path)
    parquet_path.parent.mkdir(parents=True, exist_ok=True)
    df.to_parquet(parquet_path, index=False)


def _windows_long_path(path: Path) -> Path:
    """Return a Windows extended-length path when needed."""

    resolved = path.resolve()
    if os.name != "nt" or len(str(resolved)) < 240:
        return resolved
    path_text = str(resolved)
    if path_text.startswith("\\\\?\\"):
        return resolved
    return Path(f"\\\\?\\{path_text}")
