"""Local filesystem storage for NEMWeb ingestion notebooks and tests."""

from __future__ import annotations

import csv
import os
from pathlib import Path
from typing import Any, Hashable

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
    """Append dictionaries to a CSV file, writing the header when needed."""

    if not rows:
        return
    normalised_rows = [
        {str(key): value for key, value in row.items()}
        for row in rows
    ]
    path.parent.mkdir(parents=True, exist_ok=True)
    write_header = not path.exists()
    with path.open("a", encoding="utf-8", newline="") as file:
        writer = csv.DictWriter(file, fieldnames=list(normalised_rows[0].keys()))
        if write_header:
            writer.writeheader()
        writer.writerows(normalised_rows)


def _windows_long_path(path: Path) -> Path:
    """Return a Windows extended-length path when needed."""

    resolved = path.resolve()
    if os.name != "nt" or len(str(resolved)) < 240:
        return resolved
    path_text = str(resolved)
    if path_text.startswith("\\\\?\\"):
        return resolved
    return Path(f"\\\\?\\{path_text}")
