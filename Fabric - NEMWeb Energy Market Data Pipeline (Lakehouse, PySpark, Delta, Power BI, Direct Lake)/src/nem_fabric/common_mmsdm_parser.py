"""Parser for AEMO MMSDM-style CSV files inside NEMWeb ZIP archives.

AEMO MMSDM files are not plain single-table CSVs. They use row prefixes:
`C` for control/comment rows, `I` for table headers, and `D` for data rows.
This parser preserves source metadata and supports multiple table groups in one
CSV so Bronze tables can retain the original source detail.
"""

from __future__ import annotations

import csv
import hashlib
import io
import zipfile
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import PurePosixPath
from urllib.parse import urlparse

import pandas as pd

from nem_fabric.common_nemweb_client import (
    extract_filename,
    extract_timestamp_from_filename,
)


@dataclass
class ParsedMmsdmTable:
    """Parsed MMSDM table group returned as a local pandas DataFrame."""

    package_name: str | None
    table_name: str | None
    dataframe: pd.DataFrame


def _decode_csv_bytes(csv_bytes: bytes) -> str:
    """Decode CSV bytes using common encodings seen in public data files."""

    for encoding in ("utf-8-sig", "utf-8", "cp1252", "latin-1"):
        try:
            return csv_bytes.decode(encoding)
        except UnicodeDecodeError:
            continue
    return csv_bytes.decode("latin-1", errors="replace")


def _dedupe_headers(headers: list[str]) -> list[str]:
    """Create safe, lower-case, unique column names from an MMSDM header row."""

    seen: dict[str, int] = {}
    result: list[str] = []
    for header in headers:
        clean = (header or "unnamed").strip().lower()
        count = seen.get(clean, 0)
        seen[clean] = count + 1
        result.append(clean if count == 0 else f"{clean}_{count + 1}")
    return result


def _row_hash(values: list[str], metadata: dict[str, str]) -> str:
    """Create a stable row hash using row content and source identity."""

    payload = "|".join(
        values + [metadata.get("source_url", ""), metadata.get("inner_csv_name", "")]
    )
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


def parse_mmsdm_csv_bytes(
    csv_bytes: bytes,
    source_url: str,
    inner_filename: str,
) -> list[ParsedMmsdmTable]:
    """Parse one MMSDM CSV into table-group DataFrames.

    MMSDM files use `I` rows as schema declarations and following `D` rows as
    records for that schema. Files can contain several table groups.
    """

    # Empty files are not exceptional in an ingestion pipeline; returning no
    # tables lets the caller audit the file without failing the full run.
    if not csv_bytes:
        return []

    text = _decode_csv_bytes(csv_bytes)
    if not text.strip():
        return []

    source_zip_name = extract_filename(source_url)
    file_datetime = extract_timestamp_from_filename(source_zip_name)
    metadata = {
        "source_url": source_url,
        "source_zip_name": source_zip_name,
        "inner_csv_name": inner_filename,
        "source_folder": PurePosixPath(urlparse(source_url).path).parent.name,
        "ingestion_datetime": datetime.now(timezone.utc).isoformat(),
        "file_datetime": file_datetime.isoformat() if file_datetime else "",
    }

    # Group records by package, table, and header shape. Header shape is included
    # because AEMO can emit the same logical table with changed columns over time.
    groups: dict[
        tuple[str | None, str | None, tuple[str, ...]], list[dict[str, str]]
    ] = {}
    current_headers: list[str] | None = None
    current_package: str | None = None
    current_table: str | None = None

    reader = csv.reader(io.StringIO(text))
    for raw_row in reader:
        if not raw_row:
            continue
        row_type = raw_row[0].strip().upper()
        if row_type == "I":
            # In MMSDM, the I row carries table identity and then column names.
            # The first four fields are structural metadata.
            current_package = raw_row[1].strip() if len(raw_row) > 1 else None
            current_table = raw_row[2].strip() if len(raw_row) > 2 else None
            current_headers = _dedupe_headers(
                raw_row[4:] if len(raw_row) > 4 else raw_row[1:]
            )
        elif row_type == "D" and current_headers is not None:
            # D rows follow the most recent I row. Preserve trailing unexpected
            # columns instead of dropping them so schema drift remains visible.
            values = raw_row[4:] if len(raw_row) > 4 else raw_row[1:]
            if len(values) < len(current_headers):
                values = values + [""] * (len(current_headers) - len(values))
            if len(values) > len(current_headers):
                extra_count = len(values) - len(current_headers)
                headers = current_headers + [
                    f"extra_column_{index + 1}" for index in range(extra_count)
                ]
            else:
                headers = current_headers
            record = dict(zip(headers, values, strict=False))
            # Add source lineage to every Bronze row. This is essential for
            # replay, auditing, duplicate detection, and quarantine diagnosis.
            record.update(metadata)
            record["package_name"] = current_package or ""
            record["table_name"] = current_table or ""
            record["row_hash"] = _row_hash(raw_row, metadata)
            groups.setdefault(
                (current_package, current_table, tuple(headers)), []
            ).append(record)

    parsed: list[ParsedMmsdmTable] = []
    for (package_name, table_name, _headers), rows in groups.items():
        parsed.append(ParsedMmsdmTable(package_name, table_name, pd.DataFrame(rows)))
    return parsed


def parse_zip_bytes(zip_bytes: bytes, source_url: str) -> list[ParsedMmsdmTable]:
    """Parse all CSV files inside a NEMWeb ZIP archive.

    Non-CSV files are ignored because some ZIPs may contain readme or metadata
    artefacts that are not part of MMSDM data tables.
    """

    if not zip_bytes:
        return []

    parsed: list[ParsedMmsdmTable] = []
    with zipfile.ZipFile(io.BytesIO(zip_bytes)) as archive:
        for member in archive.namelist():
            if not member.lower().endswith(".csv"):
                continue
            with archive.open(member) as file:
                parsed.extend(parse_mmsdm_csv_bytes(file.read(), source_url, member))
    return parsed
