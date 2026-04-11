"""Bronze-layer ingestion for contract disclosure files.

This module is responsible for reading one raw source folder and turning it
into persisted Bronze assets:
1. The raw unioned contract row set.
2. A schema registry showing inferred column matches.
3. A file registry describing what was ingested.
"""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import pandas as pd

from src.ingest.schema_mapping import infer_column_schema, normalise_columns, schema_inference_to_frame
from src.utils.io_helpers import discover_input_files, filter_input_files, read_tabular_file


logger = logging.getLogger(__name__)


@dataclass(slots=True)
class BronzeIngestionResult:
    """Container for Bronze-layer ingestion outputs."""

    bronze_contracts: pd.DataFrame
    bronze_schema_registry: pd.DataFrame
    bronze_file_registry: pd.DataFrame


def ingest_raw_folder_to_bronze(raw_folder_path: Path, config: dict[str, Any]) -> BronzeIngestionResult:
    """Read every supported file in a raw-data folder and persist a Bronze-ready union.

    The function keeps source-file boundaries visible through metadata columns
    so later layers can always trace outputs back to original inputs.
    """

    # Discover supported files and then drop any filenames intentionally
    # excluded by runtime configuration.
    files = filter_input_files(
        discover_input_files(raw_folder_path),
        config.get("runtime", {}).get("excluded_file_patterns", []),
    )
    if not files:
        raise FileNotFoundError(f"No CSV/XLSX files found in raw folder: {raw_folder_path}")

    bronze_frames: list[pd.DataFrame] = []
    schema_registry_frames: list[pd.DataFrame] = []
    file_registry_rows: list[dict[str, Any]] = []
    sample_size = int(config.get("runtime", {}).get("schema_sample_rows", 25))
    critical_fields = set(config.get("critical_fields", []))

    for file_path in files:
        # Read the file once, then profile it before normalising headings so
        # the schema registry can preserve source-system naming context.
        raw_frame = read_tabular_file(file_path)
        original_columns = list(raw_frame.columns.astype(str))
        sampled_schema = infer_column_schema(raw_frame, config, sample_size=sample_size)
        schema_registry = schema_inference_to_frame(sampled_schema)
        schema_registry["source_file"] = file_path.name
        schema_registry["source_folder"] = raw_folder_path.name
        detected_fields = set(schema_registry["suggested_canonical_field"].dropna().tolist())
        missing_critical_fields = ", ".join(sorted(critical_fields - detected_fields))

        # Bronze stores a heading-normalised copy of the raw file to make
        # cross-file unions feasible without losing lineage.
        normalised_mapping = normalise_columns(original_columns)
        bronze_frame = raw_frame.rename(columns=normalised_mapping).copy()
        bronze_frame["bronze_row_id"] = [f"{file_path.stem}-{index:06d}" for index in range(len(bronze_frame))]
        bronze_frame["source_file"] = file_path.name
        bronze_frame["source_folder"] = raw_folder_path.name
        bronze_frame["source_path"] = str(file_path)
        bronze_frame["source_column_lineage"] = json.dumps(normalised_mapping, sort_keys=True)

        bronze_frames.append(bronze_frame)
        schema_registry_frames.append(schema_registry)
        # The file registry is the lightweight audit trail for what was read.
        file_registry_rows.append(
            {
                "source_folder": raw_folder_path.name,
                "source_file": file_path.name,
                "source_path": str(file_path),
                "row_count": len(raw_frame),
                "column_count": len(original_columns),
                "sampled_rows_for_schema": min(sample_size, len(raw_frame)),
                "detected_columns": ", ".join(original_columns),
                "normalised_columns": ", ".join(brown_col for brown_col in bronze_frame.columns.astype(str)),
                "missing_critical_fields": missing_critical_fields,
            }
        )

        logger.info(
            "Bronze Ingest Complete | File=%s | Rows=%s | Columns=%s",
            file_path.name,
            len(raw_frame),
            len(original_columns),
        )

    return BronzeIngestionResult(
        bronze_contracts=pd.concat(bronze_frames, ignore_index=True, sort=False),
        bronze_schema_registry=pd.concat(schema_registry_frames, ignore_index=True, sort=False),
        bronze_file_registry=pd.DataFrame(file_registry_rows),
    )
