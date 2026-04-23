"""Shared file input and output helpers for the project."""

from __future__ import annotations

import json
from pathlib import Path

import pandas as pd


def ensure_dir(path: Path) -> None:
    """Create a directory path if it does not already exist."""

    # Make directory creation idempotent for repeated pipeline runs.
    path.mkdir(parents=True, exist_ok=True)


def write_dataset(df: pd.DataFrame, output_dir: Path, name: str) -> None:
    """Write a dataset to both Parquet and CSV in the target directory."""

    # Ensure the destination exists before attempting to write files.
    ensure_dir(output_dir)
    parquet_path = output_dir / f"{name}.parquet"
    csv_path = output_dir / f"{name}.csv"

    # Persist both formats so the outputs work for Python and Power BI workflows.
    df.to_parquet(parquet_path, index=False)
    df.to_csv(csv_path, index=False)


def write_json(payload: dict, path: Path) -> None:
    """Write a dictionary payload to a UTF-8 JSON file."""

    # Ensure the parent folder exists before writing the JSON artefact.
    ensure_dir(path.parent)

    # Serialize the payload with indentation for easy inspection in the repo.
    path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
