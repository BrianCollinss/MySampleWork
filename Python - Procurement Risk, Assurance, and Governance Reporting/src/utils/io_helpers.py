"""Filesystem and configuration helpers.

These helpers keep common file and config operations out of the business logic
modules so those modules stay focused on analytics behaviour.
"""

from __future__ import annotations

import shutil
from datetime import datetime, timedelta
from pathlib import Path
from typing import Iterable
import logging
import re

import pandas as pd
import yaml
from fnmatch import fnmatch


LOGGER = logging.getLogger(__name__)


def ensure_directories(paths: Iterable[Path]) -> None:
    """Create each directory if it does not already exist."""
    for path in paths:
        path.mkdir(parents=True, exist_ok=True)


def load_yaml_config(path: Path) -> dict:
    """Load a YAML configuration file into a Python dictionary."""
    with path.open("r", encoding="utf-8") as handle:
        return yaml.safe_load(handle)


def sanitise_folder_name(value: str) -> str:
    """Convert free-text labels into a filesystem-safe folder name fragment."""

    cleaned = re.sub(r'[<>:"/\\|?*]+', "", value).strip()
    cleaned = re.sub(r"\s+", " ", cleaned)
    return cleaned or "Unknown Organisation"


def build_timestamped_run_dir(
    output_root_dir: Path,
    organisation_name: str,
    timestamp: datetime | None = None,
) -> Path:
    """Create the run-specific output directory named with timestamp and organisation.

    Each pipeline run writes its reporting artefacts to a dedicated timestamped
    folder so analysts can keep a short history of runs without overwriting the
    most recent charts, tables, and executive summary.
    """
    run_timestamp = (timestamp or datetime.now()).strftime("%Y%m%d_%H%M%S")
    safe_organisation_name = sanitise_folder_name(organisation_name)
    return output_root_dir / f"{run_timestamp} - {safe_organisation_name}"


def prune_old_timestamped_outputs(output_root_dir: Path, retention_config: dict | None = None) -> list[Path]:
    """Delete timestamp-prefixed output folders older than the configured window.

    The retention rule is intentionally driven by config rather than code so an
    analyst can choose whether historical output folders should be retained for
    minutes, hours, or days.
    """
    config = retention_config or {}
    retention_value = int(config.get("value", 10))
    retention_unit = str(config.get("unit", "minutes")).strip().lower()

    unit_map = {
        "minute": "minutes",
        "minutes": "minutes",
        "hour": "hours",
        "hours": "hours",
        "day": "days",
        "days": "days",
    }
    normalised_unit = unit_map.get(retention_unit)
    if normalised_unit is None:
        raise ValueError(f"Unsupported retention unit: {retention_unit}")

    retention_delta = timedelta(**{normalised_unit: retention_value})
    cutoff = datetime.now() - retention_delta
    removed_paths: list[Path] = []

    if not output_root_dir.exists():
        return removed_paths

    # Only folders that start with the expected timestamp pattern are managed
    # by the retention policy. This still works when the folder name includes
    # extra text such as ` - Griffith University` after the timestamp.
    for child in output_root_dir.iterdir():
        if not child.is_dir():
            continue
        try:
            folder_timestamp_text = child.name.split(" - ", 1)[0]
            folder_timestamp = datetime.strptime(folder_timestamp_text, "%Y%m%d_%H%M%S")
        except ValueError:
            continue

        if folder_timestamp < cutoff:
            try:
                shutil.rmtree(child)
                removed_paths.append(child)
            except PermissionError:
                # Windows or synced folders can temporarily lock files inside an
                # old run directory. Skipping the folder keeps the current run
                # moving while allowing cleanup on a later attempt.
                LOGGER.warning("Skipping locked historical output folder during retention cleanup: %s", child)

    return removed_paths


def clean_unnecessary_workspace_files(workspace_root: Path) -> dict[str, int]:
    """Remove low-value local noise files such as Windows metadata and caches.

    This housekeeping step keeps the repo easier to review and prevents local
    operating-system artefacts from accumulating across repeated pipeline runs.
    Only clearly disposable files and directories are removed.
    """
    removed_file_count = 0
    removed_directory_count = 0

    removable_files = {"desktop.ini", "thumbs.db"}
    removable_directories = {"__pycache__", ".pytest_cache", ".mypy_cache", ".ruff_cache", ".ipynb_checkpoints"}

    for path in workspace_root.rglob("*"):
        if not path.exists():
            continue

        # Remove known machine-generated files that should never be part of
        # analytical outputs or maintained source assets.
        if path.is_file() and path.name.lower() in removable_files:
            path.unlink(missing_ok=True)
            removed_file_count += 1
            continue

        # Remove cache folders created by Python tooling and notebook editors.
        if path.is_dir() and path.name in removable_directories:
            shutil.rmtree(path, ignore_errors=True)
            removed_directory_count += 1

    return {
        "removed_files": removed_file_count,
        "removed_directories": removed_directory_count,
    }


def copy_config_snapshot(source_config_dir: Path, destination_config_dir: Path) -> None:
    """Copy the current config folder into the run output for traceability.

    Keeping a config snapshot inside each run folder makes later assurance
    review easier because the exact settings used for that run travel with the
    analytical outputs and reporting artefacts.
    """
    if destination_config_dir.exists():
        shutil.rmtree(destination_config_dir, ignore_errors=True)

    shutil.copytree(
        source_config_dir,
        destination_config_dir,
        ignore=shutil.ignore_patterns("desktop.ini", "__pycache__", ".pytest_cache", ".mypy_cache", ".ruff_cache"),
    )


def discover_input_files(input_dir: Path) -> list[Path]:
    """Return supported tabular files in a raw source folder."""
    supported = {".csv", ".xlsx", ".xls"}
    return sorted([path for path in input_dir.iterdir() if path.is_file() and path.suffix.lower() in supported])


def filter_input_files(files: list[Path], excluded_patterns: list[str] | None = None) -> list[Path]:
    """Exclude files whose names match configured ignore patterns."""
    patterns = excluded_patterns or []
    if not patterns:
        return files
    return [path for path in files if not any(fnmatch(path.name.lower(), pattern.lower()) for pattern in patterns)]


def resolve_raw_folder(raw_root_dir: Path, raw_folder_name: str) -> Path:
    """Resolve and validate the active raw-data folder for a run."""
    raw_folder_path = raw_root_dir / raw_folder_name
    if not raw_folder_path.exists():
        raise FileNotFoundError(f"Raw folder does not exist: {raw_folder_path}")
    if not raw_folder_path.is_dir():
        raise NotADirectoryError(f"Raw folder path is not a directory: {raw_folder_path}")
    return raw_folder_path


def read_tabular_file(path: Path) -> pd.DataFrame:
    """Read a CSV or Excel file with practical encoding fallbacks for CSVs."""
    if path.suffix.lower() == ".csv":
        for encoding in ("utf-8", "utf-8-sig", "cp1252", "latin-1"):
            try:
                return pd.read_csv(path, encoding=encoding)
            except UnicodeDecodeError:
                continue
        return pd.read_csv(path, encoding_errors="replace")
    return pd.read_excel(path)


def write_dataframe(frame: pd.DataFrame, path: Path) -> None:
    """Write a dataframe to CSV, creating parent directories if needed."""
    path.parent.mkdir(parents=True, exist_ok=True)
    frame.to_csv(path, index=False)
