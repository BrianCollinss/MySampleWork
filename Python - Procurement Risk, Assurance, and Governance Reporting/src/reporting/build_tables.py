"""Write business-facing output tables.

This module separates presentable reporting outputs from the persisted
Medallion layers so stakeholders can consume compact business artefacts.
"""

from __future__ import annotations

from pathlib import Path

import pandas as pd

from src.utils.io_helpers import ensure_directories, write_dataframe


def write_reporting_tables(
    output_dir: Path,
    metrics_summary: pd.DataFrame,
    exceptions: pd.DataFrame,
    data_quality_summary: pd.DataFrame,
    additional_tables: dict[str, pd.DataFrame] | None = None,
) -> dict[str, Path]:
    """Write required reporting tables and selected supporting tables.

    The core three reporting outputs are always written, while supporting Gold
    tables can be supplied as optional extras.
    """

    tables_dir = output_dir / "tables"
    # Ensure the target exists so callers do not need to manage directory
    # creation themselves.
    ensure_directories([tables_dir])

    outputs = {
        "summary_metrics": tables_dir / "summary_metrics.csv",
        "exceptions_register": tables_dir / "exceptions_register.csv",
        "data_quality_summary": tables_dir / "data_quality_summary.csv",
    }
    write_dataframe(metrics_summary, outputs["summary_metrics"])
    write_dataframe(exceptions, outputs["exceptions_register"])
    write_dataframe(data_quality_summary, outputs["data_quality_summary"])

    # Additional tables allow the reporting layer to publish convenient
    # sidecar outputs without changing the required interface.
    for name, frame in (additional_tables or {}).items():
        destination = tables_dir / f"{name}.csv"
        write_dataframe(frame, destination)
        outputs[name] = destination
    return outputs
