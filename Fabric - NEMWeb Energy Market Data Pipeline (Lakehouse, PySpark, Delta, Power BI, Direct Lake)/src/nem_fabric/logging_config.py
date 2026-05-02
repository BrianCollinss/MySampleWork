"""Logging configuration for scripts and notebooks.

Fabric notebooks and local scripts both benefit from consistent timestamped
logs, especially when debugging incremental ingestion runs.
"""

from __future__ import annotations

import logging


def configure_logging(level: str = "INFO") -> None:
    """Configure standard logging for local scripts and notebooks.

    The format includes module name and severity so mixed notebook output is
    still traceable during scheduled runs.
    """

    logging.basicConfig(
        level=getattr(logging, level.upper(), logging.INFO),
        format="%(asctime)s %(levelname)s %(name)s - %(message)s",
    )
