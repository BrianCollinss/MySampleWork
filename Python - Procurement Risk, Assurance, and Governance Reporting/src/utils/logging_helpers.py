"""Logging setup helpers.

The reporting pipeline relies on lightweight structured logging so analysts can
understand what was processed without digging into the code.
"""

from __future__ import annotations

import logging


def configure_logging(level: str = "INFO") -> None:
    """Configure a consistent application-wide logging format."""
    logging.basicConfig(
        level=getattr(logging, level.upper(), logging.INFO),
        format="%(asctime)s | %(levelname)s | %(name)s | %(message)s",
    )
