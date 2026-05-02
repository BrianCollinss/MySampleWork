"""Optional local landing helpers for smoke tests only.

The target architecture lands raw files in Fabric Lakehouse Files. This module
exists only for explicit local experimentation and should not become the main
storage path.
"""

from __future__ import annotations

from pathlib import Path


def ensure_local_landing_path(path: str | Path) -> Path:
    """Create a local landing path for explicit smoke-test use."""

    landing_path = Path(path)
    landing_path.mkdir(parents=True, exist_ok=True)
    return landing_path
