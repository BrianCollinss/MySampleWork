from __future__ import annotations

import sys
from pathlib import Path


# Add the repository root to sys.path so tests can import the project package
# regardless of how pytest is invoked.
ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))
