"""Utility script for generating a quick inventory of this project.

Run from the project root with: `python scripts/project_inventory.py`
It prints data-folder file counts plus a compact summary of notebook files,
including cell counts and the first meaningful line from each notebook.
"""

from __future__ import annotations

import json
from collections import Counter
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def count_data_files() -> list[tuple[str, int]]:
    # Summarize how many source files sit in each data subfolder so the repo
    # layout can be reviewed without manually expanding every directory.
    data_root = ROOT / "0_data"
    counter: Counter[str] = Counter()

    for file_path in data_root.rglob("*"):
        if file_path.is_file():
            rel_parent = file_path.parent.relative_to(ROOT).as_posix()
            counter[rel_parent] += 1

    return sorted(counter.items())


def notebook_inventory() -> list[tuple[str, int, str]]:
    # Capture a lightweight notebook summary using cell counts plus the first
    # meaningful line, which is usually enough to identify each notebook's role.
    notebooks = []
    for path in sorted(ROOT.rglob("*.ipynb")):
        if path.parts and path.parts[0] == ".git":
            continue

        nb = json.loads(path.read_text(encoding="utf-8"))
        preview = ""
        for cell in nb.get("cells", []):
            text = "".join(cell.get("source", [])).strip()
            if text:
                preview = text.splitlines()[0]
                break

        notebooks.append(
            (
                path.relative_to(ROOT).as_posix(),
                len(nb.get("cells", [])),
                preview[:100],
            )
        )

    return notebooks


def main() -> None:
    # Print a compact text report that is easy to paste into notes, reviews,
    # or README drafting work.
    print("# Project Inventory")
    print()

    print("## Data folders")
    for folder, count in count_data_files():
        print(f"- {folder}: {count} files")

    print()
    print("## Notebooks")
    for notebook, cell_count, preview in notebook_inventory():
        print(f"- {notebook}: {cell_count} cells | {preview}")


if __name__ == "__main__":
    main()
