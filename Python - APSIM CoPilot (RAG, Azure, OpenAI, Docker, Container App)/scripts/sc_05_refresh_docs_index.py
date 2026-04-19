"""Refresh the APSIM documentation corpus and reindex it in Azure AI Search.

This wrapper script turns the multi-step ingestion flow into one repeatable
command: fetch the latest APSIM docs, create/update the search index, optionally
upload the fetched docs to blob storage, and push fresh chunks into search.
"""

from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parents[1]
SCRIPTS_DIR = PROJECT_ROOT / "scripts"
DEFAULT_FETCH_OUTPUT_DIR = PROJECT_ROOT / "sample_data" / "docs" / "fetched"


def run_python_script(script_name: str, *script_args: str) -> None:
    """Run one helper script with the current Python interpreter."""
    command = [sys.executable, str(SCRIPTS_DIR / script_name), *script_args]
    subprocess.run(command, check=True, cwd=PROJECT_ROOT)


def parse_args() -> argparse.Namespace:
    """Parse command-line arguments for the end-to-end refresh flow."""
    parser = argparse.ArgumentParser(description="Fetch APSIM docs and refresh the Azure AI Search index.")
    parser.add_argument("--output-dir", default=str(DEFAULT_FETCH_OUTPUT_DIR))
    parser.add_argument("--max-pages", type=int, default=100)
    parser.add_argument("--topic", default="apsim")
    parser.add_argument("--skip-blob-upload", action="store_true")
    return parser.parse_args()


def main() -> None:
    """Run fetch, index creation, optional blob upload, and document indexing."""
    args = parse_args()
    output_dir = Path(args.output_dir)

    run_python_script("sc_01_create_search_index.py")
    run_python_script("sc_02_fetch_apsim_docs.py", "--output-dir", str(output_dir), "--max-pages", str(args.max_pages))
    if not args.skip_blob_upload:
        run_python_script("sc_03_upload_docs_to_blob.py", "--source-dir", str(output_dir), "--prefix", "docs/fetched")
    run_python_script("sc_04_index_documents.py", "--source-dir", str(output_dir), "--topic", args.topic)

    print("APSIM documentation refresh complete.")


if __name__ == "__main__":
    main()
