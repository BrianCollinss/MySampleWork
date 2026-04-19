"""Upload local APSIM reference files into Azure Blob Storage.

This is useful when you want the same curated documentation set available in
Azure, even though indexing still reads from a local folder for simplicity.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from dotenv import load_dotenv

PROJECT_ROOT = Path(__file__).resolve().parents[1]
if str(PROJECT_ROOT) not in sys.path:
    sys.path.insert(0, str(PROJECT_ROOT))

from app.config import get_config
from app.services.az_blob_service import BlobStorageService


def main() -> None:
    """Walk a local directory and upload each file into the blob container."""
    load_dotenv(dotenv_path=PROJECT_ROOT / ".env", override=True)
    config = get_config()
    parser = argparse.ArgumentParser(description="Upload APSIM documentation files to Azure Blob Storage.")
    parser.add_argument("--source-dir", required=True, help="Local folder containing APSIM docs and examples.")
    parser.add_argument("--prefix", default="docs", help="Blob prefix inside the container.")
    args = parser.parse_args()
    source_dir = Path(args.source_dir)
    if not config.has_blob():
        raise ValueError(
            "Azure Blob Storage configuration is incomplete. Set "
            "AZURE_STORAGE_CONNECTION_STRING directly or configure "
            "AZURE_KEY_VAULT_URL plus AZURE_STORAGE_CONNECTION_STRING_SECRET_NAME."
        )
    service = BlobStorageService(config)
    service.ensure_container()
    for path in source_dir.rglob("*"):
        if path.is_file():
            # Preserve the local folder structure under the requested blob prefix
            # so source files remain easy to browse later.
            blob_name = f"{args.prefix}/{path.relative_to(source_dir).as_posix()}"
            service.container_client.upload_blob(blob_name, path.read_bytes(), overwrite=True)
            print(f"Uploaded {path} -> {blob_name}")


if __name__ == "__main__":
    main()
