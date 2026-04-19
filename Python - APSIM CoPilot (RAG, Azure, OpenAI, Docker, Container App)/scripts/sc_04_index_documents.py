"""Chunk local APSIM documents and upload them into Azure AI Search.

The script keeps indexing deliberately transparent: it reads local files,
splits them into chunks, embeds each chunk, and uploads the resulting records.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from pathlib import Path

from azure.core.credentials import AzureKeyCredential
from azure.search.documents import SearchClient
from dotenv import load_dotenv

PROJECT_ROOT = Path(__file__).resolve().parents[1]
if str(PROJECT_ROOT) not in sys.path:
    sys.path.insert(0, str(PROJECT_ROOT))

from app.config import get_config
from app.services.oa_openai_service import OpenAIService

DEFAULT_SOURCE_DIR = PROJECT_ROOT / "sample_data" / "docs"


def chunk_text(text: str, chunk_size: int = 1200, overlap: int = 150) -> list[str]:
    """Split long text into overlapping chunks suitable for retrieval."""
    chunks: list[str] = []
    start = 0
    while start < len(text):
        end = min(start + chunk_size, len(text))
        chunks.append(text[start:end].strip())
        if end == len(text):
            break
        start = max(0, end - overlap)
    return [chunk for chunk in chunks if chunk]


def detect_source_type(path: Path) -> str:
    """Infer a simple source type label from the local file extension."""
    suffix = path.suffix.lower()
    if suffix in {".md", ".txt"}:
        return "documentation"
    if suffix == ".apsimx":
        return "example"
    return suffix.replace(".", "") or "unknown"


def main() -> None:
    """Read local documents, embed them, and upload them to Azure AI Search."""
    load_dotenv(dotenv_path=PROJECT_ROOT / ".env", override=True)
    config = get_config()
    parser = argparse.ArgumentParser(description="Chunk local APSIM docs and push them into Azure AI Search.")
    parser.add_argument(
        "--source-dir",
        default=str(DEFAULT_SOURCE_DIR),
        help="Root folder containing documents to index. Defaults to sample_data/docs.",
    )
    parser.add_argument("--topic", default="general")
    args = parser.parse_args()
    source_dir = Path(args.source_dir)
    if not source_dir.exists():
        raise ValueError(f"Source directory does not exist: {source_dir}")
    if not config.has_openai():
        raise ValueError(
            "OpenAI configuration is incomplete. Set OPENAI_API_KEY directly or "
            "configure AZURE_KEY_VAULT_URL plus OPENAI_API_KEY_SECRET_NAME."
        )
    if not config.has_search():
        raise ValueError(
            "Azure AI Search configuration is incomplete. Ensure AZURE_SEARCH_ENDPOINT, "
            "AZURE_SEARCH_INDEX_NAME, and AZURE_SEARCH_API_KEY are configured, either "
            "directly or via Azure Key Vault."
        )
    openai_service = OpenAIService(config)
    search_client = SearchClient(
        endpoint=config.azure_search_endpoint,
        index_name=config.azure_search_index_name,
        credential=AzureKeyCredential(config.azure_search_api_key),
    )
    batch: list[dict] = []
    # Recurse through the whole docs tree so manually curated files and fetched
    # APSIM website content can live side by side under one root folder.
    for path in source_dir.rglob("*"):
        if not path.is_file() or path.suffix.lower() not in {".md", ".txt", ".apsimx", ".json"}:
            continue
        text = path.read_text(encoding="utf-8", errors="ignore")
        for chunk_number, chunk in enumerate(chunk_text(text), start=1):
            # Use a deterministic id so reruns update the same logical chunk
            # instead of creating duplicate search documents.
            digest = hashlib.md5(f"{path}-{chunk_number}".encode("utf-8")).hexdigest()
            batch.append(
                {
                    "id": digest,
                    "title": path.stem,
                    "source_path": str(path.as_posix()),
                    "source_type": detect_source_type(path),
                    "topic": args.topic,
                    "chunk_id": f"{path.stem}-{chunk_number}",
                    "content": chunk,
                    "content_vector": openai_service.create_embedding(chunk),
                }
            )
    if not batch:
        print("No documents found to index.")
        return
    result = search_client.upload_documents(batch)
    print(json.dumps([item.as_dict() for item in result], indent=2))


if __name__ == "__main__":
    main()
