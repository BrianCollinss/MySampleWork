"""Create or update the Azure AI Search index used by APSIM Copilot.

This script sets up a compact index schema designed for chunked APSIM
documentation, including both plain-text content and vector embeddings.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from azure.core.credentials import AzureKeyCredential
from azure.search.documents.indexes import SearchIndexClient
from azure.search.documents.indexes.models import (
    HnswAlgorithmConfiguration,
    SearchField,
    SearchFieldDataType,
    SearchIndex,
    SearchableField,
    SemanticConfiguration,
    SemanticField,
    SemanticPrioritizedFields,
    SemanticSearch,
    SimpleField,
    VectorSearch,
    VectorSearchProfile,
)
from dotenv import load_dotenv

PROJECT_ROOT = Path(__file__).resolve().parents[1]
if str(PROJECT_ROOT) not in sys.path:
    sys.path.insert(0, str(PROJECT_ROOT))

from app.config import get_config


def build_search_index(dimensions: int, index_name: str) -> SearchIndex:
    """Construct the Azure AI Search index definition for APSIM content."""
    fields = [
        SimpleField(name="id", type=SearchFieldDataType.String, key=True),
        SearchableField(name="title", type=SearchFieldDataType.String, sortable=True),
        SimpleField(name="source_path", type=SearchFieldDataType.String, filterable=True),
        SimpleField(name="source_type", type=SearchFieldDataType.String, filterable=True),
        SimpleField(name="topic", type=SearchFieldDataType.String, filterable=True, facetable=True),
        SimpleField(name="chunk_id", type=SearchFieldDataType.String, filterable=True),
        SearchableField(name="content", type=SearchFieldDataType.String),
        # The vector field stores embeddings created from documentation chunks
        # so retrieval can combine lexical and semantic matching.
        SearchField(
            name="content_vector",
            type=SearchFieldDataType.Collection(SearchFieldDataType.Single),
            searchable=True,
            vector_search_dimensions=dimensions,
            vector_search_profile_name="content-vector-profile",
        ),
    ]
    vector_search = VectorSearch(
        algorithms=[HnswAlgorithmConfiguration(name="content-hnsw")],
        profiles=[VectorSearchProfile(name="content-vector-profile", algorithm_configuration_name="content-hnsw")],
    )
    semantic_search = SemanticSearch(
        configurations=[
            SemanticConfiguration(
                name="default",
                prioritized_fields=SemanticPrioritizedFields(
                    title_field=SemanticField(field_name="title"),
                    content_fields=[SemanticField(field_name="content")],
                ),
            )
        ]
    )
    return SearchIndex(name=index_name, fields=fields, vector_search=vector_search, semantic_search=semantic_search)


def main() -> None:
    """Load configuration and create or update the configured search index."""
    load_dotenv(dotenv_path=PROJECT_ROOT / ".env", override=True)
    config = get_config()
    parser = argparse.ArgumentParser(description="Create or update the APSIM Azure AI Search index.")
    parser.add_argument("--dimensions", type=int, default=config.azure_search_vector_dimensions)
    parser.add_argument("--index-name", type=str, default=config.azure_search_index_name)
    args = parser.parse_args()
    if not config.azure_search_endpoint.startswith("https://"):
        raise ValueError(
            "AZURE_SEARCH_ENDPOINT is missing or invalid. Expected a value like "
            "'https://<service-name>.search.windows.net' in environment variables or .env."
        )
    if not config.azure_search_api_key:
        raise ValueError(
            "AZURE_SEARCH_API_KEY is missing. Set it directly in environment variables "
            "or configure AZURE_KEY_VAULT_URL plus AZURE_SEARCH_API_KEY_SECRET_NAME."
        )
    client = SearchIndexClient(
        endpoint=config.azure_search_endpoint,
        credential=AzureKeyCredential(config.azure_search_api_key),
    )
    search_index = build_search_index(dimensions=args.dimensions, index_name=args.index_name)
    client.create_or_update_index(search_index)
    print(f"Search index '{args.index_name}' created or updated.")


if __name__ == "__main__":
    main()
