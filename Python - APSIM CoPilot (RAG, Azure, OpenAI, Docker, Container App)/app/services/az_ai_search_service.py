"""Azure AI Search integration for APSIM document retrieval.

The service performs a vector-enabled search and normalises Azure SDK results
into local dataclasses that are easier for prompt building and UI display.
"""

from __future__ import annotations

from azure.core.credentials import AzureKeyCredential
from azure.search.documents import SearchClient
from azure.search.documents.models import QueryType, VectorizedQuery

from app.config import AppConfig
from app.logging_config import get_logger
from app.models.mo_search_models import SearchDocument
from app.services.oa_openai_service import OpenAIService


class AISearchService:
    """Query the configured Azure AI Search index for APSIM content."""

    def __init__(self, config: AppConfig, openai_service: OpenAIService) -> None:
        """Create a search client tied to the configured service and index."""
        self.config = config
        self.logger = get_logger(__name__)
        self.openai_service = openai_service
        self.client = SearchClient(
            endpoint=config.azure_search_endpoint,
            index_name=config.azure_search_index_name,
            credential=AzureKeyCredential(config.azure_search_api_key),
        )

    def search(self, question: str, top_k: int | None = None) -> list[SearchDocument]:
        """Run a retrieval query and return normalised document chunks."""
        top_k = top_k or self.config.max_search_results
        # Embed the user question so the search service can perform vector
        # similarity against the indexed content vectors.
        embedding = self.openai_service.create_embedding(question)
        vector_query = VectorizedQuery(
            vector=embedding,
            fields="content_vector",
            k_nearest_neighbors=top_k,
        )
        results = self.client.search(
            search_text=question,
            top=top_k,
            vector_queries=[vector_query],
            query_type=QueryType.SEMANTIC,
            semantic_configuration_name="default",
            query_caption="extractive|highlight-true",
            # query_answer="extractive", # Can be set to skip OpenAI call if extractive answer is “good enough”
        )
        documents: list[SearchDocument] = []
        for item in results:
            documents.append(
                SearchDocument(
                    id=item.get("id", ""),
                    title=item.get("title", "Untitled"),
                    source_path=item.get("source_path", ""),
                    source_type=item.get("source_type", ""),
                    topic=item.get("topic", ""),
                    chunk_id=item.get("chunk_id", ""),
                    content=item.get("content", ""),
                    score=item.get("@search.score"),
                    metadata={k: v for k, v in item.items() if not k.startswith("@search")},
                )
            )
        self.logger.info("Azure AI Search returned %s documents.", len(documents))
        return documents

    def ping(self) -> bool:
        """Perform a minimal query to confirm the search service is reachable."""
        list(self.client.search(search_text="*", top=1))
        return True
