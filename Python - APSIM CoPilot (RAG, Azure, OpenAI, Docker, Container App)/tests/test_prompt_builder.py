"""Unit tests for prompt construction helpers."""

from app.models.mo_search_models import SearchDocument
from app.services.oa_prompt_builder import build_apsimx_user_prompt, build_csv_user_prompt, build_rag_user_prompt


def test_build_rag_user_prompt_contains_question_and_source() -> None:
    """Ensure the RAG prompt includes the user question and source metadata."""
    documents = [
        SearchDocument(
            id="1",
            title="APSIM Water Balance",
            source_path="docs/water.md",
            source_type="documentation",
            topic="water",
            chunk_id="water-1",
            content="Soil water can be reported using report variables.",
        )
    ]
    prompt = build_rag_user_prompt("How is soil water reported?", documents, max_context_characters=5000)
    assert "How is soil water reported?" in prompt
    assert "APSIM Water Balance" in prompt


def test_build_apsimx_user_prompt_contains_structured_data() -> None:
    """Ensure the APSIMX prompt includes the extracted structured summary."""
    prompt = build_apsimx_user_prompt({"simulation_name": "Demo"})
    assert "Demo" in prompt


def test_build_csv_user_prompt_contains_payload() -> None:
    """Ensure the CSV prompt includes the analysis payload."""
    prompt = build_csv_user_prompt({"row_count": 10})
    assert "row_count" in prompt
