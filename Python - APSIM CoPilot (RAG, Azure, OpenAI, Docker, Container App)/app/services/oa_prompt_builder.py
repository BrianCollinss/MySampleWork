"""Prompt templates and prompt-construction helpers.

The prompts are intentionally explicit about groundedness and uncertainty so the
assistant behaves like a cautious APSIM helper rather than a confident guesser.
"""

from __future__ import annotations

from collections.abc import Iterable
import re

from app.models.mo_search_models import SearchDocument


RAG_SYSTEM_PROMPT = """You are APSIM Copilot, an assistant for APSIM users.
Follow these rules:
- Answer only from the retrieved APSIM documentation and examples provided.
- Be accurate, grounded, and practical.
- If the context is incomplete or weak, say so clearly.
- Do not invent APSIM behaviour, settings, outputs, or file schema details.
- Keep the answer in plain English and easy to follow.
- Do not include a Sources section, citations list, reference list, file path list, or bibliography at the end.
- Do not write lines starting with Source:, Sources:, Reference:, References:, Citation:, or Citations:.
"""

APSIMX_SYSTEM_PROMPT = """You are APSIM Copilot.
Explain APSIM .apsimx files in plain English for a human reader.
Rules:
- Use only the extracted structured data supplied to you.
- If a detail is missing, say that it was not found.
- Do not infer simulation behaviour that is not supported by the extracted data.
- Highlight simulation purpose, timing, crops, soils, weather references, manager rules, and outputs when present.
- Keep the explanation clear, concise, and grounded.
"""

CSV_SYSTEM_PROMPT = """You are APSIM Copilot.
Summarise APSIM output CSV files for a human reader.
Rules:
- Use only the tabular facts and descriptive statistics supplied to you.
- Mention likely APSIM variables such as yield, biomass, rainfall, runoff, soil water, and nitrogen only when they appear in the data.
- If grouping or treatments exist, explain the main differences cautiously.
- Do not invent agronomic conclusions that are not supported by the data.
- State uncertainty when the columns are ambiguous.
- Keep the summary short, practical, and in plain English.
"""


def build_rag_user_prompt(question: str, documents: Iterable[SearchDocument], max_context_characters: int) -> str:
    """Build a retrieval prompt from the user question and search results."""
    context_parts: list[str] = []
    running_total = 0
    for doc in documents:
        block = (
            f"Source title: {doc.title}\n"
            f"Source path: {doc.source_path}\n"
            f"Topic: {doc.topic}\n"
            f"Chunk id: {doc.chunk_id}\n"
            f"Content:\n{doc.content.strip()}\n"
        )
        # Trim context to a fixed size so prompts stay within a predictable
        # budget even when the search service returns long document chunks.
        if running_total + len(block) > max_context_characters:
            break
        context_parts.append(block)
        running_total += len(block)
    joined_context = "\n---\n".join(context_parts) if context_parts else "No retrieved context."
    return (
        f"User question:\n{question.strip()}\n\n"
        f"Retrieved APSIM context:\n{joined_context}\n\n"
        f"Write a grounded answer."
    )


def build_apsimx_user_prompt(structured_summary: dict[str, object]) -> str:
    """Build the user prompt for explaining a parsed APSIMX file."""
    return (
        "Explain this APSIMX file in plain English. Mention what was found and what was not found.\n\n"
        f"Structured summary:\n{structured_summary}"
    )


def build_csv_user_prompt(summary_payload: dict[str, object]) -> str:
    """Build the user prompt for summarising a CSV analysis payload."""
    return "Summarise this APSIM output CSV analysis in plain English.\n\nAnalysis payload:\n{0}".format(summary_payload)


def strip_trailing_sources_section(text: str) -> str:
    """Remove model-added trailing source lists or references sections."""
    cleaned = text.strip()

    heading_pattern = re.compile(
        r"\n{1,}(?:#{1,6}\s*)?(?:sources?|references?|citations?)\s*:?\s*\n.*$",
        re.IGNORECASE | re.DOTALL,
    )
    cleaned = re.sub(heading_pattern, "", cleaned).strip()

    line_pattern = re.compile(
        r"(?:\n(?:\s*[-*]\s*)?(?:source|reference|citation)s?\s*:.*)+$",
        re.IGNORECASE,
    )
    cleaned = re.sub(line_pattern, "", cleaned).strip()

    return cleaned
