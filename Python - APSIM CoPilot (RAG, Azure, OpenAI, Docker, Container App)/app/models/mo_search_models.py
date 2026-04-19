"""Lightweight models shared by search and prompt-building services."""

from __future__ import annotations

from dataclasses import dataclass, field


@dataclass(slots=True)
class SearchDocument:
    """Normalised Azure AI Search result used across the app."""

    id: str
    title: str
    source_path: str
    source_type: str
    topic: str
    chunk_id: str
    content: str
    score: float | None = None
    metadata: dict[str, object] = field(default_factory=dict)
