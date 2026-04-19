"""Thin wrapper around the OpenAI Python SDK.

The wrapper keeps model names and authentication in one place so the rest of
the application can ask for chat completions and embeddings without repeating
SDK setup details.
"""

from __future__ import annotations

from openai import OpenAI

from app.config import AppConfig
from app.logging_config import get_logger


class OpenAIService:
    """Provide chat and embedding calls using the configured OpenAI models."""

    def __init__(self, config: AppConfig) -> None:
        """Initialise the OpenAI client with the API key from config."""
        self.config = config
        self.logger = get_logger(__name__)
        self.client = OpenAI(api_key=config.openai_api_key)

    def create_embedding(self, text: str) -> list[float]:
        """Create an embedding vector for retrieval and indexing workflows."""
        response = self.client.embeddings.create(
            model=self.config.openai_embedding_model,
            input=text,
        )
        return response.data[0].embedding

    def chat(self, system_prompt: str, user_prompt: str, temperature: float = 0.1) -> str:
        """Generate a chat completion from the configured OpenAI chat model."""
        response = self.client.chat.completions.create(
            model=self.config.openai_chat_model,
            temperature=temperature,
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ],
        )
        content = response.choices[0].message.content or ""
        self.logger.info("OpenAI chat completion returned %s characters.", len(content))
        return content.strip()
