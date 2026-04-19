"""Basic smoke test for APSIM Copilot configuration and service connectivity.

This script is intentionally lightweight. It confirms the environment is wired
up and makes minimal calls to the configured external services when possible.
"""

from __future__ import annotations

import sys
from pathlib import Path

from dotenv import load_dotenv

PROJECT_ROOT = Path(__file__).resolve().parents[1]
if str(PROJECT_ROOT) not in sys.path:
    sys.path.insert(0, str(PROJECT_ROOT))

from app.config import get_config
from app.services.az_ai_search_service import AISearchService
from app.services.oa_openai_service import OpenAIService

__test__ = False


def main() -> None:
    """Run a compact connectivity check across the configured services."""
    load_dotenv(dotenv_path=PROJECT_ROOT / ".env", override=True)
    config = get_config()
    print("Configuration check")
    print(f"OpenAI API configured: {config.has_openai()}")
    print(f"Azure AI Search configured: {config.has_search()}")
    print(f"Azure Blob Storage configured: {config.has_blob()}")
    print(f"Application Insights configured: {config.has_app_insights()}")
    if config.has_openai():
        openai_service = OpenAIService(config)
        embedding = openai_service.create_embedding("APSIM smoke test")
        print(f"Embedding call succeeded. Vector length: {len(embedding)}")
        reply = openai_service.chat(
            system_prompt="You are a smoke test assistant. Reply with OK only.",
            user_prompt="Return OK.",
            temperature=0,
        )
        print(f"Chat call succeeded. Reply: {reply}")
        if config.has_search():
            search_service = AISearchService(config, openai_service)
            print(f"Search connectivity succeeded: {search_service.ping()}")


if __name__ == "__main__":
    main()
