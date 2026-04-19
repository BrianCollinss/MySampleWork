"""Application configuration loaded from environment variables.

The demo keeps configuration intentionally simple: all runtime settings come
from environment variables so the same code can run locally, in CI, or inside
Azure Container Apps without hard-coded secrets.
"""

from __future__ import annotations

import os
from dataclasses import dataclass, field
from functools import lru_cache

from app.services.az_key_vault_service import get_key_vault_service


def _as_bool(value: str | None, default: bool = False) -> bool:
    """Convert common environment variable strings into a boolean value."""
    if value is None:
        return default
    return value.strip().lower() in {"1", "true", "yes", "y", "on"}


def _get_env(name: str, default: str = "") -> str:
    """Read one environment variable as a string."""
    return os.getenv(name, default)


def _detect_deployment_mode() -> str:
    """Infer the current runtime from hosting-provided environment variables."""
    # `CONTAINER_APP_NAME` is also used in `.env` for deployment scripts, so it
    # is not a reliable runtime signal. The revision and replica variables are
    # injected by Azure Container Apps at runtime and are safe to use here.
    if _get_env("CONTAINER_APP_REVISION") or _get_env("CONTAINER_APP_REPLICA_NAME"):
        return "container-apps"
    return "local"


def _resolve_secret(secret_env_var: str, secret_name_env_var: str) -> str:
    """Resolve a secret from env first, then Azure Key Vault when configured."""
    direct_value = _get_env(secret_env_var)
    if direct_value:
        return direct_value

    vault_url = _get_env("AZURE_KEY_VAULT_URL")
    secret_name = _get_env(secret_name_env_var)
    if not vault_url or not secret_name:
        return ""

    try:
        return get_key_vault_service(vault_url).get_secret(secret_name)
    except Exception:
        return ""


@dataclass(frozen=True)
class AppConfig:
    """Central configuration object for the APSIM Copilot application.

    The fields in this dataclass intentionally mirror the names used in `.env`
    so the docs, helper scripts, and deployed app all refer to the same values.
    """

    # Use default_factory so values are read when the config object is created,
    # not when this module is imported. That matters for scripts which load
    # `.env` after importing `app.config`.
    app_name: str = field(default_factory=lambda: _get_env("APP_NAME", "APSIM Copilot"))
    deployment_mode: str = field(default_factory=_detect_deployment_mode)
    log_level: str = field(default_factory=lambda: _get_env("LOG_LEVEL", "INFO"))
    streamlit_server_port: int = field(default_factory=lambda: int(_get_env("STREAMLIT_SERVER_PORT", "8501")))
    azure_key_vault_url: str = field(default_factory=lambda: _get_env("AZURE_KEY_VAULT_URL"))
    openai_api_key_secret_name: str = field(default_factory=lambda: _get_env("OPENAI_API_KEY_SECRET_NAME"))
    azure_search_api_key_secret_name: str = field(default_factory=lambda: _get_env("AZURE_SEARCH_API_KEY_SECRET_NAME"))
    azure_storage_connection_string_secret_name: str = field(
        default_factory=lambda: _get_env("AZURE_STORAGE_CONNECTION_STRING_SECRET_NAME")
    )
    applicationinsights_connection_string_secret_name: str = field(
        default_factory=lambda: _get_env("APPLICATIONINSIGHTS_CONNECTION_STRING_SECRET_NAME")
    )
    openai_api_key: str = field(default_factory=lambda: _resolve_secret("OPENAI_API_KEY", "OPENAI_API_KEY_SECRET_NAME"))
    openai_chat_model: str = field(default_factory=lambda: _get_env("OPENAI_CHAT_MODEL", "gpt-5.4-mini"))
    openai_embedding_model: str = field(default_factory=lambda: _get_env("OPENAI_EMBEDDING_MODEL", "text-embedding-3-small"))
    azure_search_endpoint: str = field(default_factory=lambda: _get_env("AZURE_SEARCH_ENDPOINT"))
    azure_search_api_key: str = field(
        default_factory=lambda: _resolve_secret("AZURE_SEARCH_API_KEY", "AZURE_SEARCH_API_KEY_SECRET_NAME")
    )
    azure_search_index_name: str = field(default_factory=lambda: _get_env("AZURE_SEARCH_INDEX_NAME", "apsim-docs"))
    azure_search_vector_dimensions: int = field(default_factory=lambda: int(_get_env("AZURE_SEARCH_VECTOR_DIMENSIONS", "1536")))
    azure_search_semantic_config: str = field(default_factory=lambda: _get_env("AZURE_SEARCH_SEMANTIC_CONFIG", "default"))
    azure_storage_connection_string: str = field(
        default_factory=lambda: _resolve_secret(
            "AZURE_STORAGE_CONNECTION_STRING", "AZURE_STORAGE_CONNECTION_STRING_SECRET_NAME"
        )
    )
    azure_storage_container_name: str = field(default_factory=lambda: _get_env("AZURE_STORAGE_CONTAINER_NAME", "apsim-copilot"))
    save_uploads_to_blob: bool = field(default_factory=lambda: _as_bool(_get_env("SAVE_UPLOADS_TO_BLOB"), default=False))
    applicationinsights_connection_string: str = field(
        default_factory=lambda: _resolve_secret(
            "APPLICATIONINSIGHTS_CONNECTION_STRING", "APPLICATIONINSIGHTS_CONNECTION_STRING_SECRET_NAME"
        )
    )
    max_search_results: int = field(default_factory=lambda: int(_get_env("MAX_SEARCH_RESULTS", "5")))
    max_context_characters: int = field(default_factory=lambda: int(_get_env("MAX_CONTEXT_CHARACTERS", "12000")))
    csv_preview_rows: int = field(default_factory=lambda: int(_get_env("CSV_PREVIEW_ROWS", "25")))

    def has_openai(self) -> bool:
        """Return True when the minimum OpenAI settings are available."""
        return all([self.openai_api_key, self.openai_chat_model, self.openai_embedding_model])

    def has_search(self) -> bool:
        """Return True when Azure AI Search settings are available."""
        return all([self.azure_search_endpoint, self.azure_search_api_key, self.azure_search_index_name])

    def has_blob(self) -> bool:
        """Return True when Azure Blob Storage settings are available."""
        return all([self.azure_storage_connection_string, self.azure_storage_container_name])

    def has_app_insights(self) -> bool:
        """Return True when Application Insights telemetry is configured."""
        return bool(self.applicationinsights_connection_string)


@lru_cache(maxsize=1)
def get_config() -> AppConfig:
    """Cache and return one application config instance per process."""
    return AppConfig()
