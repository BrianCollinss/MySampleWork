"""Azure Key Vault helper for secret resolution.

This module keeps Key Vault access lightweight and optional. When the Azure
identity and Key Vault SDK packages are installed, it can resolve secrets for
both local development and deployed environments. When they are absent, the
rest of the application can still fall back to plain environment variables.
"""

from __future__ import annotations

from functools import lru_cache

try:
    from azure.identity import DefaultAzureCredential
    from azure.keyvault.secrets import SecretClient
except ImportError:  # pragma: no cover - optional dependency path
    DefaultAzureCredential = None
    SecretClient = None


class KeyVaultService:
    """Resolve secrets from an Azure Key Vault."""

    def __init__(self, vault_url: str) -> None:
        """Create a secret client for the requested vault."""
        if DefaultAzureCredential is None or SecretClient is None:
            raise ImportError(
                "Azure Key Vault support requires 'azure-identity' and "
                "'azure-keyvault-secrets' to be installed."
            )
        self.client = SecretClient(
            vault_url=vault_url,
            credential=DefaultAzureCredential(),
        )

    def get_secret(self, name: str) -> str:
        """Fetch one secret value by name."""
        return self.client.get_secret(name).value or ""


@lru_cache(maxsize=None)
def get_key_vault_service(vault_url: str) -> KeyVaultService:
    """Cache one Key Vault client per vault URL."""
    return KeyVaultService(vault_url)
