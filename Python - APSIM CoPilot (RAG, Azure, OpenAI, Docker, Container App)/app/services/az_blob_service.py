"""Azure Blob Storage helper for optional file persistence.

The demo uses blob storage sparingly: mainly for persisting uploaded files and
optionally holding APSIM reference documents outside the app container.
"""

from __future__ import annotations

from datetime import datetime, timezone
from io import BytesIO

from azure.storage.blob import BlobServiceClient

from app.config import AppConfig
from app.logging_config import get_logger


class BlobStorageService:
    """Upload files and ensure the configured blob container exists."""

    def __init__(self, config: AppConfig) -> None:
        """Build blob clients from the configured storage connection string."""
        self.config = config
        self.logger = get_logger(__name__)
        self.service_client = BlobServiceClient.from_connection_string(config.azure_storage_connection_string)
        self.container_client = self.service_client.get_container_client(config.azure_storage_container_name)

    def ensure_container(self) -> None:
        """Create the configured container when it does not already exist."""
        try:
            self.container_client.create_container()
        except Exception:
            # Container creation is idempotent for this demo; if it already
            # exists or creation is not needed, we simply continue.
            return

    def upload_bytes(self, name: str, data: bytes, folder: str = "uploads") -> str:
        """Upload a bytes payload into a timestamped blob path."""
        timestamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        blob_name = f"{folder}/{timestamp}-{name}"
        self.container_client.upload_blob(name=blob_name, data=BytesIO(data), overwrite=True)
        self.logger.info("Uploaded blob %s.", blob_name)
        return blob_name
