"""Configuration loading for local tests and Fabric notebooks.

Configuration is split between checked-in YAML files and local environment
variables. YAML describes source systems and target table intent; environment
variables hold user-specific Fabric names and runtime limits.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

import yaml
from dotenv import load_dotenv
from pydantic import BaseModel, Field, HttpUrl, field_validator

# Resolve the repository root from the installed source tree. This keeps local
# scripts independent of the current working directory.
PROJECT_ROOT = Path(__file__).resolve().parents[2]


class Settings(BaseModel):
    """Validated runtime settings sourced from environment variables."""

    fabric_workspace_name: str = ""
    fabric_lakehouse_name: str = ""
    nemweb_base_url: HttpUrl = "https://nemweb.com.au/Reports/Current/"
    nemweb_dispatchis_url: HttpUrl = (
        "https://nemweb.com.au/Reports/Current/DispatchIS_Reports/"
    )
    nemweb_public_prices_url: HttpUrl = (
        "https://nemweb.com.au/Reports/Current/Public_Prices/"
    )
    ingestion_lookback_hours: int = Field(default=6, ge=1)
    max_zips_per_run: int = Field(default=500, ge=1)
    log_level: str = "INFO"

    @field_validator("log_level")
    @classmethod
    def normalise_log_level(cls, value: str) -> str:
        """Normalise logging levels so callers can pass `info` or `INFO`."""

        return value.upper()


def load_yaml(path: str | Path) -> dict[str, Any]:
    """Load a YAML file as a dictionary.

    Relative paths are resolved from the project root so notebooks and scripts
    can call this function from different working directories.
    """

    yaml_path = Path(path)
    if not yaml_path.is_absolute():
        yaml_path = PROJECT_ROOT / yaml_path
    with yaml_path.open("r", encoding="utf-8") as file:
        data = yaml.safe_load(file) or {}
    return data


def load_settings(env_path: str | Path | None = None) -> Settings:
    """Load `.env` values and return validated settings.

    Missing values fall back to safe defaults or blank placeholders. Secrets are
    deliberately not represented in this settings object.
    """

    if env_path is None:
        env_path = PROJECT_ROOT / ".env"
    load_dotenv(env_path)

    import os

    # Convert string environment variables into the typed Settings model. Pydantic
    # validates URL shapes and numeric bounds before the caller uses them.
    return Settings(
        fabric_workspace_name=os.getenv("FABRIC_WORKSPACE_NAME", ""),
        fabric_lakehouse_name=os.getenv("FABRIC_LAKEHOUSE_NAME", ""),
        nemweb_base_url=os.getenv(
            "NEMWEB_BASE_URL", "https://nemweb.com.au/Reports/Current/"
        ),
        nemweb_dispatchis_url=os.getenv(
            "NEMWEB_DISPATCHIS_URL",
            "https://nemweb.com.au/Reports/Current/DispatchIS_Reports/",
        ),
        nemweb_public_prices_url=os.getenv(
            "NEMWEB_PUBLIC_PRICES_URL",
            "https://nemweb.com.au/Reports/Current/Public_Prices/",
        ),
        ingestion_lookback_hours=int(os.getenv("INGESTION_LOOKBACK_HOURS", "6")),
        max_zips_per_run=int(os.getenv("MAX_ZIPS_PER_RUN", "500")),
        log_level=os.getenv("LOG_LEVEL", "INFO"),
    )
