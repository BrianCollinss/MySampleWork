"""
Shared configuration helpers for Databricks declarative pipeline files.

The actual bronze, silver, and gold dataset logic stays in the individual
pipeline scripts. This module only centralizes the small amount of repeated
setup code needed by each script.
"""


from __future__ import annotations

import os
from dataclasses import dataclass

from pyspark.sql import SparkSession
from pyspark.errors import AnalysisException


@dataclass(frozen=True)
class Settings:
    catalog: str
    landing_schema: str
    landing_volume: str
    bronze_schema: str
    silver_schema: str
    gold_schema: str

    def table(self, schema: str, name: str) -> str:
        return f"{self.catalog}.{schema}.{name}"

    def volume(self, *parts: str) -> str:
        # Centralize Unity Catalog volume path creation so every pipeline uses
        # the same folder convention for landing and archive locations.
        clean_parts = [part.strip("/") for part in parts if part]
        return f"/Volumes/{self.catalog}/{self.landing_schema}/{self.landing_volume}/{'/'.join(clean_parts)}"


def get_spark_session() -> SparkSession:
    """Return the active Spark session or create one when needed."""
    return SparkSession.getActiveSession() or SparkSession.builder.getOrCreate()


def read_config(spark_session: SparkSession, key: str, default: str) -> str:
    """Read a config value from env vars first, then Spark conf, then default."""

    # Support both Databricks pipeline configuration and local overrides when
    # scripts are executed through Databricks Connect or a notebook session.
    env_key = key.upper().replace(".", "_")
    env_value = os.getenv(env_key)
    if env_value is not None and env_value != "":
        return env_value

    try:
        value = spark_session.conf.get(key, default)
    except AnalysisException:
        return default
    if value is None:
        return default
    return str(value)


def load_configs(spark_session: SparkSession) -> Settings:
    """Load the shared project settings from Spark configuration."""
    return Settings(
        catalog=read_config(spark_session, "auto_claims.catalog", "training_0003_auto_claims"),
        landing_schema=read_config(spark_session, "auto_claims.schemas.landing", "00_landing"),
        landing_volume=read_config(spark_session, "auto_claims.volumes.landing", "landing"),
        bronze_schema=read_config(spark_session, "auto_claims.schemas.bronze", "01_bronze"),
        silver_schema=read_config(spark_session, "auto_claims.schemas.silver", "02_silver"),
        gold_schema=read_config(spark_session, "auto_claims.schemas.gold", "03_gold"),
    )


def require_existing_catalog(spark_session: SparkSession, settings: Settings) -> None:
    """Raise an error when the configured Unity Catalog catalog does not exist."""

    # Creating the catalog is intentionally left outside the project so the
    # deployment target remains explicit and under user control.
    catalog_exists = spark_session.sql(f"SHOW CATALOGS LIKE '{settings.catalog}'").limit(1).count() == 1
    if not catalog_exists:
        raise RuntimeError(
            "Required Databricks catalog does not exist: "
            f"{settings.catalog}. Create this catalog manually before deploying or running the pipelines."
        )


def ensure_schemas_and_volume(spark_session: SparkSession, settings: Settings) -> None:
    """Create the demo schemas and managed landing volume when they are missing."""

    # The demo creates schemas and the managed landing volume on demand so the
    # first run can bootstrap the workspace after the catalog already exists.
    require_existing_catalog(spark_session, settings)
    spark_session.sql(f"CREATE SCHEMA IF NOT EXISTS {settings.catalog}.{settings.landing_schema}")
    spark_session.sql(f"CREATE SCHEMA IF NOT EXISTS {settings.catalog}.{settings.bronze_schema}")
    spark_session.sql(f"CREATE SCHEMA IF NOT EXISTS {settings.catalog}.{settings.silver_schema}")
    spark_session.sql(f"CREATE SCHEMA IF NOT EXISTS {settings.catalog}.{settings.gold_schema}")
    spark_session.sql(
        f"CREATE VOLUME IF NOT EXISTS {settings.catalog}.{settings.landing_schema}.{settings.landing_volume}"
    )
