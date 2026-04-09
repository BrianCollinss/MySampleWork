"""
Step 3: define gold materialized views with Databricks declarative pipelines.

This file is intended to be registered as a Databricks pipeline whose target
schema is the gold schema. It reads standardized silver tables and builds
analytics-ready outputs for dashboards and ML features.
"""


from __future__ import annotations

import os
from dataclasses import dataclass

from pyspark import pipelines as dp
from pyspark.errors import AnalysisException
from pyspark.sql import DataFrame
from pyspark.sql import SparkSession
from pyspark.sql.functions import avg, col


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


def get_spark_session() -> SparkSession:
    """Return the active Spark session or create one when needed."""
    return SparkSession.getActiveSession() or SparkSession.builder.getOrCreate()


def read_config(spark_session: SparkSession, key: str, default: str) -> str:
    """Read a config value from env vars first, then Spark conf, then default."""
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
    """Load the gold pipeline settings from Spark configuration."""
    return Settings(
        catalog=read_config(spark_session, "auto_claims.catalog", "training_0003_auto_claims"),
        landing_schema=read_config(spark_session, "auto_claims.schemas.landing", "00_landing"),
        landing_volume=read_config(spark_session, "auto_claims.volumes.landing", "landing"),
        bronze_schema=read_config(spark_session, "auto_claims.schemas.bronze", "01_bronze"),
        silver_schema=read_config(spark_session, "auto_claims.schemas.silver", "02_silver"),
        gold_schema=read_config(spark_session, "auto_claims.schemas.gold", "03_gold"),
    )


spark = get_spark_session()
settings = load_configs(spark)


@dp.materialized_view(
    name="aggregated_telematics",
    comment="Average telematics metrics by vehicle chassis number.",
)
def aggregated_telematics() -> DataFrame:
    """Aggregate telematics events into one feature row per vehicle chassis."""

    return (
        spark.read.table(settings.table(settings.silver_schema, "telematics"))
        # Gold reduces per-event telematics into simple vehicle-level features
        # that are easy to explain in dashboards and claim investigations.
        .groupBy("chassis_no")
        .agg(
            avg("speed").alias("telematics_speed"),
            avg("latitude").alias("telematics_latitude"),
            avg("longitude").alias("telematics_longitude"),
        )
    )


@dp.materialized_view(
    name="customer_claim_policy",
    comment="Combined customer, claim, and policy view for claim investigation.",
)
def customer_claim_policy() -> DataFrame:
    """Join the latest silver claim, policy, and customer tables together."""

    policy_df = spark.read.table(settings.table(settings.silver_schema, "policy"))
    claim_df = spark.read.table(settings.table(settings.silver_schema, "claim"))
    customer_df = spark.read.table(settings.table(settings.silver_schema, "customer"))
    # Drop bronze lineage columns from the lookup sides so the final gold table
    # exposes business columns instead of repeated ingestion metadata fields.
    policy_columns = [column for column in policy_df.columns if column not in {"source_file", "bronze_loaded_at"}]
    customer_columns = [
        column for column in customer_df.columns if column not in {"customer_id", "source_file", "bronze_loaded_at"}
    ]
    # Step 1: trim the lookup tables down to the business-facing columns that
    # belong in the final serving dataset.
    policy_join_df = policy_df.select(*policy_columns)

    return (
        # Step 2: link each claim to its policy using the shared policy number.
        claim_df.join(policy_join_df, "policy_no")
        # Step 3: bring in customer details through the policy-to-customer link.
        .join(customer_df, policy_join_df.cust_id == customer_df.customer_id, "left")
        # Step 4: project the final gold schema explicitly so duplicate lineage
        # columns do not leak through the joins.
        .select(
            claim_df["*"],
            *[policy_join_df[column] for column in policy_columns if column != "policy_no"],
            *[customer_df[column] for column in customer_columns],
        )
    )


@dp.materialized_view(
    name="customer_claim_policy_telematics",
    comment="Gold investigation view combining claims, policy, customer, and telematics context.",
)
def customer_claim_policy_telematics() -> DataFrame:
    """Add aggregated telematics features to the gold investigation view."""

    customer_claim_policy_df = spark.read.table(settings.table(settings.gold_schema, "customer_claim_policy"))
    aggregated_telematics_df = spark.read.table(settings.table(settings.gold_schema, "aggregated_telematics"))

    # Keep the serving view intentionally simple: one join for customer / claim /
    # policy context, plus one join for rolled-up telematics features.
    return customer_claim_policy_df.where(col("borough").isNotNull()).join(
        # Step 1: enrich the investigation view with telematics features by
        # matching the vehicle chassis number across both gold datasets.
        aggregated_telematics_df,
        on="chassis_no",
        how="left",
    )
