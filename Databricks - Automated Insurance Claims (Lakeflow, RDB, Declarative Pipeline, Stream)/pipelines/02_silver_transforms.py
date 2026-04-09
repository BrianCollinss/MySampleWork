"""
Step 2: define silver materialized views with Databricks declarative pipelines.

This file is intended to be registered as a Databricks pipeline whose target
schema is the silver schema. It reads curated source tables from the bronze
schema and standardizes them into analytics-friendly shapes.
"""


from __future__ import annotations

import os
from dataclasses import dataclass

from pyspark import pipelines as dp
from pyspark.errors import AnalysisException
from pyspark.sql import DataFrame, Window
from pyspark.sql import SparkSession
from pyspark.sql.functions import (
    abs,
    col,
    concat,
    initcap,
    lit,
    regexp_extract,
    size,
    split,
    to_date,
    to_timestamp,
    trim,
    when,
    row_number,
)


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
    """Load the silver pipeline settings from Spark configuration."""
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


def resolve_col(df: DataFrame, *names: str):
    """Return the first matching column from a list of acceptable names."""

    # Bronze files come from slightly different source conventions, so silver
    # resolves the first matching column name instead of assuming one exact case.
    columns = {column.lower(): column for column in df.columns}
    for name in names:
        actual = columns.get(name.lower())
        if actual:
            return col(actual)
    raise KeyError(f"None of these columns exist: {names}")


def keep_latest_record(df: DataFrame, business_key: str) -> DataFrame:
    """Keep only the most recently landed record for each business key."""

    # The demo lands repeated snapshots for the same keys. Silver keeps only the
    # most recently landed version so gold reads a stable current-state table.
    latest_window = Window.partitionBy(business_key).orderBy(
        col("bronze_loaded_at").desc(),
        col("source_file").desc(),
    )
    return (
        df.withColumn("_record_rank", row_number().over(latest_window))
        .where(col("_record_rank") == 1)
        .drop("_record_rank")
    )


@dp.materialized_view(
    name="telematics",
    comment="Silver telematics records with typed coordinates, timestamps, and speed.",
)
def telematics() -> DataFrame:
    """Standardize telematics coordinates, speed, and event timestamps."""

    return (
        spark.read.table(settings.table(settings.bronze_schema, "telematics"))
        # Step 1: cast the raw landed values into analytics-friendly numeric types.
        .withColumn("latitude", col("latitude").cast("double"))
        .withColumn("longitude", col("longitude").cast("double"))
        .withColumn("speed", col("speed").cast("double"))
        # Step 2: convert the landed string timestamp into a real timestamp column.
        .withColumn("event_timestamp", to_timestamp(col("event_timestamp"), "yyyy-MM-dd HH:mm:ss"))
        # Step 3: drop obviously invalid coordinates before the data reaches gold.
        .where(col("latitude").between(-90, 90) & col("longitude").between(-180, 180))
    )


@dp.materialized_view(
    name="policy",
    comment="Silver policy records with standardized types and business keys.",
)
def policy() -> DataFrame:
    """Standardize policy snapshots and keep the latest row per policy number."""

    source_df = spark.read.table(settings.table(settings.bronze_schema, "policy"))
    # Normalize types up front so policy joins in gold do not need to reason
    # about CSV strings versus numeric/date fields.
    # Step 1: select the expected policy fields and cast them into their target types.
    standardized_df = source_df.select(
        resolve_col(source_df, "policy_no").cast("string").alias("policy_no"),
        resolve_col(source_df, "cust_id").cast("string").alias("cust_id"),
        resolve_col(source_df, "policytype").cast("string").alias("policytype"),
        to_date(resolve_col(source_df, "pol_issue_date")).alias("pol_issue_date"),
        to_date(resolve_col(source_df, "pol_eff_date")).alias("pol_eff_date"),
        to_date(resolve_col(source_df, "pol_expiry_date")).alias("pol_expiry_date"),
        resolve_col(source_df, "make").cast("string").alias("make"),
        resolve_col(source_df, "model").cast("string").alias("model"),
        resolve_col(source_df, "model_year").cast("int").alias("model_year"),
        resolve_col(source_df, "chassis_no").cast("string").alias("chassis_no"),
        resolve_col(source_df, "use_of_vehicle").cast("string").alias("use_of_vehicle"),
        resolve_col(source_df, "product").cast("string").alias("product"),
        resolve_col(source_df, "sum_insured").cast("double").alias("sum_insured"),
        abs(resolve_col(source_df, "premium").cast("double")).alias("premium"),
        resolve_col(source_df, "deductable").cast("int").alias("deductable"),
        col("source_file"),
        col("bronze_loaded_at"),
    # Step 2: ignore malformed rows that do not have a usable business key.
    ).where(col("policy_no").isNotNull())
    # Step 3: keep only the latest landed record for each policy number.
    return keep_latest_record(standardized_df, "policy_no")


@dp.materialized_view(
    name="claim",
    comment="Silver claim records with standardized dates, amounts, and incident details.",
)
def claim() -> DataFrame:
    """Standardize claim snapshots and keep the latest row per claim number."""

    source_df = spark.read.table(settings.table(settings.bronze_schema, "claim"))
    standardized_df = (
        # Step 1: normalize the claim schema and map alternate source column names
        # into one consistent silver representation.
        source_df.select(
            resolve_col(source_df, "claim_no").cast("string").alias("claim_no"),
            resolve_col(source_df, "policy_no").cast("string").alias("policy_no"),
            to_date(resolve_col(source_df, "claim_date")).alias("claim_date"),
            resolve_col(source_df, "months_as_customer").cast("int").alias("months_as_customer"),
            resolve_col(source_df, "injury").cast("double").alias("injury"),
            resolve_col(source_df, "property").cast("double").alias("property"),
            resolve_col(source_df, "vehicle").cast("double").alias("vehicle"),
            resolve_col(source_df, "total").cast("double").alias("total"),
            resolve_col(source_df, "collision_type").cast("string").alias("collision_type"),
            resolve_col(source_df, "number_of_vehicles_involved").cast("int").alias("number_of_vehicles_involved"),
            resolve_col(source_df, "driver_age", "age").cast("double").alias("driver_age"),
            resolve_col(source_df, "insured_relationship").cast("string").alias("insured_relationship"),
            to_date(resolve_col(source_df, "license_issue_date"), "dd-MM-yyyy").alias("license_issue_date"),
            to_date(resolve_col(source_df, "incident_date", "date"), "yyyy-MM-dd").alias("incident_date"),
            resolve_col(source_df, "incident_hour", "hour").cast("int").alias("incident_hour"),
            resolve_col(source_df, "incident_type", "type").cast("string").alias("incident_type"),
            resolve_col(source_df, "incident_severity", "severity").cast("string").alias("incident_severity"),
            resolve_col(source_df, "number_of_witnesses").cast("int").alias("number_of_witnesses"),
            resolve_col(source_df, "suspicious_activity").cast("string").alias("suspicious_activity"),
            col("source_file"),
            col("bronze_loaded_at"),
        )
        # Step 2: require a valid claim key before the row can move downstream.
        .where(col("claim_no").isNotNull())
        # Step 3: drop rows with impossible incident hours from the demo stream.
        .where(col("incident_hour").between(0, 23))
    )
    # Step 4: keep only the most recent snapshot for each claim number.
    return keep_latest_record(standardized_df, "claim_no")


@dp.materialized_view(
    name="customer",
    comment="Silver customer records with normalized names and location fields.",
)
def customer() -> DataFrame:
    """Normalize customer names and location fields for downstream joins."""

    source_df = spark.read.table(settings.table(settings.bronze_schema, "customer"))
    # Support both "Lastname, Firstname" and "Firstname Lastname" inputs from
    # the sample data while still producing consistent silver columns.
    normalized_name = when(
        size(split(trim(resolve_col(source_df, "name")), ",")) == 2,
        concat(
            initcap(trim(split(resolve_col(source_df, "name"), ",").getItem(1))),
            lit(" "),
            initcap(trim(split(resolve_col(source_df, "name"), ",").getItem(0))),
        ),
    ).otherwise(initcap(trim(resolve_col(source_df, "name"))))

    standardized_df = (
        # Step 1: cast the landed customer fields into their target schema.
        source_df.select(
            resolve_col(source_df, "customer_id").cast("string").alias("customer_id"),
            to_date(resolve_col(source_df, "date_of_birth"), "dd-MM-yyyy").alias("date_of_birth"),
            resolve_col(source_df, "borough").cast("string").alias("borough"),
            resolve_col(source_df, "neighborhood").cast("string").alias("neighborhood"),
            resolve_col(source_df, "zip_code").cast("string").alias("zip_code"),
            normalized_name.alias("full_name"),
            col("source_file"),
            col("bronze_loaded_at"),
        )
        # Step 2: split the normalized full name into separate first/last columns.
        .withColumn("firstname", split(col("full_name"), " ").getItem(0))
        .withColumn("lastname", regexp_extract(col("full_name"), r"^\S+\s+(.*)$", 1))
        # Step 3: derive a simple address field for downstream serving and demos.
        .withColumn("address", concat(col("borough"), lit(", "), col("zip_code")))
        .drop("full_name")
        # Step 4: require a customer key before deduplicating repeated snapshots.
        .where(col("customer_id").isNotNull())
    )
    # Step 5: keep only the latest landed customer record.
    return keep_latest_record(standardized_df, "customer_id")


@dp.materialized_view(
    name="training_images",
    comment="Silver training images enriched with extracted image ids and labels.",
)
def training_images() -> DataFrame:
    """Extract image ids and class labels from the training-image filenames."""

    return (
        spark.read.table(settings.table(settings.bronze_schema, "training_images"))
        # The file naming convention carries the label, so silver extracts it
        # once here instead of repeating regex parsing in ML or serving layers.
        # Step 1: pull the numeric image id out of the training filename.
        .withColumn(
            "image_id",
            regexp_extract(col("path"), r"/(\d+)-([a-zA-Z]+)(?: \(\d+\))?\.png$", 1),
        )
        # Step 2: pull the class label out of the same filename.
        .withColumn(
            "label",
            regexp_extract(col("path"), r"/(\d+)-([a-zA-Z]+)(?: \(\d+\))?\.png$", 2),
        )
    )


@dp.materialized_view(
    name="claim_images",
    comment="Silver claim images with extracted image filenames.",
)
def claim_images() -> DataFrame:
    """Extract landed claim-image filenames from the binary file paths."""
    return spark.read.table(settings.table(settings.bronze_schema, "claim_images")).withColumn(
        # The binary file path is the only reliable source of the landed image
        # filename, which is later used to join onto the image metadata table.
        # Step 1: extract just the final filename from the full storage path.
        "image_name",
        regexp_extract(col("path"), r".*/([^/]+\.(?i:jpg|jpeg|png))$", 1),
    )


@dp.materialized_view(
    name="claim_images_meta",
    comment="Silver claim image metadata copied from bronze for downstream joins.",
)
def claim_images_meta() -> DataFrame:
    """Pass claim-image metadata into silver without changing the schema."""
    # Step 1: keep the metadata as-is in silver because it already carries the
    # business link between image_name and claim_no.
    return spark.read.table(settings.table(settings.bronze_schema, "claim_images_meta"))
