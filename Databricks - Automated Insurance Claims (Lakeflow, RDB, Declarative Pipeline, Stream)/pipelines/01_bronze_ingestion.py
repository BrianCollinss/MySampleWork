"""
Step 1: define bronze streaming tables with Databricks declarative pipelines.

This file is intended to be registered as a Databricks pipeline whose target
schema is the bronze schema. It reads landed files from a Unity Catalog volume.
The telematics folder acts as the Kinesis replacement for demos.

Demo note:
This project keeps the DLT bronze path visible because the repo is meant to
show Spark and Lakeflow / DLT skills in a single demo. The
``auto_claims.bronze.use_dlt`` switch exists because the current demo
environment has practical operational constraints: only one pipeline can be run
at a time, and continuous bronze execution is not always available or reliable
for showing Auto Loader archive behavior. In a real implementation, one bronze
ingestion path would be chosen and maintained rather than carrying both options.
"""


from __future__ import annotations

import os
import shutil
from dataclasses import dataclass
from pathlib import Path

from pyspark import pipelines as dp
from pyspark.errors import AnalysisException
from pyspark.sql import DataFrame
from pyspark.sql import SparkSession
from pyspark.sql.functions import col, current_timestamp


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
        clean_parts = [part.strip("/") for part in parts if part]
        return f"/Volumes/{self.catalog}/{self.landing_schema}/{self.landing_volume}/{'/'.join(clean_parts)}"


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


def read_bool_config(spark_session: SparkSession, key: str, default: bool) -> bool:
    """Read a boolean config value using common truthy strings."""
    value = read_config(spark_session, key, str(default).lower()).strip().lower()
    return value in {"1", "true", "yes", "y", "on"}


def load_configs(spark_session: SparkSession) -> Settings:
    """Load the bronze pipeline settings from Spark configuration."""
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
use_dlt = read_bool_config(spark, "auto_claims.bronze.use_dlt", True)


def _matching_files(directory: Path, suffixes: tuple[str, ...]) -> list[Path]:
    """Return landed business files in a directory, excluding support folders."""

    if not directory.exists():
        return []

    # Ignore Auto Loader support folders such as _schema and only process the
    # landed source files that represent business data.
    return [
        child
        for child in directory.iterdir()
        if child.is_file() and child.suffix.lower() in suffixes and not child.name.startswith("_")
    ]


def _archive_files(source_dir: Path, archive_dir: Path, files: list[Path]) -> int:
    """Move processed source files into the matching archive directory."""

    archive_dir.mkdir(parents=True, exist_ok=True)
    moved_count = 0
    for file_path in files:
        # Overwrite any existing archive file with the same name so repeated demo
        # resets do not fail on leftover artifacts.
        target_path = archive_dir / file_path.name
        if target_path.exists():
            if target_path.is_dir():
                shutil.rmtree(target_path)
            else:
                target_path.unlink()
        shutil.move(str(file_path), str(target_path))
        moved_count += 1
    return moved_count


def _overwrite_bronze_table(table_name: str, dataframe: DataFrame) -> None:
    """Replace a bronze table with the current batch DataFrame."""

    # The non-DLT path rewrites the current bronze tables from whatever is
    # sitting in landing right now, which keeps the demo deterministic.
    dataframe.write.mode("overwrite").option("overwriteSchema", "true").saveAsTable(
        settings.table(settings.bronze_schema, table_name)
    )


def _batch_telematics() -> DataFrame:
    """Load landed telematics parquet files into the bronze telematics schema."""

    return (
        spark.read.format("parquet")
        .load(settings.volume("telematics"))
        .select(
            # Step 1: keep the business payload columns needed downstream.
            col("chassis_no"),
            col("latitude").cast("double").alias("latitude"),
            col("longitude").cast("double").alias("longitude"),
            col("event_timestamp"),
            col("speed").cast("double").alias("speed"),
            # Step 2: add file lineage and ingest time so the DLT and non-DLT
            # bronze schemas stay aligned.
            col("_metadata.file_path").alias("source_file"),
            current_timestamp().alias("ingested_at"),
        )
    )


def _batch_training_images() -> DataFrame:
    """Load landed training images as a binary-file DataFrame."""
    return spark.read.format("binaryFile").load(settings.volume("training_imgs"))


def _batch_claim_images() -> DataFrame:
    """Load landed claim images as a binary-file DataFrame."""
    return spark.read.format("binaryFile").load(settings.volume("claims", "images"))


def _batch_claim_images_meta() -> DataFrame:
    """Load landed claim-image metadata CSV files into the bronze schema."""

    return (
        spark.read.format("csv")
        .option("header", "true")
        .load(settings.volume("claims", "metadata"))
        .select(
            col("*"),
            col("_metadata.file_path").alias("source_file"),
            current_timestamp().alias("bronze_loaded_at"),
        )
    )


def _batch_csv_input(source_name: str) -> DataFrame:
    """Load one landed CSV snapshot source into the shared bronze CSV schema."""

    return (
        spark.read.format("csv")
        .option("header", "true")
        .load(settings.volume("csv_inputs", source_name))
        .select(
            col("*"),
            col("_metadata.file_path").alias("source_file"),
            current_timestamp().alias("bronze_loaded_at"),
        )
    )


def _run_non_dlt_bronze_ingestion() -> None:
    """Run the fallback non-DLT bronze load and manual archive flow."""

    # In the non-DLT demo path, bronze ingestion behaves like a regular Spark
    # batch job: load everything currently in landing, write the bronze tables,
    # then archive the processed source files manually.
    spark.sql(f"CREATE SCHEMA IF NOT EXISTS {settings.catalog}.{settings.bronze_schema}")

    telematics_dir = Path(settings.volume("telematics"))
    telematics_files = _matching_files(telematics_dir, (".parquet",))
    if telematics_files:
        # Step 1: read the landed telematics parquet files into the bronze table.
        _overwrite_bronze_table("telematics", _batch_telematics())
        # Step 2: move the processed source files into the archive folder.
        moved = _archive_files(telematics_dir, Path(settings.volume("telematics_archive")), telematics_files)
        print(f"Non-DLT bronze ingestion loaded telematics and archived {moved} parquet files.")

    training_images_dir = Path(settings.volume("training_imgs"))
    training_image_files = _matching_files(training_images_dir, (".png", ".jpg", ".jpeg"))
    if training_image_files:
        # Training images are intentionally left in place because they are reused
        # later by the demo stream generator and ML work.
        # Step 1: refresh the bronze training image table from the static source folder.
        _overwrite_bronze_table("training_images", _batch_training_images())
        print(f"Non-DLT bronze ingestion loaded {len(training_image_files)} training images without archiving them.")

    claim_images_dir = Path(settings.volume("claims", "images"))
    claim_image_files = _matching_files(claim_images_dir, (".png", ".jpg", ".jpeg"))
    if claim_image_files:
        # Step 1: load the binary claim images into bronze.
        _overwrite_bronze_table("claim_images", _batch_claim_images())
        # Step 2: archive the landed image files after the table refresh.
        moved = _archive_files(
            claim_images_dir,
            Path(settings.volume("claims", "images_archive")),
            claim_image_files,
        )
        print(f"Non-DLT bronze ingestion loaded claim images and archived {moved} files.")

    claim_metadata_dir = Path(settings.volume("claims", "metadata"))
    claim_metadata_files = _matching_files(claim_metadata_dir, (".csv",))
    if claim_metadata_files:
        # Step 1: load the claim-image metadata rows into bronze.
        _overwrite_bronze_table("claim_images_meta", _batch_claim_images_meta())
        # Step 2: archive the processed metadata files.
        moved = _archive_files(
            claim_metadata_dir,
            Path(settings.volume("claims", "metadata_archive")),
            claim_metadata_files,
        )
        print(f"Non-DLT bronze ingestion loaded claim image metadata and archived {moved} CSV files.")

    for source_name, table_name in (
        ("policies", "policy"),
        ("claims", "claim"),
        ("customers", "customer"),
    ):
        source_dir = Path(settings.volume("csv_inputs", source_name))
        source_files = _matching_files(source_dir, (".csv",))
        if source_files:
            # Step 1: refresh the bronze snapshot table from the currently landed CSV files.
            _overwrite_bronze_table(table_name, _batch_csv_input(source_name))
            # Step 2: archive the CSV files once that snapshot has been written.
            moved = _archive_files(
                source_dir,
                Path(settings.volume("csv_inputs", f"{source_name}_archive")),
                source_files,
            )
            print(f"Non-DLT bronze ingestion loaded {table_name} and archived {moved} CSV files.")


def _run_dlt_bronze_ingestion() -> None:
    """Register the DLT bronze tables for Auto Loader ingestion."""

    @dp.table(
        name="telematics",
        comment="Bronze telematics event stream loaded from landed parquet files.",
    )
    def telematics() -> DataFrame:
        """Read landed telematics parquet files into the bronze telematics table."""
        return (
            spark.readStream.format("cloudFiles")
            .option("cloudFiles.format", "parquet")
            .option("cloudFiles.schemaLocation", settings.volume("telematics", "_schema"))
            # Keep the Auto Loader cleanup options visible in the demo even though
            # triggered pipeline execution can make the archive timing hard to show.
            .option("cloudFiles.cleanSource", "MOVE")
            .option("cloudFiles.cleanSource.retentionDuration", "1 minute")
            .option("cloudFiles.cleanSource.moveDestination", settings.volume("telematics_archive"))
            .load(settings.volume("telematics"))
            .select(
                col("chassis_no"),
                col("latitude").cast("double").alias("latitude"),
                col("longitude").cast("double").alias("longitude"),
                col("event_timestamp"),
                col("speed").cast("double").alias("speed"),
                col("_metadata.file_path").alias("source_file"),
                current_timestamp().alias("ingested_at"),
            )
        )


    @dp.table(
        name="training_images",
        comment="Bronze binary training images loaded from the landing volume.",
    )
    def training_images() -> DataFrame:
        """Read landed training images into the bronze binary-image table."""
        return (
            spark.readStream.format("cloudFiles")
            .option("cloudFiles.format", "binaryFile")
            .option("cloudFiles.schemaLocation", settings.volume("training_imgs", "_schema"))
            .load(settings.volume("training_imgs"))
        )


    @dp.table(
        name="claim_images",
        comment="Bronze claim images loaded from the landing volume.",
    )
    def claim_images() -> DataFrame:
        """Read landed claim images into the bronze binary-image table."""
        return (
            spark.readStream.format("cloudFiles")
            .option("cloudFiles.format", "binaryFile")
            .option("cloudFiles.schemaLocation", settings.volume("claims", "images", "_schema"))
            .option("cloudFiles.cleanSource", "MOVE")
            .option("cloudFiles.cleanSource.retentionDuration", "1 minute")
            .option("cloudFiles.cleanSource.moveDestination", settings.volume("claims", "images_archive"))
            .load(settings.volume("claims", "images"))
        )


    @dp.table(
        name="claim_images_meta",
        comment="Bronze claim image metadata loaded from landed CSV files.",
    )
    def claim_images_meta() -> DataFrame:
        """Read landed claim-image metadata CSV files into bronze."""
        return (
            spark.readStream.format("cloudFiles")
            .option("cloudFiles.format", "csv")
            .option("cloudFiles.schemaLocation", settings.volume("claims", "metadata", "_schema"))
            .option("cloudFiles.cleanSource", "MOVE")
            .option("cloudFiles.cleanSource.retentionDuration", "1 minute")
            .option("cloudFiles.cleanSource.moveDestination", settings.volume("claims", "metadata_archive"))
            .option("header", "true")
            .load(settings.volume("claims", "metadata"))
            .select(
                col("*"),
                # Preserve source lineage so silver can keep the most recent version
                # of a business key when multiple files land over time.
                col("_metadata.file_path").alias("source_file"),
                current_timestamp().alias("bronze_loaded_at"),
            )
        )


    def csv_input_stream(source_name: str) -> DataFrame:
        """Read a landed CSV source stream with shared bronze lineage columns."""
        return (
            spark.readStream.format("cloudFiles")
            .option("cloudFiles.format", "csv")
            .option("cloudFiles.schemaLocation", settings.volume("csv_inputs", source_name, "_schema"))
            .option("cloudFiles.cleanSource", "MOVE")
            .option("cloudFiles.cleanSource.retentionDuration", "1 minute")
            .option("cloudFiles.cleanSource.moveDestination", settings.volume("csv_inputs", f"{source_name}_archive"))
            .option("header", "true")
            .load(settings.volume("csv_inputs", source_name))
            .select(
                col("*"),
                # These operational snapshots are landed repeatedly, so downstream
                # layers need file lineage and arrival time for deduplication.
                col("_metadata.file_path").alias("source_file"),
                current_timestamp().alias("bronze_loaded_at"),
            )
        )


    @dp.table(
        name="policy",
        comment="Bronze policy snapshot landed as CSV files.",
    )
    def policy() -> DataFrame:
        """Read landed policy CSV snapshots into the bronze policy table."""
        return csv_input_stream("policies")


    @dp.table(
        name="claim",
        comment="Bronze claim snapshot landed as CSV files.",
    )
    def claim() -> DataFrame:
        """Read landed claim CSV snapshots into the bronze claim table."""
        return csv_input_stream("claims")


    @dp.table(
        name="customer",
        comment="Bronze customer snapshot landed as CSV files.",
    )
    def customer() -> DataFrame:
        """Read landed customer CSV snapshots into the bronze customer table."""
        return csv_input_stream("customers")


if use_dlt:
    _run_dlt_bronze_ingestion()
else:
    _run_non_dlt_bronze_ingestion()
