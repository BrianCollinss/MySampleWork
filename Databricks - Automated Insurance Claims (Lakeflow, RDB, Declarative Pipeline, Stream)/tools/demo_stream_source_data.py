"""
Continuously land small demo batches into the landing volume every 30 seconds.

Run this job while the Databricks pipelines are in continuous mode. Stop the job
when you want the demo source feed to stop.
"""


from __future__ import annotations

import csv
import random
import shutil
import sys
import time
import uuid
from datetime import datetime, timedelta, timezone
from pathlib import Path

from pyspark.sql import Row, SparkSession


def resolve_repo_root() -> Path:
    """Resolve the repository root whether the script runs locally or in Databricks."""
    script_name = globals().get("__file__") or globals().get("filename")
    if script_name:
        return Path(str(script_name)).resolve().parents[1]

    if sys.argv and sys.argv[0]:
        candidate = Path(sys.argv[0])
        if candidate.exists():
            return candidate.resolve().parents[1]

    return Path.cwd()


REPO_ROOT = resolve_repo_root()
SAMPLE_CSV_DIR = REPO_ROOT / "data" / "csv_inputs"
SAMPLE_TRAINING_IMAGES_DIR = REPO_ROOT / "data" / "training_imgs"
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from pipelines.common import Settings, ensure_schemas_and_volume, get_spark_session, load_configs

MIN_ROWS_PER_BATCH = 1
MAX_ROWS_PER_BATCH = 3
SLEEP_SECONDS = 30
CLAIM_IMAGES_PER_CLAIM = 2

def load_csv_rows(path: Path) -> list[dict[str, str]]:
    """Load sample CSV rows into memory for repeated batch generation."""
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def current_batch_id() -> str:
    """Return a UTC timestamp token used in generated live filenames."""
    return datetime.now(timezone.utc).strftime("live_%Y%m%d_%H%M%S")


def random_batch_size() -> int:
    """Return the random number of source rows to emit for a live batch."""
    return random.randint(MIN_ROWS_PER_BATCH, MAX_ROWS_PER_BATCH)


def write_single_output_file(
    df,
    destination_dir: Path,
    file_prefix: str,
    extension: str,
    writer_format: str,
    *,
    header: bool = False,
) -> None:
    """Write a Spark DataFrame as one uniquely named landed file."""

    destination_dir.mkdir(parents=True, exist_ok=True)
    token = f"{current_batch_id()}_{uuid.uuid4().hex[:8]}"
    # Spark writes directory outputs, so each live batch is first materialized
    # into a temporary folder and then collapsed into one uniquely named file in
    # the final landing directory.
    staging_dir = destination_dir / f"_staging_{file_prefix}_{token}"
    writer = df.coalesce(1).write.mode("overwrite")
    if header:
        writer = writer.option("header", "true")
    getattr(writer, writer_format)(str(staging_dir))

    staged_file = next(staging_dir.glob("part-*"))
    final_path = destination_dir / f"{file_prefix}_{token}.{extension}"
    shutil.move(str(staged_file), str(final_path))
    shutil.rmtree(staging_dir)


def upload_telematics_batch(spark: SparkSession, settings: Settings, policies: list[dict[str, str]]) -> int:
    """Generate and land one synthetic telematics parquet batch."""

    batch_size = random_batch_size()
    landed_at = datetime.now(timezone.utc)
    rows = []
    for _ in range(batch_size):
        policy = random.choice(policies)
        rows.append(
            Row(
                # Reuse real chassis numbers from the policy sample so telematics
                # can still be joined downstream to policy and claim records.
                chassis_no=str(policy["CHASSIS_NO"]).strip(),
                latitude=round(random.uniform(-27.80, -27.20), 4),
                longitude=round(random.uniform(152.70, 153.40), 4),
                event_timestamp=(landed_at - timedelta(seconds=random.randint(0, 180))).strftime(
                    "%Y-%m-%d %H:%M:%S"
                ),
                speed=round(random.uniform(0, 115), 1),
            )
        )

    write_single_output_file(
        spark.createDataFrame(rows),
        Path(settings.volume("telematics")),
        "telematics",
        "parquet",
        "parquet",
    )
    return batch_size


def upload_policy_batch(spark: SparkSession, settings: Settings, policies: list[dict[str, str]]) -> int:
    """Generate and land one policy CSV batch from sampled policy rows."""

    selected_rows = random.sample(policies, random_batch_size())
    rows = []
    for row in selected_rows:
        rows.append(
            Row(
                policy_no=str(row["POLICY_NO"]).strip(),
                cust_id=str(row["CUST_ID"]).strip(),
                policytype=row["POLICYTYPE"],
                pol_issue_date=row["POL_ISSUE_DATE"],
                pol_eff_date=row["POL_EFF_DATE"],
                pol_expiry_date=row["POL_EXPIRY_DATE"],
                make=row["MAKE"],
                model=row["MODEL"],
                model_year=row["MODEL_YEAR"],
                chassis_no=str(row["CHASSIS_NO"]).strip(),
                use_of_vehicle=row["USE_OF_VEHICLE"],
                product=row["PRODUCT"],
                # Slight perturbations make each landed snapshot look like a new
                # operational update instead of a byte-for-byte duplicate.
                sum_insured=str(float(row["SUM_INSURED"]) + random.randint(0, 500)),
                premium=str(abs(float(row["PREMIUM"])) + random.randint(0, 50)),
                deductable=row["DEDUCTABLE"],
            )
        )

    write_single_output_file(
        spark.createDataFrame(rows),
        Path(settings.volume("csv_inputs", "policies")),
        "policies",
        "csv",
        "csv",
        header=True,
    )
    return len(rows)


def upload_claim_batch(spark: SparkSession, settings: Settings, claims: list[dict[str, str]]) -> int:
    """Generate and land one claim CSV batch from sampled claim rows."""

    selected_rows = random.sample(claims, random_batch_size())
    rows = []
    for row in selected_rows:
        rows.append(
            Row(
                claim_no=row["claim_no"],
                policy_no=str(row["policy_no"]).strip(),
                claim_date=row["claim_date"],
                months_as_customer=row["months_as_customer"],
                injury=str(float(row["injury"]) + random.randint(0, 50)),
                property=str(float(row["property"]) + random.randint(0, 50)),
                vehicle=str(float(row["vehicle"]) + random.randint(0, 50)),
                total=str(float(row["total"]) + random.randint(0, 100)),
                collision_type=row["collision_type"],
                number_of_vehicles_involved=row["number_of_vehicles_involved"],
                age=row["age"],
                insured_relationship=row["insured_relationship"],
                license_issue_date=row["license_issue_date"],
                date=row["date"],
                hour=str(random.randint(0, 23)),
                type=row["type"],
                severity=row["severity"],
                number_of_witnesses=row["number_of_witnesses"],
                suspicious_activity=row["suspicious_activity"],
            )
        )

    write_single_output_file(
        spark.createDataFrame(rows),
        Path(settings.volume("csv_inputs", "claims")),
        "claims",
        "csv",
        "csv",
        header=True,
    )
    return len(rows)


def upload_customer_batch(spark: SparkSession, settings: Settings, customers: list[dict[str, str]]) -> int:
    """Generate and land one customer CSV batch from sampled customer rows."""

    selected_rows = random.sample(customers, random_batch_size())
    rows = []
    for row in selected_rows:
        rows.append(
            Row(
                customer_id=str(row["customer_id"]).strip(),
                date_of_birth=row["date_of_birth"],
                borough=row["borough"],
                neighborhood=row["neighborhood"],
                zip_code=row["zip_code"],
                name=row["name"],
            )
        )

    write_single_output_file(
        spark.createDataFrame(rows),
        Path(settings.volume("csv_inputs", "customers")),
        "customers",
        "csv",
        "csv",
        header=True,
    )
    return len(rows)


def upload_claim_images_batch(
    spark: SparkSession,
    settings: Settings,
    claims: list[dict[str, str]],
    policies: list[dict[str, str]],
) -> int:
    """Generate claim-image files plus matching metadata rows for one live batch."""

    batch_size = random_batch_size()
    token = f"{current_batch_id()}_{uuid.uuid4().hex[:8]}"
    output_dir = Path(settings.volume("claims", "images"))
    output_dir.mkdir(parents=True, exist_ok=True)

    training_images = list(SAMPLE_TRAINING_IMAGES_DIR.glob("*.png"))
    policy_by_number = {str(row["POLICY_NO"]).strip(): row for row in policies}

    metadata_rows = []
    for index, claim in enumerate(random.sample(claims, batch_size), start=1):
        linked_policy = policy_by_number.get(str(claim["policy_no"]).strip())
        chassis_no = str(linked_policy["CHASSIS_NO"]).strip() if linked_policy else None
        # Emit two images per claim so the downstream image metadata flow can
        # demonstrate one-to-many relationships against a single claim record.
        for image_number in range(1, CLAIM_IMAGES_PER_CLAIM + 1):
            source_image = random.choice(training_images)
            target_name = f"claim_{index}_{image_number}_{token}{source_image.suffix.lower()}"
            shutil.copy2(source_image, output_dir / target_name)

            metadata_rows.append(
                Row(
                    image_name=target_name,
                    # Keep image_id deterministic within the batch so it is easy
                    # to see which two images belong to the same sampled claim.
                    image_id=f"{index}_{image_number}",
                    claim_no=claim["claim_no"],
                    chassis_no=chassis_no,
                )
            )

    write_single_output_file(
        spark.createDataFrame(metadata_rows),
        Path(settings.volume("claims", "metadata")),
        "claim_metadata",
        "csv",
        "csv",
        header=True,
    )
    return len(metadata_rows)


spark = get_spark_session()
settings = load_configs(spark)
ensure_schemas_and_volume(spark, settings)
policy_rows = load_csv_rows(SAMPLE_CSV_DIR / "policies.csv")
claim_rows = load_csv_rows(SAMPLE_CSV_DIR / "claims.csv")
customer_rows = load_csv_rows(SAMPLE_CSV_DIR / "customers.csv")

print("Starting live demo source feed. Stop the job to end the stream.")

try:
    while True:
        # Each loop simulates a fresh micro-batch from the source systems.
        telematics_count = upload_telematics_batch(spark, settings, policy_rows)
        policy_count = upload_policy_batch(spark, settings, policy_rows)
        claim_count = upload_claim_batch(spark, settings, claim_rows)
        customer_count = upload_customer_batch(spark, settings, customer_rows)
        claim_image_count = upload_claim_images_batch(spark, settings, claim_rows, policy_rows)

        print(
            "Landed live batch: "
            f"telematics={telematics_count}, "
            f"policies={policy_count}, "
            f"claims={claim_count}, "
            f"customers={customer_count}, "
            f"claim_images={claim_image_count}"
        )
        time.sleep(SLEEP_SECONDS)
except KeyboardInterrupt:
    print("Live demo source feed stopped.")
