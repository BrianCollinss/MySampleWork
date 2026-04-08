"""Upload a compressed mock batch into the S3 prefix watched by Snowpipe.

This is a shared project utility, not a Python pipeline step. Use it to simulate a
new file landing in the SQL or Python folder on S3 after the Snowpipe resources
have been created.
"""

import argparse
import csv
import gzip
import io
import os
import sys
from datetime import UTC, datetime, timedelta
from pathlib import Path

import boto3
from botocore.exceptions import NoCredentialsError

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from snowflake_project.connection import load_environment


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Upload a mock batch file to S3 for Snowpipe.")
    # The target prefix determines whether the file lands under the SQL or Python pipeline path in the bucket.
    parser.add_argument("--target-prefix", choices=("sql", "py"), default="py")
    parser.add_argument("--source-file", default="data/mock_orders_seed.csv")
    return parser.parse_args()


def build_generated_batch(source_path: Path, batch_timestamp: datetime) -> tuple[Path, bytes]:
    """Create a fresh CSV batch for this run and return its path plus payload bytes."""
    generated_dir = source_path.parent / "generated"
    generated_dir.mkdir(exist_ok=True)

    batch_token = batch_timestamp.strftime("%Y%m%d%H%M%S")
    generated_path = generated_dir / f"mock_orders_batch_{batch_token}.csv"

    with source_path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        fieldnames = list(reader.fieldnames or [])
        rows = list(reader)

    if not fieldnames:
        raise ValueError(f"Seed file is missing a CSV header: {source_path}")

    output = io.StringIO(newline="")
    writer = csv.DictWriter(output, fieldnames=fieldnames)
    writer.writeheader()

    # Shift keys and timestamps on every run so Snowpipe sees a truly new batch
    # and the downstream Silver merge has distinct records to work with.
    key_offset = int(batch_timestamp.strftime("%H%M%S"))
    for index, row in enumerate(rows):
        generated_row = dict(row)
        generated_row["order_key"] = str(int(row["order_key"]) + key_offset)
        generated_row["cust_key"] = str(int(row["cust_key"]) + index + 1)
        generated_row["ingested_at"] = (batch_timestamp + timedelta(minutes=index)).strftime("%Y-%m-%d %H:%M:%S")
        generated_row["order_comment"] = f"{row['order_comment']} | generated batch {batch_token}"
        writer.writerow(generated_row)

    payload = output.getvalue().encode("utf-8")
    generated_path.write_bytes(payload)
    return generated_path, payload


def main() -> None:
    # Load AWS settings from the same project .env file used by the Snowflake setup steps.
    load_environment()
    args = parse_args()

    bucket_url = os.environ["AWS_S3_BUCKET_URL"].rstrip("/")
    if not bucket_url.startswith("s3://"):
        raise ValueError("AWS_S3_BUCKET_URL must start with s3://")

    bucket_name, _, base_prefix = bucket_url[5:].partition("/")
    target_prefix = "/".join(part for part in (base_prefix, args.target_prefix) if part)

    source_path = Path(args.source_file)
    if not source_path.exists():
        raise FileNotFoundError(f"Seed file not found: {source_path}")

    # Generate a fresh CSV batch locally on every run, then compress it so the
    # uploaded object matches the `.csv.gz` event filter used by Snowpipe.
    batch_time = datetime.now(UTC)
    generated_path, payload = build_generated_batch(source_path, batch_time)
    timestamp = batch_time.strftime("%Y%m%dT%H%M%SZ")
    object_key = f"{target_prefix}/orders_batch_{timestamp}.csv.gz"
    compressed_payload = gzip.compress(payload)

    # Allow the script to reuse a named AWS CLI profile when one is provided.
    aws_profile = os.getenv("AWS_PROFILE")
    session = boto3.Session(profile_name=aws_profile, region_name=os.getenv("AWS_REGION"))

    try:
        session.client("s3").put_object(
            Bucket=bucket_name,
            Key=object_key,
            Body=compressed_payload,
            ContentType="text/csv",
            ContentEncoding="gzip",
        )
    except NoCredentialsError as exc:
        profile_hint = f"AWS_PROFILE={aws_profile}" if aws_profile else "no AWS_PROFILE was set"
        raise SystemExit(
            "AWS credentials were not found for boto3. "
            f"The uploader checked the default credential chain and {profile_hint}. "
            "Authenticate with `aws configure`, `aws configure sso`, or set "
            "`AWS_ACCESS_KEY_ID` / `AWS_SECRET_ACCESS_KEY` / `AWS_SESSION_TOKEN` as needed."
        ) from exc

    print(f"Generated {generated_path}")
    print(f"Uploaded {generated_path} to s3://{bucket_name}/{object_key}")


if __name__ == "__main__":
    main()
