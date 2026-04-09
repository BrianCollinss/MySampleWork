"""
Upload the current local demo files into the landing volume as the base dataset.

Run this once before starting the continuous demo feed. The bronze pipeline will
ingest these landed files as the initial state of the system.
"""


from __future__ import annotations

import sys
from pathlib import Path
import shutil


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
DATA_DIR = REPO_ROOT / "data"
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from pipelines.common import ensure_schemas_and_volume, get_spark_session, load_configs


def clear_directory(directory: Path) -> None:
    """Remove all files and folders from a directory, creating it if required."""

    # Reset the landing volume to a known starting state before re-uploading the
    # base demo dataset. This keeps repeated demo runs deterministic.
    if not directory.exists():
        directory.mkdir(parents=True, exist_ok=True)
        return

    for child in directory.iterdir():
        if child.is_dir():
            shutil.rmtree(child)
        else:
            child.unlink()


def copy_files(source_files: list[Path], destination_dir: Path) -> int:
    """Copy a list of files into the destination directory and return the count."""
    destination_dir.mkdir(parents=True, exist_ok=True)
    for source_file in source_files:
        # Preserve the original filenames because the base upload represents the
        # fixed starting state of the source systems.
        shutil.copy2(source_file, destination_dir / source_file.name)
    return len(source_files)


spark = get_spark_session()
settings = load_configs(spark)
ensure_schemas_and_volume(spark, settings)

# Wipe the full landing volume so each base upload recreates the same one-off
# source state for bronze ingestion and downstream demo runs.
clear_directory(Path(settings.volume()))

copied_telematics = copy_files(
    list((DATA_DIR / "telematics").glob("*.parquet")),
    Path(settings.volume("telematics")),
)
copied_policies = copy_files(
    [DATA_DIR / "csv_inputs" / "policies.csv"],
    Path(settings.volume("csv_inputs", "policies")),
)
copied_claims = copy_files(
    [DATA_DIR / "csv_inputs" / "claims.csv"],
    Path(settings.volume("csv_inputs", "claims")),
)
copied_customers = copy_files(
    [DATA_DIR / "csv_inputs" / "customers.csv"],
    Path(settings.volume("csv_inputs", "customers")),
)
copied_training_images = copy_files(
    list((DATA_DIR / "training_imgs").glob("*.png")),
    Path(settings.volume("training_imgs")),
)
copied_claim_images = copy_files(
    list((DATA_DIR / "claims" / "images").glob("*.*")),
    Path(settings.volume("claims", "images")),
)
copied_claim_metadata = copy_files(
    list((DATA_DIR / "claims" / "metadata").glob("*.csv")),
    Path(settings.volume("claims", "metadata")),
)

print("Base demo data uploaded to the landing volume.")
print("Batch folder: base paths without timestamp")
print(f"Telematics files: {copied_telematics}")
print(f"Policies CSV files: {copied_policies}")
print(f"Claims CSV files: {copied_claims}")
print(f"Customers CSV files: {copied_customers}")
print(f"Training images: {copied_training_images}")
print(f"Claim images: {copied_claim_images}")
print(f"Claim metadata files: {copied_claim_metadata}")
