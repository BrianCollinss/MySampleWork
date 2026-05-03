"""NEMWeb ZIP ingestion orchestration shared by local and Fabric notebooks."""

from __future__ import annotations

import hashlib
from dataclasses import dataclass
from datetime import datetime, timezone
from typing import Any, Protocol

from nem_fabric.common_nemweb_client import (
    filter_zip_links_by_lookback,
    get_zip_bytes,
    list_zip_links,
)

RAW_ROOT = "Files/nemweb/raw_zip"
MANIFEST_TABLE = "nem_raw_zip_manifest"
INGESTION_LOG_TABLE = "nem_ingestion_log"


@dataclass(frozen=True)
class IngestionConfig:
    """Runtime parameters for one NEMWeb ingestion run."""

    run_id: str
    source_name: str = ""
    max_zips_per_run: int = 500
    lookback_hours: int = 6
    dry_run: bool = False
    raw_root: str = RAW_ROOT


@dataclass(frozen=True)
class IngestionResult:
    """Summary of one ingestion run."""

    manifest_rows: list[dict[str, Any]]
    log_rows: list[dict[str, Any]]


class IngestionStore(Protocol):
    """Storage operations needed by the shared ingestion algorithm."""

    def read_existing_manifest_urls(self) -> set[str]:
        """Return ZIP URLs already recorded in the ingestion manifest."""
        ...

    def write_binary(self, relative_path: str, content: bytes) -> None:
        """Write one ZIP payload to the raw landing path."""
        ...

    def append_control_rows(
        self,
        manifest_rows: list[dict[str, Any]],
        log_rows: list[dict[str, Any]],
    ) -> None:
        """Append manifest and ingestion log rows."""
        ...


def select_enabled_sources(
    sources_config: dict[str, Any],
    source_name: str = "",
) -> list[dict[str, Any]]:
    """Return enabled source definitions, optionally filtered by source name."""

    sources = [source for source in sources_config["sources"] if source.get("enabled")]
    if source_name:
        sources = [source for source in sources if source["name"] == source_name]
    if not sources:
        raise ValueError(f"No enabled source matched source_name={source_name!r}")
    return sources


def ingest_nemweb_zip_files(
    sources: list[dict[str, Any]],
    store: IngestionStore,
    config: IngestionConfig,
) -> IngestionResult:
    """Discover, download, land, and record NEMWeb ZIP files."""

    existing_urls = store.read_existing_manifest_urls()
    manifest_rows: list[dict[str, Any]] = []
    log_rows: list[dict[str, Any]] = []

    for source in sources:
        try:
            links = list_zip_links(source["url"])
        except Exception as exc:
            error_message = str(exc)[:4000]
            checked_at = datetime.now(timezone.utc).isoformat()
            print(f"{source['name']}: failed to list ZIPs ({error_message})")
            log_rows.append(
                {
                    "run_id": config.run_id,
                    "source_name": source["name"],
                    "source_url": source["url"],
                    "source_zip_name": "",
                    "status": "failed",
                    "checksum": "",
                    "first_seen_datetime": checked_at,
                    "downloaded_datetime": "",
                    "parsed_datetime": "",
                    "row_count_bronze": 0,
                    "row_count_silver": 0,
                    "error_message": error_message,
                }
            )
            continue
        links = filter_zip_links_by_lookback(
            links,
            lookback_hours=config.lookback_hours,
        )
        unseen_links = [link for link in links if link.url not in existing_urls]
        unseen_links = sorted(
            unseen_links,
            key=lambda item: item.file_datetime
            or datetime.min.replace(tzinfo=timezone.utc),
        )[: config.max_zips_per_run]

        print(f"{source['name']}: {len(unseen_links)} unseen ZIP(s)")
        for link in unseen_links:
            status = "dry_run" if config.dry_run else "downloaded"
            error_message = ""
            checksum = ""
            byte_count = 0
            target_path = raw_zip_path(
                config.raw_root,
                source["name"],
                link.filename,
                link.file_datetime,
            )
            downloaded_at = datetime.now(timezone.utc).isoformat()

            try:
                content = get_zip_bytes(link.url, dry_run=config.dry_run)
                byte_count = len(content)
                checksum = hashlib.sha256(content).hexdigest() if content else ""
                if not config.dry_run:
                    store.write_binary(target_path, content)
            except Exception as exc:
                status = "failed"
                error_message = str(exc)[:4000]

            manifest_rows.append(
                {
                    "run_id": config.run_id,
                    "source_name": source["name"],
                    "source_url": link.url,
                    "source_zip_name": link.filename,
                    "source_folder_url": source["url"],
                    "file_datetime": (
                        link.file_datetime.isoformat() if link.file_datetime else ""
                    ),
                    "lakehouse_path": target_path,
                    "checksum": checksum,
                    "byte_count": byte_count,
                    "first_seen_datetime": downloaded_at,
                    "downloaded_datetime": (
                        downloaded_at if status == "downloaded" else ""
                    ),
                    "parsed_datetime": "",
                    "status": status,
                    "error_message": error_message,
                }
            )
            log_rows.append(
                {
                    "run_id": config.run_id,
                    "source_name": source["name"],
                    "source_url": link.url,
                    "source_zip_name": link.filename,
                    "status": status,
                    "checksum": checksum,
                    "first_seen_datetime": downloaded_at,
                    "downloaded_datetime": (
                        downloaded_at if status == "downloaded" else ""
                    ),
                    "parsed_datetime": "",
                    "row_count_bronze": 0,
                    "row_count_silver": 0,
                    "error_message": error_message,
                }
            )

    store.append_control_rows(manifest_rows, log_rows)
    return IngestionResult(manifest_rows=manifest_rows, log_rows=log_rows)


def raw_zip_path(
    raw_root: str,
    source: str,
    filename: str,
    file_dt: datetime | None,
) -> str:
    """Build a raw ZIP path partitioned by source and date."""

    dt = file_dt or datetime.now(timezone.utc)
    return f"{raw_root}/{source}/{dt:%Y/%m/%d}/{filename}"
