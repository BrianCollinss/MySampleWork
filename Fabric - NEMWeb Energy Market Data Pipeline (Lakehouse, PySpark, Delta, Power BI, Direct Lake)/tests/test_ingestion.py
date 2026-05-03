from __future__ import annotations

import csv
from datetime import datetime, timezone

from nem_fabric import common_ingestion
from nem_fabric import local_ingestion
from nem_fabric.common_nemweb_client import ZipLink


def test_select_enabled_sources_filters_by_name() -> None:
    """Source selection is common to local and Fabric notebook runs."""

    sources = common_ingestion.select_enabled_sources(
        {
            "sources": [
                {"name": "enabled", "enabled": True},
                {"name": "disabled", "enabled": False},
            ]
        },
        source_name="enabled",
    )

    assert sources == [{"name": "enabled", "enabled": True}]


def test_local_ingestion_writes_zip_and_csv_control_rows(monkeypatch, tmp_path) -> None:
    """The ingestion algorithm runs locally without Spark."""

    link = ZipLink(
        url="https://example.test/source/PUBLIC_DISPATCHIS_202601010005.zip",
        filename="PUBLIC_DISPATCHIS_202601010005.zip",
        folder_url="https://example.test/source/",
        file_datetime=datetime(2026, 1, 1, 0, 5, tzinfo=timezone.utc),
    )
    monkeypatch.setattr(common_ingestion, "list_zip_links", lambda url: [link])
    monkeypatch.setattr(
        common_ingestion,
        "filter_zip_links_by_lookback",
        lambda links, lookback_hours: links,
    )
    monkeypatch.setattr(
        common_ingestion,
        "get_zip_bytes",
        lambda url, dry_run=False: b"zip",
    )

    store = local_ingestion.LocalCsvIngestionStore(tmp_path)
    result = common_ingestion.ingest_nemweb_zip_files(
        sources=[{"name": "DispatchIS_Reports", "url": "https://example.test/source/"}],
        store=store,
        config=common_ingestion.IngestionConfig(
            run_id="test-run",
            raw_root="files/nemweb/raw_zip",
        ),
    )

    assert len(result.manifest_rows) == 1
    assert (
        tmp_path
        / "files/nemweb/raw_zip/DispatchIS_Reports/2026/01/01/PUBLIC_DISPATCHIS_202601010005.zip"
    ).read_bytes() == b"zip"

    with store.manifest_path.open("r", encoding="utf-8", newline="") as file:
        rows = list(csv.DictReader(file))
    assert rows[0]["source_url"] == link.url
    assert rows[0]["status"] == "downloaded"


def test_local_ingestion_skips_existing_manifest_url(monkeypatch, tmp_path) -> None:
    """Local CSV manifest provides idempotency without Spark tables."""

    store = local_ingestion.LocalCsvIngestionStore(tmp_path)
    existing_url = "https://example.test/source/existing.zip"
    local_ingestion.append_csv_rows(
        store.manifest_path,
        [{"source_url": existing_url, "status": "downloaded"}],
    )
    monkeypatch.setattr(
        common_ingestion,
        "list_zip_links",
        lambda url: [
            ZipLink(
                url=existing_url,
                filename="existing.zip",
                folder_url="https://example.test/source/",
            )
        ],
    )
    monkeypatch.setattr(
        common_ingestion,
        "filter_zip_links_by_lookback",
        lambda links, lookback_hours: links,
    )

    result = common_ingestion.ingest_nemweb_zip_files(
        sources=[{"name": "DispatchIS_Reports", "url": "https://example.test/source/"}],
        store=store,
        config=common_ingestion.IngestionConfig(run_id="test-run"),
    )

    assert result.manifest_rows == []


def test_local_ingestion_retries_failed_manifest_url(monkeypatch, tmp_path) -> None:
    """Failed local rows should not make later runs skip the source ZIP."""

    store = local_ingestion.LocalCsvIngestionStore(tmp_path)
    existing_url = "https://example.test/source/retry.zip"
    local_ingestion.append_csv_rows(
        store.manifest_path,
        [{"source_url": existing_url, "status": "failed"}],
    )

    assert store.read_existing_manifest_urls() == set()


def test_ingestion_records_source_listing_failure_and_continues(
    monkeypatch,
    tmp_path,
) -> None:
    """A missing NEMWeb folder should not fail the whole ingestion run."""

    good_link = ZipLink(
        url="https://example.test/good/PUBLIC_DISPATCHIS_202601010005.zip",
        filename="PUBLIC_DISPATCHIS_202601010005.zip",
        folder_url="https://example.test/good/",
        file_datetime=datetime(2026, 1, 1, 0, 5, tzinfo=timezone.utc),
    )

    def list_zip_links(url: str) -> list[ZipLink]:
        if "missing" in url:
            raise RuntimeError("404 Client Error: Not Found")
        return [good_link]

    monkeypatch.setattr(common_ingestion, "list_zip_links", list_zip_links)
    monkeypatch.setattr(
        common_ingestion,
        "filter_zip_links_by_lookback",
        lambda links, lookback_hours: links,
    )
    monkeypatch.setattr(
        common_ingestion,
        "get_zip_bytes",
        lambda url, dry_run=False: b"zip",
    )

    result = common_ingestion.ingest_nemweb_zip_files(
        sources=[
            {"name": "Missing", "url": "https://example.test/missing/"},
            {"name": "Good", "url": "https://example.test/good/"},
        ],
        store=local_ingestion.LocalCsvIngestionStore(tmp_path),
        config=common_ingestion.IngestionConfig(run_id="test-run"),
    )

    assert len(result.manifest_rows) == 1
    assert [row["status"] for row in result.log_rows] == ["failed", "downloaded"]
