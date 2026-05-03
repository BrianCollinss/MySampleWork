from __future__ import annotations

import csv
from datetime import datetime, timezone

import pandas as pd

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


def test_source_label_prefers_configured_label() -> None:
    """Source labels provide stable path and table naming."""

    assert (
        common_ingestion.source_label(
            {"name": "Trading_Cumulative_Price", "label": "trading_cumulative_price"}
        )
        == "trading_cumulative_price"
    )
    assert common_ingestion.source_label({"name": "DispatchIS_Reports"}) == (
        "dispatchis_reports"
    )


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
        sources=[
            {
                "name": "DispatchIS_Reports",
                "label": "dispatchis",
                "url": "https://example.test/source/",
            }
        ],
        store=store,
        config=common_ingestion.IngestionConfig(
            run_id="test-run",
            raw_root="files/nemweb/raw_zip",
        ),
    )

    assert len(result.manifest_rows) == 1
    assert (
        tmp_path
        / "files/nemweb/raw_zip/dispatchis/2026/01/01/PUBLIC_DISPATCHIS_202601010005.zip"
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


def test_append_csv_rows_expands_existing_header_for_new_control_columns(tmp_path) -> None:
    """Local wide CSV outputs should tolerate MMSDM schema drift."""

    control_csv_path = tmp_path / "tables" / "control.csv"

    local_ingestion.append_csv_rows(control_csv_path, [{"a": "1", "b": "2"}])
    local_ingestion.append_csv_rows(control_csv_path, [{"a": "3", "b": "4", "c": "5"}])

    with control_csv_path.open("r", encoding="utf-8", newline="") as file:
        rows = list(csv.DictReader(file))

    assert rows == [
        {"a": "1", "b": "2", "c": ""},
        {"a": "3", "b": "4", "c": "5"},
    ]


def test_append_parquet_rows_appends_local_data_table(tmp_path) -> None:
    """Local data outputs should use Parquet rather than CSV."""

    data_parquet_path = tmp_path / "tables" / "bronze_dispatchis.parquet"

    local_ingestion.append_parquet_rows(data_parquet_path, [{"a": 1}])
    local_ingestion.append_parquet_rows(data_parquet_path, [{"a": 2, "b": 3.5}])

    df = local_ingestion.read_parquet_table(data_parquet_path)

    assert df["a"].tolist() == [1, 2]
    assert pd.api.types.is_integer_dtype(df["a"])
    assert pd.isna(df["b"].iloc[0])
    assert df["b"].iloc[1] == 3.5
    assert pd.api.types.is_float_dtype(df["b"])


def test_write_parquet_part_uses_deterministic_safe_name(tmp_path) -> None:
    """Bronze part files should be deterministic for idempotent reruns."""

    part_name = local_ingestion.safe_parquet_part_name(
        "PUBLIC_DISPATCHIS_202601010005.zip",
        "DISPATCH",
        "PRICE",
    )
    part_path = tmp_path / "tables" / "bronze_dispatchis" / part_name

    local_ingestion.write_parquet_part(part_path, pd.DataFrame([{"price": 1.0}]))
    local_ingestion.write_parquet_part(part_path, pd.DataFrame([{"price": 2.0}]))

    df = local_ingestion.read_parquet_table(part_path)

    assert part_name == "202601010005.zip__DISPATCH__PRICE.parquet"
    assert df["price"].tolist() == [2.0]


def test_read_parquet_table_unions_folder_part_schemas(tmp_path) -> None:
    """Bronze folders can contain different MMSDM table schemas."""

    bronze_folder_path = tmp_path / "tables" / "bronze_dispatchis"
    local_ingestion.write_parquet_part(
        bronze_folder_path / "dispatch_price.parquet",
        pd.DataFrame([{"table_name": "PRICE", "regionid": "NSW1", "rrp": 35.0}]),
    )
    local_ingestion.write_parquet_part(
        bronze_folder_path / "dispatch_regionsum.parquet",
        pd.DataFrame(
            [{"table_name": "REGIONSUM", "regionid": "NSW1", "totaldemand": 6500.0}]
        ),
    )

    df = local_ingestion.read_parquet_table(bronze_folder_path)

    assert set(["regionid", "rrp", "totaldemand"]).issubset(df.columns)
    assert df["table_name"].tolist() == ["PRICE", "REGIONSUM"]
    assert df["rrp"].tolist()[0] == 35.0
    assert df["totaldemand"].tolist()[1] == 6500.0


def test_write_parquet_part_handles_long_paths(tmp_path) -> None:
    """Local Parquet writes should work under long nested Windows paths."""

    long_folder = tmp_path / ("nested_" + "x" * 80) / ("bronze_" + "y" * 80)
    long_part_name = local_ingestion.safe_parquet_part_name(
        "PUBLIC_DISPATCHIS_202601010005_0000000000000000",
        "DISPATCH",
        "INTERCONNECTORRES",
    )
    long_part_path = long_folder / long_part_name

    local_ingestion.write_parquet_part(long_part_path, pd.DataFrame([{"flow": 1.0}]))

    df = local_ingestion.read_parquet_table(long_part_path)
    assert df["flow"].tolist() == [1.0]


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
