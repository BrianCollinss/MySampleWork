from __future__ import annotations

import io
import zipfile
from pathlib import Path

from nem_fabric.mmsdm_parser import parse_mmsdm_csv_bytes, parse_zip_bytes

# Shared fixture values keep parser tests focused on MMSDM behaviour rather than
# repeated setup.
FIXTURE = Path(__file__).parent / "fixtures" / "sample_mmsdm.csv"
SOURCE_URL = "https://nemweb.com.au/Reports/Current/DispatchIS_Reports/PUBLIC_DISPATCHIS_202601010005.zip"


def test_parser_extracts_i_header_and_d_rows() -> None:
    """The parser should use I rows as headers and D rows as records."""

    parsed = parse_mmsdm_csv_bytes(FIXTURE.read_bytes(), SOURCE_URL, "sample.csv")
    assert len(parsed) == 1
    df = parsed[0].dataframe
    assert len(df) == 3
    assert {
        "settlementdate",
        "regionid",
        "rrp",
        "totaldemand",
        "intervention",
    }.issubset(df.columns)


def test_parser_adds_source_metadata() -> None:
    """Every parsed row should include source lineage fields."""

    df = parse_mmsdm_csv_bytes(FIXTURE.read_bytes(), SOURCE_URL, "sample.csv")[
        0
    ].dataframe
    assert df["source_url"].iloc[0] == SOURCE_URL
    assert df["source_zip_name"].iloc[0].endswith(".zip")
    assert df["inner_csv_name"].iloc[0] == "sample.csv"
    assert df["row_hash"].notna().all()


def test_parser_handles_empty_csv() -> None:
    """Empty CSV inputs should return no tables rather than raising."""

    assert parse_mmsdm_csv_bytes(b"", SOURCE_URL, "empty.csv") == []


def test_parser_handles_no_d_rows() -> None:
    """Header-only MMSDM files should produce no data tables."""

    csv_bytes = b"C,NEMWEB,SAMPLE,1\nI,DISPATCH,PRICE,1,SETTLEMENTDATE,REGIONID,RRP\n"
    assert parse_mmsdm_csv_bytes(csv_bytes, SOURCE_URL, "nodata.csv") == []


def test_parser_handles_zip_with_one_csv() -> None:
    """ZIP parsing should delegate CSV members to the MMSDM parser."""

    buffer = io.BytesIO()
    with zipfile.ZipFile(buffer, "w") as archive:
        archive.writestr("sample.csv", FIXTURE.read_bytes())
    parsed = parse_zip_bytes(buffer.getvalue(), SOURCE_URL)
    assert len(parsed) == 1
    assert len(parsed[0].dataframe) == 3
