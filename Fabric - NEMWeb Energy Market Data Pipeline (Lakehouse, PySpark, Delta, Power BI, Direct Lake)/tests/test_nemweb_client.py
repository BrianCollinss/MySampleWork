from __future__ import annotations

from datetime import datetime, timezone

from nem_fabric.nemweb_client import (
    extract_filename,
    extract_timestamp_from_filename,
    filter_zip_links_by_lookback,
    list_zip_links,
)


def test_filename_extraction() -> None:
    """The client should preserve the ZIP filename from a URL path."""

    assert extract_filename(
        "https://example.com/path/PUBLIC_DISPATCHIS_202601010005.zip"
    ) == ("PUBLIC_DISPATCHIS_202601010005.zip")


def test_timestamp_extraction() -> None:
    """Common AEMO filename timestamps should parse to datetime values."""

    timestamp = extract_timestamp_from_filename("PUBLIC_DISPATCHIS_202601010005.zip")
    assert timestamp == datetime(2026, 1, 1, 0, 5, tzinfo=timezone.utc)


def test_zip_link_filtering_using_sample_html() -> None:
    """Directory listing parsing and lookback filtering should work offline."""

    html = """
    <html><body>
      <a href="PUBLIC_DISPATCHIS_202601010005.zip">new</a>
      <a href="PUBLIC_DISPATCHIS_202512312255.zip">old</a>
      <a href="notes.txt">ignore</a>
    </body></html>
    """
    links = list_zip_links(
        "https://nemweb.com.au/Reports/Current/DispatchIS_Reports/", html=html
    )
    filtered = filter_zip_links_by_lookback(
        links,
        lookback_hours=1,
        now=datetime(2026, 1, 1, 0, 30, tzinfo=timezone.utc),
    )
    assert [link.filename for link in filtered] == [
        "PUBLIC_DISPATCHIS_202601010005.zip"
    ]
