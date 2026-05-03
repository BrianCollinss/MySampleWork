"""HTTP client helpers for public AEMO NEMWeb folders.

The client stays dependency-light so it can run in local smoke tests before the
Fabric runtime is available.
"""

from __future__ import annotations

import re
import time
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from html.parser import HTMLParser
from pathlib import PurePosixPath
from urllib.parse import urljoin, urlparse

import requests

# A clear user-agent is polite for public data sources and useful if AEMO ever
# needs to identify traffic patterns from this sample project.
USER_AGENT = "fabric-nem-dashboard/0.1 (+portfolio project)"

# AEMO filenames commonly embed interval timestamps such as YYYYMMDDHHMM. Keep
# parsing tolerant because report families do not all use identical naming.
TIMESTAMP_PATTERNS = (
    re.compile(r"(?P<ts>20\d{10})"),
    re.compile(r"(?P<ts>20\d{6}_?\d{4})"),
)


@dataclass(frozen=True)
class ZipLink:
    """NEMWeb ZIP link discovered from a folder listing."""

    url: str
    filename: str
    folder_url: str
    file_datetime: datetime | None = None
    last_modified: datetime | None = None


class _ZipLinkParser(HTMLParser):
    """Minimal HTML parser that extracts anchors from directory listings."""

    def __init__(self) -> None:
        super().__init__()
        self.hrefs: list[str] = []

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        """Capture `href` attributes from anchor tags only."""

        if tag.lower() != "a":
            return
        attrs_dict = dict(attrs)
        if "href" in attrs_dict:
            self.hrefs.append(str(attrs_dict["href"]))


def extract_filename(url: str) -> str:
    """Extract the filename component from a URL."""

    return PurePosixPath(urlparse(url).path).name


def extract_timestamp_from_filename(filename: str) -> datetime | None:
    """Extract common AEMO timestamp patterns from filenames.

    Returned datetimes are timezone-aware UTC placeholders. This is sufficient
    for lookback filtering and avoids naive datetime comparisons in scheduled
    runs.
    """

    for pattern in TIMESTAMP_PATTERNS:
        match = pattern.search(filename)
        if not match:
            continue
        raw = match.group("ts").replace("_", "")
        try:
            return datetime.strptime(raw[:12], "%Y%m%d%H%M").replace(
                tzinfo=timezone.utc
            )
        except ValueError:
            continue
    return None


def list_zip_links(folder_url: str, html: str | None = None) -> list[ZipLink]:
    """List ZIP links from a NEMWeb folder URL.

    Tests can supply `html` directly so link parsing is validated without a
    network call.
    """

    if html is None:
        response = requests.get(
            folder_url, headers={"User-Agent": USER_AGENT}, timeout=30
        )
        response.raise_for_status()
        html = response.text

    # NEMWeb current report pages are simple directory listings, so extracting
    # anchors is enough and avoids a heavier HTML dependency.
    parser = _ZipLinkParser()
    parser.feed(html)
    links: list[ZipLink] = []
    for href in parser.hrefs:
        if not href.lower().endswith(".zip"):
            continue
        url = urljoin(folder_url, href)
        filename = extract_filename(url)
        last_modified = None
        links.append(
            ZipLink(
                url=url,
                filename=filename,
                folder_url=folder_url,
                file_datetime=extract_timestamp_from_filename(filename),
                last_modified=last_modified,
            )
        )
    return links


def filter_zip_links_by_lookback(
    links: list[ZipLink],
    lookback_hours: int,
    now: datetime | None = None,
) -> list[ZipLink]:
    """Filter ZIP links by parsed file timestamp where available.

    Files without parseable timestamps are retained so unusual but valid AEMO
    files are not accidentally skipped.
    """

    if now is None:
        now = datetime.now(timezone.utc)
    cutoff = now - timedelta(hours=lookback_hours)
    return [
        link
        for link in links
        if link.file_datetime is None or link.file_datetime >= cutoff
    ]


def get_zip_bytes(
    url: str,
    *,
    dry_run: bool = False,
    timeout: int = 30,
    retries: int = 3,
) -> bytes:
    """Download ZIP bytes with small retry handling.

    `dry_run=True` returns empty bytes and is intended for pipeline validation
    where the operator wants to inspect intended downloads first.
    """

    if dry_run:
        return b""

    last_error: Exception | None = None
    for attempt in range(1, retries + 1):
        try:
            response = requests.get(
                url, headers={"User-Agent": USER_AGENT}, timeout=timeout
            )
            response.raise_for_status()
            return response.content
        except requests.RequestException as exc:
            last_error = exc
            if attempt < retries:
                # Exponential backoff keeps transient network failures from
                # immediately failing a scheduled 5-minute pipeline run.
                time.sleep(min(2**attempt, 10))
    raise RuntimeError(f"Failed to download {url}") from last_error
