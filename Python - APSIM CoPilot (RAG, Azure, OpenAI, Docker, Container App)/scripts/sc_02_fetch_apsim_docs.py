"""Fetch APSIM documentation pages into a local folder for indexing.

This script gives the demo a lightweight sync step so APSIM documentation can
be refreshed from the public APSIM websites without manually copying pages into
`sample_data/docs`. The fetched output is still stored locally as markdown-like
files so indexing remains transparent and repeatable.

IMPORTANT IMPLEMENTATION NOTE
-----------------------------
This fetcher is intentionally a best-effort helper, not a complete website
mirroring solution. In particular, `docs.apsim.info` behaves like a client-side
application and some deeper pages are not yet fetched reliably by the current
implementation. The script is useful for demos and partial corpus refreshes,
but it should not be presented as a fully complete or authoritative ingestion
pipeline for all APSIM web documentation.
"""

from __future__ import annotations

import argparse
import re
import shutil
import sys
from collections import deque
from collections.abc import Iterable
from contextlib import AbstractContextManager
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import urljoin, urlparse
from xml.etree import ElementTree

import requests
from bs4 import BeautifulSoup

try:
    from playwright.sync_api import TimeoutError as PlaywrightTimeoutError
    from playwright.sync_api import sync_playwright
except ImportError:  # pragma: no cover - optional runtime dependency path
    PlaywrightTimeoutError = None
    sync_playwright = None

PROJECT_ROOT = Path(__file__).resolve().parents[1]
if str(PROJECT_ROOT) not in sys.path:
    sys.path.insert(0, str(PROJECT_ROOT))


DEFAULT_SEED_URLS = [
    "https://apsimnextgeneration.netlify.app/",
    "https://apsimnextgeneration.netlify.app/usage/",
    "https://apsimnextgeneration.netlify.app/modeldocumentation/",
    "https://docs.apsim.info/",
    "https://docs.apsim.info/validation",
    "https://docs.apsim.info/tutorials",
    "https://docs.apsim.info/models",
]
DEFAULT_OUTPUT_DIR = PROJECT_ROOT / "sample_data" / "docs" / "fetched"
REQUEST_TIMEOUT_SECONDS = 30
USER_AGENT = "apsim-copilot-doc-sync/1.0"
PLACEHOLDER_PAGE_TEXT = {"loading", "loading..."}
DOCS_ARTICLE_SELECTOR_TIMEOUT_MS = 5000
DOCS_ARTICLE_TEXT_TIMEOUT_MS = 7000
BODY_TEXT_TIMEOUT_MS = 4000
DYNAMIC_DISCOVERY_RENDER_LIMIT = 6
FETCHER_LIMITATION_MESSAGE = (
    "NOTE: 02_fetch_apsim_docs.py is a best-effort fetcher. Support for dynamic "
    "APSIM sites, especially docs.apsim.info deep pages, is not complete yet."
)


@dataclass
class FetchedPage:
    """Normalised representation of one fetched documentation page."""

    title: str
    source_url: str
    source_domain: str
    fetched_at: str
    content: str


class PlaywrightRenderer(AbstractContextManager["PlaywrightRenderer"]):
    """Reuse one headless Chromium session across many dynamic page renders."""

    def __init__(self) -> None:
        """Initialise empty renderer state until the context is entered."""
        self._playwright = None
        self._browser = None
        self._page = None

    def __enter__(self) -> "PlaywrightRenderer":
        """Launch Chromium once for the whole fetch run."""
        if sync_playwright is None:
            return self
        self._playwright = sync_playwright().start()
        self._browser = self._playwright.chromium.launch(headless=True)
        self._page = self._browser.new_page(user_agent=USER_AGENT)
        return self

    def __exit__(self, exc_type, exc, exc_tb) -> None:
        """Shut down browser resources cleanly when the fetch run ends."""
        if self._page is not None:
            self._page.close()
        if self._browser is not None:
            self._browser.close()
        if self._playwright is not None:
            self._playwright.stop()

    def render(self, url: str) -> str | None:
        """Render one client-side page in the reused browser session.

        `docs.apsim.info` is a hydrated client-side app, so a plain
        `domcontentloaded` wait often returns only the loading shell. For that
        domain, wait specifically for the final article container and for it to
        contain substantial non-placeholder text before capturing the page HTML.
        """
        if self._page is None:
            return None

        try:
            self._page.goto(url, wait_until="domcontentloaded", timeout=REQUEST_TIMEOUT_SECONDS * 1000)

            if urlparse(url).netloc == "docs.apsim.info":
                try:
                    self._page.wait_for_selector("article.content", timeout=DOCS_ARTICLE_SELECTOR_TIMEOUT_MS)
                    self._page.wait_for_function(
                        """
                        () => {
                            const article = document.querySelector("article.content");
                            if (!article) return false;
                            const text = (article.innerText || "").trim();
                            return text.length > 400 && !/^loading\\.?$/i.test(text);
                        }
                        """,
                        timeout=DOCS_ARTICLE_TEXT_TIMEOUT_MS,
                    )
                except Exception as exc:
                    # Only swallow Playwright's selector/text wait timeout here.
                    # Other errors should still bubble to the outer handler.
                    if PlaywrightTimeoutError is None or not isinstance(exc, PlaywrightTimeoutError):
                        raise
            else:
                try:
                    self._page.wait_for_function(
                        """
                        () => {
                            const text = (document.body?.innerText || '').trim().toLowerCase();
                            return text && text !== 'loading' && text !== 'loading...';
                        }
                        """,
                        timeout=BODY_TEXT_TIMEOUT_MS,
                    )
                except Exception as exc:
                    # Only swallow Playwright's timeout for the "Loading" shell check.
                    # Other exceptions should still surface to the outer handler.
                    if PlaywrightTimeoutError is None or not isinstance(exc, PlaywrightTimeoutError):
                        raise

            return self._page.content()
        except Exception:
            return None


def normalise_url(url: str) -> str:
    """Return a canonical URL string without fragments or trailing slashes."""
    parsed = urlparse(url.strip())
    path = parsed.path or "/"
    if path != "/":
        path = path.rstrip("/")
    return parsed._replace(path=path, fragment="", query="").geturl()


def slugify(value: str) -> str:
    """Create a filesystem-friendly slug from a URL component or title."""
    collapsed = re.sub(r"[^a-zA-Z0-9]+", "-", value).strip("-").lower()
    return collapsed or "index"


def within_allowed_domains(url: str, allowed_domains: set[str]) -> bool:
    """Return True when the URL belongs to one of the requested APSIM domains."""
    return urlparse(url).netloc in allowed_domains


def discover_sitemap_urls(base_url: str) -> list[str]:
    """Return common sitemap locations for a given documentation site."""
    base = normalise_url(base_url)
    return [urljoin(base + "/", "sitemap.xml"), urljoin(base + "/", "sitemap_index.xml")]


def fetch_sitemap_urls(session: requests.Session, sitemap_url: str, allowed_domains: set[str]) -> set[str]:
    """Read a sitemap or sitemap index and return allowed page URLs."""
    response = session.get(sitemap_url, timeout=REQUEST_TIMEOUT_SECONDS)
    if response.status_code != 200 or "xml" not in response.headers.get("Content-Type", "").lower():
        return set()

    root = ElementTree.fromstring(response.text)
    namespace = {"sm": "http://www.sitemaps.org/schemas/sitemap/0.9"}
    urls: set[str] = set()

    # Sitemap indexes contain nested sitemap locations; regular sitemaps contain
    # page URLs. We support both so the script can adapt to either APSIM site.
    nested_sitemaps = [node.text for node in root.findall(".//sm:sitemap/sm:loc", namespace) if node.text]
    if nested_sitemaps:
        for nested_url in nested_sitemaps:
            urls.update(fetch_sitemap_urls(session, nested_url, allowed_domains))
        return urls

    for node in root.findall(".//sm:url/sm:loc", namespace):
        if node.text:
            candidate = normalise_url(node.text)
            if within_allowed_domains(candidate, allowed_domains):
                urls.add(candidate)
    return urls


def collect_candidate_urls(
    session: requests.Session,
    seed_urls: list[str],
    allowed_domains: set[str],
    max_pages: int,
    renderer: PlaywrightRenderer | None = None,
) -> list[str]:
    """Collect candidate APSIM documentation URLs from sitemaps and crawling.

    This discovery logic is intentionally simple. It works well for static
    sites and partly hydrated pages, but it is not yet a complete crawler for
    JavaScript-heavy APSIM documentation experiences.
    """
    discovered_urls: list[str] = []
    discovered_set: set[str] = set()
    seed_url_set = {normalise_url(url) for url in seed_urls}
    dynamic_discovery_renders = 0

    def add_candidate(url: str) -> None:
        """Append a candidate URL once while preserving discovery order."""
        if url not in discovered_set and within_allowed_domains(url, allowed_domains):
            discovered_set.add(url)
            discovered_urls.append(url)

    def looks_like_loading_shell(html: str) -> bool:
        """Return True when a response looks like an unhydrated client shell."""
        soup = BeautifulSoup(html, "html.parser")
        body_text = soup.get_text(" ", strip=True).lower()
        if body_text in PLACEHOLDER_PAGE_TEXT:
            return True
        return "loading" in body_text and len(body_text) < 300

    def docs_path_depth(url: str) -> int:
        """Return the number of path segments in a URL."""
        return len([part for part in urlparse(url).path.split("/") if part])

    for seed_url in seed_urls:
        for sitemap_url in discover_sitemap_urls(seed_url):
            try:
                for discovered_url in fetch_sitemap_urls(session, sitemap_url, allowed_domains):
                    add_candidate(discovered_url)
            except Exception:
                # Some sites expose a sitemap, some do not. A missing sitemap is
                # not fatal because the script can still fall back to crawling.
                continue

    crawl_queue: deque[str] = deque(normalise_url(url) for url in seed_urls)
    visited: set[str] = set()

    while crawl_queue and len(discovered_urls) < max_pages:
        current_url = crawl_queue.popleft()
        if current_url in visited or not within_allowed_domains(current_url, allowed_domains):
            continue
        visited.add(current_url)
        add_candidate(current_url)

        try:
            response = session.get(current_url, timeout=REQUEST_TIMEOUT_SECONDS)
            response.raise_for_status()
        except Exception:
            continue

        discovery_html = response.text
        current_domain = urlparse(current_url).netloc

        # `docs.apsim.info` exposes some links only after hydration, but doing a
        # full Playwright render for every crawled page is far too slow. Restrict
        # dynamic discovery to obvious shell pages near the top of the site.
        should_render_for_discovery = (
            current_domain == "docs.apsim.info"
            and renderer is not None
            and dynamic_discovery_renders < DYNAMIC_DISCOVERY_RENDER_LIMIT
            and looks_like_loading_shell(discovery_html)
            and (current_url in seed_url_set or docs_path_depth(current_url) <= 1)
        )
        if should_render_for_discovery:
            rendered_html = renderer.render(current_url)
            if rendered_html:
                discovery_html = rendered_html
                dynamic_discovery_renders += 1

        soup = BeautifulSoup(discovery_html, "html.parser")
        for anchor in soup.find_all("a", href=True):
            candidate = normalise_url(urljoin(current_url, anchor["href"]))
            if candidate not in visited and within_allowed_domains(candidate, allowed_domains):
                crawl_queue.append(candidate)

    return discovered_urls[:max_pages]


def extract_page_content(html: str, page_url: str) -> FetchedPage | None:
    """Extract readable text from a documentation page."""
    soup = BeautifulSoup(html, "html.parser")

    for selector in ("script", "style", "nav", "footer", "header", "form", "noscript"):
        for node in soup.select(selector):
            node.decompose()

    content_root = (
        soup.select_one("article.content .docs-content")
        or soup.select_one("article.content")
        or soup.find("main")
        or soup.find("article")
        or soup.body
    )
    if content_root is None:
        return None

    title = soup.title.get_text(" ", strip=True) if soup.title else urlparse(page_url).path.strip("/") or page_url
    text = content_root.get_text("\n", strip=True)
    cleaned_lines = [line.strip() for line in text.splitlines() if line.strip()]
    content = "\n".join(cleaned_lines)
    if not content:
        return None

    return FetchedPage(
        title=title,
        source_url=page_url,
        source_domain=urlparse(page_url).netloc,
        fetched_at=datetime.now(timezone.utc).isoformat(),
        content=content,
    )


def needs_dynamic_render(page: FetchedPage | None) -> bool:
    """Return True when the fetched page looks like a client-rendered shell."""
    if page is None:
        return True
    compact_content = page.content.strip().lower()
    if compact_content in PLACEHOLDER_PAGE_TEXT:
        return True
    if page.source_domain == "docs.apsim.info" and compact_content.endswith("loading"):
        return True
    return False


def build_output_path(output_dir: Path, page: FetchedPage) -> Path:
    """Map a fetched page URL into a stable local markdown path."""
    parsed = urlparse(page.source_url)
    path_part = parsed.path.strip("/") or "index"
    slug = slugify(path_part.replace("/", "-"))
    domain_slug = slugify(parsed.netloc)
    return output_dir / domain_slug / f"{slug}.md"


def write_page(output_dir: Path, page: FetchedPage) -> Path:
    """Write one fetched page into the local docs folder."""
    output_path = build_output_path(output_dir, page)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(
        "\n".join(
            [
                f"# {page.title}",
                "",
                f"Source URL: {page.source_url}",
                f"Source domain: {page.source_domain}",
                f"Fetched at: {page.fetched_at}",
                "",
                page.content,
                "",
            ]
        ),
        encoding="utf-8",
    )
    return output_path


def clear_output_dir(output_dir: Path) -> None:
    """Delete previously fetched files so the output mirrors the current crawl.

    The fetched docs directory is owned by this script, so clearing it at the
    start of a run keeps stale pages from older crawls from lingering in the
    local corpus.
    """
    if output_dir.exists():
        shutil.rmtree(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)


def fetch_pages(session: requests.Session, urls: Iterable[str], renderer: PlaywrightRenderer | None = None) -> list[FetchedPage]:
    """Fetch and extract content from a sequence of APSIM documentation URLs.

    The current implementation retries suspected client-rendered shells with
    Playwright, but that fallback is still incomplete for some APSIM pages.
    """
    url_list = list(urls)
    pages: list[FetchedPage] = []
    total_urls = len(url_list)

    for index, url in enumerate(url_list, start=1):
        print(f"[{index}/{total_urls}] Fetching {url}")
        try:
            response = session.get(url, timeout=REQUEST_TIMEOUT_SECONDS)
            response.raise_for_status()
            page = extract_page_content(response.text, url)
            if needs_dynamic_render(page):
                print(f"  -> Detected client-rendered shell, retrying with Playwright")
                rendered_html = renderer.render(url) if renderer is not None else None
                if rendered_html:
                    page = extract_page_content(rendered_html, url)
            if page is not None and not needs_dynamic_render(page):
                pages.append(page)
                print(f"  -> Captured {len(page.content)} characters")
            else:
                print("  -> Skipped because no useful content was captured")
        except Exception:
            print("  -> Skipped due to fetch/render error")
            continue
    return pages


def parse_args() -> argparse.Namespace:
    """Parse command-line arguments for the documentation fetch job."""
    parser = argparse.ArgumentParser(description="Fetch APSIM documentation pages into a local folder.")
    parser.add_argument("--output-dir", default=str(DEFAULT_OUTPUT_DIR))
    parser.add_argument("--max-pages", type=int, default=200)
    parser.add_argument("--seed-url", action="append", dest="seed_urls")
    return parser.parse_args()


def main() -> None:
    """Fetch APSIM docs, save them locally, and print a compact sync summary."""
    args = parse_args()
    seed_urls = args.seed_urls or DEFAULT_SEED_URLS
    allowed_domains = {urlparse(url).netloc for url in seed_urls}
    output_dir = Path(args.output_dir)
    # Start from a clean fetched-docs folder so the written files reflect the
    # current crawl exactly instead of mixing old and new pages together.
    clear_output_dir(output_dir)
    print(f"Fetching APSIM docs into {output_dir}")
    print(FETCHER_LIMITATION_MESSAGE)
    print(f"Seed URLs: {', '.join(seed_urls)}")
    print(f"Maximum pages to fetch: {args.max_pages}")

    session = requests.Session()
    session.headers.update({"User-Agent": USER_AGENT})

    with PlaywrightRenderer() as renderer:
        print("Collecting candidate URLs from sitemaps and site crawling...")
        candidate_urls = collect_candidate_urls(
            session=session,
            seed_urls=seed_urls,
            allowed_domains=allowed_domains,
            max_pages=args.max_pages,
            renderer=renderer,
        )
        print(f"Discovered {len(candidate_urls)} candidate URLs.")
        print("Fetching page content...")
        pages = fetch_pages(session, candidate_urls, renderer=renderer)

    written_paths = [write_page(output_dir, page) for page in pages]
    print(f"Fetched {len(written_paths)} APSIM documentation pages into {output_dir}")
    for path in written_paths[:10]:
        print(f"- {path.relative_to(PROJECT_ROOT)}")
    if len(written_paths) > 10:
        print(f"... and {len(written_paths) - 10} more files")


if __name__ == "__main__":
    main()
