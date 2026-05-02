"""Local smoke test for NEMWeb listing, download, and MMSDM parsing.

This script validates the public HTTP client and parser before Fabric is
available. It reads only the latest DispatchIS ZIPs in memory and deliberately
does not write raw source data into the repository.
"""

from __future__ import annotations

from nem_fabric.config import load_settings
from nem_fabric.logging_config import configure_logging
from nem_fabric.mmsdm_parser import parse_zip_bytes
from nem_fabric.nemweb_client import get_zip_bytes, list_zip_links


def main() -> None:
    """Download a small number of recent ZIPs and print detected table groups."""

    settings = load_settings()
    configure_logging(settings.log_level)

    # DispatchIS is the first source because it contains near real-time price,
    # demand, and interconnector records useful for the dashboard.
    links = list_zip_links(str(settings.nemweb_dispatchis_url))
    latest_links = sorted(
        links,
        key=lambda link: link.file_datetime or link.filename,
        reverse=True,
    )[:2]

    for link in latest_links:
        print(f"Downloading {link.filename}")
        zip_bytes = get_zip_bytes(link.url)
        tables = parse_zip_bytes(zip_bytes, link.url)
        print(f"Detected {len(tables)} table group(s)")
        for table in tables:
            columns = ", ".join(table.dataframe.columns[:10])
            print(
                f"- {table.package_name}.{table.table_name}: {len(table.dataframe)} rows; {columns}"
            )


if __name__ == "__main__":
    main()
