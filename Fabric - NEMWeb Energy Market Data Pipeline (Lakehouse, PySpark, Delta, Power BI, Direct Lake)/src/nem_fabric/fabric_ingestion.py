"""Fabric Spark storage for NEMWeb ingestion notebooks."""

from __future__ import annotations

from pathlib import Path
from typing import Any

from nem_fabric.common_ingestion import INGESTION_LOG_TABLE, MANIFEST_TABLE


class FabricSparkIngestionStore:
    """Fabric implementation using Lakehouse Files and Delta tables."""

    def __init__(self, spark_session: Any, lakehouse_root: str | Path) -> None:
        self.spark = spark_session
        self.lakehouse_root = Path(lakehouse_root)

    def read_existing_manifest_urls(self) -> set[str]:
        """Read processed ZIP URLs from the Fabric Delta manifest."""

        if not self.spark.catalog.tableExists(MANIFEST_TABLE):
            return set()
        rows = (
            self.spark.table(MANIFEST_TABLE)
            .select("source_url")
            .distinct()
            .collect()
        )
        return {row.source_url for row in rows}

    def write_binary(self, relative_path: str, content: bytes) -> None:
        """Write ZIP bytes to the default Lakehouse Files mount."""

        target = self.lakehouse_root / relative_path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(content)

    def append_control_rows(
        self,
        manifest_rows: list[dict[str, Any]],
        log_rows: list[dict[str, Any]],
    ) -> None:
        """Append Fabric Delta control rows."""

        if manifest_rows:
            self.spark.createDataFrame(manifest_rows).write.format("delta").mode(
                "append"
            ).saveAsTable(MANIFEST_TABLE)
        if log_rows:
            self.spark.createDataFrame(log_rows).write.format("delta").mode(
                "append"
            ).saveAsTable(INGESTION_LOG_TABLE)
