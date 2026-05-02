# Lakehouse Design

## Files and Tables

Lakehouse Files hold immutable raw ZIP files and quarantine artefacts. Lakehouse Tables hold Delta tables for control, Bronze, Silver, and Gold layers.

## Medallion Layers

Bronze preserves raw parsed MMSDM records with metadata. Silver applies typing, deduplication, region mapping, and quality checks. Gold provides Power BI-ready facts, KPIs, and freshness tables.

## Naming

Use `nem_raw_*`, `nem_bronze_*`, `nem_silver_*`, and `nem_gold_*`.

## Partitioning

Partition large tables by `trading_date`, `region`, or `source_name` where useful. Avoid over-partitioning small tables.

## Table Grain

Silver price/demand is one row per settlement interval and region. Gold 30-minute and daily tables aggregate that grain.

## Idempotency

Track URL, filename, checksum, run ID, and status in control tables. Process only unseen ZIPs and use deterministic keys for Silver.
