# Fabric Pipeline Design

## Activities

1. `00_environment_check` manually or before deployment validation.
2. `01_ingest_nemweb_zip_files`.
3. `02_parse_bronze_tables`.
4. `03_build_silver_tables`.
5. `04_build_gold_dashboard_tables`.
6. Optional semantic model or report refresh trigger if available.

## Dependencies

Each production notebook depends on successful completion of the previous activity. Parsing should not run if ingestion fails; Silver should not run if Bronze parsing fails.

## Failure Handling

- Retry transient HTTP and Spark failures.
- Quarantine malformed files.
- Write errors to `nem_ingestion_log` and `nem_raw_file_audit`.
- Alert on repeated failures, stale data, or quarantine growth.

## Parameters

`run_id`, `source_name`, `lookback_hours`, `max_zips_per_run`, and `dry_run`.

## Logging Tables

`nem_ingestion_log`, `nem_raw_zip_manifest`, `nem_raw_file_audit`, and `nem_gold_data_freshness`.

## Manual Fabric Steps

Create the pipeline in Fabric UI, select the workspace, attach notebooks to the Lakehouse, configure parameters, set retries, and enable alerts. Do not invent IDs in repo files.

Before the first run, upload local folder `src/nem_fabric` to
`Files/libs/nem_fabric` as described in `fabric/deployment_steps.md`.
The notebooks add `FABRIC_NOTEBOOK_LIB_PATH`, defaulting to
`/lakehouse/default/Files/libs`, to `sys.path` so Fabric Pipeline runs can import
the package.
