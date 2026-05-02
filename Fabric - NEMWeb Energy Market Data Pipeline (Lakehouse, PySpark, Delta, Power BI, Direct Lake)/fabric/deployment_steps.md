# Deployment Steps

This document contains the detailed manual steps required to move the local repo
into Microsoft Fabric. Do not invent workspace IDs, Lakehouse IDs, capacity
names, or credentials; use the Fabric UI selections available to your account.

## Prerequisites

- A Fabric-enabled workspace. A Power BI Pro-only workspace cannot create
  Lakehouses, Environments, or Fabric notebooks.
- A Fabric Lakehouse in that workspace.
- VS Code with the recommended extensions from `.vscode/extensions.json`.
- Local `.env` copied from `.env.example`.
- The `src/nem_fabric` package available to notebooks through a Fabric
  Environment library or Lakehouse Files source-library path.

## Local Preparation

1. Open the repo in VS Code.
2. Install the recommended extensions.
3. Create the local Conda environment using `environment.yml`.
4. Copy `.env.example` to `.env`.
5. Fill only non-secret local values such as workspace and Lakehouse names.
6. Run parser tests locally before publishing notebooks.

Parameter sources:

- `FABRIC_WORKSPACE_NAME`: local `.env`; display/documentation only.
- `FABRIC_LAKEHOUSE_NAME`: local `.env`; display/documentation only.
- `FABRIC_NOTEBOOK_LIB_PATH`: optional notebook environment variable; defaults
  to `/lakehouse/default/Files/libs`.
- NEMWeb URLs and ingestion limits: local `.env` for scripts, notebook
  parameters or checked-in config files for Fabric runs.

## Deploy Python Package

Fabric Pipelines do not automatically upload or install local source code when
they run notebooks. Each notebook runtime must be able to import `nem_fabric`.

Recommended production option:

1. Add or maintain packaging metadata such as `pyproject.toml`.
2. Build a wheel locally.
3. Create a Fabric Environment item in a Fabric-enabled workspace.
4. Upload the wheel as a custom library.
5. Publish the Environment.
6. Attach the Environment to each notebook or Spark job.
7. Run notebooks from the Fabric Pipeline.

This gives versioned package deployment and keeps notebook imports aligned with
local tests. It requires Fabric Environment support and a rebuild/re-upload
whenever `src/nem_fabric` changes.

Current project option:

Upload the local folder:

```text
src/nem_fabric
```

to the Lakehouse Files path:

```text
Files/libs/nem_fabric
```

The resulting Fabric runtime path should be:

```text
/lakehouse/default/Files/libs/nem_fabric
```

The notebooks add the parent folder to `sys.path`:

```text
/lakehouse/default/Files/libs
```

If you use a different folder, set `FABRIC_NOTEBOOK_LIB_PATH` to the parent
folder containing `nem_fabric`.

Notebook bootstrap pattern:

```python
import os
import sys

fabric_lib_path = os.getenv("FABRIC_NOTEBOOK_LIB_PATH", "/lakehouse/default/Files/libs")
if fabric_lib_path not in sys.path:
    sys.path.insert(0, fabric_lib_path)
```

Re-upload `src/nem_fabric` whenever files under that folder change. Notebook-only
changes do not require re-uploading the package.

## Publish Notebooks

1. Sign into Fabric from VS Code or the Fabric web UI.
2. Select the target Fabric workspace.
3. Publish notebooks from `notebooks/`.
4. Attach the Lakehouse to each notebook.
5. Confirm notebooks `01` to `04` have access to `nem_fabric` through the
   bootstrap cell.

## Validate Environment

Run `00_environment_check.ipynb`.

This notebook validates:

- Spark session availability.
- Basic run metadata.
- Project config-file visibility where applicable.
- Lakehouse table write/read access.

## Run Notebooks Manually

Run the notebooks in this order:

1. `01_ingest_nemweb_zip_files.ipynb`
2. `02_parse_bronze_tables.ipynb`
3. `03_build_silver_tables.ipynb`
4. `04_build_gold_dashboard_tables.ipynb`

Manual parameter guidance:

- `run_id`: generated in the notebook unless supplied by a Pipeline.
- `source_name`: blank means all enabled sources from `config/sources.yml`.
- `max_zips_per_run`: safety cap for each run.
- `lookback_hours`: recent period used for current-report discovery.
- `dry_run`: set `True` to test discovery without writing ZIP files.

## Create Fabric Pipeline

1. Create a new Fabric Data Pipeline.
2. Add notebook activities for notebooks `01` to `04`.
3. Set dependencies so each notebook runs only after the previous one succeeds.
4. Pass pipeline parameters where required.
5. Configure retry policy for transient HTTP or Spark failures.
6. Configure failure alerts.

See `fabric/pipeline_design.md` for detailed orchestration design.

## Schedule

Recommended production schedule:

- Every 5 minutes for current reports.
- Every 15 minutes for development.
- Separate daily job for future archive backfill.

See `fabric/schedule_design.md`.

## Validate Tables

After the first successful run, confirm these tables exist:

- `nem_raw_zip_manifest`
- `nem_ingestion_log`
- `nem_raw_file_audit`
- `nem_bronze_mmsdm_rows`
- `nem_silver_price_demand_5min`
- `nem_gold_region_5min`
- `nem_gold_dashboard_kpis`
- `nem_gold_data_freshness`

## Build Power BI Report

Create a Direct Lake semantic model from the Gold tables where available. Use
Import mode only if Direct Lake is unavailable.

See:

- `powerbi/semantic_model_tables.md`
- `powerbi/measures_dax.md`
- `powerbi/dashboard_pages.md`
- `powerbi/report_build_checklist.md`
