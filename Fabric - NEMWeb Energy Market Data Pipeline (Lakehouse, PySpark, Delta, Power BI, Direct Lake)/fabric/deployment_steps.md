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
- `FABRIC_ONELAKE_ACCESS_TOKEN`: optional local-only bearer token for
  `scripts/upload_fabric_assets.py` when Azure CLI is unavailable.
- `FABRIC_API_ACCESS_TOKEN`: optional local-only Fabric REST API bearer token
  for publishing Notebook workspace items when Azure CLI/device-code auth is
  unavailable.
- `FABRIC_NOTEBOOK_LIB_PATH`: optional notebook environment variable; defaults
  to `/lakehouse/default/Files/libs`.
- NEMWeb URLs and ingestion limits: local `.env` for scripts, notebook
  parameters or checked-in config files for Fabric runs.

## Create Fabric Environment

Create a new Fabric Environment before running the notebooks. In the Environment
library settings, add these packages from external repositories/PyPI:

- `pydantic`
- `python-dotenv`
- `pyyaml`
- `requests`
- `beautifulsoup4`
- `lxml`

Save and publish the Environment. Then attach that published Environment to each
Fabric notebook. Libraries with status `Saved` are not available to notebook
sessions until the Environment is published and the notebook session is
restarted.

## Deploy Python Package

Fabric Pipelines do not automatically upload or install local source code when
they run notebooks. Each notebook runtime must be able to import `nem_fabric`.

The package uses filename prefixes to separate runtime dependencies:

- `common_` modules run locally and in Fabric.
- `fabric_` modules contain Spark/Lakehouse-specific implementations.
- `local_` modules contain local filesystem implementations for notebook and
  unit-test runs without Spark.

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

Re-upload whenever local Python package files, checked-in configuration, or
notebooks change. The upload script clears each managed Lakehouse Files target
folder before uploading the replacement package/config files, so renamed or
removed local files are also removed from Lakehouse Files. Notebooks are
published as Fabric workspace Notebook items and are created or updated by
display name.

You can upload the package, checked-in YAML configuration, and notebooks with:

```powershell
# Interactive browser sign-in. Use this for accounts protected by MFA.
az login --use-device-code --allow-no-subscriptions
python scripts/upload_fabric_assets.py
```

If Azure CLI is not installed, use the script's device-code flow instead:

```powershell
python scripts/upload_fabric_assets.py --device-code
```

The script uses `FABRIC_WORKSPACE_NAME` and `FABRIC_LAKEHOUSE_NAME` from `.env`
unless you pass `--workspace` and `--lakehouse`. `--workspace` may be a Fabric
workspace display name or workspace ID. It uploads:

- `src/nem_fabric` to `Files/libs/nem_fabric`.
- `config` to `Files/config`.
- `notebooks/*.ipynb` as Fabric workspace Notebook items.

Use `--dry-run` to preview paths, or `--skip-notebooks` when only package and
configuration files need uploading. Publishing Fabric Notebook items and
attaching the Lakehouse still remain Fabric UI or VS Code steps.

You can also provide short-lived bearer tokens through
`FABRIC_ONELAKE_ACCESS_TOKEN` and `FABRIC_API_ACCESS_TOKEN` in your local shell
or `.env`, or pass them with `--access-token` and `--fabric-access-token`. Keep
populated token values out of source control.

If local interactive login is blocked by Conditional Access or MFA policy, sign
in using the browser/device-code flow from a trusted device, or use a service
principal that has the required Fabric workspace/Lakehouse access:

```powershell
az login --service-principal `
  --username "<app-client-id>" `
  --password "<client-secret-or-certificate>" `
  --tenant "<tenant-id>"
```

Keep service principal secrets out of `.env` and shell history where possible;
prefer a secure secret store for repeatable automation.

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

If the notebook prints `lakehouse_name=none_attached`, the notebook item is not
attached to a default Lakehouse. In the Fabric notebook editor, select **Add
Lakehouse** or **Lakehouse** from the notebook explorer, choose the target
Lakehouse, save the notebook, and rerun the environment check.

If the notebook reports missing `config/sources.yml`, `config/tables.yml`, or
`config/dashboard_requirements.yml`, upload the checked-in config folder to
Lakehouse Files:

```powershell
az login --use-device-code
python scripts/upload_fabric_assets.py
```

The expected Fabric path is `Files/config`. The notebook runtime sees that path
as `/lakehouse/default/Files/config`.

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

Notebook `01` can also run locally without Spark. Local runs use the same
`nem_fabric.common_ingestion` algorithm, write raw ZIP files under repo-root
`data/files`, and append CSV control files under repo-root
`data/tables`. Fabric runs use the same algorithm with Lakehouse Files and
Delta control tables.

## Create Fabric Pipeline

1. Create a new Fabric Data Pipeline.
2. Add notebook activities for notebooks `01` to `04`.
3. Set dependencies so each notebook runs only after the previous one succeeds.
4. Pass pipeline parameters where required.

## Pipeline Troubleshooting

If a notebook succeeds when run manually but fails from a Pipeline with
`TooManyRequestsForCapacity` or HTTP `430`, Fabric could not create a Spark
session because the capacity hit a Spark compute, queue, or API admission limit.
This is capacity contention rather than notebook logic failure.

Immediate actions:

- Check **Monitoring hub** for active or queued Spark jobs and cancel stale runs.
- In **Workspace settings -> Job management**, review Job Concurrency and Queue
  Monitoring to see which jobs are consuming the capacity.
- Rerun the Pipeline after active notebook sessions have stopped.
- Keep notebook activities sequential unless the capacity SKU can support
  parallel Spark sessions.
- Reduce concurrent manual notebook runs while testing scheduled Pipelines.
- For repeatable production schedules, move the workspace to a larger Fabric
  capacity SKU if the current SKU regularly reaches the queue limit.

For small capacities that can only admit two Spark sessions, a sequential
Pipeline can still fail between notebook activities because the previous
notebook session may not have fully released before the next notebook requests a
new session. To reduce that handover spike:

- Enable **Workspace settings -> Data Engineering/Science -> Spark settings ->
  High concurrency -> For pipeline running multiple notebooks**.
- Set the same **session tag** on notebook activities `01` to `04` in the
  Pipeline advanced settings, for example `nemweb-medallion`.
- Confirm all four notebooks use the same default Lakehouse, Spark compute
  configuration, Environment, and library set so Fabric can pack them into the
  same high-concurrency session.
- Add a retry policy to each notebook activity with a short delay, for example
  2 to 3 retries with a 2 to 5 minute interval, so transient session-admission
  failures can clear without manual reruns.

If the first notebook activity fails before any notebook code starts, the
Pipeline handover is not the cause. Check these capacity-level issues:

- The Data Pipeline orchestration itself should not consume a Spark session;
  the first Notebook activity is the Spark session request.
- Open the Fabric Capacity Metrics app and check whether Spark usage is already
  throttled or carrying smoothed utilisation from previous manual notebook
  tests. Wait for the capacity to cool down before rerunning the Pipeline.
- Confirm queueing is enabled for Spark jobs on the capacity. When a capacity is
  already throttled, Fabric can reject new Spark jobs instead of queueing them.
- Add retry policy to Notebook activity `01` as well as downstream activities.
  Use a longer delay on small SKUs, for example 3 retries with a 5 to 10 minute
  interval.
- Reduce the Spark compute requested by the notebook Environment or workspace
  Spark settings where possible. Notebook admission is based on Spark vCores,
  and the session must fit within the capacity's available vCore budget.
- If the small SKU still rejects the first Notebook activity when no other Spark
  work is active, pause manual testing and either wait for capacity smoothing to
  clear or temporarily scale up the Fabric capacity for the Pipeline run.

See Microsoft Fabric's Spark concurrency and queueing documentation for current
SKU-specific limits and queue behaviour:
https://learn.microsoft.com/en-us/fabric/data-engineering/spark-job-concurrency-and-queueing

See Fabric's high-concurrency notebook documentation for session sharing in
Pipelines:
https://learn.microsoft.com/en-us/fabric/data-engineering/configure-high-concurrency-session-notebooks-in-pipelines
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
