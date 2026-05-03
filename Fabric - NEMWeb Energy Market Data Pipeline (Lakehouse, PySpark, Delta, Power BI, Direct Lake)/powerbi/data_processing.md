# Data Processing Guide

This project processes AEMO NEMWeb ZIP files through Raw, Bronze, Silver, and
Gold layers. Local runs use CSV files under `data/`. Fabric runs use Lakehouse
Files for raw ZIPs and Delta tables for curated data.

## Storage Layout

| Layer | Local path | Fabric location | Purpose |
| --- | --- | --- | --- |
| Raw ZIP files | `data/Files/nemweb/raw_zip/{source}/yyyy/mm/dd/*.zip` | `Files/nemweb/raw_zip/{source}/yyyy/mm/dd/*.zip` | Immutable downloaded NEMWeb ZIP payloads. |
| Control tables | `data/tables/*.csv` | Lakehouse Delta tables | Ingestion manifest, run log, and raw file audit. |
| Bronze tables | `data/tables/nem_bronze_*.csv` | Lakehouse Delta tables | Parsed MMSDM rows with source metadata and source-shaped columns. |
| Silver tables | `data/tables/nem_silver_*.csv` | Lakehouse Delta tables | Typed, deduplicated market facts at useful grains. |
| Gold tables | `data/tables/nem_gold_*.csv` | Lakehouse Delta tables | Power BI-ready facts, aggregates, KPIs, and freshness tables. |

Local CSV tables mirror Fabric table names. For example,
`data/tables/nem_gold_region_5min.csv` is the local equivalent of Fabric table
`nem_gold_region_5min`.

## Raw and Control Data

### Raw ZIP Files

Raw ZIP files are downloaded from enabled sources in `config/sources.yml`. The
landing path is generated as:

`Files/nemweb/raw_zip/{source_name}/{yyyy}/{mm}/{dd}/{source_zip_name}`

The raw file contains the original AEMO ZIP bytes. It is not transformed before
landing. The checksum is calculated from the ZIP bytes and recorded in control
tables.

### `nem_raw_zip_manifest`

One row per discovered ZIP file.

| Column | Description |
| --- | --- |
| `run_id` | Unique ingestion run identifier. |
| `source_name` | Source name from `config/sources.yml`. |
| `source_url` | Full ZIP URL. |
| `source_zip_name` | ZIP filename. |
| `source_folder_url` | NEMWeb folder URL where the ZIP was discovered. |
| `file_datetime` | Timestamp parsed from the filename where available. |
| `lakehouse_path` | Local/Fabric raw ZIP landing path. |
| `checksum` | SHA-256 checksum of downloaded ZIP bytes. |
| `byte_count` | Downloaded byte count. |
| `first_seen_datetime` | UTC time the ZIP was first discovered by this run. |
| `downloaded_datetime` | UTC time the ZIP was successfully downloaded. |
| `parsed_datetime` | Reserved for parse completion tracking. |
| `status` | `downloaded`, `dry_run`, or `failed`. |
| `error_message` | Truncated failure message when discovery/download fails. |

### `nem_ingestion_log`

Run-level log rows for each ZIP discovery/download attempt.

| Column | Description |
| --- | --- |
| `run_id` | Unique ingestion run identifier. |
| `source_name` | Source name from `config/sources.yml`. |
| `source_url` | Source ZIP URL or source folder URL on listing failure. |
| `source_zip_name` | ZIP filename when available. |
| `status` | `downloaded`, `dry_run`, or `failed`. |
| `checksum` | SHA-256 checksum for downloaded ZIP bytes. |
| `first_seen_datetime` | UTC discovery timestamp. |
| `downloaded_datetime` | UTC download timestamp for successful downloads. |
| `parsed_datetime` | Reserved for parse completion tracking. |
| `row_count_bronze` | Reserved count of Bronze rows from the ZIP. |
| `row_count_silver` | Reserved count of Silver rows from the ZIP. |
| `error_message` | Truncated error details. |

### `nem_raw_file_audit`

One row per parsed raw ZIP/file group written by the Bronze notebook.

Typical columns:

| Column | Description |
| --- | --- |
| `run_id` | Parse run identifier. |
| `source_url` | Source ZIP URL. |
| `source_zip_name` | ZIP filename. |
| `inner_csv_name` | CSV member inside the ZIP. |
| `package_name` | MMSDM package from the `I` row, such as `DISPATCH`. |
| `table_name` | MMSDM table from the `I` row, such as `PRICE`. |
| `row_count` | Parsed rows for the table group/file. |
| `parsed_datetime` | UTC parse timestamp. |
| `status` | Parse status. |
| `error_message` | Parse error details when applicable. |

## Bronze Tables

Bronze parsing uses MMSDM row prefixes:

| Prefix | Meaning | Handling |
| --- | --- | --- |
| `I` | Table identity and header row | Captures package, table, and source column names. |
| `D` | Data row | Parsed into fields from the latest `I` row. |
| `C` | Control/comment row | Ignored for business rows. |

Header names are lower-case and deduplicated. Extra trailing values are kept as
`extra_column_1`, `extra_column_2`, and so on to make schema drift visible.

### `nem_bronze_mmsdm_rows`

The canonical Bronze table. It contains all parsed MMSDM `D` rows from all
supported ZIPs and table groups.

Columns vary by source MMSDM table. Common metadata columns are always added:

| Column | Description |
| --- | --- |
| Source MMSDM columns | Lower-case columns from the `I` header row, for example `settlementdate`, `regionid`, `rrp`, `totaldemand`. |
| `source_url` | Source ZIP URL. |
| `source_zip_name` | ZIP filename. |
| `inner_csv_name` | CSV member name inside the ZIP. |
| `source_folder` | NEMWeb folder name parsed from the source URL. |
| `ingestion_datetime` | UTC parse timestamp for the row. |
| `file_datetime` | Timestamp parsed from the ZIP filename where available. |
| `package_name` | MMSDM package name from the `I` row. |
| `table_name` | MMSDM table name from the `I` row. |
| `row_hash` | SHA-256 hash of raw row values plus source identity. |

### Bronze Convenience Tables

Configured Bronze convenience names include:

| Table | Intended content |
| --- | --- |
| `nem_bronze_dispatchis` | Dispatch interval data parsed from DispatchIS reports. |
| `nem_bronze_public_prices` | Public price table extracts where available. |
| `nem_bronze_unit_scada` | Unit SCADA source rows where available. |
| `nem_bronze_interconnector` | Interconnector source rows where available. |
| `nem_bronze_generation` | Generation source rows where available. |
| `nem_bronze_gas_prices` | Gas price source rows where available. |

The implemented Silver and Gold notebooks currently read from
`nem_bronze_mmsdm_rows`.

## Silver Tables

### `nem_silver_price_demand_5min`

Five-minute regional price and demand fact. Built from Bronze rows where
`package_name = DISPATCH`.

Sources:

| Bronze source | Usage |
| --- | --- |
| `table_name = PRICE` | Price, region, intervention, price row hash, source lineage. |
| `table_name = REGIONSUM` | Demand, availability, interchange, excess generation, regionsum row hash. |

Natural key: `settlement_datetime`, `region`, `intervention`.

| Column | Processing |
| --- | --- |
| `settlement_datetime` | Parsed from Bronze `settlementdate`. |
| `region` | Upper-case Bronze `regionid`. |
| `region_name` | Mapped from `region`: QLD1, NSW1, VIC1, SA1, TAS1. |
| `intervention` | Numeric cast from Bronze `intervention`. |
| `price_aud_mwh` | Numeric cast from PRICE `rrp`. |
| `source_url` | Copied from PRICE Bronze row. |
| `source_zip_name` | Copied from PRICE Bronze row. |
| `price_row_hash` | PRICE `row_hash`. |
| `demand_mw` | Numeric cast from REGIONSUM `totaldemand`. |
| `available_generation_mw` | Numeric cast from `availablegeneration`. |
| `available_load_mw` | Numeric cast from `availableload`. |
| `demand_forecast_mw` | Numeric cast from `demandforecast`. |
| `dispatchable_generation_mw` | Numeric cast from `dispatchablegeneration`. |
| `dispatchable_load_mw` | Numeric cast from `dispatchableload`. |
| `net_interchange_mw` | Numeric cast from `netinterchange`. |
| `excess_generation_mw` | Numeric cast from `excessgeneration`. |
| `regionsum_row_hash` | REGIONSUM `row_hash`. |
| `trading_date` | Date from `settlement_datetime`. |
| `year` | Year from `settlement_datetime`. |
| `month` | Month from `settlement_datetime`. |
| `day` | Day from `settlement_datetime`. |
| `interval_hour` | Hour from `settlement_datetime`. |
| `interval_minute` | Minute from `settlement_datetime`. |
| `silver_loaded_datetime` | Silver load timestamp. |
| `run_id` | Silver notebook run ID. |

### `nem_silver_regional_dispatch`

Currently written with the same rows and columns as
`nem_silver_price_demand_5min`. It is available as a semantic alias for regional
dispatch reporting.

### `nem_silver_interconnector_flows`

Five-minute interconnector flow records from Bronze `DISPATCH` /
`INTERCONNECTORRES`.

| Column | Processing |
| --- | --- |
| `settlement_datetime` | Parsed from Bronze `settlementdate`. |
| `interconnector_id` | Renamed from `interconnectorid`. |
| `intervention` | Numeric cast from `intervention`. |
| `metered_flow_mw` | Numeric cast from `meteredmwflow`. |
| `flow_mw` | Numeric cast from `mwflow`. |
| `losses_mw` | Numeric cast from `mwlosses`. |
| `marginal_value` | Numeric cast from `marginalvalue`. |
| `export_limit_mw` | Numeric cast from `exportlimit`. |
| `import_limit_mw` | Numeric cast from `importlimit`. |
| `trading_date` | Date from `settlement_datetime`. |
| `silver_loaded_datetime` | Silver load timestamp. |
| `run_id` | Silver notebook run ID. |

### `nem_silver_generation_by_unit`

Optional unit generation table. Built when Bronze data contains both `duid` and
`dispatchablegeneration`.

| Column | Processing |
| --- | --- |
| `settlement_datetime` | Parsed from Bronze `settlementdate`. |
| `duid` | Dispatch unit identifier from Bronze. |
| `generation_mw` | Numeric cast from `dispatchablegeneration`. |
| `trading_date` | Date from `settlement_datetime`. |
| `silver_loaded_datetime` | Silver load timestamp. |
| `run_id` | Silver notebook run ID. |

### Configured but Not Yet Implemented Silver Tables

These names are reserved in `config/tables.yml`, but no implemented notebook
currently writes them:

| Table | Intended content |
| --- | --- |
| `nem_silver_generation_by_fuel` | Generation aggregated or mapped to fuel type. |
| `nem_silver_market_notices` | Market notice source records. |
| `nem_silver_gas_prices` | Typed gas price records. |

## Gold Tables

Gold tables are deterministic reporting tables. Fabric writes Delta tables.
Local runs overwrite matching CSV files in `data/tables`.

### `nem_gold_region_5min`

Main Power BI regional fact table. Built from
`nem_silver_price_demand_5min`.

| Gold column | Raw-to-Gold processing |
| --- | --- |
| `settlement_datetime` | Raw `D` row value under Bronze `settlementdate`; parsed to timestamp in Silver; carried to Gold. |
| `trading_date` | Derived from `settlement_datetime` in Silver and recalculated/retained in Gold. |
| `year` | Derived from `settlement_datetime`. |
| `month` | Derived from `settlement_datetime`. |
| `day` | Derived from `settlement_datetime`. |
| `interval_hour` | Derived from `settlement_datetime`. |
| `interval_minute` | Derived from `settlement_datetime`. |
| `region` | Raw `regionid`; upper-cased in Silver; carried to Gold. |
| `region_name` | Derived from `region` using the project region map. |
| `intervention` | Raw `intervention`; cast to integer/numeric in Silver; carried to Gold. |
| `price_aud_mwh` | Raw PRICE `rrp`; cast to numeric in Silver; carried to Gold. |
| `demand_mw` | Raw REGIONSUM `totaldemand`; cast to numeric in Silver; carried to Gold. |
| `available_generation_mw` | Raw `availablegeneration`; cast to numeric in Silver; carried to Gold. |
| `available_load_mw` | Raw `availableload`; cast to numeric in Silver; carried to Gold. |
| `demand_forecast_mw` | Raw `demandforecast`; cast to numeric in Silver; carried to Gold. |
| `dispatchable_generation_mw` | Raw `dispatchablegeneration`; cast to numeric in Silver; carried to Gold. |
| `dispatchable_load_mw` | Raw `dispatchableload`; cast to numeric in Silver; carried to Gold. |
| `net_interchange_mw` | Raw `netinterchange`; cast to numeric in Silver; carried to Gold. |
| `excess_generation_mw` | Raw `excessgeneration`; cast to numeric in Silver; carried to Gold. |
| `dashboard_demand_mw` | Raw `clearedsupply`; aligns the NEM dashboard demand bar. |
| `semi_scheduled_generation_mw` | Raw `semischedule_clearedmw`; aligns NEM dashboard semi-scheduled generation. |
| `scheduled_generation_mw` | `dispatchable_generation_mw - semi_scheduled_generation_mw`. |
| `dashboard_generation_mw` | `dispatchable_generation_mw`; aligns the NEM dashboard total generation bar. |
| `price_band` | Derived in Gold from `price_aud_mwh`: `< 0` Negative, `0-299.99` Normal, `300-999.99` High, `>= 1000` Extreme. |
| `is_negative_price` | `price_aud_mwh < 0`. |
| `is_high_price` | `price_aud_mwh >= 300`. |
| `is_extreme_price` | `price_aud_mwh >= 1000`. |
| `rolling_avg_price_1h` | One-hour rolling average of `price_aud_mwh` by `region`, ordered by `settlement_datetime`. |
| `rolling_avg_demand_1h` | One-hour rolling average of `demand_mw` by `region`, ordered by `settlement_datetime`. |
| `gold_loaded_datetime` | Gold load timestamp. |
| `run_id` | Gold notebook run ID. |

### `nem_gold_dashboard_current_snapshot`

Latest interval per region from `nem_gold_region_5min`.

Columns are the same as `nem_gold_region_5min`. Processing selects the latest
`settlement_datetime` for each `region`.

### `nem_gold_dashboard_supply_demand_components`

Current long-format supply and demand component table from
`nem_gold_dashboard_current_snapshot`. It supports AEMO-style regional stacked
bar visuals.

| Gold column | Processing |
| --- | --- |
| `settlement_datetime` | Carried from current snapshot. |
| `trading_date` | Carried from current snapshot. |
| `region` | Carried from current snapshot. |
| `region_name` | Carried from current snapshot. |
| `metric_group` | `Demand` or `Generation`. |
| `component` | `Demand`, `Scheduled Generation`, or `Semi-scheduled Generation`. |
| `component_sort_order` | Sort key for component display. |
| `value_mw` | Demand or generation component MW value. |
| `gold_loaded_datetime` | Carried from current snapshot. |
| `run_id` | Carried from current snapshot. |

### `nem_gold_region_30min`

Thirty-minute regional aggregate from `nem_gold_region_5min`.

| Gold column | Processing |
| --- | --- |
| `region` | Grouping column from `nem_gold_region_5min`. |
| `region_name` | Grouping column from `nem_gold_region_5min`. |
| `settlement_30min` | `settlement_datetime` floored/windowed to a 30-minute interval. |
| `price_aud_mwh` | Average `price_aud_mwh` in the 30-minute region group. |
| `demand_mw` | Average `demand_mw` in the 30-minute region group. |
| `max_price_aud_mwh` | Maximum `price_aud_mwh` in the group. |
| `min_price_aud_mwh` | Minimum `price_aud_mwh` in the group. |
| `high_price_interval_count` | Count of five-minute rows where `is_high_price` is true. |
| `negative_price_interval_count` | Count of five-minute rows where `is_negative_price` is true. |
| `trading_date` | Date from `settlement_30min`. |
| `interval_hour` | Hour from `settlement_30min`. |
| `interval_minute` | Minute from `settlement_30min`. |

### `nem_gold_region_daily`

Daily regional summary from `nem_gold_region_5min`.

| Gold column | Processing |
| --- | --- |
| `region` | Grouping column from `nem_gold_region_5min`. |
| `region_name` | Grouping column from `nem_gold_region_5min`. |
| `trading_date` | Grouping date from `nem_gold_region_5min`. |
| `daily_avg_price` | Average daily `price_aud_mwh`. |
| `daily_max_price` | Maximum daily `price_aud_mwh`. |
| `daily_min_price` | Minimum daily `price_aud_mwh`. |
| `daily_price_volatility` | Standard deviation of daily `price_aud_mwh`. |
| `daily_avg_demand` | Average daily `demand_mw`. |
| `daily_max_demand` | Maximum daily `demand_mw`. |
| `high_price_interval_count` | Count of high-price five-minute intervals. |
| `extreme_price_interval_count` | Count of extreme-price five-minute intervals. |
| `negative_price_interval_count` | Count of negative-price five-minute intervals. |

### `nem_gold_price_spikes`

Drill-through event table from `nem_gold_region_5min`.

Columns are the same as `nem_gold_region_5min`. Processing keeps rows where
`is_high_price` or `is_negative_price` is true. Extreme price rows are included
because every extreme row also has `is_high_price = true`.

### `nem_gold_dashboard_kpis`

Single-row dashboard KPI table from `nem_gold_dashboard_current_snapshot`.

| Gold column | Processing |
| --- | --- |
| `latest_settlement_datetime` | Maximum `settlement_datetime` in the current snapshot. |
| `avg_current_price_aud_mwh` | Average `price_aud_mwh` across latest regional rows. |
| `current_total_demand_mw` | Sum of `demand_mw` across latest regional rows. |
| `regions_available` | Distinct count of `region` in the current snapshot. |
| `run_id` | Gold notebook run ID. |
| `gold_loaded_datetime` | Gold load timestamp. |

### `nem_gold_data_freshness`

Operational freshness table from `nem_gold_dashboard_kpis`.

| Gold column | Processing |
| --- | --- |
| `latest_settlement_datetime` | Copied from `nem_gold_dashboard_kpis`. |
| `last_successful_ingestion_datetime` | `gold_loaded_datetime` renamed from KPI table. |
| `run_id` | Copied from `nem_gold_dashboard_kpis`. |
| `freshness_minutes` | Difference in minutes between current/load time and `latest_settlement_datetime`. |
| `status` | `Fresh` when `<= 15` minutes, `Delayed` when `<= 60`, otherwise `Stale`. |

### `nem_gold_interconnector_flows_5min`

Optional interconnector reporting table from `nem_silver_interconnector_flows`.

| Gold column | Processing |
| --- | --- |
| `settlement_datetime` | Carried from Silver. |
| `interconnector_id` | Carried from Silver. |
| `intervention` | Carried from Silver. |
| `metered_flow_mw` | Carried from Silver. |
| `flow_mw` | Carried from Silver. |
| `losses_mw` | Carried from Silver. |
| `marginal_value` | Carried from Silver. |
| `export_limit_mw` | Carried from Silver for export limit monitoring. |
| `import_limit_mw` | Carried from Silver for import limit monitoring. |
| `trading_date` | Carried from Silver. |
| `silver_loaded_datetime` | Carried from Silver. |
| `run_id` | Carried from Silver in local output; Fabric output keeps source columns and adds derived fields. |
| `flow_direction` | `Forward` when `flow_mw >= 0`, otherwise `Reverse`. |
| `interval_hour` | Hour from `settlement_datetime`. |
| `interval_minute` | Minute from `settlement_datetime`. |

### Configured but Not Yet Implemented Gold Tables

These names are reserved in `config/tables.yml`, but no implemented notebook
currently writes them:

| Table | Intended content |
| --- | --- |
| `nem_gold_generation_mix_5min` | Five-minute generation mix by fuel or technology. |
| `nem_gold_renewable_penetration` | Renewable share and penetration metrics. |
| `nem_gold_supply_demand_balance` | Supply, demand, reserve, and balance metrics. |
| `nem_gold_gas_price_summary` | Gas price reporting summary. |

## End-to-End Processing Flow

1. `01_ingest_nemweb_zip_files.ipynb` discovers enabled NEMWeb source folders,
   filters ZIPs by lookback window, skips already successful manifest URLs, and
   lands unseen ZIP bytes.
2. `02_parse_bronze_tables.ipynb` reads unparsed raw ZIPs, parses MMSDM `I` and
   `D` rows, writes `nem_bronze_mmsdm_rows`, writes available Bronze convenience
   tables, and records parse audit rows.
3. `03_build_silver_tables.ipynb` converts source-shaped Bronze rows into typed
   Silver facts for regional price/demand, interconnector flows, and optional
   generation by unit.
4. `04_build_gold_dashboard_tables.ipynb` builds Power BI-ready Gold tables from
   Silver data. Local runs now execute the Gold steps and write CSV outputs;
   Fabric runs write Delta tables.

## Idempotency and Reprocessing

- Ingestion skips ZIP URLs already recorded with successful manifest statuses.
- Raw ZIP paths are deterministic from source name, file date, and filename.
- Silver tables are deduplicated by natural keys in local helpers.
- Gold tables are deterministic aggregates and are overwritten on each Gold run.
