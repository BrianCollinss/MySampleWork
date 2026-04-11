# Procurement Risk, Assurance, and Governance Reporting

This is a portfolio-quality Python project that simulates a recurring monthly procurement risk, assurance, and governance reporting workflow using a true Medallion architecture.

The project is designed for public-sector governance, procurement assurance, internal controls, and internal audit use cases. It ingests raw contract disclosure files from a named source folder, infers schema from headings and sampled rows, standardises structurally different files into a canonical Silver layer, and publishes Gold marts for reporting and dashboarding.

## Why this repo is relevant

This project is intentionally aligned to roles mentioning:

- risk reporting
- assurance reporting
- governance reporting
- procurement analytics
- audit analytics
- internal controls analytics

It emphasises:

- traceability from raw source files to business outputs
- configurable risk rules and explicit metric definitions
- schema drift handling across disclosure extracts
- reproducibility through a conda environment and config-driven execution
- clean, modular Python rather than notebook-only analysis

## Medallion architecture

This repository now uses explicit Bronze, Silver, and Gold layers.

### Bronze

Raw input starts in `data/<raw-folder-name>/`.

For example:

```text
data/
  Griffith University Contract Disclosure Report/
    griffith-university-contract-disclosure-report-2024-2025.csv
    griffith-university-contract-disclosure-report-2025-2026.csv
```

The Bronze layer:

- reads every CSV/XLSX file in the selected raw folder
- normalises source headings to `snake_case`
- samples headings and rows to infer lightweight schema characteristics
- scores likely canonical matches for columns with different source names
- persists a unioned raw row store plus schema and file registries

Persisted Bronze outputs for each run:

- `outputs/<timestamp> - <organisation>/data/1_bronze/bronze_contracts.csv`
- `outputs/<timestamp> - <organisation>/data/1_bronze/bronze_schema_registry.csv`
- `outputs/<timestamp> - <organisation>/data/1_bronze/bronze_file_registry.csv`

### Silver

The Silver layer transforms Bronze into a canonical contracts table by:

- resolving similar columns to a standard schema
- standardising dates and numeric values
- preserving unmapped attributes in metadata
- applying reusable data quality and validation checks

Persisted Silver outputs for each run:

- `outputs/<timestamp> - <organisation>/data/2_silver/silver_contracts.csv`
- `outputs/<timestamp> - <organisation>/data/2_silver/silver_validation_issues.csv`
- `outputs/<timestamp> - <organisation>/data/2_silver/silver_data_quality_summary.csv`

### Gold

The Gold layer produces BI-ready marts for dashboarding and governance reporting:

- summary metrics by reporting period
- supplier concentration marts
- supplier concentration decomposition marts
- top supplier marts
- supplier segmentation marts
- agency and category marts
- exceptions register
- scorecard outputs

Persisted Gold outputs for each run:

- `outputs/<timestamp> - <organisation>/data/3_gold/gold_summary_metrics.csv`
- `outputs/<timestamp> - <organisation>/data/3_gold/gold_supplier_concentration.csv`
- `outputs/<timestamp> - <organisation>/data/3_gold/gold_supplier_concentration_decomposition.csv`
- `outputs/<timestamp> - <organisation>/data/3_gold/gold_top_suppliers_by_value.csv`
- `outputs/<timestamp> - <organisation>/data/3_gold/gold_supplier_segments.csv`
- `outputs/<timestamp> - <organisation>/data/3_gold/gold_supplier_cluster_summary.csv`
- `outputs/<timestamp> - <organisation>/data/3_gold/gold_agency_summary.csv`
- `outputs/<timestamp> - <organisation>/data/3_gold/gold_category_summary.csv`
- `outputs/<timestamp> - <organisation>/data/3_gold/gold_missing_data_rates.csv`
- `outputs/<timestamp> - <organisation>/data/3_gold/gold_exceptions_register.csv`
- `outputs/<timestamp> - <organisation>/data/3_gold/gold_scorecard.csv`

Business-facing outputs are also written to timestamped, organisation-labelled run folders:

- `outputs/<timestamp> - <organisation>/tables/`
- `outputs/<timestamp> - <organisation>/charts/`
- `outputs/<timestamp> - <organisation>/reports/executive_summary.md`

## Schema inference and standardisation approach

The ingestion layer does not assume source files share identical columns.

Instead it:

1. Reads headings and a configurable sample of rows per file.
2. Infers a lightweight column profile for each field:
   `normalised_name`, inferred dtype, non-null rate, and sample values.
3. Attempts exact alias matching using configured synonyms in `config/analysis_config.yaml`.
4. Falls back to a similarity algorithm combining:
   name token overlap, string similarity, and dtype compatibility.
5. Stores the recommended canonical match and score in the Bronze schema registry.
6. Uses those matches to build the canonical Silver contracts table.

This makes the pipeline more resilient to schema drift such as:

- `Agency Name` vs `Department`
- `Reference Number` vs `Contract Number`
- `Vendor Name` vs `Supplier`
- `Total Value` vs `Contract Amount`

## Supplier risk segmentation

The Gold layer now includes supplier risk segmentation to support governance and assurance review.

It builds supplier-level features such as:

- total spend
- contract count
- average contract value
- repeated award frequency
- concentration exposure
- procurement method diversity
- limited/direct method rate
- data quality issue rate
- missing data rate

Suppliers are then clustered into business-readable segments such as:

- `high-value strategic`
- `fragmented low-value repeat`
- `sporadic high-risk`
- `low-information vendor`
- `mixed profile`

The segmentation output is written to:

- `outputs/<timestamp> - <organisation>/data/3_gold/gold_supplier_segments.csv`
- `outputs/<timestamp> - <organisation>/data/3_gold/gold_supplier_cluster_summary.csv`
- `outputs/<timestamp> - <organisation>/tables/supplier_segments.csv`
- `outputs/<timestamp> - <organisation>/tables/supplier_cluster_summary.csv`
- `outputs/<timestamp> - <organisation>/charts/supplier_risk_segments.png`

## Supplier concentration decomposition

The Gold layer also includes supplier concentration decomposition so the report
can show which suppliers drove concentration movement from one reporting period
to the next.

This analysis goes beyond HHI and Top-N share by calculating, for each adjacent
period pair:

- each supplier's prior-period spend share
- each supplier's current-period spend share
- the change in supplier share
- whether the supplier sat inside the Top-N supplier group in either period
- the supplier's contribution to the net change in Top-N concentration

Outputs include:

- `outputs/<timestamp> - <organisation>/data/3_gold/gold_supplier_concentration_decomposition.csv`
- `outputs/<timestamp> - <organisation>/tables/supplier_concentration_decomposition.csv`
- `outputs/<timestamp> - <organisation>/charts/supplier_concentration_change_drivers.png`

## Canonical contract schema

The Silver layer targets these core fields:

- `source_file`
- `source_folder`
- `source_agency`
- `contract_id`
- `supplier_name`
- `supplier_abn`
- `contract_title`
- `procurement_category`
- `procurement_method`
- `contract_start_date`
- `contract_end_date`
- `contract_value`
- `reporting_period`
- `publish_date`

Unmapped raw attributes are preserved in `raw_metadata`.

## Governance and assurance use case

A governance and assurance team wants a recurring monthly reporting pack answering questions such as:

- How much procurement activity occurred this period?
- Which suppliers dominate spend and where is concentration increasing?
- Which contracts sit near thresholds or suggest repeat small awards?
- Which files have missing governance fields or weak disclosure quality?
- What changed from the prior period and what should be reviewed next?

## Data source notes

The project is built for Australian public contract disclosure extracts such as Commonwealth or Queensland releases. A demo input folder is included at [data/Griffith University Contract Disclosure Report](data/Griffith%20University%20Contract%20Disclosure%20Report).

For portfolio use, replace or supplement the demo folder with downloaded public disclosure files and pass that folder name to the CLI.

## Licence

This project includes a proprietary portfolio licence in [LICENSE](LICENSE).

The code and project materials are provided as portfolio sample work rather
than as an open-source reusable package. Public procurement disclosure data
used as source input remains subject to the original publisher's dataset
conditions.

## Repository structure

```text
procurement-risk-assurance-reporting/
  config/
    analysis_config.yaml
    global_config.yaml
  data/
    <raw-folder-name>/
  src/
    ingest/
    clean/
    metrics/
    reporting/
    utils/
    main.py
  outputs/
    <timestamp> - <organisation>/
      config/
        analysis_config.yaml
        global_config.yaml
      data/
        1_bronze/
        2_silver/
        3_gold/
      charts/
      tables/
      reports/
  tests/
  environment.yml
  requirements.txt
```

## Conda environment

Create the environment:

```bash
conda env create -f environment.yml
```

Activate it:

```bash
conda activate procurement-risk-assurance-reporting
```

## Analyst workflow

If you are the analyst running this project, the normal sequence is:

1. Put your source files into a single folder under `data/`.

Example:

```text
data/
  Griffith University Contract Disclosure Report/
    griffith-university-contract-disclosure-report-2023-2024.csv
    griffith-university-contract-disclosure-report-2024-2025.csv
```

2. Open [config/analysis_config.yaml](config/analysis_config.yaml).

Update these fields first:

- `runtime.organisation_name`: the reporting entity name
- `runtime.default_raw_folder`: the source folder you want to run by default
- `runtime.excluded_file_patterns`: optional files to ignore

You only need to update the schema aliases (schema_mappings) or risk thresholds (risk_rules)
if the new source files behave differently or the reporting rules need tuning.
You can also tune `advanced_analytics.supplier_segmentation` if you want to change the number of supplier clusters.

3. Open [config/global_config.yaml](config/global_config.yaml).

Use this file to control run-output retention:

- `output_management.retention.value`: how long timestamped output folders are kept
- `output_management.retention.unit`: `minutes`, `hours`, or `days`

Each time the pipeline runs, it checks `outputs/` and removes older timestamp-prefixed run folders outside that retention window.
The run also removes obvious local noise such as `desktop.ini`, `__pycache__`, `.pytest_cache`, and similar cache folders.

4. Run the pipeline.

If you want to use the folder named in config:

```bash
python -m src.main
```

If you want to override the folder for a one-off run:

```bash
python -m src.main --raw-folder "Some Agency Disclosure Pack"
```

5. Review the Medallion outputs written under the current run folder.

- Config snapshot: `outputs/<timestamp> - <organisation>/config/`
- Bronze: `outputs/<timestamp> - <organisation>/data/1_bronze/`
- Silver: `outputs/<timestamp> - <organisation>/data/2_silver/`
- Gold: `outputs/<timestamp> - <organisation>/data/3_gold/`

6. Review the business-facing reporting outputs.

- `outputs/<timestamp> - <organisation>/tables/`
- `outputs/<timestamp> - <organisation>/charts/`
- `outputs/<timestamp> - <organisation>/reports/executive_summary.md`

## How to run

The quickest path is:

1. Activate the conda environment.
2. Put files in `data/<your-folder-name>/`.
3. Set `runtime.default_raw_folder` in `config/analysis_config.yaml`.
4. Set output retention in `config/global_config.yaml` if you want something other than the default `10 minutes`.
5. Run:

```bash
python -m src.main
```

Optional config override:

```bash
python -m src.main --raw-folder "Some Agency Disclosure Pack" --config config/analysis_config.yaml
```

## Reporting outputs

The reporting layer generates:

- `outputs/<timestamp> - <organisation>/tables/summary_metrics.csv`
- `outputs/<timestamp> - <organisation>/tables/exceptions_register.csv`
- `outputs/<timestamp> - <organisation>/tables/data_quality_summary.csv`
- `outputs/<timestamp> - <organisation>/tables/supplier_segments.csv`
- `outputs/<timestamp> - <organisation>/tables/supplier_cluster_summary.csv`
- executive-style chart PNGs when `matplotlib` is available
- `outputs/<timestamp> - <organisation>/reports/executive_summary.md`

## Metric definitions

- `total_contract_value`: Sum of standardised contract values for the reporting grain
- `total_contract_count`: Count of contracts for the reporting grain
- `average_contract_value`: Mean contract value
- `median_contract_value`: Median contract value
- `top_supplier_share`: Largest supplier spend share within a reporting period
- `spend_concentration_hhi`: Supplier concentration index scaled from 0 to 10,000
- `period_on_period_spend_change_pct`: Percentage change in spend from the prior period
- `missing_data_rate`: Null rate across tracked Silver fields

## Rule definitions

Rules are configured in [config/analysis_config.yaml](config/analysis_config.yaml). Key thresholds include:

- supplier concentration
- repeated small contracts
- abrupt spend growth
- near-threshold contract bands
- missing critical governance fields
- high null rates

Each exception includes:

- `exception_id`
- `category`
- `rule_name`
- `severity`
- `affected_record_or_key`
- `explanation`
- `recommended_follow_up`

## Tests

Run:

```bash
pytest -q tests -p no:cacheprovider
```

## Limitations and assumptions

- Similarity-based schema matching is robust for light schema drift, but highly ambiguous fields may still need alias tuning in config.
- Public disclosure extracts do not always provide ABN, full procurement category, or reliable period fields.
- Duplicate detection is heuristic and intended for assurance triage rather than definitive legal deduplication.
- Chart generation is skipped gracefully if `matplotlib` is not installed in the active environment.

## Future enhancements

- Add jurisdiction-specific mapping packs for Commonwealth, Queensland, and university disclosures
- Persist parquet alongside CSV for larger monthly data volumes
- Add interactive Gold presentation marts for BI tools
- Add trend anomaly detection and richer audit-evidence outputs
