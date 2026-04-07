# Databricks Lakehouse Data Engineering Pipeline (Retail, Two Companies)

This project demonstrates a Databricks lakehouse pipeline that combines retail data from a parent company and a child company into a shared analytics model. The workflow uses PySpark, Spark SQL, Delta Lake, Unity Catalog, and Databricks SQL to ingest raw files, standardize dimensions, aggregate fact data, and publish a dashboard-ready denormalized view.

## Project Summary

The project is built to showcase a realistic multi-entity data engineering scenario rather than a single-source toy pipeline:

- Parent-company data arrives as curated full-load CSV extracts plus an incremental refresh file.
- Child-company data arrives as raw dimension extracts and daily order files that need cleansing and monthly aggregation.
- Gold-layer tables reconcile both companies into a common dimensional model for reporting.

This version adds light production-minded polish similar to the e-Commerce project:

- a root README with run order and architecture notes
- clearer notebook-level context for setup, dimensions, and fact processing
- a lightweight automation script to inventory the repo structure and notebook estate

## Resume-Ready Version

Databricks Lakehouse Data Engineering Pipeline (Retail, Two Companies): Built a Databricks lakehouse pipeline that consolidated retail data from parent and child business entities into shared Gold-layer dimensions and facts. Ingested raw CSV data into Bronze Delta tables, applied PySpark cleansing and standardization in Silver, and merged curated outputs into analytics-ready customer, product, price, date, and order models. Implemented monthly aggregation logic for child-company transactions so they aligned with the parent company grain, then exposed a denormalized serving view for downstream BI and dashboarding.

## Architecture

The pipeline follows a lakehouse pattern with explicit merge logic between company datasets:

- Bronze: ingest raw child-company extracts and stage incoming fact files
- Silver: clean and standardize child-company dimension and order data
- Gold: publish child-company conformed tables, merge them into parent-company Gold tables, and create a serving view

## Repository Structure

```text
0_data/
  1_parent_company/
    full_load/
    incremental_load/
  2_child_company/
    full_load/
      customers/
      gross_price/
      orders/landing/
      products/
    incremental_load/
      orders/
1_setup/
  dim_date_table_creation.ipynb
  setup_catalog.ipynb
  utilities.ipynb
2_dimension_data_processing/
  1_customers_data_processing.ipynb
  2_products_data_processing.ipynb
  3_pricing_data_processing.ipynb
3_fact_data_processing/
  1_full_load_fact.ipynb
  2_incremental_load_fact.ipynb
  3_import_parent_incremental_data.dbquery.ipynb
4_dashboarding/
  1_large_denormalized_view.dbquery.ipynb
scripts/
  project_inventory.py
README.md
```

## Data Layout

The local data folder is split by business entity and load type:

- `0_data/1_parent_company/full_load/`: baseline customer, product, price, and fact extracts
- `0_data/1_parent_company/incremental_load/`: parent incremental fact refresh plus the source query
- `0_data/2_child_company/full_load/`: child master data plus landed daily order files
- `0_data/2_child_company/incremental_load/orders/`: child incremental daily order drops

Within Databricks, the notebooks expect:

- child dimension and fact source files in S3-style paths parameterized by widget inputs
- parent incremental fact files in Unity Catalog volume paths referenced by the SQL notebook

## Pipeline Flow

### 1. Environment Setup

`1_setup/setup_catalog.ipynb`

- creates the `fmcg` catalog if it does not already exist
- creates the `bronze`, `silver`, and `gold` schemas

`1_setup/utilities.ipynb`

- centralizes commonly reused schema names

`1_setup/dim_date_table_creation.ipynb`

- generates a month-grain date dimension in `fmcg.gold.dim_date`

### 2. Dimension Processing

`2_dimension_data_processing/1_customers_data_processing.ipynb`

- ingests child customer data to Bronze
- standardizes names, city values, casing, and customer attributes in Silver
- maps child attributes into the shared Gold customer model
- merges child customers into the parent Gold customer table

`2_dimension_data_processing/2_products_data_processing.ipynb`

- ingests child product data to Bronze
- fixes duplicates, casing issues, and attribute inconsistencies in Silver
- standardizes product fields to align with the parent-company model
- merges child products into the parent Gold product table

`2_dimension_data_processing/3_pricing_data_processing.ipynb`

- ingests child gross-price data to Bronze
- normalizes month and price values in Silver
- derives monthly product prices for Gold
- merges child pricing into the parent Gold price table

### 3. Fact Processing

`3_fact_data_processing/1_full_load_fact.ipynb`

- ingests child full-load order files from a landing area
- archives processed files
- cleans order data and joins products
- aggregates child daily orders to monthly grain
- merges the child result into the parent `fact_orders` table

`3_fact_data_processing/2_incremental_load_fact.ipynb`

- processes newly arrived child incremental order files
- uses staging tables to isolate the current delta
- recalculates affected months only
- merges refreshed monthly totals into the parent `fact_orders` table

`3_fact_data_processing/3_import_parent_incremental_data.dbquery.ipynb`

- loads parent-company incremental fact data into the shared Gold fact table

### 4. Serving Layer

`4_dashboarding/1_large_denormalized_view.dbquery.ipynb`

- creates `fmcg.gold.vw_fact_orders_enriched`
- joins the fact table to conformed dimensions for BI-friendly querying

## Suggested Run Order

1. `1_setup/setup_catalog.ipynb`
2. `1_setup/dim_date_table_creation.ipynb`
3. Dimension notebooks in `2_dimension_data_processing/`
4. `3_fact_data_processing/1_full_load_fact.ipynb`
5. `3_fact_data_processing/3_import_parent_incremental_data.dbquery.ipynb`
6. `3_fact_data_processing/2_incremental_load_fact.ipynb` when incremental child files are available
7. `4_dashboarding/1_large_denormalized_view.dbquery.ipynb`

## Processing Notes

- The project is a hybrid full-load plus incremental pattern.
- Child-company transactions originate at daily grain, but reporting is standardized to monthly grain before merge.
- Parent-company incremental updates are applied separately through Databricks SQL.
- The current notebooks are designed as interactive notebook jobs rather than a fully orchestrated production workflow.

## Lightweight Automation

Use the inventory script to generate a quick structure summary from the repo root:

```powershell
python scripts/project_inventory.py
```

It reports:

- folder-level file counts for `0_data`
- notebook cell counts and first non-empty cell previews
- a compact inventory useful for repo review or README refresh work

## Technologies Used

- Databricks
- PySpark
- Spark SQL
- Delta Lake
- Unity Catalog
- Databricks SQL
- CSV-based batch ingestion
