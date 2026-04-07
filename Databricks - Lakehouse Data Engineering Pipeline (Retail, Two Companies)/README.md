# Databricks Lakehouse Data Engineering Pipeline (Retail, Two Companies)

This project demonstrates a Databricks lakehouse pipeline that combines retail data from a parent company and a child company into a shared analytics model. The workflow uses PySpark, Spark SQL, Delta Lake, Unity Catalog, and Databricks SQL to ingest child-company raw files, standardize dimensions, aggregate facts, and publish a dashboard-ready denormalized view.

## Project Summary

The project is built to showcase a realistic multi-entity data engineering scenario rather than a single-source toy pipeline:

- The parent company is treated as an existing lakehouse environment with its core Medallion architecture already in place.
- The main implementation focus of this repo is the child-company Medallion pipeline and the logic needed to conform it to the parent reporting model.
- Parent-company data arrives as curated full-load CSV extracts plus an incremental refresh file used for integration.
- Child-company data arrives as raw dimension extracts and daily order files that need cleansing, harmonization, and monthly aggregation.
- Gold-layer tables reconcile both companies into a common dimensional model for reporting.

## Resume-Ready Version

Databricks Lakehouse Data Engineering Pipeline (Retail, Two Companies): Built a Databricks lakehouse pipeline that integrated a child retail business into an existing parent-company analytics model. Ingested child-company raw CSV data into Bronze Delta tables, applied PySpark cleansing and standardization in Silver, and merged curated outputs into shared Gold customer, product, price, date, and order models. Implemented incremental fact-processing logic that recalculates only affected months from newly arrived child order files so daily operational data aligns with the parent company’s monthly reporting grain, then exposed a denormalized serving view for downstream BI and dashboarding.

## Architecture

The pipeline follows a lakehouse pattern with explicit merge logic between company datasets:

- Parent-company assumption: parent Bronze, Silver, and Gold tables already exist and are the target model to align to
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

This repo focuses on the child-company side of the solution. The parent-company Medallion layers are assumed to already exist, so the project work centers on ingesting, standardizing, and integrating the child-company data into that established environment.

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
- recalculates affected months only rather than rebuilding the entire fact history
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
- The parent company is assumed to already have an operational Medallion architecture, so this repo does not rebuild the full parent pipeline from raw source.
- The child-company pipeline is the primary implementation focus and is built across Bronze, Silver, and Gold layers.
- Child-company dimension processing is handled as batch-style full loads from raw source files.
- Child-company fact processing supports incremental updates by reading newly arrived daily order files into staging tables, identifying the impacted months, recomputing monthly totals for just those months, and merging the refreshed aggregates into the shared Gold fact table.
- This means the project is not row-level CDC or streaming; it is incremental at the monthly aggregate refresh level for the child-company fact pipeline.
- Parent-company incremental updates are applied separately through Databricks SQL.
- The current notebooks are designed as interactive notebook jobs rather than a fully orchestrated production workflow.

## Technologies Used

- Databricks
- PySpark
- Spark SQL
- Delta Lake
- Unity Catalog
- Databricks SQL
- CSV-based batch ingestion
