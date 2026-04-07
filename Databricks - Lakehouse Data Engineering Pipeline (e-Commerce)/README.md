# Databricks Lakehouse Data Engineering Pipeline (e-Commerce)

This project demonstrates an end-to-end e-commerce data engineering workflow built in Databricks using the medallion architecture. Raw CSV data is ingested into Bronze Delta tables, cleaned and standardized in Silver, and transformed into Gold dimension and fact tables designed for analytics and BI reporting.

## Project Summary

I built this project to showcase practical lakehouse engineering skills across ingestion, transformation, data quality remediation, dimensional modeling, analytics-ready table design, and lightweight production-minded improvements such as configuration-driven paths, managed reference tables, and validation checks.

Highlights:
- Built a medallion pipeline in Databricks across Bronze, Silver, and Gold layers
- Created and managed Unity Catalog objects for a dedicated e-commerce training environment
- Ingested raw dimension and fact data from volume-backed CSV sources into Delta tables
- Applied data quality rules to handle duplicates, nulls, malformed values, inconsistent casing, spelling issues, and mixed date/timestamp formats
- Modeled Gold-layer dimension and fact tables for downstream BI and reporting use cases
- Added business-ready enrichments such as customer region mapping, date attributes, coupon flags, and normalized INR revenue metrics
- Added configurable catalog and source-path settings plus managed reference tables for reusable lookup data
- Added lightweight validation checks to fail early if configured inputs or curated outputs are empty

## Resume-Ready Version

Databricks Lakehouse Data Engineering Pipeline (e-Commerce): Designed and built an end-to-end e-commerce lakehouse pipeline in Databricks using the medallion architecture and Delta tables. Ingested raw dimension and order data into Bronze, applied PySpark-based cleansing and standardization in Silver, and produced BI-ready Gold dimension and fact tables for products, customers, dates, and order items. Implemented data quality remediation for malformed numeric fields, duplicates, nulls, mixed timestamp formats, inconsistent codes, and product attribute anomalies, then enriched curated outputs with region mapping, calendar attributes, coupon indicators, and currency-normalized sales metrics.

## Architecture

The pipeline follows a standard medallion pattern:

- Bronze: raw ingestion from CSV files into Delta tables with source metadata and ingestion timestamps
- Silver: cleansing, type casting, deduplication, anomaly correction, and standardization
- Gold: star-schema-style curated outputs for analytics and dashboarding

## Processing Mode

This project currently runs as a batch full-refresh pipeline, not a true incremental pipeline.

- Bronze, Silver, and Gold tables are rebuilt with overwrite-style writes
- The setup process uploads the full CSV set, including all daily `order_items` files, into the source volume
- The notebooks do not currently use watermarks, merge logic, CDC patterns, or Auto Loader / streaming ingestion

The project is still incremental-friendly in structure because order data is stored as daily files and Bronze includes ingestion metadata, but incremental processing has not been implemented in the current version.

## Repository Structure

```text
0_data/
  brands.csv
  category.csv
  customers.csv
  date.csv
  products.csv
  order_items/
    order_items_2025-08-01.csv
    ...
    order_items_2025-10-31.csv
1_setup/
  setup_catalog.ipynb
2_medallion_processing_dim/
  1_dim_bronze.ipynb
  2_dim_silver.ipynb
  3_dim_gold.ipynb
3_medallion_processing_fact/
  1_fact_bronze.ipynb
  2_fact_silver.ipynb
  3_fact_gold.ipynb
4_dashboard/
  denormalise_table.dbquery.ipynb
README.md
```

## Data Assets

The repository also includes the local raw source files used for the pipeline:

- `0_data/brands.csv`
- `0_data/category.csv`
- `0_data/customers.csv`
- `0_data/date.csv`
- `0_data/products.csv`
- `0_data/order_items/` with daily order snapshots from `2025-08-01` through `2025-10-31`

In Databricks, these files are intended to be loaded into volume-backed paths under `/Volumes/training_0002_ecommerce/source_data/raw_data/...` before the Bronze notebooks are executed.

## Pipeline Flow

### 1. Environment Setup

`1_setup/setup_catalog.ipynb`

- Creates the `training_0002_ecommerce` catalog by default
- Creates `bronze`, `silver`, `gold`, and `reference` schemas
- Includes an optional commented teardown step for resetting the catalog

### 2. Dimension Pipeline

#### Bronze

`2_medallion_processing_dim/1_dim_bronze.ipynb`

Ingests raw CSV files into Bronze Delta tables:
- `brz_brands`
- `brz_category`
- `brz_products`
- `brz_customers`
- `brz_calendar`

Source datasets include:
- brands
- categories
- products
- customers
- date/calendar

#### Silver

`2_medallion_processing_dim/2_dim_silver.ipynb`

Applies cleansing and transformation rules such as:
- trimming and normalizing brand values
- removing non-alphanumeric characters from brand codes
- deduplicating category and calendar data
- converting product weights like `200g` into integers
- replacing comma decimal separators in product length values
- standardizing category and brand codes to uppercase
- correcting material spelling issues such as `Coton`, `Alumium`, and `Ruber`
- fixing negative `rating_count` values
- dropping null `customer_id` rows
- filling missing customer phone values with `Not Available`
- parsing string dates into proper date fields
- normalizing day names and week values

Silver outputs:
- `slv_brands`
- `slv_category`
- `slv_products`
- `slv_customers`
- `slv_calendar`

#### Gold

`2_medallion_processing_dim/3_dim_gold.ipynb`

Builds analytics-ready dimension tables:
- `gld_dim_products`
- `gld_dim_customers`
- `gld_dim_date`

Gold enrichments include:
- product dimension joins across products, brands, and categories
- customer region assignment from country and state mappings persisted as a managed reference table
- date dimension derivations such as `date_id`, `month_name`, and `is_weekend`

### 3. Fact Pipeline

#### Bronze

`3_medallion_processing_fact/1_fact_bronze.ipynb`

Ingests raw order item CSV files into:
- `brz_order_items`

#### Silver

`3_medallion_processing_fact/2_fact_silver.ipynb`

Cleanses and standardizes transaction data by:
- removing duplicate order item records
- converting text quantities like `Two` into numeric values
- stripping currency and percent symbols from price and discount fields
- parsing mixed timestamp formats
- normalizing coupon codes
- standardizing sales channels such as `web` to `Website` and `app` to `Mobile`
- casting dates, timestamps, sequence values, and tax amounts into usable types
- adding a processing timestamp

Silver output:
- `slv_order_items`

#### Gold

`3_medallion_processing_fact/3_fact_gold.ipynb`

Builds the analytics fact table:
- `gld_fact_order_items`

Derived business metrics include:
- `gross_amount`
- `discount_amount`
- `net_amount`
- `coupon_flag`
- `net_amount_inr`
- `date_id`

The Gold fact layer also applies fixed exchange-rate logic to normalize transaction values into INR for reporting, with the FX lookup persisted as a managed reference table.

### 4. Dashboard / Serving Layer

`4_dashboard/denormalise_table.dbquery.ipynb`

Creates a denormalized Gold-layer view:
- `training_0002_ecommerce.gold.fact_transactions_denorm`

This view joins:
- `gld_fact_order_items`
- `gld_dim_date`
- `gld_dim_products`

It exposes a flatter analytics model with time, product, category, brand, and transaction attributes in a single queryable object for dashboards and BI consumption.

## Technologies Used

- Databricks
- PySpark
- Spark SQL
- Delta Lake
- Unity Catalog
- CSV ingestion from Databricks Volumes

## Analytics Model

The final Gold layer resembles a lightweight star schema:

- Dimensions:
  - products
  - customers
  - dates
- Fact:
  - order items / transactions

This structure supports common BI use cases such as:
- sales trend analysis
- product and category performance
- channel analysis
- customer segmentation by geography
- coupon usage analysis
- revenue reporting with normalized currency values
- simplified dashboard querying through a denormalized serving view
