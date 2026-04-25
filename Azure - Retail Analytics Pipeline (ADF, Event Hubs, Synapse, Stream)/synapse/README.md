# Synapse

Synapse is the core transformation and analytics layer of the project. This repository uses serverless SQL only.

## Modelling Approach

- `bronze`: external files landed by ADF and Stream Analytics
- `silver`: cleaned dimensions and facts
- `gold`: analytics-ready reporting models

## External Table Assumptions

- Raw batch files are stored in ADLS Gen2 under the `bronze` container
- Stream Analytics outputs are stored in a path such as `streaming/funnel_realtime_5min/`
- Synapse serverless has access to the storage account via workspace managed identity or scoped credentials
- Scripts can be implemented as views over external data, CETAS outputs, or a combination depending on demo preference

## Suggested Execution Order

1. `01_setup.sql`
2. `02_silver_dim.sql`
3. `03_silver_facts.sql`
4. `04_gold.sql`
5. `05_validation.sql`

## What Is Done Locally

- Review SQL scripts under `synapse/sql/`

## What Is Done In Azure Portal

Workspace provisioning and storage permissions are covered in `docs/03-azure-portal-tasks.md` and `docs/05-synapse-tasks.md`.

## What Is Done In Service UI

Detailed SQL execution steps are covered in `docs/05-synapse-tasks.md`.

## What Is Provided In Repo

- SQL scripts for setup, silver views, gold views, and validation
- Validation queries

## What Can Be Automated Later

- SQL deployment pipelines
- Metadata-driven orchestration

## Sample Queries

```sql
SELECT TOP 10 * FROM gold.gold_customer_360;
SELECT * FROM gold.gold_daily_sales_summary ORDER BY order_date DESC;
SELECT * FROM gold.gold_funnel_realtime_5min ORDER BY window_end_utc DESC;
```
