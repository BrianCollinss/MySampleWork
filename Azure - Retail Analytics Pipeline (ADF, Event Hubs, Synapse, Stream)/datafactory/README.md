# Data Factory

ADF is responsible for raw batch ingestion into the lake. It does not own business transformations in this project.

## Pipelines

- `pl_master_ecommerce_analytics`

## Responsibility Split

- `ADF`: lands raw files into `bronze`
- `Synapse pipelines`: runs SQL transformations from `bronze` to `silver` and `gold`

This separation keeps ingestion and modelling responsibilities clearly separated.

## Refresh Pattern

The project uses a scheduled batch refresh. New or updated CSV files are copied into `bronze`, and Synapse serverless views read the refreshed files at query time.

In production, the scheduled trigger could be replaced or supplemented with storage-event triggers. Storage events use Event Grid behind the scenes so a new file arrival in ADLS can start the ADF pipeline automatically.

## What Is Done Locally

- Review importable ADF JSON definitions in `linkedServices/`, `datasets/`, and `pipelines/`
- Prepare sample data files

## What Is Done In Azure Portal

- Use the existing Data Factory: `adf-retail-analytics-demo`
- In the storage account, go to **Access Control (IAM)** and assign the Data Factory managed identity the `Storage Blob Data Contributor` role

## What Is Done In Service UI

- Import or recreate the linked service, dataset, and pipelines from the JSON files
- Trigger the master pipeline

## What Is Provided In Repo

- Importable ADF JSON definitions
- Naming conventions

## Import Order In ADF Studio

1. Import `linkedServices/ls_adls_retailanalyticsdemo.json`
2. Keep the storage URL as `https://stretailanalyticsdemo.dfs.core.windows.net/`
3. Configure the linked service authentication in ADF Studio so ADF generates the real `encryptedCredential`
4. Import `datasets/ds_adls_csv_parameterized.json`
5. Import `pipelines/pl_master_ecommerce_analytics.json`
6. Upload the CSV files into `retailanalyticsdemo/raw/`
7. Debug or trigger `pl_master_ecommerce_analytics`
