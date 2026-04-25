# Data Factory Tasks

## Purpose

ADF handles raw batch ingestion only. It lands files into `bronze` and keeps orchestration focused on ingestion.

## What Is Done Locally

- Place source CSV files under `sample-data/raw/`
- Review importable ADF JSON definitions in `datafactory/`

## What Is Done In Azure Portal

- Use the existing Data Factory: `adf-retail-analytics-demo`
- Open the storage account, then go to **Access Control (IAM)** > **Add role assignment**
- Assign `Storage Blob Data Contributor` to the Data Factory managed identity

## What Is Done In Service UI

- Import or recreate the linked service from `datafactory/linkedServices/`
- Import or recreate the dataset from `datafactory/datasets/`
- Import or recreate `pl_master_ecommerce_analytics` from `datafactory/pipelines/`
- Confirm the master pipeline has one Copy activity for each source CSV

## What Is Provided In Repo

- Importable ADF JSON definitions
- Naming conventions

## What Can Be Automated Later

- ARM or Bicep deployment for ADF assets
- CI/CD promotion across environments

## Suggested Responsibility Split

- `ADF`: raw ingestion, file movement, landing zone management
- `Synapse pipelines`: SQL model execution and downstream transformation control
