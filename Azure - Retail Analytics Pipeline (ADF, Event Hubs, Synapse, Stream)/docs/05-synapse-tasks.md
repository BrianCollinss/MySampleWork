# Synapse Tasks

## Purpose

Synapse is the modelling layer of the project. Use serverless SQL only.

## What Is Done Locally

- Review the SQL scripts under `synapse/sql/`
- No path changes are needed if you used the documented storage account, container, and folder structure

## What Is Done In Azure Portal

- Use the existing Synapse workspace
- In the storage account, assign the Synapse workspace managed identity `Storage Blob Data Reader` on the storage account or `retailanalyticsdemo` container

## What Is Done In Service UI

- Open Synapse Studio > **Develop** > **SQL script**
- Connect to the built-in serverless SQL pool
- Run `synapse/sql/01_setup.sql` to create the database, schemas, credential, and external data sources
- If `retailanalyticsdemo` was already created without UTF-8 collation, drop and recreate the database before running the view scripts
- Run `synapse/sql/02_silver_dim.sql`
- Run `synapse/sql/03_silver_facts.sql`
- Run `synapse/sql/04_gold.sql`
- Run `synapse/sql/05_validation.sql`
- Create optional Synapse pipelines to sequence SQL scripts

## What Is Provided In Repo

- Dimension SQL
- Fact SQL
- Gold reporting models
- Validation queries
- README guidance on serverless assumptions

## What Can Be Automated Later

- Script deployment through release pipelines
- Metadata-driven model execution

## Suggested Pipeline Role

Use Synapse pipelines for:

- silver-to-gold SQL execution
- model dependencies
- validation checks after ingestion

Keep ADF focused on ingestion.
