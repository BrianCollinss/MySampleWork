-- Sets up the Synapse serverless SQL database.
-- Run this first. It recreates the database, schemas, managed identity credential,
-- and external data sources used by the silver and gold views.

USE master;
GO

DROP DATABASE IF EXISTS retailanalyticsdemo;
GO

CREATE DATABASE retailanalyticsdemo
COLLATE Latin1_General_100_BIN2_UTF8;
GO

USE retailanalyticsdemo;
GO

CREATE SCHEMA silver;
GO

CREATE SCHEMA gold;
GO

-- Required before creating a database scoped credential.
-- This is a SQL encryption password for the database master key, not your Azure login password.
CREATE MASTER KEY ENCRYPTION BY PASSWORD = 'AbCdEf1029384756';
GO

-- Uses the Synapse workspace managed identity to access ADLS Gen2.
-- Grant that managed identity Storage Blob Data Reader on the storage account or container before querying files.
CREATE DATABASE SCOPED CREDENTIAL msi_retail_storage
WITH IDENTITY = 'Managed Identity';
GO

-- Points OPENROWSET calls at the curated batch/streaming raw data under bronze.
CREATE EXTERNAL DATA SOURCE ds_bronze_retail
WITH (
    LOCATION = 'abfss://retailanalyticsdemo@stretailanalyticsdemo.dfs.core.windows.net/bronze/',
    CREDENTIAL = msi_retail_storage
);
GO

-- Points OPENROWSET calls at Stream Analytics aggregate outputs under gold.
CREATE EXTERNAL DATA SOURCE ds_gold_retail
WITH (
    LOCATION = 'abfss://retailanalyticsdemo@stretailanalyticsdemo.dfs.core.windows.net/gold/',
    CREDENTIAL = msi_retail_storage
);
GO
