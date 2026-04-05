"""
Data Ingestion and Basic Cleaning Pipeline for ADLS Gen2

This script reads a raw house prices dataset from Azure Data Lake Storage Gen2,
performs minimal data cleaning, and writes the processed dataset back to a
separate storage container.

Workflow:
1. Load raw CSV data from the ADLS Gen2 "raw" container using Spark.
2. Automatically infer schema and read header columns.
3. Perform basic data cleaning by removing rows containing null values.
4. Write the cleaned dataset to the ADLS Gen2 "processed" container.

Purpose:
Provides a simple data preparation step in a data engineering or ML pipeline,
moving data from the raw ingestion layer to a cleaned/processed layer suitable
for downstream analytics, feature engineering, or model training.

Input:
- Raw CSV dataset stored in ADLS Gen2.

Output:
- Cleaned CSV dataset written to the processed container in ADLS Gen2.
"""


# Path to the raw data from ADLS Gen2
raw_path = "abfss://raw@<storageaccount>.dfs.core.windows.net/house_prices.csv"

df = spark.read.csv(
    raw_path,
    header=True,
    inferSchema=True
)

# Minimal cleaning
df = df.dropna()

# Write PROCESSED data back to ADLS Gen2
processed_path = "abfss://processed@<storageaccount>.dfs.core.windows.net/house_prices_clean.csv"

df.write.mode("overwrite").csv(
    processed_path,
    header=True
)

print(f"Wrote processed data to: {processed_path}")
