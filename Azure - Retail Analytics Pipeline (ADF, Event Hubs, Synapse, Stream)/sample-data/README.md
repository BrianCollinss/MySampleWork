# Sample Data

This folder contains small but realistic retail source files for the batch ingestion path.

## Files

- `raw/customers.csv`
- `raw/products.csv`
- `raw/orders.csv`
- `raw/order_items.csv`
- `raw/campaigns.csv`

## What Is Done Locally

- Review or regenerate sample files
- Run the validation script before uploading

```powershell
python sample-data/scripts/validate_sample_data.py
```

## What Is Done In Azure Portal

Azure storage setup is covered in `docs/03-azure-portal-tasks.md`.

## What Is Done In Service UI

ADF dataset and pipeline setup is covered in `docs/04-data-factory-tasks.md`.

## What Is Provided In Repo

- Prebuilt CSVs
- Generator and validation scripts

## What Can Be Automated Later

- Larger synthetic data generation
- Automated upload to ADLS landing zones
