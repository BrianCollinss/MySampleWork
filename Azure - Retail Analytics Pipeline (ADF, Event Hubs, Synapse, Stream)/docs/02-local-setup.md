# Local Setup

## Purpose

Local setup is only required for sample data utilities and the streaming event producer.

## What Is Done Locally

1. Install Conda.
2. Create the project environment.
3. Install streaming dependencies through `environment.yml`.
4. Optionally generate fresh sample CSVs.

```powershell
conda env create -f environment.yml
conda activate azure-retail-analytics-hybrid
python sample-data/scripts/validate_sample_data.py
```

If the environment already exists, update it instead:

```powershell
conda env update -f environment.yml --prune
```

## What Is Done In Azure Portal

Nothing in this file.

## What Is Done In Service UI

Nothing in this file.

## What Is Provided In Repo

- `environment.yml`
- `streaming/requirements.txt`
- `streaming/config.example.json`
- `sample-data/scripts/generate_sample_data.py`
- `sample-data/scripts/validate_sample_data.py`

## Configuration Notes

Copy `streaming/config.example.json` to `streaming/config.json` and populate placeholders:

- Event Hubs namespace
- hub name
- connection string
- producer pacing settings

Do not commit secrets.
