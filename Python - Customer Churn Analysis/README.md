# Customer Churn Analysis Demo Project

This repository is a polished sample-work project that demonstrates how to turn raw customer churn CSV data into a professional Python analytics workflow using Conda, parquet storage, reusable helper modules, and a Jupyter notebook-driven exploratory analysis.

## Project Highlights

- Converts raw CSV source files into analysis-ready parquet datasets.
- Cleans and standardises columns for consistent downstream use.
- Generates reusable summary tables and publication-ready visualisations.
- Uses a modular `src/` package for data loading, analysis, and plotting helpers.
- Keeps exploratory work in notebooks and reusable logic in Python modules.

## Repository Structure

```text
.
|-- data/
|   |-- customer_churn_dataset-training-master.csv
|   |-- customer_churn_dataset-testing-master.csv
|   |-- train.parquet
|   |-- test.parquet
|   `-- combined.parquet
|-- notebooks/
|   `-- 01_customer_churn_analysis.ipynb
|-- outputs/
|   |-- figures/
|   `-- tables/
|-- reports/
|   `-- executive_summary.md
|-- scripts/
|   `-- run_data_pipeline.py
|-- src/
|   `-- customer_churn_analysis/
|       |-- __init__.py
|       |-- analysis.py
|       |-- config.py
|       |-- data.py
|       `-- visualization.py
`-- environment.yml
```

## Conda Setup

Create the environment:

```bash
conda env create -f environment.yml
```

Activate it:

```bash
conda activate churn-analysis
```

### PowerShell Note

If PowerShell raises an error like `Invoke-Expression ... ParameterArgumentValidationErrorEmptyStringNotAllowed` when you try to activate the environment, initialise Conda for PowerShell and reopen the terminal:

```powershell
conda init powershell
```

After reopening PowerShell:

```powershell
conda activate churn-analysis
```

You can also use the `Makefile` targets below to avoid manual activation for most tasks.

## Makefile Shortcuts

If you have `make` available, the repository includes setup shortcuts:

```bash
make help
make setup
make pipeline
make notebook
```

Main targets:

- `make setup`: create the Conda environment from `environment.yml`
- `make pipeline`: run the parquet conversion and summary-table pipeline with `conda run`
- `make notebook`: launch Jupyter Lab inside the Conda environment
- `make activate-help`: print the PowerShell activation fix

## Run The Data Pipeline

This step reads the raw CSV files, removes the single blank record in the training split, standardises column names, and saves parquet files plus summary tables.

```bash
python scripts/run_data_pipeline.py
```

## Open The Notebook

```bash
jupyter lab
```

Then open `notebooks/01_customer_churn_analysis.ipynb`.

## Key Analytical Focus Areas

- Dataset quality and train/test consistency
- Churn rate comparison across splits
- Numeric feature relationships with churn
- Categorical segment performance by gender, subscription type, and contract length
- Commercial interpretation for retention strategy and stakeholder reporting

## Notes

- The training CSV contains one fully blank row, which is removed during preprocessing.
- Both provided files contain a `Churn` label, so they are treated as labelled train/test splits rather than as an unlabeled scoring set.
- Daily project commands can be run without activating the environment first by using `conda run -n churn-analysis ...` or the included `Makefile`.
