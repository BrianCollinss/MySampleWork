# R + Python Project with reticulate

This is a simple starter structure for projects where R is the main language and Python is used selectively through `reticulate`.

## Suggested workflow

- Put reusable R functions in `R/`
- Put reusable Python helpers in `python/`
- Keep environment and bootstrap files in `setup/`
- Run analyses from `scripts/`
- Keep mixed-language notebooks in `notebooks/`
- Keep raw data in `data/raw/`
- Write derived data to `data/processed/`
- Write tables and figures to `output/`

## Setup

### R

Install the small set of R packages used by this project:

```r
source("setup/R/setup_r_environment.R")
```

### Python

Create or update the conda environment from the project root:

```powershell
conda env create -f setup/python/environment.yml
# or, if it already exists
conda env update -f setup/python/environment.yml --prune
```

Then activate it when needed:

```powershell
conda activate r-reticulate
```

## Notebooks

[`notebooks/00_r_first_test_notebook.Rmd`](notebooks/00_r_first_test_notebook.Rmd) is an R Notebook for verifying that the R environment, `reticulate`, and simple `ggplot2` visualizations are all working.

[`notebooks/01_mixed_language_notebook.Rmd`](notebooks/01_mixed_language_notebook.Rmd) is a slightly fuller R Notebook example that calls Python through `reticulate` and brings the result back into R.

These notebooks are intended to be run from RStudio.

## Launching This Project In RStudio

If you want to open this project in RStudio while preferring `R 4.5.2` without changing the global RStudio setting, run:

```powershell
powershell -ExecutionPolicy Bypass -File .\open_rstudio_4_5_2.ps1
```

This launcher keeps the override local to the launched RStudio session for this project.

## Notes

- `usethis` is included for project scaffolding and structure-oriented helpers.
- `here` provides stable project-relative paths across scripts and notebooks.
- `fs` handles directory and path operations more cleanly than base path utilities.
- `setup/R/setup_reticulate.R` points `reticulate` at the `r-reticulate` conda environment by default.
- `scripts/01_run_analysis.R` is a minimal example showing R calling Python.
- `setup/R/setup_r_environment.R` installs the R packages needed for the RStudio workflow.
- The project no longer depends on Jupyter or `renv`.
