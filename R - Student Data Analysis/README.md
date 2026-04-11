# Student Data Analysis

This project is a compact R workflow for exploring the workbook in `data/raw/student_data_analysis.xlsx`.
It uses `renv` for package management, so the project library is restored from the root `renv.lock` rather than assembled manually.
The project has been set up and run in R 4.5.2.

## Project layout

- `scripts/student_data_analysis.R`: main analysis script
- `setup/R/setup_r_environment.R`: `renv` restore helper
- `data/raw/`: raw workbook input
- `data/processed/`: optional derived data
- `output/`: generated notes, tables, and figures
- `renv/` and `renv.lock`: reproducible R environment

## Notes

- `.Rprofile` loads the project `renv` library automatically when `renv` is installed.
- `setup/R/setup_r_environment.R` restores from the root `renv.lock`.
- `renv::snapshot()` is for intentional dependency changes, not routine startup.
- Given the limited time available for the exercise, only minimal effort was put into polishing the fine details of figure appearance; the priority was analytical coverage, reproducibility, and clear outputs.

## Any Results Or Questions To Explore Further With More Time

With more time, the next steps I would prioritise are:

- testing whether the observed differences in retention across study areas remain stable after stronger controls for year, term, and cohort composition
- extending the subgroup analysis to compare unique students rather than only student-term rows
- checking whether some of the large study-area differences are driven by attendance mode, domestic or international status, or prior academic performance
- reviewing whether 2025 should be treated as a censoring issue everywhere in the workflow, not only in the logistic model
- refining the visual presentation of the report further once the analytical story is locked in

## How to start

### Option 1: open in RStudio

From the project root, run:

```powershell
powershell -ExecutionPolicy Bypass -File .\open_rstudio_4_5_2.ps1
```

When RStudio opens, restore the project library once in the R console:

```r
source("setup/R/setup_r_environment.R")
```

If RStudio later prompts for `renv::snapshot()`, that usually means the installed package set has drifted from the lockfile. Only run:

```r
renv::snapshot()
```

when you intentionally changed project dependencies and want to record those changes in the root `renv.lock`.

Then run the analysis:

```r
source("scripts/student_data_analysis.R")
```

## Option 2: run from the command line

Restore the `renv` library:

```powershell
& 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe' -e "source('setup/R/setup_r_environment.R')"
```

Run the analysis script:

```powershell
& 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe' 'scripts/student_data_analysis.R'
```

## What the analysis produces

Running the script writes:

- tables to `output/tables/`
- figures to `output/figures/`