# R - Time Series Analysis and Visualisation (Climate Extremes, ggplot2, SILO)

## Overview

This project contains R scripts and generated analytical outputs used to study long-term compound hot-dry weather extremes across Australia. The workflow combines gridded climate data, return-period analysis, and trend analysis to examine how compound extremes have changed through time.

## Demo Context

The folder is a portfolio-ready extract of the research workflow. It includes the shareable analysis scripts and output figures used to communicate results, while avoiding the need to publish a full research data-processing environment.

## Tools Used

- R for time-series analysis, statistical testing, and figure generation
- SILO climate data as the core gridded weather source referenced by the workflow
- Trend, return-period, and compound-event analysis methods implemented in script form
- ggplot2-style visualisation workflows reflected in the exported publication figures

## What The Project Does

```mermaid
flowchart LR
    A[SILO climate data] --> B[R analysis scripts]
    B --> C[Compound hot-dry metrics]
    C --> D[Trend and spatial analysis]
    D --> E[Figures and maps]
```

- Downloads or references SILO climate data
- Computes annual and seasonal weather metrics and compound-event return periods
- Analyses temporal and spatial trends in extreme-event behaviour
- Produces publication-ready charts and maps

## Repository Structure

```text
01_R/
  RunDownloadDataAustralia.R
  RunAnalysis.R
  SILO.html
  SILO.png
03_Analyses/
  *.pdf
README.md
```

## Data Assets

- `01_R/` contains the analytical scripts and SILO reference artefacts.
- `03_Analyses/` contains the exported figures covering correlations, trends, affected area, and mapped outputs.
- The repository snapshot emphasises the reproducible analysis layer and final artefacts rather than a complete raw-data store.

## Notes

- The output set reflects a research workflow spanning national-scale climate analysis over a long historical period.
- This project is best reviewed as a statistical climate-analysis case study.
