# UniSQ Short-Term Forecast for Irrigation Management

## Overview

This project contains R-based research workflow assets used to evaluate how weather forecast quality influences irrigation management outcomes in maize systems. The folder combines R scripts, APSIM template files, and generated analytical outputs used to support a published study.

## Demo Context

This repository snapshot focuses on the parts of the workflow that can be shared in a portfolio setting: orchestration scripts, model templates, and output figures. It is intended to show the modelling and analysis structure rather than to reproduce the full research data pipeline.

## What The Project Does

```mermaid
flowchart LR
    A[Weather and soils inputs] --> B[R orchestration]
    B --> C[APSIM simulation setup]
    C --> D[Scenario analysis]
    D --> E[Publication figures and summaries]
```

- Prepares APSIM inputs and supporting utilities
- Runs scenario analyses for irrigation, nitrogen, soils, and forecast-confidence settings
- Produces analytical figures, summaries, and supporting outputs for publication

## Scale

- 864 scenarios
- 17M simulated seasonal records
- +677M simulated daily records

## Repository Structure

```text
01_R/
  AddProbToMetFile.R
  BundleAPSIM.R
  DownloadSILOGridData.R
  DownloadSoils.R
  PrepareAPSIM.R
  ReadAPSIM.R
  RunAnalysis.R
  SetupPBS.R
  UpdateSoils.R
  Utilities.R
  APSIMTemplates/
  RShared/
03_Analyses/
  ANOVA.txt
  *.pdf
README.md
```

## Data Assets

- `01_R/APSIMTemplates/` contains APSIM template files, databases, and supporting setup artefacts.
- `03_Analyses/` contains exported PDF figures and text outputs used for interpretation and publication support.
- The repository does not include a full raw-data archive; instead it focuses on the model-running and analysis layers.

## Notes

- The analytical outputs document the impacts of different forecast-confidence and management configurations.
- This folder is most useful for reviewing research workflow organisation and APSIM-centred analysis logic.
