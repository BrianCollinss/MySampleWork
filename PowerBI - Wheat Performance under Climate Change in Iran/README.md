# Power BI Wheat Performance under Climate Change in Iran

## Overview

This project is a Power BI analytics sample exploring simulated wheat performance in Iran under historical and future climate conditions. It combines crop simulation outputs with irrigation, nitrogen fertilisation, climate scenario, and Global Climate Model dimensions to support interactive comparison of wheat yield and water-use outcomes.

## Demo Context

The dashboard is intended as a portfolio example of agricultural data modelling, climate scenario analysis, and Power BI reporting. The underlying data is owned by Dr Brian Collins and is included only for review of this project. It must not be used, copied, shared, analysed, republished, or reused for any purpose without prior written permission.

## What The Project Does

- Compares wheat performance across historical conditions and future climate scenarios
- Includes Shared Socioeconomic Pathways `SSP126`, `SSP245`, and `SSP585`
- Uses multiple GCM projections, including `ACCESS`, `CNRM`, `GFDL`, `HadGEM3`, `MPI`, and `MRI`
- Compares flood, sprinkler, and drip irrigation systems across defined management regimes
- Evaluates nitrogen fertilisation scenarios from `F50` to `F300`, representing 50 to 300 kg N/ha
- Presents results through an interactive Power BI report

## Irrigation Scenarios

The dashboard compares six irrigation regimes:

- `Flood A`: traditional flood irrigation, 60% soil water deficit trigger, 1000 mm effective soil depth, 80 mm application water, 12-day interval
- `Flood B`: optimised flood irrigation, 50% soil water deficit trigger, 800 mm effective soil depth, 60 mm application water, 8-day interval
- `Sprinkler A`: conventional sprinkler irrigation, 45% soil water deficit trigger, 800 mm effective soil depth, 40 mm application water, 6-day interval
- `Sprinkler B`: optimised sprinkler irrigation, 40% soil water deficit trigger, 600 mm effective soil depth, 30 mm application water, 4-day interval
- `Drip A`: subsurface drip irrigation, 20% soil water deficit trigger, 300 mm effective soil depth, 20 mm application water, 2-day interval
- `Drip B`: subsurface drip irrigation, 25% soil water deficit trigger, 400 mm effective soil depth, 20 mm application water, 2-day interval

## Repository Structure

```text
data/
  Seasonal.parquet
powerbi/
  powerbi.pbix
README.md
LICENCE
```

## Data Assets

- `data/Seasonal.parquet` contains the seasonal simulation data used by the report.
- `powerbi/powerbi.pbix` is the Power BI Desktop report.

## Notes

- This project is provided for demonstration and portfolio review only.
- All data remains the property of Dr Brian Collins.
- No permission is granted to use the data in any sense without prior written permission.