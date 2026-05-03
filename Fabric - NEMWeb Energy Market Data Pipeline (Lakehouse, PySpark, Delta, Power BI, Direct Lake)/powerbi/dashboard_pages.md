# Dashboard Pages

The included `powerbi/powerbi.pbix` is a work-in-progress Power BI Desktop
report. More pages and visuals will be added as the Gold tables and semantic
model mature. The Desktop file was used because the available Fabric licence was
nearing expiry; the model design still supports Direct Lake where Fabric is
available.

## Page 1: NEM Overview

- KPI cards: latest price by selected region, latest demand, daily average price, daily max price, and data freshness.
- Line chart: price and demand over the last 24 hours.
- Region slicer.
- Time range slicer.
- Data freshness indicator.

## Page 2: Regional Prices

- Small multiples or line charts by region.
- Heatmap: price by hour and region.
- Price spike table.
- Negative price intervals.

## Page 3: Demand and Supply

- Demand time series.
- Supply/demand balance.
- Regional comparison.
- Rolling average demand.

## Page 4: Generation Mix

- Stacked area chart by fuel type.
- Renewable penetration line.
- Fuel type share.
- Battery, solar, wind, coal, and gas categories where data allows.

## Page 5: Interconnector Flows

- Interconnector flow time series.
- Import/export direction.
- Regional net flow summary.

## Page 6: Price Events and Volatility

- High price events above $300/MWh.
- Extreme events above $1,000/MWh.
- Daily volatility.
- Event detail table.

## Page 7: Data Operations

- Last ingestion.
- Last parsed ZIP.
- Number of files processed.
- Number of quarantined files.
- Data quality checks.
- Pipeline status.
