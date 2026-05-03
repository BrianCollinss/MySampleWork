# Semantic Model Tables

The semantic model is currently maintained in `powerbi/powerbi.pbix` as a
Power BI Desktop work-in-progress file. Direct Lake remains the preferred target
when Fabric workspace access is available.

## Dimensions

- `dim_region`: NEM region code and region name.
- `dim_date`: trading date, year, month, quarter, and day attributes.
- `dim_time`: interval hour, minute, and time bucket attributes.
- `dim_price_band`: Negative, Normal, High, and Extreme price bands.
- `dim_fuel_type`: fuel classifications if generation mix is available.

## Facts

- `fact_region_5min_price_demand`: from `gold_region_5min`.
- `fact_region_30min_price_demand`: from `gold_region_30min`.
- `fact_region_daily`: from `gold_region_daily`.
- `fact_price_spikes`: from `gold_price_spikes`.
- `fact_generation_mix_5min`: from `gold_generation_mix_5min`.
- `fact_interconnector_flows_5min`: from `gold_interconnector_flows_5min`.
- `fact_kpis`: from `gold_kpis`.

## Relationships

Relate facts to `dim_region` by `region`, to `dim_date` by `trading_date`, to `dim_time` by interval fields, and to `dim_price_band` by `price_band`.
