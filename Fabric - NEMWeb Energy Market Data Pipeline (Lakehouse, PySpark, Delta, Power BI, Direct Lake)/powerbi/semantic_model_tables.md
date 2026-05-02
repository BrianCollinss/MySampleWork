# Semantic Model Tables

## Dimensions

- `dim_region`: NEM region code and region name.
- `dim_date`: trading date, year, month, quarter, and day attributes.
- `dim_time`: interval hour, minute, and time bucket attributes.
- `dim_price_band`: Negative, Normal, High, and Extreme price bands.
- `dim_fuel_type`: fuel classifications if generation mix is available.

## Facts

- `fact_region_5min_price_demand`: from `nem_gold_region_5min`.
- `fact_region_30min_price_demand`: from `nem_gold_region_30min`.
- `fact_region_daily`: from `nem_gold_region_daily`.
- `fact_price_spikes`: from `nem_gold_price_spikes`.
- `fact_generation_mix_5min`: from `nem_gold_generation_mix_5min`.
- `fact_interconnector_flows_5min`: from `nem_gold_interconnector_flows_5min`.
- `fact_dashboard_kpis`: from `nem_gold_dashboard_kpis`.

## Relationships

Relate facts to `dim_region` by `region`, to `dim_date` by `trading_date`, to `dim_time` by interval fields, and to `dim_price_band` by `price_band`.
