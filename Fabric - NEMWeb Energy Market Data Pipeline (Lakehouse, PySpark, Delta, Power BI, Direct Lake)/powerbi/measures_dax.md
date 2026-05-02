# DAX Measures

```DAX
Current Price AUD/MWh = MAX(fact_region_5min_price_demand[price_aud_mwh])
Current Demand MW = MAX(fact_region_5min_price_demand[demand_mw])
Average Price AUD/MWh = AVERAGE(fact_region_5min_price_demand[price_aud_mwh])
Max Price AUD/MWh = MAX(fact_region_5min_price_demand[price_aud_mwh])
Min Price AUD/MWh = MIN(fact_region_5min_price_demand[price_aud_mwh])
Price Volatility AUD/MWh = STDEV.P(fact_region_5min_price_demand[price_aud_mwh])
High Price Interval Count = CALCULATE(COUNTROWS(fact_region_5min_price_demand), fact_region_5min_price_demand[is_high_price] = TRUE())
Extreme Price Interval Count = CALCULATE(COUNTROWS(fact_region_5min_price_demand), fact_region_5min_price_demand[is_extreme_price] = TRUE())
Negative Price Interval Count = CALCULATE(COUNTROWS(fact_region_5min_price_demand), fact_region_5min_price_demand[is_negative_price] = TRUE())
Average Demand MW = AVERAGE(fact_region_5min_price_demand[demand_mw])
Max Demand MW = MAX(fact_region_5min_price_demand[demand_mw])
Renewable Penetration % = DIVIDE(SUM(fact_generation_mix_5min[renewable_generation_mw]), SUM(fact_generation_mix_5min[generation_mw]))
Coal Generation MW = CALCULATE(SUM(fact_generation_mix_5min[generation_mw]), dim_fuel_type[fuel_type] = "Coal")
Gas Generation MW = CALCULATE(SUM(fact_generation_mix_5min[generation_mw]), dim_fuel_type[fuel_type] = "Gas")
Solar Generation MW = CALCULATE(SUM(fact_generation_mix_5min[generation_mw]), dim_fuel_type[fuel_type] = "Solar")
Wind Generation MW = CALCULATE(SUM(fact_generation_mix_5min[generation_mw]), dim_fuel_type[fuel_type] = "Wind")
Battery Generation MW = CALCULATE(SUM(fact_generation_mix_5min[generation_mw]), dim_fuel_type[fuel_type] = "Battery")
Net Interconnector Flow MW = SUM(fact_interconnector_flows_5min[flow_mw])
Data Freshness Minutes = MIN(nem_gold_data_freshness[freshness_minutes])
Last Successful Ingestion Time = MAX(nem_gold_data_freshness[last_successful_ingestion_datetime])
```
