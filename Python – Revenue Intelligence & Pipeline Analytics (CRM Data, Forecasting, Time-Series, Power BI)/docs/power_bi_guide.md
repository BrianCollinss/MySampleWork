# Power BI Model Guide

## Recommended model

- Date dimension: `gold_dim_date`
- Shared dimensions:
  - `gold_dim_account`
  - `gold_dim_sales_rep`
  - `gold_dim_product`
  - `gold_dim_stage`
- Facts:
  - `gold_fct_pipeline_snapshot`
  - `gold_fct_closed_deals`
  - `gold_fct_opportunity_scoring`
- Aggregated marts:
  - `gold_mart_sales_performance`
  - `gold_mart_sales_performance_by_rep`
  - `gold_mart_sales_performance_by_team`
  - `gold_mart_pipeline_conversion`
  - `gold_mart_forecasting`
  - `gold_mart_customer_performance`
  - `gold_mart_product_performance`
  - `gold_mart_yearly_summary`
  - `gold_mart_kpi_summary`

Metric definitions for derived fields such as `pipeline_coverage_ratio`, `weighted_pipeline_value`, `target_attainment_ratio`, and `days_to_close` are documented in [metric_definitions.md](metric_definitions.md).

## Relationships

- `gold_dim_date[date_key]` -> `gold_fct_pipeline_snapshot[date_key]`
- `gold_dim_date[date_key]` -> `gold_fct_closed_deals[date_key]`
- `gold_dim_date[date_key]` -> `gold_mart_sales_performance[date_key]`
- `gold_dim_account[account_key]` -> fact and mart account keys
- `gold_dim_sales_rep[sales_rep_key]` -> fact and mart rep keys
- `gold_dim_product[product_key]` -> fact and mart product keys
- `gold_dim_stage[stage_key]` -> fact stage keys

## Suggested DAX measures

```DAX
Total Revenue = SUM(gold_fct_closed_deals[revenue])

Total Pipeline = SUM(gold_fct_pipeline_snapshot[pipeline_value])

Weighted Pipeline = SUM(gold_fct_pipeline_snapshot[weighted_pipeline_value])

Win Rate =
DIVIDE(
    CALCULATE(COUNTROWS(gold_fct_closed_deals), gold_fct_closed_deals[deal_stage] = "Won"),
    COUNTROWS(gold_fct_closed_deals)
)

Revenue YTD = TOTALYTD([Total Revenue], gold_dim_date[date])

Revenue Prior Year =
CALCULATE([Total Revenue], SAMEPERIODLASTYEAR(gold_dim_date[date]))

Revenue YoY % =
DIVIDE([Total Revenue] - [Revenue Prior Year], [Revenue Prior Year])

Rolling 3M Revenue =
CALCULATE(
    [Total Revenue],
    DATESINPERIOD(gold_dim_date[date], MAX(gold_dim_date[date]), -3, MONTH)
)
```

## Recommended visuals

- Monthly revenue trend with rolling 3-month average
- Weighted pipeline by month
- Rep performance small multiples
- Stage conversion trend by month
- Customer cohort heatmap
- Product revenue trend
- Forecast actual vs projected line chart
