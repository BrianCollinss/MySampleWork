# Metric Definitions

This document defines the main derived metrics used across the Gold layer so fields in the data model have clear business meaning.

## Pipeline and Revenue Metrics

### `pipeline_value`

- Meaning: Total nominal value of an opportunity or aggregated group of opportunities.
- Typical source: `close_value` from the opportunity data.
- Used in:
  - `gold_fct_pipeline_snapshot`
  - `gold_mart_sales_performance`
  - `gold_mart_sales_performance_by_rep`
  - `gold_mart_product_performance`

### `weighted_pipeline_value`

- Meaning: Probability-adjusted pipeline value used to estimate likely future revenue.
- Formula:
  - `weighted_pipeline_value = close_value * stage_probability`
- `stage_probability` comes from the configured stage map, with modelled win probability used as a fallback where relevant.

### `closed_won_revenue`

- Meaning: Revenue from opportunities closed as `Won`.
- Formula:
  - `SUM(close_value WHERE deal_stage = 'Won')`

### `closed_lost_value`

- Meaning: Opportunity value that ended as `Lost`.
- Formula:
  - `SUM(close_value WHERE deal_stage = 'Lost')`

### `pipeline_coverage_ratio`

- Meaning: Indicates how much pipeline exists relative to realised closed-won revenue in the same reporting period.
- Formula in this project:
  - `pipeline_coverage_ratio = monthly_pipeline_value / closed_won_revenue`
- Interpretation:
  - Higher values indicate more pipeline relative to booked revenue.
  - Undefined where `closed_won_revenue = 0`; these periods are left null.

## Conversion and Performance Metrics

### `win_rate`

- Meaning: Share of closed opportunities that were won.
- Formula:
  - `win_rate = won_opportunities / closed_opportunities`
- In SQL terms:
  - `AVG(CAST(is_won AS DOUBLE) WHERE is_closed = TRUE)`

### `conversion_rate`

- Meaning: Rep-level rate of converting closed deals into wins.
- Formula in this project:
  - `conversion_rate = won_count / closed_count`

### `target_attainment_ratio`

- Meaning: Performance against the derived monthly revenue target.
- Formula:
  - `target_attainment_ratio = closed_won_revenue / revenue_target`
- Interpretation:
  - `> 1.0` means the rep or team exceeded target.
  - `< 1.0` means the rep or team was below target.

### `exceeded_target`

- Meaning: Boolean flag showing whether actual closed-won revenue met or exceeded the derived target.
- Formula:
  - `exceeded_target = closed_won_revenue >= revenue_target`

### `revenue_target`

- Meaning: Portfolio-style benchmark used because the source dataset does not contain explicit quotas.
- Formula in this project:
  - trailing 3-month average of prior actual `closed_won_revenue`
  - where insufficient prior history exists, the average historical revenue for that rep or team is used

## Sales Cycle and Time Metrics

### `days_to_close`

- Meaning: Number of days between opportunity creation and close.
- Formula:
  - `days_to_close = close_date - created_date`
- Applies only to opportunities with a close date.

### `opportunity_age_days`

- Meaning: Age of the opportunity in days.
- Formula in this project:
  - for closed opportunities: `close_date - created_date`
  - for open opportunities: `analysis_cutoff_date - created_date`

### `avg_days_to_close`

- Meaning: Average sales cycle length for the reporting grain.
- Formula:
  - `AVG(days_to_close for closed opportunities)`

### `closed_won_revenue_rolling_3m`

- Meaning: Three-month rolling average of monthly closed-won revenue.
- Formula:
  - rolling mean of the current month plus previous 2 months

### `closed_won_revenue_rolling_6m`

- Meaning: Six-month rolling average of monthly closed-won revenue.
- Formula:
  - rolling mean of the current month plus previous 5 months

## Forecasting and Scoring Metrics

### `predicted_win_probability`

- Meaning: Model-estimated probability that an opportunity will be won.
- Produced by:
  - `train_win_probability_model()`
- Used in:
  - `gold_fct_opportunity_scoring`
  - `gold_fct_pipeline_snapshot`
  - weighted-pipeline logic where required

### `forecast_revenue`

- Meaning: Revenue value in the forecasting output, representing either an actual historical month or a forecast month.
- Used in:
  - `gold_mart_forecasting`

### `forecast_type`

- Meaning: Distinguishes whether a row in the forecasting mart is historical or projected.
- Values:
  - `actual`
  - `forecast`

## Customer and Cohort Metrics

### `cohort_month`

- Meaning: First month in which an account recorded closed-won revenue.
- Formula:
  - `MIN(year_month) for each account among won deals`

### `customer_status`

- Meaning: Flags whether a customer-month reflects a new or returning customer.
- Logic:
  - `new` when `year_month = cohort_month`
  - `returning` otherwise
