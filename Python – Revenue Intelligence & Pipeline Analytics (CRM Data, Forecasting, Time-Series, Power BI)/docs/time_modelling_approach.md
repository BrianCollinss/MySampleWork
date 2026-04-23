# Time Modelling Approach

## Principles

- The opportunity lifecycle is modeled around `created_date`, `close_date`, and `activity_date`.
- `gold_dim_date` is the shared calendar dimension for Power BI time intelligence.
- All major marts include `date`, `year`, `month`, and `year_month`.

## Standardized date usage

- Opportunity creation analysis uses `created_date`.
- Revenue realization uses `close_date`.
- Snapshot-style pipeline reporting uses `snapshot_date`, derived at month grain.
- Activity proxy uses `activity_date`, set to close date when available, otherwise create date.

## Derived metrics

- `days_to_close`: close date minus create date for closed opportunities
- `opportunity_age_days`: current or closed age of an opportunity
- `closed_won_revenue_rolling_3m`
- `closed_won_revenue_rolling_6m`
- customer monthly cohort from first won month

## Business relevance

- Monthly and quarterly trends show whether the sales engine is accelerating or slowing.
- Rolling averages smooth volatility for forecast and target setting.
- Cohort timing shows retention and account expansion patterns.
- Sales-cycle timing surfaces operational bottlenecks.
