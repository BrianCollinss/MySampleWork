# Data Dictionary

## Raw tables

| Table | Description |
| --- | --- |
| `accounts` | Customer firmographic attributes |
| `products` | Product catalog with price |
| `sales_teams` | Sales rep hierarchy and region |
| `sales_pipeline` | Opportunities with stage, create date, close date, and value |

## Gold dimensions

| Table | Key | Description |
| --- | --- | --- |
| `gold_dim_date` | `date_key` | Calendar table for time intelligence |
| `gold_dim_account` | `account_key` | Customer/account attributes |
| `gold_dim_sales_rep` | `sales_rep_key` | Sales rep, manager, office |
| `gold_dim_product` | `product_key` | Product attributes |
| `gold_dim_stage` | `stage_key` | Pipeline stage metadata |

## Gold facts and marts

| Table | Grain | Description |
| --- | --- | --- |
| `gold_fct_pipeline_snapshot` | opportunity-month snapshot | Pipeline value and weighted pipeline |
| `gold_fct_closed_deals` | closed opportunity | Closed revenue and cycle metrics |
| `gold_fct_opportunity_scoring` | opportunity | Predicted win probability |
| `gold_mart_sales_performance` | month | Company monthly KPI trend |
| `gold_mart_sales_performance_by_rep` | rep-month | Sales performance over time |
| `gold_mart_pipeline_conversion` | stage-month | Funnel conversion trend |
| `gold_mart_forecasting` | month | Actual and forecast revenue |
| `gold_mart_customer_performance` | account-month | Customer revenue and cohorts |
| `gold_mart_product_performance` | product-month | Product revenue trend |
| `gold_mart_yearly_summary` | year | Yearly pre-aggregated KPI summary |
