"""Gold-layer dimension, fact, and mart builders for BI-ready outputs."""

from __future__ import annotations

import duckdb
import pandas as pd

from src.analytics_modeling import forecast_monthly_revenue, train_win_probability_model
from src.config import CONFIG
from src.io_files import write_dataset


def _build_date_dim(opportunities: pd.DataFrame) -> pd.DataFrame:
    """Create the canonical date dimension spanning the opportunity lifecycle range."""

    # Derive the calendar bounds from the earliest create date and latest close date.
    min_date = opportunities["created_date"].min().floor("D")
    max_close = opportunities["close_date"].max()
    max_date = (max_close if pd.notna(max_close) else opportunities["created_date"].max()).floor("D")

    # Generate the date dimension and add Power BI-friendly calendar attributes.
    dates = pd.DataFrame({"date": pd.date_range(min_date, max_date, freq="D")})
    dates["date_key"] = dates["date"].dt.strftime("%Y%m%d").astype(int)
    dates["year"] = dates["date"].dt.year
    dates["quarter"] = "Q" + dates["date"].dt.quarter.astype(str)
    dates["month"] = dates["date"].dt.month
    dates["month_name"] = dates["date"].dt.month_name()
    dates["year_month"] = dates["date"].dt.strftime("%Y-%m")
    dates["week"] = dates["date"].dt.isocalendar().week.astype(int)
    dates["day_of_month"] = dates["date"].dt.day
    dates["day_of_week"] = dates["date"].dt.day_name()
    dates["is_weekend"] = dates["date"].dt.dayofweek >= 5
    dates["financial_year"] = dates["year"] + (dates["month"] >= CONFIG.fiscal_year_start_month).astype(int)

    return dates


def build_gold(silver_tables: dict[str, pd.DataFrame]) -> dict[str, pd.DataFrame]:
    """Build Gold dimensions, facts, marts, and model outputs from Silver tables."""

    # Start from clean Silver tables so Gold logic remains conformed and reproducible.
    accounts = silver_tables["accounts"].copy()
    products = silver_tables["products"].copy()
    reps = silver_tables["sales_teams"].copy()
    opportunities = silver_tables["opportunities"].copy()

    # Score opportunities and derive the weighted pipeline fields used in reporting.
    opportunities, win_prob_model_metrics = train_win_probability_model(opportunities)
    opportunities["stage_probability"] = opportunities["deal_stage"].map(CONFIG.stage_probability_map).fillna(
        opportunities["predicted_win_probability"]
    )
    opportunities["weighted_pipeline_value"] = opportunities["close_value"] * opportunities["stage_probability"]
    opportunities["created_date_key"] = opportunities["created_date"].dt.strftime("%Y%m%d").astype("Int64")
    opportunities["close_date_key"] = opportunities["close_date"].dt.strftime("%Y%m%d").astype("Int64")
    opportunities["activity_date_key"] = opportunities["activity_date"].dt.strftime("%Y%m%d").astype("Int64")
    opportunities["snapshot_date"] = opportunities["close_date"].fillna(pd.Timestamp("2017-12-31")).dt.to_period("M").dt.to_timestamp()
    opportunities["snapshot_date_key"] = opportunities["snapshot_date"].dt.strftime("%Y%m%d").astype(int)

    # Assign surrogate keys to each dimension table and build the shared date dimension.
    date_dim = _build_date_dim(opportunities)
    accounts["account_key"] = range(1, len(accounts) + 1)
    reps["sales_rep_key"] = range(1, len(reps) + 1)
    products["product_key"] = range(1, len(products) + 1)

    # Create a small stage dimension to support funnel analysis in the semantic model.
    stages = pd.DataFrame({"stage_name": sorted(opportunities["deal_stage"].dropna().unique())})
    stages["stage_key"] = range(1, len(stages) + 1)
    stages["default_probability"] = stages["stage_name"].map(CONFIG.stage_probability_map).fillna(0.0)

    # Join surrogate keys back into the opportunity grain before aggregating.
    opportunities = opportunities.merge(accounts[["account_key", "account"]], on="account", how="left")
    opportunities = opportunities.merge(reps[["sales_rep_key", "sales_agent"]], on="sales_agent", how="left")
    opportunities = opportunities.merge(products[["product_key", "product"]], on="product", how="left")
    opportunities = opportunities.merge(stages[["stage_key", "stage_name"]], left_on="deal_stage", right_on="stage_name", how="left")

    # Register the working tables in DuckDB so the Gold marts can be expressed in SQL.
    con = duckdb.connect()
    con.register("opportunities", opportunities)
    con.register("accounts", accounts)
    con.register("products", products)
    con.register("reps", reps)

    # Build the opportunity-level pipeline snapshot fact.
    pipeline_snapshot = con.sql(
        """
        select
            snapshot_date as date,
            snapshot_date_key as date_key,
            created_date_key,
            close_date_key,
            activity_date_key,
            opportunity_id,
            account_key,
            sales_rep_key,
            product_key,
            stage_key,
            deal_stage,
            close_value as pipeline_value,
            weighted_pipeline_value,
            predicted_win_probability,
            opportunity_age_days,
            days_to_close,
            strftime(snapshot_date, '%Y-%m') as year_month,
            year(snapshot_date) as year,
            month(snapshot_date) as month
        from opportunities
        """
    ).df()

    # Build the closed-deal fact for realised revenue analysis.
    closed_deals = con.sql(
        """
        select
            close_date as date,
            close_date_key as date_key,
            created_date_key,
            activity_date_key,
            opportunity_id,
            account_key,
            sales_rep_key,
            product_key,
            stage_key,
            deal_stage,
            close_value as revenue,
            predicted_win_probability,
            days_to_close,
            opportunity_age_days,
            strftime(close_date, '%Y-%m') as year_month,
            year(close_date) as year,
            month(close_date) as month
        from opportunities
        where is_closed and close_date is not null
        """
    ).df()

    # Build the rep-month performance mart with rolling revenue windows.
    sales_performance = con.sql(
        """
        with monthly as (
            select
                date_trunc('month', coalesce(close_date, created_date)) as date,
                strftime(date_trunc('month', coalesce(close_date, created_date)), '%Y-%m') as year_month,
                year(date_trunc('month', coalesce(close_date, created_date))) as year,
                month(date_trunc('month', coalesce(close_date, created_date))) as month,
                sales_rep_key,
                sales_agent,
                sum(close_value) as monthly_pipeline_value,
                sum(weighted_pipeline_value) as monthly_weighted_pipeline_value,
                sum(case when is_won then close_value else 0 end) as closed_won_revenue,
                sum(case when is_lost then close_value else 0 end) as closed_lost_value,
                avg(case when is_closed then days_to_close end) as avg_days_to_close,
                avg(case when is_closed then cast(is_won as double) end) as win_rate,
                avg(close_value) as average_deal_size,
                sum(case when is_won then 1 else 0 end) * 1.0 / nullif(sum(case when is_closed then 1 else 0 end), 0) as conversion_rate
            from opportunities
            group by 1,2,3,4,5,6
        )
        select
            *,
            avg(closed_won_revenue) over (
                partition by sales_rep_key
                order by date
                rows between 2 preceding and current row
            ) as closed_won_revenue_rolling_3m,
            avg(closed_won_revenue) over (
                partition by sales_rep_key
                order by date
                rows between 5 preceding and current row
            ) as closed_won_revenue_rolling_6m
        from monthly
        """
    ).df()
    sales_performance = sales_performance[sales_performance["date"].notna()].copy()
    sales_performance["revenue_target"] = sales_performance.groupby("sales_rep_key")["closed_won_revenue"].transform(
        lambda s: s.shift(1).rolling(3, min_periods=1).mean()
    )
    sales_performance["revenue_target"] = sales_performance["revenue_target"].fillna(
        sales_performance.groupby("sales_rep_key")["closed_won_revenue"].transform("mean")
    )
    sales_performance["target_attainment_ratio"] = sales_performance["closed_won_revenue"] / sales_performance["revenue_target"].replace(0, pd.NA)
    sales_performance["exceeded_target"] = sales_performance["closed_won_revenue"] >= sales_performance["revenue_target"]

    # Build the stage-month conversion mart for funnel reporting.
    pipeline_conversion = con.sql(
        """
        select
            date_trunc('month', created_date) as date,
            strftime(date_trunc('month', created_date), '%Y-%m') as year_month,
            year(date_trunc('month', created_date)) as year,
            month(date_trunc('month', created_date)) as month,
            deal_stage,
            count(*) as opportunities_created,
            sum(case when is_won then 1 else 0 end) as won_count,
            sum(case when is_lost then 1 else 0 end) as lost_count,
            avg(case when is_closed then cast(is_won as double) end) as stage_conversion_rate,
            avg(opportunity_age_days) as avg_opportunity_age_days,
            avg(days_to_close) as avg_days_to_close
        from opportunities
        group by 1,2,3,4,5
        """
    ).df()
    pipeline_conversion = pipeline_conversion[pipeline_conversion["date"].notna()].copy()

    # Build the account-month customer mart and tag each month as new or returning.
    customer_performance = con.sql(
        """
        with won as (
            select
                date_trunc('month', close_date) as date,
                strftime(date_trunc('month', close_date), '%Y-%m') as year_month,
                year(date_trunc('month', close_date)) as year,
                month(date_trunc('month', close_date)) as month,
                account_key,
                account,
                sum(close_value) as closed_won_revenue
            from opportunities
            where is_won and close_date is not null
            group by 1,2,3,4,5,6
        ),
        cohorts as (
            select
                account_key,
                min(year_month) as cohort_month
            from won
            group by 1
        )
        select
            w.*,
            c.cohort_month,
            case when w.year_month = c.cohort_month then 'new' else 'returning' end as customer_status
        from won w
        left join cohorts c using (account_key)
        """
    ).df()
    customer_performance = customer_performance[customer_performance["date"].notna()].copy()

    # Build the product-month trend mart for product-level performance analysis.
    product_performance = con.sql(
        """
        select
            date_trunc('month', close_date) as date,
            strftime(date_trunc('month', close_date), '%Y-%m') as year_month,
            year(date_trunc('month', close_date)) as year,
            month(date_trunc('month', close_date)) as month,
            o.product_key,
            o.product,
            p.series,
            sum(case when is_won then close_value else 0 end) as closed_won_revenue,
            sum(close_value) as pipeline_value,
            avg(predicted_win_probability) as avg_predicted_win_probability
        from opportunities o
        left join products p
            on o.product = p.product
        where close_date is not null
        group by 1,2,3,4,5,6,7
        """
    ).df()
    product_performance = product_performance[product_performance["date"].notna()].copy()

    # Build a team-month mart so manager and regional office performance can be compared over time.
    team_performance = con.sql(
        """
        select
            date_trunc('month', coalesce(o.close_date, o.created_date)) as date,
            strftime(date_trunc('month', coalesce(o.close_date, o.created_date)), '%Y-%m') as year_month,
            year(date_trunc('month', coalesce(o.close_date, o.created_date))) as year,
            month(date_trunc('month', coalesce(o.close_date, o.created_date))) as month,
            r.manager,
            r.regional_office,
            sum(case when o.is_won then o.close_value else 0 end) as closed_won_revenue,
            sum(o.close_value) as monthly_pipeline_value,
            avg(case when o.is_closed then cast(o.is_won as double) end) as win_rate,
            avg(case when o.is_closed then o.days_to_close end) as avg_days_to_close
        from opportunities o
        left join reps r
            on o.sales_agent = r.sales_agent
        group by 1,2,3,4,5,6
        """
    ).df()
    team_performance = team_performance[team_performance["date"].notna()].copy()
    team_performance["revenue_target"] = team_performance.groupby(["manager", "regional_office"])["closed_won_revenue"].transform(
        lambda s: s.shift(1).rolling(3, min_periods=1).mean()
    )
    team_performance["revenue_target"] = team_performance["revenue_target"].fillna(
        team_performance.groupby(["manager", "regional_office"])["closed_won_revenue"].transform("mean")
    )
    team_performance["target_attainment_ratio"] = team_performance["closed_won_revenue"] / team_performance["revenue_target"].replace(0, pd.NA)
    team_performance["exceeded_target"] = team_performance["closed_won_revenue"] >= team_performance["revenue_target"]

    # Build the company-level monthly mart and derive rolling and coverage metrics.
    monthly_company = con.sql(
        """
        select
            date_trunc('month', coalesce(close_date, created_date)) as date,
            strftime(date_trunc('month', coalesce(close_date, created_date)), '%Y-%m') as year_month,
            year(date_trunc('month', coalesce(close_date, created_date))) as year,
            month(date_trunc('month', coalesce(close_date, created_date))) as month,
            sum(close_value) as monthly_pipeline_value,
            sum(weighted_pipeline_value) as monthly_weighted_pipeline_value,
            sum(case when is_won then close_value else 0 end) as closed_won_revenue,
            sum(case when is_lost then close_value else 0 end) as closed_lost_value,
            avg(case when is_closed then days_to_close end) as avg_days_to_close,
            avg(case when is_closed then cast(is_won as double) end) as win_rate,
            avg(close_value) as average_deal_size
        from opportunities
        group by 1,2,3,4
        order by 1
        """
    ).df()
    monthly_company = monthly_company[monthly_company["date"].notna()].copy()
    monthly_company["closed_won_revenue_rolling_3m"] = monthly_company["closed_won_revenue"].rolling(3, min_periods=1).mean()
    monthly_company["closed_won_revenue_rolling_6m"] = monthly_company["closed_won_revenue"].rolling(6, min_periods=1).mean()
    monthly_company["pipeline_coverage_ratio"] = monthly_company["monthly_pipeline_value"] / monthly_company["closed_won_revenue"].replace(0, pd.NA)
    monthly_company["date_key"] = monthly_company["date"].dt.strftime("%Y%m%d").astype("Int64")

    # Aggregate the monthly mart into a compact yearly summary table.
    yearly_summary = (
        monthly_company.groupby("year", as_index=False)[
            [
                "monthly_pipeline_value",
                "monthly_weighted_pipeline_value",
                "closed_won_revenue",
                "closed_lost_value",
            ]
        ]
        .sum()
        .rename(
            columns={
                "monthly_pipeline_value": "yearly_pipeline_value",
                "monthly_weighted_pipeline_value": "yearly_weighted_pipeline_value",
            }
        )
    )

    # Build the forward-looking forecasting mart from historical monthly revenue.
    forecasting, forecast_metrics = forecast_monthly_revenue(
        monthly_company[["date", "closed_won_revenue"]], CONFIG.forecast_horizon_months
    )
    forecasting["year"] = forecasting["date"].dt.year
    forecasting["month"] = forecasting["date"].dt.month
    forecasting["year_month"] = forecasting["date"].dt.strftime("%Y-%m")
    forecasting["date_key"] = forecasting["date"].dt.strftime("%Y%m%d").astype(int)

    # Build a compact KPI summary table answering common business questions directly.
    overall_win_rate = float(opportunities["is_won"].sum() / opportunities["is_closed"].sum())
    avg_sales_cycle_days = float(opportunities.loc[opportunities["is_closed"], "days_to_close"].mean())
    engaging_pipeline_value = float(opportunities.loc[opportunities["deal_stage"] == "Engaging", "close_value"].sum())
    rep_target_summary = (
        sales_performance.groupby(["sales_rep_key", "sales_agent"], as_index=False)
        .agg(
            months_exceeding_target=("exceeded_target", "sum"),
            total_months=("exceeded_target", "count"),
            total_closed_won_revenue=("closed_won_revenue", "sum"),
            average_target_attainment=("target_attainment_ratio", "mean"),
        )
        .sort_values(["months_exceeding_target", "total_closed_won_revenue"], ascending=[False, False])
    )
    team_target_summary = (
        team_performance.groupby(["manager", "regional_office"], as_index=False)
        .agg(
            months_exceeding_target=("exceeded_target", "sum"),
            total_months=("exceeded_target", "count"),
            total_closed_won_revenue=("closed_won_revenue", "sum"),
            average_target_attainment=("target_attainment_ratio", "mean"),
        )
        .sort_values(["months_exceeding_target", "total_closed_won_revenue"], ascending=[False, False])
    )
    kpi_summary = pd.DataFrame(
        [
            {
                "metric_name": "overall_win_rate",
                "metric_value": overall_win_rate,
                "metric_unit": "ratio",
                "metric_description": "Share of closed opportunities that ended as won.",
            },
            {
                "metric_name": "average_sales_cycle_days",
                "metric_value": avg_sales_cycle_days,
                "metric_unit": "days",
                "metric_description": "Average days between created date and close date for closed opportunities.",
            },
            {
                "metric_name": "engaging_stage_pipeline_value",
                "metric_value": engaging_pipeline_value,
                "metric_unit": "currency",
                "metric_description": "Current potential revenue sitting in the Engaging stage.",
            },
            {
                "metric_name": "top_rep_by_target_attainment",
                "metric_value": rep_target_summary.iloc[0]["average_target_attainment"],
                "metric_unit": "ratio",
                "metric_description": f"Highest average target attainment achieved by sales agent {rep_target_summary.iloc[0]['sales_agent']}.",
            },
            {
                "metric_name": "top_team_by_target_attainment",
                "metric_value": team_target_summary.iloc[0]["average_target_attainment"],
                "metric_unit": "ratio",
                "metric_description": f"Highest average target attainment achieved by manager {team_target_summary.iloc[0]['manager']} in {team_target_summary.iloc[0]['regional_office']}.",
            },
        ]
    )

    # Assemble the final Gold outputs for export to BI and downstream analysis.
    gold_tables = {
        "gold_dim_date": date_dim,
        "gold_dim_account": accounts[
            ["account_key", "account", "sector", "year_established", "revenue", "employees", "office_location", "subsidiary_of"]
        ],
        "gold_dim_sales_rep": reps[["sales_rep_key", "sales_agent", "manager", "regional_office"]],
        "gold_dim_product": products[["product_key", "product", "series", "sales_price"]],
        "gold_dim_stage": stages[["stage_key", "stage_name", "default_probability"]],
        "gold_fct_pipeline_snapshot": pipeline_snapshot,
        "gold_fct_closed_deals": closed_deals,
        "gold_mart_sales_performance": monthly_company,
        "gold_mart_sales_performance_by_rep": sales_performance,
        "gold_mart_sales_performance_by_team": team_performance,
        "gold_mart_pipeline_conversion": pipeline_conversion,
        "gold_mart_forecasting": forecasting,
        "gold_mart_customer_performance": customer_performance,
        "gold_mart_product_performance": product_performance,
        "gold_mart_yearly_summary": yearly_summary,
        "gold_mart_kpi_summary": kpi_summary,
        "gold_fct_opportunity_scoring": opportunities[
            [
                "opportunity_id",
                "account_key",
                "sales_rep_key",
                "product_key",
                "stage_key",
                "created_date",
                "close_date",
                "close_value",
                "predicted_win_probability",
                "weighted_pipeline_value",
                "opportunity_age_days",
                "days_to_close",
                "created_year_month",
                "close_year_month",
            ]
        ],
    }

    # Persist all Gold outputs and return them to the pipeline entry point.
    for name, df in gold_tables.items():
        write_dataset(df, CONFIG.gold_dir, name)

    return gold_tables | {
        "win_prob_model_metrics": pd.DataFrame([win_prob_model_metrics]),
        "forecast_metrics": pd.DataFrame([forecast_metrics]),
    }
