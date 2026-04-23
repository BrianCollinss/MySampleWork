"""Pipeline entry point for the end-to-end CRM revenue analytics workflow."""

from __future__ import annotations

import pandas as pd

from src.analytics_visuals import create_charts
from src.config import CONFIG
from src.io_files import write_json
from src.layer_bronze import build_bronze
from src.layer_gold import build_gold
from src.layer_silver import build_silver
from src.quality_checks import run_quality_checks


def _build_summary_card_metrics(gold_tables: dict[str, pd.DataFrame]) -> dict[str, object]:
    """Create dashboard summary cards aligned to Power BI totals plus QoQ deltas."""

    # Pull the closed-deal fact because wins, win rate, and sales-cycle metrics all depend on closed outcomes.
    closed_deals = gold_tables["gold_fct_closed_deals"].copy()

    # Derive the quarter bucket from the close date so each KPI can be compared to the prior closed quarter.
    closed_deals["quarter"] = closed_deals["date"].dt.to_period("Q")

    # Limit win-rate calculations to pursued opportunities, matching the Won versus Lost denominator used elsewhere.
    pursued_deals = closed_deals[closed_deals["deal_stage"].isin(["Won", "Lost"])].copy()

    # Limit sales-cycle calculations to won deals with a populated cycle length, matching the Power BI DAX definition.
    won_deals = closed_deals[(closed_deals["deal_stage"] == "Won") & (closed_deals["days_to_close"].notna())].copy()

    # Aggregate quarter-level metrics so the latest quarter can be compared with the immediately previous quarter.
    wins_by_quarter = closed_deals.groupby("quarter").apply(lambda frame: int(frame["deal_stage"].eq("Won").sum())).sort_index()
    closed_deals_by_quarter = pursued_deals.groupby("quarter").size().sort_index()
    lost_deals_by_quarter = closed_deals.groupby("quarter").apply(lambda frame: int(frame["deal_stage"].eq("Lost").sum())).sort_index()
    win_rate_by_quarter = pursued_deals.groupby("quarter").apply(lambda frame: float(frame["deal_stage"].eq("Won").mean())).sort_index()
    avg_scl_by_quarter = won_deals.groupby("quarter")["days_to_close"].mean().sort_index()
    avg_closed_deal_size_by_quarter = pursued_deals.groupby("quarter")["revenue"].mean().sort_index()

    # Determine the latest and previous quarter values, guarding against sparse quarter histories.
    latest_quarter = str(wins_by_quarter.index.max())
    previous_quarter = str(wins_by_quarter.index[-2]) if len(wins_by_quarter) > 1 else None

    # Use overall KPI values for the cards so the JSON matches the Power BI summary cards.
    current_closed_deals = int(len(pursued_deals))
    latest_closed_deals = int(closed_deals_by_quarter.iloc[-1])
    previous_closed_deals = int(closed_deals_by_quarter.iloc[-2]) if len(closed_deals_by_quarter) > 1 else 0
    current_deals_win = int(closed_deals["deal_stage"].eq("Won").sum())
    latest_deals_win = int(wins_by_quarter.iloc[-1])
    previous_deals_win = int(wins_by_quarter.iloc[-2]) if len(wins_by_quarter) > 1 else 0
    current_deals_lost = int(closed_deals["deal_stage"].eq("Lost").sum())
    latest_deals_lost = int(lost_deals_by_quarter.iloc[-1])
    previous_deals_lost = int(lost_deals_by_quarter.iloc[-2]) if len(lost_deals_by_quarter) > 1 else 0
    current_win_pct = float(pursued_deals["deal_stage"].eq("Won").mean())
    latest_win_pct = float(win_rate_by_quarter.iloc[-1])
    previous_win_pct = float(win_rate_by_quarter.iloc[-2]) if len(win_rate_by_quarter) > 1 else 0.0
    current_avg_scl = float(won_deals["days_to_close"].mean())
    latest_avg_scl = float(avg_scl_by_quarter.iloc[-1])
    previous_avg_scl = float(avg_scl_by_quarter.iloc[-2]) if len(avg_scl_by_quarter) > 1 else 0.0
    current_avg_closed_deal_size = float(pursued_deals["revenue"].mean())
    latest_avg_closed_deal_size = float(avg_closed_deal_size_by_quarter.iloc[-1])
    previous_avg_closed_deal_size = float(avg_closed_deal_size_by_quarter.iloc[-2]) if len(avg_closed_deal_size_by_quarter) > 1 else 0.0
    current_revenue = float(closed_deals.loc[closed_deals["deal_stage"] == "Won", "revenue"].sum())
    revenue_by_quarter = closed_deals.loc[closed_deals["deal_stage"] == "Won"].groupby("quarter")["revenue"].sum().sort_index()
    latest_revenue = float(revenue_by_quarter.iloc[-1])
    previous_revenue = float(revenue_by_quarter.iloc[-2]) if len(revenue_by_quarter) > 1 else 0.0
    current_avg_revenue = float(closed_deals.loc[closed_deals["deal_stage"] == "Won", "revenue"].mean())
    avg_revenue_by_quarter = closed_deals.loc[closed_deals["deal_stage"] == "Won"].groupby("quarter")["revenue"].mean().sort_index()
    latest_avg_revenue = float(avg_revenue_by_quarter.iloc[-1])
    previous_avg_revenue = float(avg_revenue_by_quarter.iloc[-2]) if len(avg_revenue_by_quarter) > 1 else 0.0

    # Express the last-quarter movement as a percentage change when a valid prior-quarter base exists.
    closed_deals_delta = ((latest_closed_deals - previous_closed_deals) / previous_closed_deals) if previous_closed_deals else 0.0
    deals_win_delta = ((latest_deals_win - previous_deals_win) / previous_deals_win) if previous_deals_win else 0.0
    deals_lost_delta = ((latest_deals_lost - previous_deals_lost) / previous_deals_lost) if previous_deals_lost else 0.0
    win_pct_delta = ((latest_win_pct - previous_win_pct) / previous_win_pct) if previous_win_pct else 0.0
    avg_scl_delta = ((latest_avg_scl - previous_avg_scl) / previous_avg_scl) if previous_avg_scl else 0.0
    revenue_delta = ((latest_revenue - previous_revenue) / previous_revenue) if previous_revenue else 0.0
    avg_revenue_delta = ((latest_avg_revenue - previous_avg_revenue) / previous_avg_revenue) if previous_avg_revenue else 0.0
    avg_closed_deal_size_delta = ((latest_avg_closed_deal_size - previous_avg_closed_deal_size) / previous_avg_closed_deal_size) if previous_avg_closed_deal_size else 0.0

    return {
        "as_of_quarter": latest_quarter,
        "previous_quarter": previous_quarter,
        "closed_deals": {
            "current_value": current_closed_deals,
            "display_value": f"{current_closed_deals / 1_000:.2f}K",
            "last_quarter_change_ratio": round(closed_deals_delta, 4),
            "last_quarter_change_pct": f"{closed_deals_delta:.1%}",
            "definition": "Count of pursued opportunities with deal_stage equal to Won or Lost across the full dataset, with quarter-over-quarter change based on the latest closed quarter versus the previous quarter.",
        },
        "deals_win": {
            "current_value": current_deals_win,
            "display_value": f"{current_deals_win / 1_000:.2f}K",
            "last_quarter_change_ratio": round(deals_win_delta, 4),
            "last_quarter_change_pct": f"{deals_win_delta:.1%}",
            "definition": "Count of won opportunities across the full dataset, with quarter-over-quarter change based on the latest closed quarter versus the previous quarter.",
        },
        "deals_lost": {
            "current_value": current_deals_lost,
            "display_value": f"{current_deals_lost / 1_000:.2f}K",
            "last_quarter_change_ratio": round(deals_lost_delta, 4),
            "last_quarter_change_pct": f"{deals_lost_delta:.1%}",
            "definition": "Count of lost opportunities across the full dataset, with quarter-over-quarter change based on the latest closed quarter versus the previous quarter.",
        },
        "win_pct": {
            "current_value": round(current_win_pct, 4),
            "display_value": f"{current_win_pct:.1%}",
            "last_quarter_change_ratio": round(win_pct_delta, 4),
            "last_quarter_change_pct": f"{win_pct_delta:.1%}",
            "definition": "Won opportunities divided by pursued opportunities across the full dataset, with quarter-over-quarter change based on the latest closed quarter versus the previous quarter.",
        },
        "avg_scl": {
            "current_value": round(current_avg_scl, 2),
            "display_value": f"{current_avg_scl:.2f}",
            "last_quarter_change_ratio": round(avg_scl_delta, 4),
            "last_quarter_change_pct": f"{avg_scl_delta:.1%}",
            "definition": "Average days_to_close for won opportunities with a populated close date across the full dataset, with quarter-over-quarter change based on the latest closed quarter versus the previous quarter.",
        },
        "revenue": {
            "current_value": round(current_revenue, 1),
            "display_value": f"${current_revenue / 1_000_000:.0f}M",
            "last_quarter_change_ratio": round(revenue_delta, 4),
            "last_quarter_change_pct": f"{revenue_delta:.1%}",
            "definition": "Total revenue from won opportunities across the full dataset, with quarter-over-quarter change based on the latest closed quarter versus the previous quarter.",
        },
        "avg_revenue": {
            "current_value": round(current_avg_revenue, 1),
            "display_value": f"${current_avg_revenue / 1_000:.2f}K",
            "last_quarter_change_ratio": round(avg_revenue_delta, 4),
            "last_quarter_change_pct": f"{avg_revenue_delta:.1%}",
            "definition": "Average revenue per won opportunity across the full dataset, with quarter-over-quarter change based on the latest closed quarter versus the previous quarter.",
        },
        "avg_closed_deal_size": {
            "current_value": round(current_avg_closed_deal_size, 1),
            "display_value": f"${current_avg_closed_deal_size / 1_000:.2f}K",
            "last_quarter_change_ratio": round(avg_closed_deal_size_delta, 4),
            "last_quarter_change_pct": f"{avg_closed_deal_size_delta:.1%}",
            "definition": "Average revenue per closed opportunity across the full dataset, with quarter-over-quarter change based on the latest closed quarter versus the previous quarter.",
        },
    }


def main() -> None:
    """Run Bronze, Silver, Gold, quality, and reporting steps in sequence."""

    # Build the medallion layers from the raw CSV sources.
    bronze_tables = build_bronze()
    silver_tables = build_silver(bronze_tables)
    gold_tables = build_gold(silver_tables)
    quality_results = run_quality_checks(silver_tables)

    # Persist reporting artefacts after the Gold layer has been created.
    win_prob_metrics_df = gold_tables.pop("win_prob_model_metrics")
    forecast_metrics_df = gold_tables.pop("forecast_metrics")
    summary_card_metrics = _build_summary_card_metrics(gold_tables)
    create_charts(gold_tables, CONFIG.reports_dir)

    write_json(quality_results, CONFIG.reports_metrics_dir / "data_quality_summary.json")
    write_json(win_prob_metrics_df.iloc[0].to_dict(), CONFIG.reports_metrics_dir / "win_prob_model_metrics.json")
    write_json(forecast_metrics_df.iloc[0].to_dict(), CONFIG.reports_metrics_dir / "forecast_metrics.json")
    write_json(summary_card_metrics, CONFIG.reports_metrics_dir / "summary_card_metrics.json")


if __name__ == "__main__":
    # Allow the pipeline to be executed directly from the command line.
    main()
