"""Data quality checks for conformed Silver-layer entities."""

from __future__ import annotations

import pandas as pd


def run_quality_checks(silver_tables: dict[str, pd.DataFrame]) -> dict[str, object]:
    """Calculate a compact set of integrity and coverage checks for Silver data."""

    # Pull the conformed entities needed for dimension coverage and null checks.
    opps = silver_tables["opportunities"]
    accounts = silver_tables["accounts"]
    products = silver_tables["products"]
    reps = silver_tables["sales_teams"]

    # Summarise key data-quality signals for reporting and pipeline validation.
    checks = {
        "opportunity_id_unique": bool(opps["opportunity_id"].is_unique),
        "account_dimension_coverage": float(opps["account"].isin(accounts["account"]).mean()),
        "product_dimension_coverage": float(opps["product"].isin(products["product"]).mean()),
        "sales_rep_dimension_coverage": float(opps["sales_agent"].isin(reps["sales_agent"]).mean()),
        "created_date_nulls": int(opps["created_date"].isna().sum()),
        "close_date_nulls": int(opps["close_date"].isna().sum()),
    }

    return checks
