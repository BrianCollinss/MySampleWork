"""Silver-layer cleaning and conformance logic for CRM entities."""

from __future__ import annotations

import pandas as pd

from src.config import CONFIG
from src.io_files import write_dataset


def _clean_accounts(df: pd.DataFrame) -> pd.DataFrame:
    """Standardise the account dimension and fix known source issues."""

    # Normalise column names and account text fields.
    cleaned = df.copy()
    cleaned.columns = [col.strip().lower() for col in cleaned.columns]
    cleaned["sector"] = cleaned["sector"].replace({"technolgy": "technology"})
    cleaned["account"] = cleaned["account"].str.strip()
    cleaned["office_location"] = cleaned["office_location"].str.strip()

    # Cast numeric attributes into analysis-friendly types.
    cleaned["employees"] = pd.to_numeric(cleaned["employees"], errors="coerce").astype("Int64")
    cleaned["revenue"] = pd.to_numeric(cleaned["revenue"], errors="coerce")

    return cleaned.drop_duplicates(subset=["account"])


def _clean_products(df: pd.DataFrame) -> pd.DataFrame:
    """Standardise the product dimension and harmonise product naming."""

    # Normalise column names and fix product naming inconsistencies.
    cleaned = df.copy()
    cleaned.columns = [col.strip().lower() for col in cleaned.columns]
    cleaned["product"] = cleaned["product"].str.replace("GTXPro", "GTX Pro", regex=False)
    cleaned["series"] = cleaned["series"].str.strip()

    # Cast price fields to numeric so they can support downstream comparisons.
    cleaned["sales_price"] = pd.to_numeric(cleaned["sales_price"], errors="coerce")

    return cleaned.drop_duplicates(subset=["product"])


def _clean_sales_teams(df: pd.DataFrame) -> pd.DataFrame:
    """Standardise the sales-team dimension and remove duplicate reps."""

    # Normalise the sales-team column names before de-duplicating the rep list.
    cleaned = df.copy()
    cleaned.columns = [col.strip().lower() for col in cleaned.columns]

    return cleaned.drop_duplicates(subset=["sales_agent"])


def _clean_sales_pipeline(df: pd.DataFrame) -> pd.DataFrame:
    """Standardise the opportunity table (sales_pipeline) and derive temporal lifecycle features."""

    # Normalise column names, product labels, and date fields.
    cleaned = df.copy()
    cleaned.columns = [col.strip().lower() for col in cleaned.columns]
    cleaned["product"] = cleaned["product"].str.replace("GTXPro", "GTX Pro", regex=False)
    cleaned["engage_date"] = pd.to_datetime(cleaned["engage_date"], errors="coerce")
    cleaned["close_date"] = pd.to_datetime(cleaned["close_date"], errors="coerce")
    cleaned["close_value"] = pd.to_numeric(cleaned["close_value"], errors="coerce").fillna(0.0)
    cleaned["deal_stage"] = cleaned["deal_stage"].str.title()

    # Create conformed lifecycle fields used across the Gold model.
    cleaned["created_date"] = cleaned["engage_date"]
    cleaned["activity_date"] = cleaned["close_date"].fillna(cleaned["engage_date"])
    cleaned["is_closed"] = cleaned["deal_stage"].isin(["Won", "Lost"])
    cleaned["is_won"] = cleaned["deal_stage"].eq("Won")
    cleaned["is_lost"] = cleaned["deal_stage"].eq("Lost")

    # Derive time-aware features for age, cycle length, and monthly reporting.
    today = pd.Timestamp("2017-12-31")
    cleaned["opportunity_age_days"] = cleaned["close_date"].fillna(today).sub(cleaned["created_date"]).dt.days
    cleaned["days_to_close"] = cleaned["close_date"].sub(cleaned["created_date"]).dt.days
    cleaned["created_year_month"] = cleaned["created_date"].dt.strftime("%Y-%m")
    cleaned["close_year_month"] = cleaned["close_date"].dt.strftime("%Y-%m")
    cleaned["snapshot_month"] = cleaned["close_date"].fillna(today).dt.to_period("M").dt.to_timestamp()

    return cleaned.drop_duplicates(subset=["opportunity_id"])


def build_silver(bronze_tables: dict[str, pd.DataFrame]) -> dict[str, pd.DataFrame]:
    """Clean Bronze tables into conformed Silver entities and persist them."""

    # Apply entity-specific cleaning logic to each Bronze table.
    silver_tables = {
        "accounts": _clean_accounts(bronze_tables["accounts"]),
        "products": _clean_products(bronze_tables["products"]),
        "sales_teams": _clean_sales_teams(bronze_tables["sales_teams"]),
        "opportunities": _clean_sales_pipeline(bronze_tables["sales_pipeline"]),
    }

    # Persist each Silver entity for reuse by Gold and notebook workflows.
    for name, df in silver_tables.items():
        write_dataset(df, CONFIG.silver_dir, f"silver_{name}")

    return silver_tables
