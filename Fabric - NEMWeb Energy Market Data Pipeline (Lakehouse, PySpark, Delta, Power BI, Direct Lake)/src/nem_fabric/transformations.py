"""Local pandas transformation helpers mirroring Fabric Spark logic.

These functions support tests and exploratory local runs. In production Fabric
notebooks, the same logic should be implemented with Spark DataFrames for Delta
table writes at Lakehouse scale.
"""

from __future__ import annotations

import pandas as pd

from nem_fabric.schema import PRICE_THRESHOLDS, REGION_MAP


def _resolve_column(df: pd.DataFrame, column: str) -> str | None:
    """Return the actual DataFrame column matching a source name."""

    if column in df.columns:
        return column
    normalised = column.lower()
    for candidate in df.columns:
        if candidate.lower() == normalised:
            return candidate
    return None


def normalise_timestamps(
    df: pd.DataFrame, column: str = "settlementdate"
) -> pd.DataFrame:
    """Parse AEMO settlement timestamps into a standard datetime column."""

    result = df.copy()
    source_column = _resolve_column(result, column)
    if source_column:
        result["settlement_datetime"] = pd.to_datetime(
            result[source_column], errors="coerce"
        )
    return result


def cast_numeric_fields(df: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    """Cast selected columns to numeric values, coercing bad records to null."""

    result = df.copy()
    for column in columns:
        if column in result.columns:
            result[column] = pd.to_numeric(result[column], errors="coerce")
    return result


def standardise_region_names(
    df: pd.DataFrame, column: str = "regionid"
) -> pd.DataFrame:
    """Add standard `region` and `region_name` columns from AEMO region IDs."""

    result = df.copy()
    source_column = _resolve_column(result, column)
    if source_column:
        result["region"] = result[source_column].str.upper()
        result["region_name"] = result["region"].map(REGION_MAP)
    return result


def remove_duplicate_rows(
    df: pd.DataFrame, subset: list[str] | None = None
) -> pd.DataFrame:
    """Remove duplicate rows using either a supplied key or full-row comparison."""

    return df.drop_duplicates(subset=subset).reset_index(drop=True)


def add_interval_fields(df: pd.DataFrame) -> pd.DataFrame:
    """Derive date and interval helper fields for Gold and Power BI tables."""

    result = df.copy()
    if "settlement_datetime" not in result.columns:
        return result
    dt = result["settlement_datetime"]
    result["trading_date"] = dt.dt.date
    result["year"] = dt.dt.year
    result["month"] = dt.dt.month
    result["day"] = dt.dt.day
    result["interval_hour"] = dt.dt.hour
    result["interval_minute"] = dt.dt.minute
    return result


def build_region_5min(df: pd.DataFrame) -> pd.DataFrame:
    """Build a local 5-minute regional price and demand shape."""

    result = df.copy()
    # DispatchIS Bronze columns arrive lower-case from the parser. Rename only
    # known business fields and leave unknown columns available for future use.
    rename_map = {"rrp": "price_aud_mwh", "totaldemand": "demand_mw"}
    result = result.rename(
        columns={k: v for k, v in rename_map.items() if k in result.columns}
    )
    result = normalise_timestamps(result)
    result = standardise_region_names(result)
    result = cast_numeric_fields(result, ["price_aud_mwh", "demand_mw"])
    result = add_interval_fields(result)
    if "price_aud_mwh" in result.columns:
        # Precompute report flags so Power BI visuals and measures stay simple.
        result["is_negative_price"] = (
            result["price_aud_mwh"]
            < PRICE_THRESHOLDS["negative_price_threshold_aud_mwh"]
        )
        result["is_high_price"] = (
            result["price_aud_mwh"] >= PRICE_THRESHOLDS["high_price_threshold_aud_mwh"]
        )
        result["is_extreme_price"] = (
            result["price_aud_mwh"]
            >= PRICE_THRESHOLDS["extreme_price_threshold_aud_mwh"]
        )
        result["price_band"] = pd.cut(
            result["price_aud_mwh"],
            bins=[float("-inf"), 0, 300, 1000, float("inf")],
            labels=["Negative", "Normal", "High", "Extreme"],
        )
    return remove_duplicate_rows(result)


def build_current_snapshot(df: pd.DataFrame) -> pd.DataFrame:
    """Return the latest interval per region for snapshot KPI cards."""

    if df.empty or "settlement_datetime" not in df.columns:
        return df.head(0)
    return (
        df.sort_values("settlement_datetime").groupby("region", as_index=False).tail(1)
    )


def build_30min_region_aggregation(df: pd.DataFrame) -> pd.DataFrame:
    """Aggregate 5-minute regional records to 30-minute regional grain."""

    if df.empty:
        return df
    result = df.copy()
    result["settlement_30min"] = result["settlement_datetime"].dt.floor("30min")
    return (
        result.groupby(["region", "region_name", "settlement_30min"], dropna=False)
        .agg(price_aud_mwh=("price_aud_mwh", "mean"), demand_mw=("demand_mw", "mean"))
        .reset_index()
    )


def build_daily_region_summary(df: pd.DataFrame) -> pd.DataFrame:
    """Aggregate regional records into daily price and demand summaries."""

    if df.empty:
        return df
    return (
        df.groupby(["region", "region_name", "trading_date"], dropna=False)
        .agg(
            daily_avg_price=("price_aud_mwh", "mean"),
            daily_max_price=("price_aud_mwh", "max"),
            daily_min_price=("price_aud_mwh", "min"),
            daily_avg_demand=("demand_mw", "mean"),
        )
        .reset_index()
    )


def build_price_spikes(df: pd.DataFrame) -> pd.DataFrame:
    """Return intervals flagged as high, extreme, or negative price events."""

    if "is_high_price" not in df.columns:
        return df.head(0)
    return df[df["is_high_price"] | df["is_negative_price"]].copy()
