"""Local pandas transformation helpers mirroring Fabric Spark logic.

These functions support tests and exploratory local runs. In production Fabric
notebooks, the same logic should be implemented with Spark DataFrames for Delta
table writes at Lakehouse scale.
"""

from __future__ import annotations

import pandas as pd

from nem_fabric.common_schema import PRICE_THRESHOLDS, REGION_MAP


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


def _numeric_column(df: pd.DataFrame, column: str) -> pd.Series:
    """Return a numeric Series for a column, or nulls when the column is absent."""

    if column in df.columns:
        return pd.to_numeric(df[column], errors="coerce")
    return pd.Series([pd.NA] * len(df), index=df.index, dtype="Float64")


def _first_existing_column(df: pd.DataFrame, candidates: list[str]) -> str | None:
    """Return the first candidate column present in a DataFrame."""

    for column in candidates:
        resolved = _resolve_column(df, column)
        if resolved:
            return resolved
    return None


def _source_rows(bronze: pd.DataFrame, source_name: str) -> pd.DataFrame:
    """Filter Bronze rows from a configured source folder or source name."""

    source_mask = pd.Series(False, index=bronze.index)
    for column in ["source_folder", "source_name"]:
        if column in bronze.columns:
            source_mask = source_mask | bronze[column].str.contains(
                source_name, case=False, na=False
            )
    return bronze[source_mask].copy()


def _normalise_region_from_any(df: pd.DataFrame) -> pd.DataFrame:
    """Add region fields from common AEMO region column variants."""

    for column in ["regionid", "region", "region_id"]:
        if _resolve_column(df, column):
            return standardise_region_names(df, column)
    return df.copy()


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


def build_silver_price_demand(
    bronze: pd.DataFrame,
    run_id: str,
) -> pd.DataFrame:
    """Build local Silver regional price and demand at 5-minute grain."""

    price = bronze[
        (bronze["package_name"] == "DISPATCH") & (bronze["table_name"] == "PRICE")
    ].copy()
    regionsum = bronze[
        (bronze["package_name"] == "DISPATCH") & (bronze["table_name"] == "REGIONSUM")
    ].copy()

    if price.empty:
        return pd.DataFrame()

    price = normalise_timestamps(price)
    price = standardise_region_names(price)
    price["intervention"] = _numeric_column(price, "intervention")
    price["price_aud_mwh"] = _numeric_column(price, "rrp")
    price = price.rename(columns={"row_hash": "price_row_hash"})

    price_columns = [
        "settlement_datetime",
        "region",
        "region_name",
        "intervention",
        "price_aud_mwh",
        "source_url",
        "source_zip_name",
        "price_row_hash",
    ]
    price = price[[column for column in price_columns if column in price.columns]]

    if not regionsum.empty:
        regionsum = normalise_timestamps(regionsum)
        regionsum = standardise_region_names(regionsum)
        regionsum["intervention"] = _numeric_column(regionsum, "intervention")
        rename_map = {
            "totaldemand": "demand_mw",
            "availablegeneration": "available_generation_mw",
            "availableload": "available_load_mw",
            "demandforecast": "demand_forecast_mw",
            "dispatchablegeneration": "dispatchable_generation_mw",
            "dispatchableload": "dispatchable_load_mw",
            "netinterchange": "net_interchange_mw",
            "excessgeneration": "excess_generation_mw",
            "clearedsupply": "cleared_supply_mw",
            "semischedule_clearedmw": "semi_scheduled_generation_mw",
            "row_hash": "regionsum_row_hash",
        }
        regionsum = regionsum.rename(
            columns={
                key: value for key, value in rename_map.items() if key in regionsum
            }
        )
        numeric_columns = [
            "demand_mw",
            "available_generation_mw",
            "available_load_mw",
            "demand_forecast_mw",
            "dispatchable_generation_mw",
            "dispatchable_load_mw",
            "net_interchange_mw",
            "excess_generation_mw",
            "cleared_supply_mw",
            "semi_scheduled_generation_mw",
        ]
        regionsum = cast_numeric_fields(regionsum, numeric_columns)
        if {
            "dispatchable_generation_mw",
            "semi_scheduled_generation_mw",
        }.issubset(regionsum.columns):
            regionsum["scheduled_generation_mw"] = (
                regionsum["dispatchable_generation_mw"]
                - regionsum["semi_scheduled_generation_mw"]
            )
            numeric_columns.append("scheduled_generation_mw")
        regionsum_columns = [
            "settlement_datetime",
            "region",
            "intervention",
            *numeric_columns,
            "regionsum_row_hash",
        ]
        regionsum = regionsum[
            [column for column in regionsum_columns if column in regionsum.columns]
        ]
        result = price.merge(
            regionsum,
            on=["settlement_datetime", "region", "intervention"],
            how="left",
        )
    else:
        result = price

    result = add_interval_fields(result)
    result["silver_loaded_datetime"] = pd.Timestamp.now("UTC").isoformat()
    result["run_id"] = run_id
    return remove_duplicate_rows(
        result,
        subset=["settlement_datetime", "region", "intervention"],
    )


def build_silver_interconnector_flows(
    bronze: pd.DataFrame,
    run_id: str,
) -> pd.DataFrame:
    """Build local Silver interconnector flow records."""

    rows = bronze[
        (bronze["package_name"] == "DISPATCH")
        & (bronze["table_name"] == "INTERCONNECTORRES")
    ].copy()
    if rows.empty:
        return pd.DataFrame()

    rows = normalise_timestamps(rows)
    rows = rows.rename(
        columns={
            "interconnectorid": "interconnector_id",
            "meteredmwflow": "metered_flow_mw",
            "mwflow": "flow_mw",
            "mwlosses": "losses_mw",
            "marginalvalue": "marginal_value",
            "exportlimit": "export_limit_mw",
            "importlimit": "import_limit_mw",
        }
    )
    rows["intervention"] = _numeric_column(rows, "intervention")
    rows = cast_numeric_fields(
        rows,
        [
            "metered_flow_mw",
            "flow_mw",
            "losses_mw",
            "marginal_value",
            "export_limit_mw",
            "import_limit_mw",
        ],
    )
    rows = add_interval_fields(rows)
    rows["silver_loaded_datetime"] = pd.Timestamp.now("UTC").isoformat()
    rows["run_id"] = run_id
    output_columns = [
        "settlement_datetime",
        "interconnector_id",
        "intervention",
        "metered_flow_mw",
        "flow_mw",
        "losses_mw",
        "marginal_value",
        "export_limit_mw",
        "import_limit_mw",
        "trading_date",
        "silver_loaded_datetime",
        "run_id",
    ]
    return remove_duplicate_rows(
        rows[[column for column in output_columns if column in rows.columns]],
        subset=["settlement_datetime", "interconnector_id", "intervention"],
    )


def build_silver_generation_by_unit(
    bronze: pd.DataFrame,
    run_id: str,
) -> pd.DataFrame:
    """Build local Silver generation-by-unit records when source columns exist."""

    generation_column = _first_existing_column(
        bronze, ["dispatchablegeneration", "scadavalue", "generation_mw"]
    )
    if "duid" not in bronze.columns or generation_column is None:
        return pd.DataFrame()

    rows = bronze[bronze["duid"].fillna("") != ""].copy()
    if rows.empty:
        return pd.DataFrame()

    rows = normalise_timestamps(rows)
    rows = rows.rename(columns={generation_column: "generation_mw"})
    rows["generation_mw"] = pd.to_numeric(rows["generation_mw"], errors="coerce")
    rows = add_interval_fields(rows)
    rows["silver_loaded_datetime"] = pd.Timestamp.now("UTC").isoformat()
    rows["run_id"] = run_id
    output_columns = [
        "settlement_datetime",
        "duid",
        "generation_mw",
        "trading_date",
        "silver_loaded_datetime",
        "run_id",
    ]
    return remove_duplicate_rows(
        rows[[column for column in output_columns if column in rows.columns]],
        subset=["settlement_datetime", "duid"],
    )


def build_silver_predispatch_forecast(
    bronze: pd.DataFrame,
    run_id: str,
) -> pd.DataFrame:
    """Build Silver pre-dispatch regional forecast rows where available."""

    rows = _source_rows(bronze, "Predispatch")
    if rows.empty:
        return pd.DataFrame()

    rows = normalise_timestamps(rows)
    rows = _normalise_region_from_any(rows)
    price_col = _first_existing_column(rows, ["rrp", "price", "forecast_price"])
    demand_col = _first_existing_column(
        rows, ["demand", "totaldemand", "demandforecast", "forecast_demand"]
    )
    if price_col is None and demand_col is None:
        return pd.DataFrame()

    result = rows.copy()
    result["forecast_run_datetime"] = pd.to_datetime(
        result.get("file_datetime", pd.Series([pd.NA] * len(result))),
        errors="coerce",
    )
    result["forecast_settlement_datetime"] = result["settlement_datetime"]
    if price_col:
        result["forecast_price_aud_mwh"] = pd.to_numeric(
            result[price_col], errors="coerce"
        )
    if demand_col:
        result["forecast_demand_mw"] = pd.to_numeric(result[demand_col], errors="coerce")
    value_columns = [
        column
        for column in ["forecast_price_aud_mwh", "forecast_demand_mw"]
        if column in result.columns
    ]
    result = result[
        result["forecast_settlement_datetime"].notna()
        & result.get("region", pd.Series("", index=result.index)).fillna("").ne("")
        & result[value_columns].notna().any(axis=1)
    ]
    if result.empty:
        return pd.DataFrame()
    result["silver_loaded_datetime"] = pd.Timestamp.now("UTC").isoformat()
    result["run_id"] = run_id
    output_columns = [
        "forecast_run_datetime",
        "forecast_settlement_datetime",
        "region",
        "region_name",
        "forecast_price_aud_mwh",
        "forecast_demand_mw",
        "source_url",
        "source_zip_name",
        "row_hash",
        "silver_loaded_datetime",
        "run_id",
    ]
    return remove_duplicate_rows(
        result[[column for column in output_columns if column in result.columns]],
        subset=["forecast_run_datetime", "forecast_settlement_datetime", "region"],
    )


def build_silver_cumulative_price(bronze: pd.DataFrame, run_id: str) -> pd.DataFrame:
    """Build Silver trading cumulative price rows."""

    rows = _source_rows(bronze, "Trading_Cumulative_Price")
    if rows.empty:
        return pd.DataFrame()
    rows = normalise_timestamps(rows)
    rows = _normalise_region_from_any(rows)
    price_col = _first_existing_column(
        rows, ["cumulativeprice", "cumul_price", "cumulative_price", "periodcumulativeprice"]
    )
    if price_col is None:
        return pd.DataFrame()
    result = rows.copy()
    result["cumulative_price_aud_mwh"] = pd.to_numeric(result[price_col], errors="coerce")
    apc_col = _first_existing_column(result, ["apcflag", "administeredpricecap", "apc_status"])
    result["administered_price_cap_status"] = (
        result[apc_col].replace({"0": "Inactive", "1": "Active"}) if apc_col else "Unknown"
    )
    result = add_interval_fields(result)
    result["silver_loaded_datetime"] = pd.Timestamp.now("UTC").isoformat()
    result["run_id"] = run_id
    output_columns = [
        "settlement_datetime",
        "trading_date",
        "region",
        "region_name",
        "cumulative_price_aud_mwh",
        "administered_price_cap_status",
        "source_url",
        "source_zip_name",
        "row_hash",
        "silver_loaded_datetime",
        "run_id",
    ]
    return remove_duplicate_rows(
        result[[column for column in output_columns if column in result.columns]],
        subset=["settlement_datetime", "region"],
    )


def build_silver_seven_day_outlook(bronze: pd.DataFrame, run_id: str) -> pd.DataFrame:
    """Build Silver seven-day outlook rows from available regional outlook fields."""

    rows = _source_rows(bronze, "Seven_Day_Outlook")
    if rows.empty:
        rows = _source_rows(bronze, "SEVENDAYOUTLOOK")
    if rows.empty:
        return pd.DataFrame()
    rows = normalise_timestamps(rows)
    rows = _normalise_region_from_any(rows)
    metric_map = {
        "scheduled_demand_mw": ["scheduleddemand", "demand", "demand10", "maximumdemand"],
        "scheduled_capacity_mw": ["scheduledcapacity", "capacity", "availablegeneration"],
        "scheduled_reserve_mw": ["scheduledreserve", "reserve", "reserverequirement"],
        "net_interchange_mw": ["netinterchange", "interchange"],
    }
    result = rows.copy()
    for output, candidates in metric_map.items():
        column = _first_existing_column(result, candidates)
        if column:
            result[output] = pd.to_numeric(result[column], errors="coerce")
    if not any(column in result.columns for column in metric_map):
        return pd.DataFrame()
    result = add_interval_fields(result)
    result["outlook_date"] = result["settlement_datetime"].dt.date
    result["silver_loaded_datetime"] = pd.Timestamp.now("UTC").isoformat()
    result["run_id"] = run_id
    output_columns = [
        "settlement_datetime",
        "outlook_date",
        "region",
        "region_name",
        *metric_map.keys(),
        "source_url",
        "source_zip_name",
        "row_hash",
        "silver_loaded_datetime",
        "run_id",
    ]
    return remove_duplicate_rows(
        result[[column for column in output_columns if column in result.columns]],
        subset=["settlement_datetime", "region"],
    )


def build_silver_rooftop_pv(bronze: pd.DataFrame, run_id: str) -> pd.DataFrame:
    """Build Silver rooftop PV actual rows for renewable reporting."""

    rows = _source_rows(bronze, "Intermittent_Generation")
    if rows.empty:
        rows = _source_rows(bronze, "ROOFTOP")
    if rows.empty:
        return pd.DataFrame()
    rows = normalise_timestamps(rows)
    rows = _normalise_region_from_any(rows)
    pv_col = _first_existing_column(
        rows, ["power", "rooftoppv", "rooftop_pv", "measurement", "scadavalue", "actualmw"]
    )
    if pv_col is None:
        return pd.DataFrame()
    result = rows.copy()
    result["rooftop_pv_mw"] = pd.to_numeric(result[pv_col], errors="coerce")
    result = add_interval_fields(result)
    result["silver_loaded_datetime"] = pd.Timestamp.now("UTC").isoformat()
    result["run_id"] = run_id
    output_columns = [
        "settlement_datetime",
        "trading_date",
        "region",
        "region_name",
        "rooftop_pv_mw",
        "source_url",
        "source_zip_name",
        "row_hash",
        "silver_loaded_datetime",
        "run_id",
    ]
    return remove_duplicate_rows(
        result[[column for column in output_columns if column in result.columns]],
        subset=["settlement_datetime", "region"],
    )


def build_current_snapshot(df: pd.DataFrame) -> pd.DataFrame:
    """Return the latest interval per region for snapshot KPI cards."""

    if df.empty or "settlement_datetime" not in df.columns:
        return df.head(0)
    return (
        df.sort_values("settlement_datetime").groupby("region", as_index=False).tail(1)
    )


def build_supply_demand_components(snapshot: pd.DataFrame) -> pd.DataFrame:
    """Build current dashboard demand/generation components for stacked bars."""

    columns = [
        "settlement_datetime",
        "trading_date",
        "region",
        "region_name",
        "demand_mw",
        "cleared_supply_mw",
        "scheduled_generation_mw",
        "semi_scheduled_generation_mw",
        "gold_loaded_datetime",
        "run_id",
    ]
    if snapshot.empty:
        return pd.DataFrame(columns=columns + ["metric_group", "component", "value_mw"])

    available = snapshot[[column for column in columns if column in snapshot.columns]].copy()
    if "cleared_supply_mw" in available.columns and "demand_mw" in available.columns:
        available["demand_component_mw"] = pd.to_numeric(
            available["cleared_supply_mw"], errors="coerce"
        ).fillna(pd.to_numeric(available["demand_mw"], errors="coerce"))
    elif "cleared_supply_mw" in available.columns:
        available["demand_component_mw"] = pd.to_numeric(
            available["cleared_supply_mw"], errors="coerce"
        )
    elif "demand_mw" in available.columns:
        available["demand_component_mw"] = pd.to_numeric(
            available["demand_mw"], errors="coerce"
        )
    rows: list[pd.DataFrame] = []
    component_specs = [
        ("Demand", "Demand", "demand_component_mw", 1),
        ("Generation", "Scheduled Generation", "scheduled_generation_mw", 1),
        ("Generation", "Semi-scheduled Generation", "semi_scheduled_generation_mw", 2),
    ]
    id_columns = [
        column
        for column in available.columns
        if column
        not in {
            "cleared_supply_mw",
            "demand_mw",
            "demand_component_mw",
            "scheduled_generation_mw",
            "semi_scheduled_generation_mw",
        }
    ]
    for metric_group, component, value_column, sort_order in component_specs:
        if value_column not in available.columns:
            continue
        component_frame = available[id_columns].copy()
        component_frame["metric_group"] = metric_group
        component_frame["component"] = component
        component_frame["component_sort_order"] = sort_order
        component_frame["value_mw"] = pd.to_numeric(
            available[value_column], errors="coerce"
        )
        rows.append(component_frame)
    if not rows:
        return pd.DataFrame(columns=columns + ["metric_group", "component", "value_mw"])
    return pd.concat(rows, ignore_index=True)


def build_gold_region_5min(silver: pd.DataFrame, run_id: str) -> pd.DataFrame:
    """Build the local Gold 5-minute regional fact from Silver price/demand."""

    if silver.empty:
        return pd.DataFrame()

    result = silver.copy()
    result["settlement_datetime"] = pd.to_datetime(
        result["settlement_datetime"], errors="coerce"
    )
    result = cast_numeric_fields(
        result,
        [
            "intervention",
            "price_aud_mwh",
            "demand_mw",
            "available_generation_mw",
            "available_load_mw",
            "demand_forecast_mw",
            "dispatchable_generation_mw",
            "dispatchable_load_mw",
            "net_interchange_mw",
            "excess_generation_mw",
            "cleared_supply_mw",
            "semi_scheduled_generation_mw",
            "scheduled_generation_mw",
        ],
    )
    result = add_interval_fields(result)
    result["price_band"] = pd.cut(
        result["price_aud_mwh"],
        bins=[float("-inf"), 0, 300, 1000, float("inf")],
        labels=["Negative", "Normal", "High", "Extreme"],
    ).astype(str)
    result["is_negative_price"] = result["price_aud_mwh"] < 0
    result["is_high_price"] = (
        result["price_aud_mwh"] >= PRICE_THRESHOLDS["high_price_threshold_aud_mwh"]
    )
    result["is_extreme_price"] = (
        result["price_aud_mwh"] >= PRICE_THRESHOLDS["extreme_price_threshold_aud_mwh"]
    )
    result = result.sort_values(["region", "settlement_datetime"])
    indexed = result.set_index("settlement_datetime")
    price_rolling = indexed.groupby("region", dropna=False)["price_aud_mwh"].rolling(
        "3600s"
    )
    demand_rolling = indexed.groupby("region", dropna=False)["demand_mw"].rolling(
        "3600s"
    )
    result["rolling_avg_price_1h"] = (
        price_rolling.mean().reset_index(level=0, drop=True).to_numpy()
    )
    result["rolling_avg_demand_1h"] = (
        demand_rolling.mean().reset_index(level=0, drop=True).to_numpy()
    )
    result["gold_loaded_datetime"] = pd.Timestamp.now("UTC").isoformat()
    result["run_id"] = run_id

    output_columns = [
        "settlement_datetime",
        "trading_date",
        "year",
        "month",
        "day",
        "interval_hour",
        "interval_minute",
        "region",
        "region_name",
        "intervention",
        "price_aud_mwh",
        "demand_mw",
        "available_generation_mw",
        "available_load_mw",
        "demand_forecast_mw",
        "dispatchable_generation_mw",
        "dispatchable_load_mw",
        "net_interchange_mw",
        "excess_generation_mw",
        "cleared_supply_mw",
        "semi_scheduled_generation_mw",
        "scheduled_generation_mw",
        "price_band",
        "is_negative_price",
        "is_high_price",
        "is_extreme_price",
        "rolling_avg_price_1h",
        "rolling_avg_demand_1h",
        "gold_loaded_datetime",
        "run_id",
    ]
    return result[[column for column in output_columns if column in result.columns]]


def build_30min_region_aggregation(df: pd.DataFrame) -> pd.DataFrame:
    """Aggregate 5-minute regional records to 30-minute regional grain."""

    if df.empty:
        return df
    result = df.copy()
    result["settlement_30min"] = result["settlement_datetime"].dt.floor("30min")
    aggregated = (
        result.groupby(["region", "region_name", "settlement_30min"], dropna=False)
        .agg(
            price_aud_mwh=("price_aud_mwh", "mean"),
            demand_mw=("demand_mw", "mean"),
            max_price_aud_mwh=("price_aud_mwh", "max"),
            min_price_aud_mwh=("price_aud_mwh", "min"),
            high_price_interval_count=("is_high_price", "sum"),
            negative_price_interval_count=("is_negative_price", "sum"),
        )
        .reset_index()
    )
    aggregated["trading_date"] = aggregated["settlement_30min"].dt.date
    aggregated["interval_hour"] = aggregated["settlement_30min"].dt.hour
    aggregated["interval_minute"] = aggregated["settlement_30min"].dt.minute
    return aggregated


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
            daily_price_volatility=("price_aud_mwh", "std"),
            daily_avg_demand=("demand_mw", "mean"),
            daily_max_demand=("demand_mw", "max"),
            high_price_interval_count=("is_high_price", "sum"),
            extreme_price_interval_count=("is_extreme_price", "sum"),
            negative_price_interval_count=("is_negative_price", "sum"),
        )
        .reset_index()
    )


def build_price_spikes(df: pd.DataFrame) -> pd.DataFrame:
    """Return intervals flagged as high, extreme, or negative price events."""

    if "is_high_price" not in df.columns:
        return df.head(0)
    return df[df["is_high_price"] | df["is_negative_price"]].copy()


def build_kpis(snapshot: pd.DataFrame, run_id: str) -> pd.DataFrame:
    """Build single-row dashboard KPI values from the current snapshot."""

    loaded_datetime = pd.Timestamp.now("UTC").isoformat()
    if snapshot.empty:
        return pd.DataFrame(
            [
                {
                    "latest_settlement_datetime": pd.NaT,
                    "avg_current_price_aud_mwh": pd.NA,
                    "current_total_demand_mw": pd.NA,
                    "regions_available": 0,
                    "run_id": run_id,
                    "gold_loaded_datetime": loaded_datetime,
                }
            ]
        )
    return pd.DataFrame(
        [
            {
                "latest_settlement_datetime": snapshot["settlement_datetime"].max(),
                "avg_current_price_aud_mwh": snapshot["price_aud_mwh"].mean(),
                "current_total_demand_mw": snapshot["demand_mw"].sum(),
                "regions_available": snapshot["region"].nunique(),
                "run_id": run_id,
                "gold_loaded_datetime": loaded_datetime,
            }
        ]
    )


def build_data_freshness(kpis: pd.DataFrame) -> pd.DataFrame:
    """Build Gold operational freshness status from dashboard KPIs."""

    if kpis.empty:
        return pd.DataFrame()
    result = kpis[
        ["latest_settlement_datetime", "gold_loaded_datetime", "run_id"]
    ].copy()
    latest = pd.to_datetime(
        result["latest_settlement_datetime"], errors="coerce", utc=True
    )
    loaded = pd.to_datetime(result["gold_loaded_datetime"], errors="coerce", utc=True)
    result["freshness_minutes"] = (loaded - latest).dt.total_seconds() / 60.0
    result["status"] = pd.cut(
        result["freshness_minutes"],
        bins=[float("-inf"), 15, 60, float("inf")],
        labels=["Fresh", "Delayed", "Stale"],
    ).astype(str)
    return result.rename(
        columns={"gold_loaded_datetime": "last_successful_ingestion_datetime"}
    )


def build_gold_interconnector_flows(interconnector: pd.DataFrame) -> pd.DataFrame:
    """Build optional local Gold interconnector flow records from Silver data."""

    if interconnector.empty:
        return pd.DataFrame()
    result = interconnector.copy()
    result["settlement_datetime"] = pd.to_datetime(
        result["settlement_datetime"], errors="coerce"
    )
    result = cast_numeric_fields(
        result, ["flow_mw", "export_limit_mw", "import_limit_mw"]
    )
    result["flow_direction"] = "Reverse"
    result.loc[
        result["flow_mw"].notna() & (result["flow_mw"] >= 0), "flow_direction"
    ] = "Forward"
    result["interval_hour"] = result["settlement_datetime"].dt.hour
    result["interval_minute"] = result["settlement_datetime"].dt.minute
    return result


def build_gold_predispatch_forecast(silver: pd.DataFrame, run_id: str) -> pd.DataFrame:
    """Build Power BI-ready pre-dispatch forecast rows."""

    if silver.empty:
        return pd.DataFrame()
    result = silver.copy()
    result["forecast_run_datetime"] = pd.to_datetime(
        result["forecast_run_datetime"], errors="coerce", utc=True
    )
    result["forecast_settlement_datetime"] = pd.to_datetime(
        result["forecast_settlement_datetime"], errors="coerce", utc=True
    )
    result["forecast_horizon_minutes"] = (
        result["forecast_settlement_datetime"] - result["forecast_run_datetime"]
    ).dt.total_seconds() / 60.0
    result["trading_date"] = result["forecast_settlement_datetime"].dt.date
    result["interval_hour"] = result["forecast_settlement_datetime"].dt.hour
    result["interval_minute"] = result["forecast_settlement_datetime"].dt.minute
    result["gold_loaded_datetime"] = pd.Timestamp.now("UTC").isoformat()
    result["run_id"] = run_id
    return result


def build_gold_cumulative_price(silver: pd.DataFrame, run_id: str) -> pd.DataFrame:
    """Build Power BI-ready cumulative price and APC status rows."""

    if silver.empty:
        return pd.DataFrame()
    result = silver.copy()
    result["settlement_datetime"] = pd.to_datetime(
        result["settlement_datetime"], errors="coerce"
    )
    result["cumulative_price_aud_mwh"] = pd.to_numeric(
        result["cumulative_price_aud_mwh"], errors="coerce"
    )
    result = add_interval_fields(result)
    result["gold_loaded_datetime"] = pd.Timestamp.now("UTC").isoformat()
    result["run_id"] = run_id
    return result


def build_gold_seven_day_outlook(silver: pd.DataFrame, run_id: str) -> pd.DataFrame:
    """Build Power BI-ready seven-day outlook rows."""

    if silver.empty:
        return pd.DataFrame()
    result = silver.copy()
    result["settlement_datetime"] = pd.to_datetime(
        result["settlement_datetime"], errors="coerce"
    )
    result = cast_numeric_fields(
        result,
        [
            "scheduled_demand_mw",
            "scheduled_capacity_mw",
            "scheduled_reserve_mw",
            "net_interchange_mw",
        ],
    )
    result = add_interval_fields(result)
    result["gold_loaded_datetime"] = pd.Timestamp.now("UTC").isoformat()
    result["run_id"] = run_id
    return result


def build_gold_generation_mix(
    generation_by_unit: pd.DataFrame,
    rooftop_pv: pd.DataFrame | None,
    run_id: str,
) -> pd.DataFrame:
    """Build a conservative generation mix table from available generation sources."""

    frames: list[pd.DataFrame] = []
    if not generation_by_unit.empty:
        unit = generation_by_unit.copy()
        unit["settlement_datetime"] = pd.to_datetime(
            unit["settlement_datetime"], errors="coerce"
        )
        unit["generation_mw"] = pd.to_numeric(unit["generation_mw"], errors="coerce")
        unit["fuel_type"] = "Unmapped"
        unit["is_renewable"] = False
        frames.append(
            unit[["settlement_datetime", "fuel_type", "is_renewable", "generation_mw"]]
        )
    if rooftop_pv is not None and not rooftop_pv.empty:
        pv = rooftop_pv.copy()
        pv["settlement_datetime"] = pd.to_datetime(
            pv["settlement_datetime"], errors="coerce"
        )
        pv["generation_mw"] = pd.to_numeric(pv["rooftop_pv_mw"], errors="coerce")
        pv["fuel_type"] = "Solar"
        pv["is_renewable"] = True
        frames.append(
            pv[["settlement_datetime", "fuel_type", "is_renewable", "generation_mw"]]
        )
    if not frames:
        return pd.DataFrame()
    result = pd.concat(frames, ignore_index=True)
    result = (
        result.groupby(["settlement_datetime", "fuel_type", "is_renewable"], dropna=False)
        .agg(generation_mw=("generation_mw", "sum"))
        .reset_index()
    )
    result = add_interval_fields(result)
    result["renewable_generation_mw"] = result["generation_mw"].where(
        result["is_renewable"], 0.0
    )
    result["gold_loaded_datetime"] = pd.Timestamp.now("UTC").isoformat()
    result["run_id"] = run_id
    return result


def build_gold_renewable_penetration(
    generation_mix: pd.DataFrame,
    run_id: str,
) -> pd.DataFrame:
    """Build renewable penetration by interval from generation mix."""

    if generation_mix.empty:
        return pd.DataFrame()
    result = (
        generation_mix.groupby(["settlement_datetime", "trading_date"], dropna=False)
        .agg(
            renewable_generation_mw=("renewable_generation_mw", "sum"),
            total_generation_mw=("generation_mw", "sum"),
        )
        .reset_index()
    )
    result["renewable_penetration_pct"] = (
        result["renewable_generation_mw"] / result["total_generation_mw"] * 100.0
    )
    result["gold_loaded_datetime"] = pd.Timestamp.now("UTC").isoformat()
    result["run_id"] = run_id
    return result
