from __future__ import annotations

import pandas as pd

from nem_fabric.common_transformations import (
    build_30min_region_aggregation,
    build_dashboard_kpis,
    build_data_freshness,
    build_gold_region_5min,
    build_silver_interconnector_flows,
    build_silver_price_demand,
    normalise_timestamps,
    standardise_region_names,
)


def test_normalise_timestamps_accepts_uppercase_aemo_column() -> None:
    """AEMO source columns may arrive in original uppercase form."""

    df = pd.DataFrame({"SETTLEMENTDATE": ["2026/01/01 00:05:00"]})

    result = normalise_timestamps(df)

    assert result["settlement_datetime"].iloc[0] == pd.Timestamp("2026-01-01 00:05:00")


def test_standardise_region_names_accepts_uppercase_aemo_column() -> None:
    """Region normalisation should not depend on source header casing."""

    df = pd.DataFrame({"REGIONID": ["nsw1"]})

    result = standardise_region_names(df)

    assert result["region"].iloc[0] == "NSW1"
    assert result["region_name"].iloc[0] == "New South Wales"


def test_build_silver_price_demand_joins_price_and_regionsum() -> None:
    """Local Silver price/demand should mirror the Spark join grain."""

    bronze = pd.DataFrame(
        [
            {
                "package_name": "DISPATCH",
                "table_name": "PRICE",
                "settlementdate": "2026/01/01 00:05:00",
                "regionid": "NSW1",
                "intervention": "0",
                "rrp": "100.50",
                "source_url": "https://example.test/price.zip",
                "source_zip_name": "price.zip",
                "row_hash": "price-hash",
            },
            {
                "package_name": "DISPATCH",
                "table_name": "REGIONSUM",
                "settlementdate": "2026/01/01 00:05:00",
                "regionid": "NSW1",
                "intervention": "0",
                "totaldemand": "8000",
                "row_hash": "regionsum-hash",
            },
        ]
    )

    result = build_silver_price_demand(bronze, run_id="run-1")

    assert len(result) == 1
    assert result["price_aud_mwh"].iloc[0] == 100.50
    assert result["demand_mw"].iloc[0] == 8000
    assert result["region_name"].iloc[0] == "New South Wales"


def test_build_silver_interconnector_flows_casts_numeric_fields() -> None:
    """Local Silver interconnector output should use typed numeric columns."""

    bronze = pd.DataFrame(
        [
            {
                "package_name": "DISPATCH",
                "table_name": "INTERCONNECTORRES",
                "settlementdate": "2026/01/01 00:05:00",
                "interconnectorid": "N-Q-MNSP1",
                "intervention": "0",
                "meteredmwflow": "12.5",
                "mwflow": "11.5",
                "mwlosses": "1.0",
                "marginalvalue": "2.0",
            }
        ]
    )

    result = build_silver_interconnector_flows(bronze, run_id="run-1")

    assert result["interconnector_id"].iloc[0] == "N-Q-MNSP1"
    assert result["flow_mw"].iloc[0] == 11.5


def test_build_gold_region_5min_adds_power_bi_fields() -> None:
    """Local Gold 5-minute output should include report-ready flags and rolling values."""

    silver = pd.DataFrame(
        [
            {
                "settlement_datetime": "2026-01-01 00:05:00",
                "region": "NSW1",
                "region_name": "New South Wales",
                "intervention": 0,
                "price_aud_mwh": 100.0,
                "demand_mw": 8000.0,
            },
            {
                "settlement_datetime": "2026-01-01 00:10:00",
                "region": "NSW1",
                "region_name": "New South Wales",
                "intervention": 0,
                "price_aud_mwh": 1200.0,
                "demand_mw": 8200.0,
            },
        ]
    )

    result = build_gold_region_5min(silver, run_id="run-1")

    assert result["price_band"].iloc[1] == "Extreme"
    assert result["is_high_price"].iloc[1]
    assert result["is_extreme_price"].iloc[1]
    assert result["rolling_avg_price_1h"].iloc[1] == 650.0
    assert result["trading_date"].iloc[0] == pd.Timestamp("2026-01-01").date()


def test_build_gold_aggregates_and_freshness() -> None:
    """Local Gold aggregate helpers should produce notebook-equivalent tables."""

    gold = pd.DataFrame(
        [
            {
                "settlement_datetime": pd.Timestamp("2026-01-01 00:05:00"),
                "trading_date": pd.Timestamp("2026-01-01").date(),
                "region": "NSW1",
                "region_name": "New South Wales",
                "price_aud_mwh": 100.0,
                "demand_mw": 8000.0,
                "is_high_price": False,
                "is_extreme_price": False,
                "is_negative_price": False,
            },
            {
                "settlement_datetime": pd.Timestamp("2026-01-01 00:10:00"),
                "trading_date": pd.Timestamp("2026-01-01").date(),
                "region": "NSW1",
                "region_name": "New South Wales",
                "price_aud_mwh": 400.0,
                "demand_mw": 8200.0,
                "is_high_price": True,
                "is_extreme_price": False,
                "is_negative_price": False,
            },
        ]
    )

    aggregate = build_30min_region_aggregation(gold)
    kpis = build_dashboard_kpis(gold.tail(1), run_id="run-1")
    freshness = build_data_freshness(kpis)

    assert aggregate["high_price_interval_count"].iloc[0] == 1
    assert kpis["regions_available"].iloc[0] == 1
    assert freshness["status"].iloc[0] in {"Fresh", "Delayed", "Stale"}
