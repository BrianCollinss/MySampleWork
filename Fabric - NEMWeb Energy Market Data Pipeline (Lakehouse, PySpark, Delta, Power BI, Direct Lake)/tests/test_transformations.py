from __future__ import annotations

import pandas as pd
import pytest

from nem_fabric.common_transformations import (
    build_30min_region_aggregation,
    build_kpis,
    build_supply_demand_components,
    build_data_freshness,
    build_gold_generation_mix,
    build_gold_predispatch_forecast,
    build_gold_renewable_penetration,
    build_gold_region_5min,
    build_silver_cumulative_price,
    build_silver_generation_by_unit,
    build_silver_interconnector_flows,
    build_silver_predispatch_forecast,
    build_silver_price_demand,
    build_silver_rooftop_pv,
    build_silver_seven_day_outlook,
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
                "dispatchablegeneration": "7000",
                "clearedsupply": "8050",
                "semischedule_clearedmw": "1500",
                "row_hash": "regionsum-hash",
            },
        ]
    )

    result = build_silver_price_demand(bronze, run_id="run-1")

    assert len(result) == 1
    assert result["price_aud_mwh"].iloc[0] == 100.50
    assert result["demand_mw"].iloc[0] == 8000
    assert result["region_name"].iloc[0] == "New South Wales"
    assert result["cleared_supply_mw"].iloc[0] == 8050
    assert result["semi_scheduled_generation_mw"].iloc[0] == 1500
    assert result["scheduled_generation_mw"].iloc[0] == 5500
    assert result["dispatchable_generation_mw"].iloc[0] == 7000


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
                "exportlimit": "100.0",
                "importlimit": "-90.0",
            }
        ]
    )

    result = build_silver_interconnector_flows(bronze, run_id="run-1")

    assert result["interconnector_id"].iloc[0] == "N-Q-MNSP1"
    assert result["flow_mw"].iloc[0] == 11.5
    assert result["export_limit_mw"].iloc[0] == 100.0
    assert result["import_limit_mw"].iloc[0] == -90.0


def test_new_source_silver_builders_process_forecast_and_outlook_rows() -> None:
    """New enabled sources should have explicit Silver transformation paths."""

    bronze = pd.DataFrame(
        [
            {
                "source_folder": "PredispatchIS_Reports",
                "settlementdate": "2026/01/01 00:30:00",
                "file_datetime": "2026-01-01T00:00:00+00:00",
                "regionid": "NSW1",
                "rrp": "80",
                "demandforecast": "7000",
            },
            {
                "source_folder": "PredispatchIS_Reports",
                "file_datetime": "2026-01-01T00:00:00+00:00",
                "rrp": "",
                "demandforecast": "",
            },
            {
                "source_folder": "Trading_Cumulative_Price",
                "settlementdate": "2026/01/01 00:05:00",
                "regionid": "NSW1",
                "cumulativeprice": "120",
                "apcflag": "0",
            },
            {
                "source_folder": "Seven_Day_Outlook_Full",
                "settlementdate": "2026/01/02 18:00:00",
                "regionid": "NSW1",
                "scheduleddemand": "9000",
                "scheduledcapacity": "11000",
                "scheduledreserve": "2000",
                "netinterchange": "-500",
            },
            {
                "source_folder": "Intermittent_Generation",
                "settlementdate": "2026/01/01 00:30:00",
                "regionid": "NSW1",
                "power": "450",
            },
        ]
    ).fillna("")

    forecast = build_silver_predispatch_forecast(bronze, run_id="run-1")
    cumulative = build_silver_cumulative_price(bronze, run_id="run-1")
    outlook = build_silver_seven_day_outlook(bronze, run_id="run-1")
    rooftop = build_silver_rooftop_pv(bronze, run_id="run-1")

    assert forecast["forecast_price_aud_mwh"].iloc[0] == 80
    assert len(forecast) == 1
    assert cumulative["cumulative_price_aud_mwh"].iloc[0] == 120
    assert outlook["scheduled_reserve_mw"].iloc[0] == 2000
    assert rooftop["rooftop_pv_mw"].iloc[0] == 450


def test_generation_mix_and_renewable_penetration_use_scada_and_rooftop() -> None:
    """SCADA and rooftop PV should flow to Gold generation and renewable tables."""

    bronze = pd.DataFrame(
        [
            {
                "source_folder": "Dispatch_SCADA",
                "settlementdate": "2026/01/01 00:05:00",
                "duid": "UNIT1",
                "scadavalue": "100",
            }
        ]
    )
    generation = build_silver_generation_by_unit(bronze, run_id="run-1")
    rooftop = pd.DataFrame(
        [
            {
                "settlement_datetime": "2026-01-01 00:05:00",
                "trading_date": "2026-01-01",
                "region": "NSW1",
                "rooftop_pv_mw": 50.0,
            }
        ]
    )

    mix = build_gold_generation_mix(generation, rooftop, run_id="run-1")
    penetration = build_gold_renewable_penetration(mix, run_id="run-1")

    assert set(mix["fuel_type"]) == {"Unmapped", "Solar"}
    assert penetration["renewable_penetration_pct"].iloc[0] == pytest.approx(100 / 3)


def test_gold_predispatch_forecast_handles_mixed_timezone_inputs() -> None:
    """Forecast horizon calculation should tolerate mixed timestamp strings."""

    silver = pd.DataFrame(
        [
            {
                "forecast_run_datetime": "2026-01-01T00:00:00+00:00",
                "forecast_settlement_datetime": "2026-01-01 00:30:00",
                "region": "NSW1",
                "forecast_price_aud_mwh": 80.0,
            }
        ]
    )

    result = build_gold_predispatch_forecast(silver, run_id="run-1")

    assert result["forecast_horizon_minutes"].iloc[0] == 30.0


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
    kpis = build_kpis(gold.tail(1), run_id="run-1")
    freshness = build_data_freshness(kpis)

    assert aggregate["high_price_interval_count"].iloc[0] == 1
    assert kpis["regions_available"].iloc[0] == 1
    assert freshness["status"].iloc[0] in {"Fresh", "Delayed", "Stale"}


def test_build_supply_demand_components_returns_long_format() -> None:
    """Current snapshot should reshape into demand and generation components."""

    snapshot = pd.DataFrame(
        [
            {
                "settlement_datetime": pd.Timestamp("2026-01-01 00:05:00"),
                "trading_date": pd.Timestamp("2026-01-01").date(),
                "region": "NSW1",
                "region_name": "New South Wales",
                "demand_mw": 8000.0,
                "cleared_supply_mw": 8050.0,
                "scheduled_generation_mw": 5500.0,
                "semi_scheduled_generation_mw": 1500.0,
                "gold_loaded_datetime": "2026-01-01T00:06:00+00:00",
                "run_id": "run-1",
            }
        ]
    )

    result = build_supply_demand_components(snapshot)

    assert set(result["metric_group"]) == {"Demand", "Generation"}
    assert set(result["component"]) == {
        "Demand",
        "Scheduled Generation",
        "Semi-scheduled Generation",
    }
    assert result["value_mw"].sum() == 15050.0


def test_build_supply_demand_components_falls_back_to_total_demand() -> None:
    """Demand bars should not blank when cleared supply is absent for a region."""

    snapshot = pd.DataFrame(
        [
            {
                "settlement_datetime": pd.Timestamp("2026-05-03 23:40:00"),
                "trading_date": pd.Timestamp("2026-05-03").date(),
                "region": "SA1",
                "region_name": "South Australia",
                "demand_mw": 1466.98,
                "cleared_supply_mw": pd.NA,
                "scheduled_generation_mw": 114.99918,
                "semi_scheduled_generation_mw": 1759.88082,
                "gold_loaded_datetime": "2026-05-03T23:41:00+00:00",
                "run_id": "run-1",
            }
        ]
    )

    result = build_supply_demand_components(snapshot)
    demand = result[result["component"] == "Demand"]

    assert demand["value_mw"].iloc[0] == 1466.98
