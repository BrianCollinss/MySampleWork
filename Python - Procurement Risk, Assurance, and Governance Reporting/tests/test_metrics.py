from __future__ import annotations

import pandas as pd

from src.metrics.procurement_metrics import build_metric_pack, calculate_hhi


def _sample_contracts() -> pd.DataFrame:
    """Create a compact fixture with enough variation for metric testing."""
    return pd.DataFrame(
        {
            "source_file": ["a.csv", "a.csv", "b.csv"],
            "source_folder": ["Folder A", "Folder A", "Folder A"],
            "source_agency": ["Dept A", "Dept A", "Dept B"],
            "contract_id": ["1", "2", "3"],
            "supplier_name": ["Supplier X", "Supplier X", "Supplier Y"],
            "supplier_abn": ["1", "1", "2"],
            "contract_title": ["Alpha", "Beta", "Gamma"],
            "procurement_category": ["ICT", "ICT", "Consulting"],
            "procurement_method": ["Open", "Limited", "Open"],
            "contract_start_date": pd.to_datetime(["2025-01-01", "2025-01-15", "2025-02-01"]),
            "contract_end_date": pd.to_datetime(["2025-06-01", "2025-03-30", "2025-07-31"]),
            "contract_value": [100.0, 300.0, 600.0],
            "reporting_period": ["2025-01", "2025-01", "2025-02"],
            "publish_date": pd.to_datetime(["2025-01-05", "2025-01-17", "2025-02-05"]),
            "data_quality_flags": [[], [], []],
            "raw_metadata": ["{}", "{}", "{}"],
        }
    )


def test_calculate_hhi_returns_expected_value() -> None:
    """HHI should equal 5000 when two suppliers split spend evenly."""
    values = pd.Series([50.0, 50.0])
    assert calculate_hhi(values) == 5000.0


def test_build_metric_pack_monthly_spend_and_top_suppliers() -> None:
    """Metric pack should produce expected period totals and supplier rankings."""
    metric_pack = build_metric_pack(_sample_contracts(), {"reporting": {"top_n_suppliers": 5}})

    january_value = metric_pack.monthly_spend.loc[
        metric_pack.monthly_spend["reporting_period"] == "2025-01", "total_contract_value"
    ].iloc[0]
    top_supplier_by_value = metric_pack.top_suppliers_by_value.iloc[0]["supplier_name"]
    top_supplier_by_count = metric_pack.top_suppliers_by_count.iloc[0]["supplier_name"]

    assert january_value == 400.0
    assert top_supplier_by_value == "Supplier Y"
    assert top_supplier_by_count == "Supplier X"


def test_build_metric_pack_supplier_concentration_decomposition_tracks_contributors() -> None:
    """Concentration decomposition should show which suppliers drove share shifts."""

    metric_pack = build_metric_pack(_sample_contracts(), {"reporting": {"top_n_suppliers": 2}})
    decomposition = metric_pack.supplier_concentration_decomposition

    supplier_x_row = decomposition.loc[
        (decomposition["reporting_period"] == "2025-02") & (decomposition["supplier_name"] == "Supplier X")
    ].iloc[0]
    supplier_y_row = decomposition.loc[
        (decomposition["reporting_period"] == "2025-02") & (decomposition["supplier_name"] == "Supplier Y")
    ].iloc[0]

    assert supplier_x_row["prior_reporting_period"] == "2025-01"
    assert supplier_x_row["concentration_change_contribution"] == -1.0
    assert supplier_y_row["concentration_change_contribution"] == 1.0
