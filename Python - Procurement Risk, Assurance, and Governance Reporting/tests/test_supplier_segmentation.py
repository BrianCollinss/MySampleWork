from __future__ import annotations

import pandas as pd

from src.metrics.supplier_segmentation import build_supplier_segmentation


def test_build_supplier_segmentation_returns_supplier_and_cluster_outputs() -> None:
    """Supplier segmentation should produce cluster labels and summary outputs."""

    contracts = pd.DataFrame(
        {
            "supplier_name": [
                "Supplier A",
                "Supplier A",
                "Supplier B",
                "Supplier B",
                "Supplier C",
                "Supplier D",
            ],
            "contract_id": ["A1", "A2", "B1", "B2", "C1", "D1"],
            "contract_value": [900000.0, 850000.0, 45000.0, 50000.0, 300000.0, 15000.0],
            "reporting_period": ["2025-01", "2025-02", "2025-01", "2025-02", "2025-03", "2025-03"],
            "procurement_method": ["Open", "Open", "Limited", "Limited", "Direct", "Open"],
            "contract_start_date": pd.to_datetime(["2025-01-01", "2025-02-01", "2025-01-15", "2025-02-15", "2025-03-01", "2025-03-10"]),
            "contract_end_date": pd.to_datetime(["2025-06-30", "2025-07-31", "2025-01-31", "2025-02-28", "2025-04-30", None]),
            "procurement_category": ["ICT", "ICT", "Services", "Services", "Construction", None],
            "data_quality_flags": [
                [],
                ["unusually_large_contract_value"],
                [],
                ["missing_supplier_name"],
                ["possible_duplicate_contract"],
                [],
            ],
        }
    )
    config = {
        "advanced_analytics": {
            "supplier_segmentation": {
                "enabled": True,
                "cluster_count": 3,
                "random_state": 7,
                "max_iter": 50,
            }
        }
    }

    result = build_supplier_segmentation(contracts, config)

    assert not result.supplier_segments.empty
    assert not result.cluster_summary.empty
    assert "segment_label" in result.supplier_segments.columns
    assert "segment_description" in result.supplier_segments.columns
    assert result.supplier_segments["cluster_id"].nunique() <= 3
    assert set(result.cluster_summary["segment_label"]).issubset(
        {
            "high-value strategic",
            "fragmented low-value repeat",
            "sporadic high-risk",
            "low-information vendor",
            "mixed profile",
        }
    )
