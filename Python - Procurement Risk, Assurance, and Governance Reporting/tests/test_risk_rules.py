from __future__ import annotations

import pandas as pd

from src.clean.validate_contracts import build_data_quality_outputs, detect_possible_duplicates
from src.metrics.procurement_metrics import build_metric_pack
from src.metrics.risk_rules import evaluate_risk_rules


def _contracts() -> pd.DataFrame:
    """Create a fixture with repeat small contracts and concentration signals."""
    return pd.DataFrame(
        {
            "source_file": ["demo.csv", "demo.csv", "demo.csv", "other.csv"],
            "source_folder": ["Folder A", "Folder A", "Folder A", "Folder A"],
            "source_agency": ["Dept A", "Dept A", "Dept A", "Dept B"],
            "contract_id": ["1", "2", "3", "4"],
            "supplier_name": ["Supplier X", "Supplier X", "Supplier X", "Supplier Y"],
            "supplier_abn": ["1", "1", "1", "2"],
            "contract_title": ["Alpha", "Beta", "Gamma", "Delta"],
            "procurement_category": ["ICT", "ICT", "ICT", "Consulting"],
            "procurement_method": ["Limited", "Limited", "Limited", "Open"],
            "contract_start_date": pd.to_datetime(["2025-01-01", "2025-01-20", "2025-02-10", "2025-02-01"]),
            "contract_end_date": pd.to_datetime(["2025-03-01", "2025-03-15", "2025-04-10", "2025-08-01"]),
            "contract_value": [97000.0, 98000.0, 99000.0, 500000.0],
            "reporting_period": ["2025-01", "2025-01", "2025-02", "2025-02"],
            "publish_date": pd.to_datetime(["2025-01-03", "2025-01-25", "2025-02-11", "2025-02-05"]),
            "data_quality_flags": [[], [], [], []],
            "raw_metadata": ["{}", "{}", "{}", "{}"],
        }
    )


def test_detect_possible_duplicates_flags_matching_business_keys() -> None:
    """Duplicate heuristic should flag repeated business-key combinations."""
    frame = _contracts().copy()
    frame.loc[1, "contract_title"] = "Alpha"
    frame.loc[1, "contract_value"] = 97000.0
    frame.loc[1, "contract_start_date"] = pd.Timestamp("2025-01-01")
    duplicates = detect_possible_duplicates(frame)
    assert duplicates.iloc[0]
    assert duplicates.iloc[1]


def test_evaluate_risk_rules_returns_expected_exception_categories() -> None:
    """Risk rules should emit concentration, financial, and process exceptions."""
    contracts = _contracts()
    config = {
        "critical_fields": ["source_agency", "contract_id", "supplier_name", "contract_value", "contract_start_date"],
        "risk_rules": {
            "supplier_concentration_threshold": 0.45,
            "concentration_amber_threshold": 0.35,
            "repeated_small_contract_value_threshold": 100000,
            "repeated_small_contract_count_threshold": 3,
            "repeated_small_contract_window_days": 90,
            "abrupt_spend_growth_threshold": 0.2,
            "spend_growth_amber_threshold": 0.1,
            "near_threshold_band_lower": 95000,
            "near_threshold_band_upper": 100000,
            "missing_critical_fields_threshold": 0.1,
            "high_null_rate_threshold": 0.2,
            "unusually_large_value_quantile": 0.99,
            "unusually_small_positive_value_quantile": 0.05,
        },
        "reporting": {"top_n_suppliers": 10},
    }
    metric_pack = build_metric_pack(contracts, config)
    file_log = pd.DataFrame(
        {
            "source_file": ["demo.csv", "other.csv"],
            "row_count": [3, 1],
            "missing_critical_fields": ["", ""],
        }
    )
    validation_outputs = build_data_quality_outputs(contracts, file_log, config)
    exceptions = evaluate_risk_rules(contracts, metric_pack, validation_outputs, config)

    assert not exceptions.empty
    assert "concentration_risk" in exceptions["category"].values
    assert "financial_risk" in exceptions["category"].values
    assert "process_risk" in exceptions["category"].values
