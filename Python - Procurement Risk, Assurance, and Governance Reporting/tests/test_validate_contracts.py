from __future__ import annotations

import pandas as pd
import pytest

import src.clean.validate_contracts as validate_contracts
from src.clean.validate_contracts import _issue_rows, build_data_quality_outputs, detect_possible_duplicates


def test_detect_possible_duplicates_flags_matching_business_keys() -> None:
    """Duplicate detection should flag rows that share the same business key."""

    contracts = pd.DataFrame(
        {
            "source_agency": ["Agency A", "Agency A", "Agency B"],
            "supplier_name": ["Supplier One", "Supplier One", "Supplier Two"],
            "contract_title": ["Support Services", "Support Services", "Advisory Services"],
            "contract_value": [100000.0, 100000.0, 50000.0],
            "contract_start_date": pd.to_datetime(["2025-01-10", "2025-01-10", "2025-02-15"]),
        }
    )

    duplicate_mask = detect_possible_duplicates(contracts)

    assert duplicate_mask.tolist() == [True, True, False]


def test_issue_rows_returns_expected_issue_register_columns() -> None:
    """Issue materialisation should keep key columns and rule metadata together."""

    contracts = pd.DataFrame(
        {
            "source_file": ["file_a.csv", "file_b.csv"],
            "source_agency": ["Agency A", "Agency B"],
            "contract_id": ["A-001", "B-001"],
            "supplier_name": ["Supplier One", "Supplier Two"],
            "contract_value": [100000.0, 250000.0],
        }
    )
    mask = pd.Series([True, False])

    issues = _issue_rows(
        mask=mask,
        contracts=contracts,
        rule_name="missing_supplier_name",
        severity="high",
        explanation="Supplier name missing from disclosed record.",
    )

    assert list(issues.columns) == [
        "source_file",
        "source_agency",
        "contract_id",
        "supplier_name",
        "contract_value",
        "rule_name",
        "severity",
        "issue_type",
        "explanation",
    ]
    assert len(issues) == 1
    assert issues.loc[0, "contract_id"] == "A-001"
    assert issues.loc[0, "rule_name"] == "missing_supplier_name"
    assert issues.loc[0, "issue_type"] == "validation"


def test_build_data_quality_outputs_creates_flags_issues_and_file_summary(monkeypatch: pytest.MonkeyPatch) -> None:
    """Validation output builder should handle every possible flag combination.

    This test drives all 2^8 combinations of the current validation rules by
    monkeypatching the rule builder directly. That makes the test exhaustive
    for output assembly without relying on awkward real-world data setups for
    every edge-case combination.
    """

    rule_definitions = [
        ("missing_supplier_name", "high", "Supplier name missing from disclosed record."),
        ("missing_or_zero_contract_value", "high", "Contract value missing or non-positive."),
        ("malformed_start_date", "medium", "Start date missing or could not be parsed."),
        ("end_date_before_start_date", "high", "Contract end date is earlier than start date."),
        ("possible_duplicate_contract", "medium", "Potential duplicate contract detected using a business key."),
        ("unusually_large_contract_value", "medium", "Contract value sits in the extreme upper tail."),
        ("unusually_small_contract_value", "low", "Contract value is unusually small but positive."),
        ("missing_critical_governance_fields", "high", "Multiple critical governance fields are missing."),
    ]
    row_count = 2 ** len(rule_definitions)

    canonical_contracts = pd.DataFrame(
        {
            "source_file": ["file_a.csv"] * row_count,
            "source_folder": ["Folder A"] * row_count,
            "source_agency": ["Agency A"] * row_count,
            "contract_id": [f"A-{index:03d}" for index in range(row_count)],
            "supplier_name": [f"Supplier {index}" for index in range(row_count)],
            "supplier_abn": [None] * row_count,
            "contract_title": [f"Contract {index}" for index in range(row_count)],
            "procurement_category": ["Services"] * row_count,
            "procurement_method": ["Open"] * row_count,
            "contract_start_date": pd.to_datetime(["2025-01-01"] * row_count),
            "contract_end_date": pd.to_datetime(["2025-01-02"] * row_count),
            "contract_value": [1000.0] * row_count,
            "reporting_period": ["2025-01"] * row_count,
            "publish_date": pd.to_datetime(["2025-01-15"] * row_count),
            "financial_year": ["2024-2025"] * row_count,
        }
    )
    file_log = pd.DataFrame(
        {
            "source_file": ["file_a.csv"],
            "row_count": [row_count],
            "missing_critical_fields": [""],
        }
    )
    config = {
        "critical_fields": [
            "source_agency",
            "contract_id",
            "supplier_name",
            "contract_value",
            "contract_start_date",
        ],
        "risk_rules": {
            "missing_critical_fields_threshold": 0.1,
        },
    }

    def _fake_rule_builder(contracts: pd.DataFrame, config: dict) -> list[tuple[pd.Series, str, str, str]]:
        rules: list[tuple[pd.Series, str, str, str]] = []
        for bit_index, (rule_name, severity, explanation) in enumerate(rule_definitions):
            mask = pd.Series([(row_number >> bit_index) & 1 == 1 for row_number in range(len(contracts))], index=contracts.index)
            rules.append((mask, rule_name, severity, explanation))
        return rules

    monkeypatch.setattr(validate_contracts, "_build_validation_rules", _fake_rule_builder)

    outputs = build_data_quality_outputs(canonical_contracts, file_log, config)

    for row_number in range(row_count):
        expected_flags = [
            rule_name
            for bit_index, (rule_name, _, _) in enumerate(rule_definitions)
            if ((row_number >> bit_index) & 1) == 1
        ]
        assert outputs.contracts_with_flags.loc[row_number, "data_quality_flags"] == expected_flags

    # Each rule should appear exactly once per flagged row in the issues table.
    expected_issue_count = sum(mask.sum() for mask, _, _, _ in _fake_rule_builder(canonical_contracts, config))
    assert len(outputs.validation_issues) == expected_issue_count
    assert set(outputs.validation_issues["rule_name"]) == {rule_name for rule_name, _, _ in rule_definitions}

    # File-level summary should still be produced alongside the exhaustive row
    # combinations so reporting outputs remain available.
    summary = outputs.data_quality_summary.set_index("source_file")
    assert summary.loc["file_a.csv", "validation_issue_count"] == expected_issue_count
    assert 0 <= summary.loc["file_a.csv", "quality_score"] <= 100
