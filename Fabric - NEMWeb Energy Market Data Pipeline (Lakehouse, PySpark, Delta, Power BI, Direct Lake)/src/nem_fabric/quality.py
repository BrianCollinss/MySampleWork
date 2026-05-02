"""Data quality checks for local validation and notebook reuse.

Checks return small structured results so they can be printed locally or written
to Fabric audit tables during notebook runs.
"""

from __future__ import annotations

from dataclasses import dataclass

import pandas as pd


@dataclass(frozen=True)
class QualityResult:
    """Outcome of one data quality check."""

    check_name: str
    passed: bool
    detail: str


def null_check(df: pd.DataFrame, columns: list[str]) -> QualityResult:
    """Check whether selected columns contain null values."""

    missing = {
        column: int(df[column].isna().sum())
        for column in columns
        if column in df.columns
    }
    return QualityResult(
        "null_check", all(value == 0 for value in missing.values()), str(missing)
    )


def duplicate_check(df: pd.DataFrame, subset: list[str]) -> QualityResult:
    """Check duplicate count for a natural key or complete row."""

    duplicates = (
        int(df.duplicated(subset=subset).sum())
        if subset
        else int(df.duplicated().sum())
    )
    return QualityResult("duplicate_check", duplicates == 0, f"duplicates={duplicates}")


def timestamp_continuity_check(df: pd.DataFrame, column: str) -> QualityResult:
    """Perform a simple timestamp presence and parseability check."""

    if column not in df.columns or df.empty:
        return QualityResult(
            "timestamp_continuity_check", False, "timestamp column missing or empty"
        )
    sorted_values = pd.to_datetime(df[column], errors="coerce").dropna().sort_values()
    return QualityResult(
        "timestamp_continuity_check",
        not sorted_values.empty,
        f"rows={len(sorted_values)}",
    )


def region_coverage_check(
    df: pd.DataFrame, expected_regions: set[str]
) -> QualityResult:
    """Check that expected NEM regions are represented in a DataFrame."""

    actual = set(df["region"].dropna().unique()) if "region" in df.columns else set()
    missing = expected_regions - actual
    return QualityResult(
        "region_coverage_check", not missing, f"missing={sorted(missing)}"
    )


def freshness_check(
    df: pd.DataFrame, column: str, max_age_minutes: int
) -> QualityResult:
    """Check whether the latest timestamp is within the allowed freshness window."""

    if column not in df.columns or df.empty:
        return QualityResult(
            "freshness_check", False, "timestamp column missing or empty"
        )
    latest = pd.to_datetime(df[column], errors="coerce").max()
    age_minutes = (
        pd.Timestamp.utcnow().tz_localize(None) - latest.tz_localize(None)
    ).total_seconds() / 60
    return QualityResult(
        "freshness_check",
        age_minutes <= max_age_minutes,
        f"age_minutes={age_minutes:.1f}",
    )


def schema_drift_check(df: pd.DataFrame, expected_columns: set[str]) -> QualityResult:
    """Compare actual columns with an expected schema contract."""

    actual = set(df.columns)
    missing = expected_columns - actual
    extra = actual - expected_columns
    return QualityResult(
        "schema_drift_check",
        not missing,
        f"missing={sorted(missing)}, extra={sorted(extra)}",
    )


def row_count_reconciliation(
    raw_count: int, bronze_count: int, silver_count: int, gold_count: int
) -> QualityResult:
    """Check broad row-count consistency across medallion layers."""

    passed = raw_count >= bronze_count >= silver_count >= 0 and gold_count >= 0
    return QualityResult(
        "row_count_reconciliation",
        passed,
        f"raw={raw_count}, bronze={bronze_count}, silver={silver_count}, gold={gold_count}",
    )
