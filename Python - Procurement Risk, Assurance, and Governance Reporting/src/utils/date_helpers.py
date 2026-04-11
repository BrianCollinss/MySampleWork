"""Date parsing and period helpers.

Date handling is centralised here so the rest of the pipeline can use
consistent parsing and reporting-period derivation rules.
"""

from __future__ import annotations

import re

import pandas as pd


def _parse_mixed_dates(values: pd.Series) -> pd.Series:
    """Parse mixed-format dates using Australian-style day-first interpretation."""

    series = values.astype("string")

    def _parse_one(value: str) -> pd.Timestamp | pd.NaT:
        if pd.isna(value) or not str(value).strip() or str(value) == "<NA>":
            return pd.NaT
        try:
            # Values such as `Jan-20` or `May-20` in disclosure exports should
            # be interpreted as month-year markers and normalised to the first
            # day of that month, not as ambiguous day-month strings.
            if re.match(r"^[A-Za-z]{3}[-/]\d{2,4}$", value):
                return pd.to_datetime(value, errors="raise", format="%b-%y")
            if re.match(r"^[A-Za-z]{3}[-/]\d{4}$", value):
                return pd.to_datetime(value, errors="raise", format="%b-%Y")
            if re.match(r"^\d{4}[-/]\d{1,2}[-/]\d{1,2}$", value):
                return pd.to_datetime(value, errors="raise", dayfirst=False, format="mixed")
            return pd.to_datetime(value, errors="raise", dayfirst=True, format="mixed")
        except (ValueError, TypeError, OverflowError, pd.errors.OutOfBoundsDatetime):
            return pd.NaT

    return series.map(_parse_one)


def parse_date_series(values: pd.Series) -> pd.Series:
    """Parse a series into datetimes, coercing invalid values to null."""
    return _parse_mixed_dates(values)


def coerce_reporting_period(values: pd.Series, fallback_date: pd.Series) -> pd.Series:
    """Derive a monthly reporting period, falling back to date fields if needed."""
    parsed_direct = _parse_mixed_dates(values)
    result = parsed_direct.dt.to_period("M").astype("string")
    fallback_result = _parse_mixed_dates(fallback_date).dt.to_period("M").astype("string")
    combined = result.where(result.ne("<NA>"), fallback_result)
    return combined.fillna("Unknown")


def derive_financial_year(series: pd.Series, start_month: int = 7) -> pd.Series:
    """Derive an Australian-style financial-year label from a date series.

    Example:
    - 2024-06-30 -> 2023-2024
    - 2024-07-01 -> 2024-2025
    """

    parsed = _parse_mixed_dates(series)
    start_year = parsed.dt.year.astype("Int64") - (parsed.dt.month < start_month).fillna(False).astype("Int64")
    return start_year.map(lambda value: f"{int(value)}-{int(value) + 1}" if pd.notna(value) else pd.NA)


def extract_financial_year_from_text(text: str) -> str | None:
    """Extract a financial-year label such as 2019-2020 from text when present."""

    match = re.search(r"(20\d{2})[-_](20\d{2})", text)
    if not match:
        return None
    start_year, end_year = match.groups()
    return f"{start_year}-{end_year}"
