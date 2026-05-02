from __future__ import annotations

import pandas as pd

from nem_fabric.transformations import normalise_timestamps, standardise_region_names


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
