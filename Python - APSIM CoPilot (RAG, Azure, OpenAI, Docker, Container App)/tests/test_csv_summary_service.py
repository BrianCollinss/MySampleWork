"""Unit tests for CSV analysis helpers."""

from app.models.mo_csv_summary_service import analyse_csv_bytes, detect_group_column, detect_interesting_columns


def test_detect_interesting_columns() -> None:
    """Ensure APSIM-like column names are mapped to known summary labels."""
    columns = ["Yield", "SoilWater", "Rain", "Treatment"]
    assert detect_interesting_columns(columns) == ["Yield", "SoilWater", "Rain"]


def test_detect_group_column() -> None:
    """Ensure an explicit treatment column is preferred as the grouping field."""
    import pandas as pd

    dataframe = pd.DataFrame({"Treatment": ["A", "B"], "Yield": [4.0, 5.5]})
    assert detect_group_column(dataframe) == "Treatment"


def test_analyse_csv_bytes() -> None:
    """Ensure end-to-end CSV analysis returns the expected summary structure."""
    content = b"Treatment,Yield,Rain\nA,4.2,120\nB,5.1,110\n"
    dataframe, result = analyse_csv_bytes("sample.csv", content)
    assert dataframe.shape == (2, 3)
    assert result.group_column == "Treatment"
    assert "Yield" in result.interesting_columns
    assert "A" in result.group_comparison
