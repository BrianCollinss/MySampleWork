"""CSV analysis helpers for APSIM output summaries.

The goal here is not full statistical analysis. Instead, the code extracts a
small, explainable set of facts that the LLM can turn into a grounded summary.
"""

from __future__ import annotations

from dataclasses import dataclass
from io import BytesIO

import pandas as pd


INTERESTING_COLUMN_HINTS = {
    "yield": "Yield",
    "biomass": "Biomass",
    "lai": "LAI",
    "rain": "Rain",
    "runoff": "Runoff",
    "soilwater": "SoilWater",
    "sw": "SoilWater",
    "nitrogen": "Nitrogen",
    "nitrate": "Nitrogen",
}
GROUP_COLUMN_HINTS = {"treatment", "scenario", "simulation", "zone", "field", "name"}


@dataclass
class CsvAnalysisResult:
    """Structured summary of one uploaded CSV file."""

    file_name: str
    row_count: int
    column_count: int
    numeric_columns: list[str]
    interesting_columns: list[str]
    group_column: str | None
    descriptive_stats: dict
    group_comparison: dict
    preview_records: list[dict]

    def to_prompt_payload(self) -> dict:
        """Return the subset of analysis data needed for prompt generation."""
        return {
            "file_name": self.file_name,
            "row_count": self.row_count,
            "column_count": self.column_count,
            "numeric_columns": self.numeric_columns,
            "interesting_columns": self.interesting_columns,
            "group_column": self.group_column,
            "descriptive_stats": self.descriptive_stats,
            "group_comparison": self.group_comparison,
        }


def analyse_csv_bytes(file_name: str, content: bytes, preview_rows: int = 25) -> tuple[pd.DataFrame, CsvAnalysisResult]:
    """Read an uploaded CSV file and build a compact analysis result."""
    dataframe = pd.read_csv(BytesIO(content))
    numeric_columns = dataframe.select_dtypes(include="number").columns.tolist()
    interesting_columns = detect_interesting_columns(dataframe.columns.tolist())
    group_column = detect_group_column(dataframe)
    descriptive_stats = dataframe[numeric_columns].describe().round(3).fillna("").to_dict() if numeric_columns else {}
    group_comparison = build_group_comparison(dataframe, group_column, numeric_columns)
    # Group comparison is deliberately simple for the demo: it uses mean values
    # so the model can compare treatments without inventing complex statistics.
    result = CsvAnalysisResult(
        file_name=file_name,
        row_count=len(dataframe),
        column_count=len(dataframe.columns),
        numeric_columns=numeric_columns,
        interesting_columns=interesting_columns,
        group_column=group_column,
        descriptive_stats=descriptive_stats,
        group_comparison=group_comparison,
        preview_records=dataframe.head(preview_rows).to_dict(orient="records"),
    )
    return dataframe, result


def detect_interesting_columns(columns: list[str]) -> list[str]:
    """Identify APSIM-like variables based on column name hints."""
    matches: list[str] = []
    for column in columns:
        normalised = column.replace("_", "").replace(" ", "").lower()
        for hint, label in INTERESTING_COLUMN_HINTS.items():
            if hint in normalised and label not in matches:
                matches.append(label)
    return matches


def detect_group_column(dataframe: pd.DataFrame) -> str | None:
    """Pick a likely grouping column such as Treatment or Scenario."""
    for column in dataframe.columns:
        normalised = column.replace("_", "").replace(" ", "").lower()
        if normalised in GROUP_COLUMN_HINTS:
            return column
    for column in dataframe.select_dtypes(exclude="number").columns:
        unique_values = dataframe[column].nunique(dropna=True)
        if 1 < unique_values <= 12:
            return column
    return None


def build_group_comparison(dataframe: pd.DataFrame, group_column: str | None, numeric_columns: list[str]) -> dict:
    """Aggregate mean numeric values by group for simple comparisons."""
    if not group_column or not numeric_columns:
        return {}
    grouped = dataframe.groupby(group_column)[numeric_columns].mean(numeric_only=True).round(3)
    return grouped.to_dict(orient="index")
