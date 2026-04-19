"""Unit tests for the APSIMX parser.

These tests focus on realistic high-value extraction behaviour rather than
exhaustive APSIM schema coverage.
"""

from app.models.mo_apsimx_parser import extract_apsimx_summary


def test_extract_apsimx_summary_basic_fields() -> None:
    """Confirm the parser extracts the main APSIMX concepts from a sample tree."""
    payload = {
        "Name": "WheatSimulation",
        "$type": "Models.Core.Simulation, Models",
        "Children": [
            {"Name": "Clock", "$type": "Models.Clock", "Start": "2021-01-01", "End": "2021-12-31"},
            {"Name": "Soil", "$type": "Models.Soils.Soil, Models"},
            {"Name": "Weather", "$type": "Models.Climate.Weather, Models", "FileName": "met/example.met"},
            {"Name": "Wheat", "$type": "Models.PMF.Plant, Models"},
            {"Name": "Manager", "$type": "Models.Manager, Models"},
            {"Name": "Report", "$type": "Models.Report, Models", "Variables": ["[Wheat].Yield", "[Clock].Today"]},
        ],
    }
    summary = extract_apsimx_summary(payload)
    assert summary["simulation_name"] == "WheatSimulation"
    assert summary["clock"]["start_date"] == "2021-01-01"
    assert "Wheat" in summary["crops_or_plant_modules"]
    assert "Soil" in summary["soils"]
    assert "met/example.met" in summary["weather_references"]
    assert "[Wheat].Yield" in summary["report_output_variables"]
