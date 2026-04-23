"""Project-wide configuration for the CRM revenue analytics pipeline."""

from __future__ import annotations

from pathlib import Path

from pydantic import BaseModel, Field


class ProjectConfig(BaseModel):
    """Store shared paths and modelling constants for all pipeline layers."""

    project_root: Path = Field(default_factory=lambda: Path(__file__).resolve().parents[1])  # Base project directory used to derive all other paths.
    raw_dir: Path | None = None  # Landing folder for original source CSV files before any transformation.
    bronze_dir: Path | None = None  # Output folder for minimally transformed Bronze datasets with ingestion metadata.
    silver_dir: Path | None = None  # Output folder for cleaned and conformed Silver datasets.
    gold_dir: Path | None = None  # Output folder for BI-ready Gold dimensions, facts, and marts.
    docs_dir: Path | None = None  # Folder containing project documentation and Power BI guidance.
    reports_dir: Path | None = None  # Root reporting folder containing generated figures, tables, and metrics subfolders.
    reports_figures_dir: Path | None = None  # Folder containing generated chart and dashboard PNG outputs.
    reports_tables_dir: Path | None = None  # Folder containing exported report tables such as CSV summaries.
    reports_metrics_dir: Path | None = None  # Folder containing JSON metric and quality summary artefacts.
    forecast_horizon_months: int = 6  # Number of future months to generate in the revenue forecast mart.
    fiscal_year_start_month: int = 7  # Calendar month used as the start of the financial year for the date dimension.
    stage_probability_map: dict[str, float] = {
        # Default commercial probability assigned to each pipeline stage for weighted-pipeline reporting.
        "Prospecting": 0.1,
        "Engaging": 0.25,
        "Proposal": 0.5,
        "Won": 1.0,
        "Lost": 0.0,
    }

    def model_post_init(self, __context: object) -> None:
        """Populate derived directory paths once the base project path is known."""

        # Resolve the standard medallion, documentation, and report directories.
        self.raw_dir = self.project_root / "data" / "raw"
        self.bronze_dir = self.project_root / "data" / "bronze"
        self.silver_dir = self.project_root / "data" / "silver"
        self.gold_dir = self.project_root / "data" / "gold"
        self.docs_dir = self.project_root / "docs"
        self.reports_dir = self.project_root / "reports"
        self.reports_figures_dir = self.reports_dir / "figures"
        self.reports_tables_dir = self.reports_dir / "tables"
        self.reports_metrics_dir = self.reports_dir / "metrics"


# Expose a single shared configuration instance for the package.
CONFIG = ProjectConfig()
