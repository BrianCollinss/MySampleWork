"""CLI entry point for the Medallion procurement reporting pipeline.

This module is intentionally small and orchestration-focused. Its job is to:
1. Read runtime settings from config and CLI arguments.
2. Resolve the active raw-data folder for the run.
3. Execute Bronze, Silver, and Gold transformations in sequence.
4. Persist both analytical layer outputs and business-facing report outputs.
"""

from __future__ import annotations

import argparse
import logging
import sys
from dataclasses import dataclass
from pathlib import Path

# Allow direct execution via `python src/main.py` as well as `python -m src.main`.
if __package__ in {None, ""}:
    sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from src.clean.standardise_contracts import standardise_bronze_to_silver
from src.clean.validate_contracts import build_data_quality_outputs
from src.ingest.load_contract_files import ingest_raw_folder_to_bronze
from src.metrics.procurement_metrics import build_metric_pack
from src.metrics.risk_rules import evaluate_risk_rules
from src.metrics.scoring import build_scorecard
from src.metrics.supplier_segmentation import build_supplier_segmentation
from src.reporting.build_charts import build_all_charts
from src.reporting.build_tables import write_reporting_tables
from src.reporting.write_executive_summary import write_executive_summary
from src.utils.io_helpers import (
    build_timestamped_run_dir,
    clean_unnecessary_workspace_files,
    copy_config_snapshot,
    ensure_directories,
    load_yaml_config,
    prune_old_timestamped_outputs,
    resolve_raw_folder,
    write_dataframe,
)
from src.utils.logging_helpers import configure_logging


@dataclass(slots=True)
class RuntimePaths:
    """Filesystem paths used by the pipeline.

    Keeping path resolution in one structure makes the rest of the pipeline
    easier to read and avoids scattering path-building logic everywhere.
    """

    root: Path
    raw_root_dir: Path
    raw_folder_path: Path
    bronze_dir: Path
    silver_dir: Path
    gold_dir: Path
    output_root_dir: Path
    run_output_dir: Path
    run_data_dir: Path
    run_config_dir: Path
    tables_dir: Path
    charts_dir: Path
    report_dir: Path
    config_path: Path
    global_config_path: Path


def parse_args() -> argparse.Namespace:
    """Parse optional CLI arguments for a reporting run.

    Analysts can usually run the pipeline with no arguments and rely on config,
    but these options make one-off reruns or alternative source folders easy.
    """
    parser = argparse.ArgumentParser(
        description="Build a Bronze/Silver/Gold procurement risk, assurance, and governance reporting pack."
    )
    parser.add_argument("--raw-folder", type=str, default=None)
    parser.add_argument("--config", type=Path, default=Path("config/analysis_config.yaml"))
    parser.add_argument("--global-config", type=Path, default=Path("config/global_config.yaml"))
    parser.add_argument("--output-dir", type=Path, default=None)
    return parser.parse_args()


def build_runtime_paths(args: argparse.Namespace, config: dict, global_config: dict) -> RuntimePaths:
    """Resolve the filesystem locations used during the current run.

    This converts config-relative locations into absolute project paths and
    links the requested raw-folder name to the configured raw root directory.
    """
    root = Path.cwd()
    runtime_config = config.get("runtime", {})
    output_management_config = global_config.get("output_management", {})
    raw_root_dir = (root / runtime_config.get("raw_root_dir", "data")).resolve()
    raw_folder_name = args.raw_folder or runtime_config.get("default_raw_folder", "")
    raw_folder_path = resolve_raw_folder(raw_root_dir, raw_folder_name)
    output_root_dir = (root / (args.output_dir or output_management_config.get("output_dir", "outputs"))).resolve()
    organisation_name = runtime_config.get("organisation_name", "Unknown Organisation")
    run_output_dir = build_timestamped_run_dir(output_root_dir, organisation_name)
    run_data_dir = (run_output_dir / "data").resolve()

    return RuntimePaths(
        root=root,
        raw_root_dir=raw_root_dir,
        raw_folder_path=raw_folder_path,
        bronze_dir=(run_data_dir / runtime_config.get("bronze_dir", "1_bronze")).resolve(),
        silver_dir=(run_data_dir / runtime_config.get("silver_dir", "2_silver")).resolve(),
        gold_dir=(run_data_dir / runtime_config.get("gold_dir", "3_gold")).resolve(),
        output_root_dir=output_root_dir,
        run_output_dir=run_output_dir,
        run_data_dir=run_data_dir,
        run_config_dir=(run_output_dir / "config").resolve(),
        tables_dir=(run_output_dir / "tables").resolve(),
        charts_dir=(run_output_dir / "charts").resolve(),
        report_dir=(run_output_dir / "reports").resolve(),
        config_path=(root / args.config).resolve(),
        global_config_path=(root / args.global_config).resolve(),
    )


def main() -> None:
    """Run the full Medallion reporting pipeline from raw data to reports.

    The main function deliberately mirrors the business pipeline:
    raw folder selection -> Bronze ingestion -> Silver cleansing and validation
    -> Gold metrics and risk outputs -> report tables/charts/summary.
    """
    args = parse_args()
    config = load_yaml_config((Path.cwd() / args.config).resolve())
    global_config = load_yaml_config((Path.cwd() / args.global_config).resolve())
    paths = build_runtime_paths(args, config, global_config)

    configure_logging(config.get("project", {}).get("log_level", "INFO"))
    logger = logging.getLogger(__name__)

    # Remove common local noise before the run so the repo and outputs stay
    # tidy without requiring manual cleanup by the analyst.
    cleanup_summary = clean_unnecessary_workspace_files(paths.root)

    # Before creating the next run folder, remove older timestamped runs using
    # the retention rule from the global operational config.
    removed_output_dirs = prune_old_timestamped_outputs(
        paths.output_root_dir,
        global_config.get("output_management", {}).get("retention", {}),
    )

    # Create every required output location before any processing starts so
    # later steps can focus on transformation logic rather than path hygiene.
    ensure_directories(
        [
            paths.bronze_dir,
            paths.silver_dir,
            paths.gold_dir,
            paths.output_root_dir,
            paths.run_output_dir,
            paths.run_data_dir,
            paths.run_config_dir,
            paths.tables_dir,
            paths.charts_dir,
            paths.report_dir,
        ]
    )

    # Copy the active configuration files into the run folder so future review
    # can inspect the exact settings used to generate the outputs.
    copy_config_snapshot(paths.config_path.parent, paths.run_config_dir)

    logger.info("Starting Medallion Pipeline | Organisation: %s | Folder: %s",
        config.get("runtime", {}).get("organisation_name", "Unknown"),
        paths.raw_folder_path.name
    )
    logger.info(
        "Output Run Directory Prepared | Run Output=%s | Removed Historical Runs=%s",
        paths.run_output_dir,
        len(removed_output_dirs),
    )
    logger.info(
        "Workspace Housekeeping Complete | Removed Files=%s | Removed Cache Dirs=%s",
        cleanup_summary["removed_files"],
        cleanup_summary["removed_directories"],
    )

    # Bronze persists the raw unioned dataset and schema observations exactly
    # as the ingestion layer saw them, ready for downstream standardisation.
    bronze = ingest_raw_folder_to_bronze(paths.raw_folder_path, config)
    write_dataframe(bronze.bronze_contracts, paths.bronze_dir / "bronze_contracts.csv")
    write_dataframe(bronze.bronze_schema_registry, paths.bronze_dir / "bronze_schema_registry.csv")
    write_dataframe(bronze.bronze_file_registry, paths.bronze_dir / "bronze_file_registry.csv")

    # Silver standardises the heterogeneous Bronze rows into a canonical
    # contract table and pairs that with reusable data quality outputs.
    silver_contracts = standardise_bronze_to_silver(
        bronze_contracts=bronze.bronze_contracts,
        bronze_schema_registry=bronze.bronze_schema_registry,
        config=config,
    )
    validation_outputs = build_data_quality_outputs(silver_contracts, bronze.bronze_file_registry, config)
    write_dataframe(validation_outputs.contracts_with_flags, paths.silver_dir / "silver_contracts.csv")
    write_dataframe(validation_outputs.validation_issues, paths.silver_dir / "silver_validation_issues.csv")
    write_dataframe(validation_outputs.data_quality_summary, paths.silver_dir / "silver_data_quality_summary.csv")

    # Gold converts validated Silver data into business-ready marts such as
    # trends, concentration outputs, exceptions, and governance scorecards.
    metric_pack = build_metric_pack(validation_outputs.contracts_with_flags, config)
    exceptions = evaluate_risk_rules(
        validation_outputs.contracts_with_flags,
        metric_pack,
        validation_outputs,
        config,
    )
    scorecard = build_scorecard(metric_pack, validation_outputs, exceptions, config)
    supplier_segmentation = build_supplier_segmentation(validation_outputs.contracts_with_flags, config)

    write_dataframe(metric_pack.summary_metrics, paths.gold_dir / "gold_summary_metrics.csv")
    write_dataframe(metric_pack.financial_year_summary, paths.gold_dir / "gold_financial_year_summary.csv")
    write_dataframe(metric_pack.top_suppliers_by_value, paths.gold_dir / "gold_top_suppliers_by_value.csv")
    write_dataframe(metric_pack.supplier_concentration, paths.gold_dir / "gold_supplier_concentration.csv")
    write_dataframe(metric_pack.supplier_concentration_decomposition, paths.gold_dir / "gold_supplier_concentration_decomposition.csv")
    write_dataframe(metric_pack.agency_summary, paths.gold_dir / "gold_agency_summary.csv")
    write_dataframe(metric_pack.category_summary, paths.gold_dir / "gold_category_summary.csv")
    write_dataframe(metric_pack.missing_data_rates, paths.gold_dir / "gold_missing_data_rates.csv")
    write_dataframe(exceptions, paths.gold_dir / "gold_exceptions_register.csv")
    write_dataframe(scorecard, paths.gold_dir / "gold_scorecard.csv")
    write_dataframe(supplier_segmentation.supplier_segments, paths.gold_dir / "gold_supplier_segments.csv")
    write_dataframe(supplier_segmentation.cluster_summary, paths.gold_dir / "gold_supplier_cluster_summary.csv")

    # These reporting artefacts sit alongside the Medallion layers and are
    # intended for immediate stakeholder consumption.
    reporting_tables = write_reporting_tables(
        output_dir=paths.run_output_dir,
        metrics_summary=metric_pack.summary_metrics,
        exceptions=exceptions,
        data_quality_summary=validation_outputs.data_quality_summary,
        additional_tables={
            "financial_year_summary": metric_pack.financial_year_summary,
            "supplier_spend": metric_pack.top_suppliers_by_value,
            "supplier_concentration": metric_pack.supplier_concentration,
            "supplier_concentration_decomposition": metric_pack.supplier_concentration_decomposition,
            "scorecard": scorecard,
            "agency_summary": metric_pack.agency_summary,
            "category_summary": metric_pack.category_summary,
            "supplier_segments": supplier_segmentation.supplier_segments,
            "supplier_cluster_summary": supplier_segmentation.cluster_summary,
        },
    )

    # Executive outputs combine the Gold data products into presentation-ready
    # files for decision makers.
    chart_paths = build_all_charts(
        output_dir=paths.charts_dir,
        metric_pack=metric_pack,
        exceptions=exceptions,
        data_quality_summary=validation_outputs.data_quality_summary,
        config=config,
        supplier_segmentation=supplier_segmentation,
    )

    write_executive_summary(
        report_path=paths.report_dir / "executive_summary.md",
        metric_pack=metric_pack,
        exceptions=exceptions,
        data_quality_summary=validation_outputs.data_quality_summary,
        scorecard=scorecard,
        config=config,
        chart_paths=chart_paths,
        supplier_segmentation=supplier_segmentation,
    )

    logger.info(
        "Pipeline Completed | Organisation=%s | Bronze Rows=%s | Silver Rows=%s | Exceptions=%s",
        config.get("runtime", {}).get("organisation_name", "Unknown"),
        len(bronze.bronze_contracts),
        len(validation_outputs.contracts_with_flags),
        len(exceptions),
    )

    # logger.info("Gold/Report Outputs Written: %s", json.dumps({k: str(v) for k, v in reporting_tables.items()}))


if __name__ == "__main__":
    main()
