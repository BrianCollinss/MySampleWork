"""RAG scoring logic for governance indicators.

The scorecard is deliberately simple and transparent. It turns several
governance indicators into a weighted overall status for reporting packs.
"""

from __future__ import annotations

from typing import Any

import pandas as pd

from src.clean.validate_contracts import ValidationOutputs
from src.metrics.procurement_metrics import MetricPack


def _rag_from_score(score: float, thresholds: dict[str, float]) -> str:
    """Convert a numeric score into a Red/Amber/Green label."""
    if score < thresholds.get("red", 40):
        return "Red"
    if score < thresholds.get("amber", 70):
        return "Amber"
    return "Green"


def build_scorecard(
    metric_pack: MetricPack,
    validation_outputs: ValidationOutputs,
    exceptions: pd.DataFrame,
    config: dict[str, Any],
) -> pd.DataFrame:
    """Build an explicit monthly governance scorecard.

    The latest available reporting period is scored using configurable weights
    and thresholds so analysts can explain the status logic clearly.
    """

    rules = config.get("risk_rules", {})
    scoring_config = config.get("scoring", {})
    weights = scoring_config.get("weights", {})
    thresholds = scoring_config.get("rag_thresholds", {})

    # The scorecard is designed as a current-period snapshot, so it uses the
    # latest period available in the metric pack.
    latest_period = metric_pack.monthly_spend["reporting_period"].max()
    
    latest_concentration = metric_pack.supplier_concentration[metric_pack.supplier_concentration["reporting_period"] == latest_period]
    concentration_share = (float(latest_concentration["top_supplier_share"].iat[0]) if not latest_concentration.empty else 0.0)
    
    latest_growth = metric_pack.monthly_spend.loc[
        metric_pack.monthly_spend["reporting_period"] == latest_period
        ]["period_on_period_spend_change_pct"]
    spend_growth = float(latest_growth.iat[0]) if not latest_growth.empty and pd.notna(latest_growth.iat[0]) else 0.0

    repeated_small_count = int((exceptions["rule_name"] == "repeated_small_contracts").sum()) if not exceptions.empty else 0
    worst_quality = (
        float(validation_outputs.data_quality_summary["quality_score"].min())
        if not validation_outputs.data_quality_summary.empty
        else 100.0
    )
    transparency_count = int((exceptions["category"] == "transparency_risk").sum()) if not exceptions.empty else 0

    # Each component score is scaled to a simple 0-100 range before weighting.
    concentration_score = max(0.0, 100 * (1 - concentration_share / max(rules.get("supplier_concentration_threshold", 0.45), 0.01)))
    data_quality_score = worst_quality
    spend_growth_score = max(0.0, 100 * (1 - max(spend_growth, 0) / max(rules.get("abrupt_spend_growth_threshold", 0.5), 0.01)))
    repeated_small_score = max(0.0, 100 - (repeated_small_count * 30))
    transparency_score = max(0.0, 100 - (transparency_count * 20))

    # Keep the measures in one list so weighting and row generation use the
    # same set of indicators.
    measures = [
        ("concentration", concentration_score),
        ("data_quality", data_quality_score),
        ("spend_growth", spend_growth_score),
        ("repeated_small_contracts", repeated_small_score),
        ("transparency", transparency_score),
    ]
    weighted_score = sum(score * weights.get(name, 0) for name, score in measures)

    rows = [
        {"indicator": name, "score": round(score, 2), "status": _rag_from_score(score, thresholds), "reporting_period": latest_period}
        for name, score in measures
    ]
    rows.append(
        {
            "indicator": "overall_governance_score",
            "score": round(weighted_score, 2),
            "status": _rag_from_score(weighted_score, thresholds),
            "reporting_period": latest_period,
        }
    )
    return pd.DataFrame(rows)
