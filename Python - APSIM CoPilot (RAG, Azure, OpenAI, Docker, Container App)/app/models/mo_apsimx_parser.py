"""Heuristic parser for APSIMX files.

APSIMX files are JSON documents, but their structure varies across examples and
models. This parser walks the tree defensively and extracts the fields that are
most useful for a plain-English explanation.
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field


PLANT_HINTS = {"wheat", "maize", "sorghum", "soybean", "canola", "lucerne", "cotton", "plant"}
SOIL_HINTS = {"soil", "physical", "organic", "chemical"}
WEATHER_HINTS = {"weather", "metfile"}
MANAGER_HINTS = {"manager", "script", "operation", "fertilise", "sow", "harvest", "irrigate"}
ZONE_HINTS = {"zone", "paddock", "field"}
CLOCK_HINTS = {"clock"}


@dataclass
class ApsimxSummary:
    """Structured summary extracted from an APSIMX file."""

    simulation_name: str | None = None
    start_date: str | None = None
    end_date: str | None = None
    crops_or_plant_modules: list[str] = field(default_factory=list)
    soils: list[str] = field(default_factory=list)
    weather_references: list[str] = field(default_factory=list)
    manager_scripts_or_rules: list[str] = field(default_factory=list)
    report_output_variables: list[str] = field(default_factory=list)
    zones_or_paddocks: list[str] = field(default_factory=list)
    raw_keys_seen: list[str] = field(default_factory=list)

    def to_dict(self) -> dict[str, object]:
        """Convert the dataclass into a UI- and prompt-friendly dictionary."""
        return {
            "simulation_name": self.simulation_name,
            "clock": {"start_date": self.start_date, "end_date": self.end_date},
            "crops_or_plant_modules": self.crops_or_plant_modules,
            "soils": self.soils,
            "weather_references": self.weather_references,
            "manager_scripts_or_rules": self.manager_scripts_or_rules,
            "report_output_variables": self.report_output_variables,
            "zones_or_paddocks": self.zones_or_paddocks,
            "raw_keys_seen": self.raw_keys_seen[:100],
        }


def parse_apsimx_content(content: bytes) -> dict[str, object]:
    """Parse raw APSIMX bytes and return a structured summary."""
    parsed = json.loads(content.decode("utf-8"))
    return extract_apsimx_summary(parsed)


def extract_apsimx_summary(data: dict[str, object]) -> dict[str, object]:
    """Walk an APSIMX-like JSON object and extract the main simulation details."""
    summary = ApsimxSummary()
    _walk_node(data, summary)
    return summary.to_dict()


def _walk_node(node: object, summary: ApsimxSummary) -> None:
    """Recursively traverse dictionaries and lists inside the APSIMX payload."""
    if isinstance(node, dict):
        _capture_from_dict(node, summary)
        for value in node.values():
            _walk_node(value, summary)
    elif isinstance(node, list):
        for item in node:
            _walk_node(item, summary)


def _capture_from_dict(node: dict[str, object], summary: ApsimxSummary) -> None:
    """Inspect one JSON object and harvest useful APSIM-related hints."""
    keys = {str(key).lower(): value for key, value in node.items()}
    summary.raw_keys_seen.extend(list(keys.keys()))
    name_value = node.get("Name") or node.get("name")
    type_value = str(node.get("$type") or node.get("Type") or node.get("type") or "").lower()
    if summary.simulation_name is None and _looks_like_simulation(type_value, name_value):
        summary.simulation_name = str(name_value)
    if _looks_like_clock(type_value, name_value):
        if "start" in keys and summary.start_date is None:
            summary.start_date = str(keys["start"])
        if "end" in keys and summary.end_date is None:
            summary.end_date = str(keys["end"])
    # APSIMX type names are the most reliable clue for identifying broad model
    # categories such as plants, soils, weather, and manager components.
    if any(hint in type_value for hint in PLANT_HINTS) and name_value:
        _append_unique(summary.crops_or_plant_modules, str(name_value))
    if any(hint in type_value for hint in SOIL_HINTS) and name_value:
        _append_unique(summary.soils, str(name_value))
    if any(hint in type_value for hint in WEATHER_HINTS):
        weather_ref = node.get("FileName") or node.get("filename") or name_value
        if weather_ref:
            _append_unique(summary.weather_references, str(weather_ref))
    if any(hint in type_value for hint in MANAGER_HINTS) and name_value:
        _append_unique(summary.manager_scripts_or_rules, str(name_value))
    if any(hint in type_value for hint in ZONE_HINTS) and name_value:
        _append_unique(summary.zones_or_paddocks, str(name_value))
    if "variables" in keys and isinstance(keys["variables"], list):
        for item in keys["variables"]:
            _append_unique(summary.report_output_variables, str(item))
    if "filename" in keys and "met" in str(keys["filename"]).lower():
        _append_unique(summary.weather_references, str(keys["filename"]))


def _append_unique(items: list[str], value: str) -> None:
    """Append a value only once while preserving original order."""
    if value not in items:
        items.append(value)


def _looks_like_simulation(type_value: str, name_value: object) -> bool:
    """Return True when the node appears to represent a simulation root."""
    return bool(name_value) and ("simulation" in type_value or type_value.endswith(".simulations"))


def _looks_like_clock(type_value: str, name_value: object) -> bool:
    """Return True when the node appears to represent the APSIM clock."""
    name_text = str(name_value).lower() if name_value else ""
    return any(hint in type_value for hint in CLOCK_HINTS) or name_text in CLOCK_HINTS
