"""Canonical schema constants for NEM dashboard modelling.

These constants define stable names and mappings used by local transforms,
Fabric notebooks, and Power BI documentation.
"""

# AEMO region identifiers are terse; Gold tables include both code and friendly
# name so Power BI report authors do not need to duplicate the mapping.
REGION_MAP = {
    "QLD1": "Queensland",
    "NSW1": "New South Wales",
    "VIC1": "Victoria",
    "SA1": "South Australia",
    "TAS1": "Tasmania",
}

# Thresholds align to dashboard event bands used for quick operational scanning.
PRICE_THRESHOLDS = {
    "high_price_threshold_aud_mwh": 300,
    "extreme_price_threshold_aud_mwh": 1000,
    "negative_price_threshold_aud_mwh": 0,
}

# Canonical column groups document the intended Silver/Gold table contracts.
PRICE_DEMAND_COLUMNS = [
    "settlement_datetime",
    "region",
    "region_name",
    "price_aud_mwh",
    "demand_mw",
    "intervention",
]

GENERATION_COLUMNS = [
    "settlement_datetime",
    "duid",
    "fuel_type",
    "generation_mw",
    "region",
]

INTERCONNECTOR_COLUMNS = [
    "settlement_datetime",
    "interconnector_id",
    "from_region",
    "to_region",
    "flow_mw",
    "export_limit_mw",
    "import_limit_mw",
]

INGESTION_LOG_COLUMNS = [
    "run_id",
    "source_name",
    "source_url",
    "source_zip_name",
    "status",
    "checksum",
    "first_seen_datetime",
    "downloaded_datetime",
    "parsed_datetime",
    "row_count_bronze",
    "row_count_silver",
    "error_message",
]
