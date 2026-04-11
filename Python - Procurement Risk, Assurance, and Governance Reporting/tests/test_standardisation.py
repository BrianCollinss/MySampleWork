from __future__ import annotations

import pandas as pd

from src.clean.standardise_contracts import standardise_bronze_to_silver
from src.ingest.schema_mapping import infer_column_schema, schema_inference_to_frame
from src.utils.date_helpers import parse_date_series


def test_schema_inference_matches_similar_columns_to_canonical_fields() -> None:
    """Schema inference should recognise common alternative source headings."""
    config = {
        "schema_mappings": {
            "source_agency": ["agency_name"],
            "contract_id": ["reference_number"],
            "supplier_name": ["vendor_name"],
            "contract_value": ["total_value"],
            "contract_start_date": ["commencement"],
            "publish_date": ["disclosure_date"],
        }
    }
    frame = pd.DataFrame(
        {
            "Agency Name": ["Dept A"],
            "Reference Number": ["ABC-123"],
            "Vendor Name": ["Vendor Pty Ltd"],
            "Total Value": ["250000"],
            "Commencement": ["2025-01-10"],
            "Disclosure Date": ["2025-01-12"],
        }
    )

    # Infer the source schema and then flatten it to the same tabular form used
    # by the Bronze schema registry.
    inferred = infer_column_schema(frame, config, sample_size=10)
    inferred_frame = schema_inference_to_frame(inferred)

    suggested = dict(zip(inferred_frame["normalised_column"], inferred_frame["suggested_canonical_field"]))
    assert suggested["agency_name"] == "source_agency"
    assert suggested["reference_number"] == "contract_id"
    assert suggested["vendor_name"] == "supplier_name"
    assert suggested["total_value"] == "contract_value"


def test_standardise_bronze_to_silver_maps_inferred_columns() -> None:
    """Silver standardisation should use inferred mappings to populate canonical fields."""
    config = {
        "canonical_contract_schema": [
            "source_file",
            "source_folder",
            "source_agency",
            "contract_id",
            "supplier_name",
            "supplier_abn",
            "contract_title",
            "procurement_category",
            "procurement_method",
            "contract_start_date",
            "contract_end_date",
            "contract_value",
            "reporting_period",
            "publish_date",
        ]
    }
    bronze_contracts = pd.DataFrame(
        {
            "source_file": ["demo.csv"],
            "source_folder": ["Folder A"],
            "agency_name": ["Dept A"],
            "reference_number": ["A-001"],
            "vendor_name": ["Vendor Pty Ltd"],
            "total_value": ["150000"],
            "commencement": ["2025-01-10"],
            "disclosure_date": ["2025-01-12"],
            "source_path": ["data/folder/demo.csv"],
            "source_column_lineage": ["{}"],
        }
    )
    bronze_schema_registry = pd.DataFrame(
        {
            "source_file": ["demo.csv"] * 6,
            "source_folder": ["Folder A"] * 6,
            "normalised_column": [
                "agency_name",
                "reference_number",
                "vendor_name",
                "total_value",
                "commencement",
                "disclosure_date",
            ],
            "suggested_canonical_field": [
                "source_agency",
                "contract_id",
                "supplier_name",
                "contract_value",
                "contract_start_date",
                "publish_date",
            ],
            "match_score": [1.0, 1.0, 1.0, 1.0, 1.0, 1.0],
        }
    )

    # The standardiser should populate the canonical contract schema even
    # though the Bronze headings differ from the target field names.
    result = standardise_bronze_to_silver(bronze_contracts, bronze_schema_registry, config)

    assert result.loc[0, "source_agency"] == "Dept A"
    assert result.loc[0, "contract_id"] == "A-001"
    assert result.loc[0, "supplier_name"] == "Vendor Pty Ltd"
    assert result.loc[0, "contract_value"] == 150000
    assert result.loc[0, "reporting_period"] == "2025-01"


def test_parse_date_series_handles_month_year_values_as_first_day_of_month() -> None:
    """Month-year disclosure values should parse to the first day of the month."""

    parsed = parse_date_series(pd.Series(["Jan-20", "May-20"]))

    assert parsed.iloc[0] == pd.Timestamp("2020-01-01")
    assert parsed.iloc[1] == pd.Timestamp("2020-05-01")
