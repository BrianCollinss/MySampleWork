"""Snowpark bootstrap for the Python pipeline.

Step purpose:
1. Open a Snowpark session using the project .env settings.
2. Create the warehouse and the Bronze, Silver, and Gold schemas.
3. Provision the Python pipeline file format and base tables for the Snowpipe path.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from snowflake_project.connection import get_snowpark_session
from snowflake_project.project import load_project_settings, run_sql, set_session_context


def main() -> None:
    settings = load_project_settings(output_prefix="py")

    with get_snowpark_session() as session:
        # Apply only the role first so the warehouse and database can be created
        # under the intended security context on a clean rerun.
        run_sql(session, f"USE ROLE {settings.project_role}", "py bootstrap use role")

        # Create the shared warehouse first so the later schema and table DDL has
        # somewhere predictable to run, even on a brand-new Snowflake account.
        run_sql(
            session,
            f"""
            CREATE WAREHOUSE IF NOT EXISTS {settings.project_warehouse}
              WAREHOUSE_SIZE = 'XSMALL'
              AUTO_SUSPEND = 60
              AUTO_RESUME = TRUE
              INITIALLY_SUSPENDED = TRUE
            """,
            "py bootstrap warehouse",
        )
        # Switch into the warehouse immediately after creating it, then create
        # the project database before any code tries to `USE DATABASE` on it.
        run_sql(session, f"USE WAREHOUSE {settings.project_warehouse}", "py bootstrap use warehouse")
        run_sql(session, f"CREATE DATABASE IF NOT EXISTS {settings.project_database}", "py bootstrap database")
        set_session_context(session, settings)

        # The medallion schemas keep the landing, curation, and serving layers
        # physically separate so the twin pipelines share the same layout.
        for schema_name in (settings.bronze_schema, settings.silver_schema, settings.gold_schema):
            run_sql(session, f"CREATE SCHEMA IF NOT EXISTS {schema_name}", f"py create {schema_name}")

        # A single reusable CSV file format keeps the Snowpipe stage and COPY
        # logic aligned on column parsing rules.
        run_sql(
            session,
            f"""
            CREATE OR REPLACE FILE FORMAT {settings.bronze_schema}.orders_csv_ff
            TYPE = CSV
            SKIP_HEADER = 1
            FIELD_OPTIONALLY_ENCLOSED_BY = '"'
            NULL_IF = ('NULL', '')
            """,
            "py bootstrap file format",
        )

        # Bootstrap every empty table up front so later steps can focus on data
        # movement rather than DDL. The names mirror the SQL pipeline exactly.
        create_table_statements = [
            (
                settings.bronze_object("customer_bronze"),
                """
                cust_key NUMBER,
                customer_name STRING,
                customer_address STRING,
                nation_key NUMBER,
                nation_name STRING,
                region_name STRING,
                phone STRING,
                account_balance NUMBER(12,2),
                market_segment STRING,
                customer_comment STRING
                """,
            ),
            (
                settings.bronze_object("orders_bronze"),
                """
                order_key NUMBER,
                cust_key NUMBER,
                order_status STRING,
                total_price NUMBER(12,2),
                order_date DATE,
                order_priority STRING,
                clerk STRING,
                ship_priority NUMBER,
                order_comment STRING
                """,
            ),
            (
                settings.bronze_object("lineitem_bronze"),
                """
                order_key NUMBER,
                part_key NUMBER,
                supplier_key NUMBER,
                line_number NUMBER,
                quantity NUMBER(12,2),
                extended_price NUMBER(12,2),
                discount NUMBER(12,2),
                tax NUMBER(12,2),
                return_flag STRING,
                line_status STRING,
                ship_date DATE,
                commit_date DATE,
                receipt_date DATE,
                ship_instruct STRING,
                ship_mode STRING
                """,
            ),
            (
                settings.bronze_object("snowpipe_orders_bronze"),
                """
                order_key NUMBER,
                cust_key NUMBER,
                order_status STRING,
                total_price NUMBER(12,2),
                order_date DATE,
                order_priority STRING,
                clerk STRING,
                ship_priority NUMBER,
                order_comment STRING,
                ingested_at TIMESTAMP_NTZ,
                source_filename STRING
                """,
            ),
            (
                settings.silver_object("customer_silver"),
                """
                cust_key NUMBER,
                customer_name STRING,
                customer_address STRING,
                nation_key NUMBER,
                nation_name STRING,
                region_name STRING,
                phone STRING,
                account_balance NUMBER(12,2),
                market_segment STRING,
                customer_comment STRING
                """,
            ),
            (
                settings.silver_object("orders_silver"),
                """
                order_key NUMBER,
                cust_key NUMBER,
                order_status STRING,
                total_price NUMBER(12,2),
                order_date DATE,
                order_priority STRING,
                clerk STRING,
                ship_priority NUMBER,
                order_comment STRING
                """,
            ),
            (
                settings.silver_object("lineitem_silver"),
                """
                order_key NUMBER,
                part_key NUMBER,
                supplier_key NUMBER,
                line_number NUMBER,
                quantity NUMBER(12,2),
                extended_price NUMBER(12,2),
                discount NUMBER(12,2),
                tax NUMBER(12,2),
                return_flag STRING,
                line_status STRING,
                ship_date DATE,
                commit_date DATE,
                receipt_date DATE,
                ship_instruct STRING,
                ship_mode STRING
                """,
            ),
            (
                settings.silver_object("mock_orders_silver"),
                """
                order_key NUMBER,
                cust_key NUMBER,
                order_status STRING,
                total_price NUMBER(12,2),
                order_date DATE,
                order_priority STRING,
                clerk STRING,
                ship_priority NUMBER,
                order_comment STRING,
                ingested_at TIMESTAMP_NTZ,
                source_filename STRING,
                load_method STRING
                """,
            ),
            (
                settings.gold_object("order_daily_gold"),
                """
                order_date DATE,
                region_name STRING,
                market_segment STRING,
                order_count NUMBER,
                customer_count NUMBER,
                gross_order_value NUMBER(18,2),
                net_line_revenue NUMBER(18,2)
                """,
            ),
        ]

        for table_name, columns_sql in create_table_statements:
            # Each table is recreated so reruns start from a known schema shape.
            run_sql(
                session,
                f"CREATE OR REPLACE TABLE {table_name} ({columns_sql})",
                f"py bootstrap {table_name}",
            )


if __name__ == "__main__":
    main()
