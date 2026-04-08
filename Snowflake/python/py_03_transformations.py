"""Snowpark Silver and Gold transformations for the Python twin path.

Step 03 purpose:
1. Apply lightweight quality filters while moving tables from Bronze to Silver.
2. Build the Gold daily metric table from the Silver layer.
3. Publish Gold views that summarize pipeline health and high-value customers.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from snowflake.snowpark.functions import col, count_distinct, sum as sum_

from snowflake_project.connection import get_snowpark_session
from snowflake_project.project import load_project_settings, run_sql, set_session_context


def main() -> None:
    settings = load_project_settings(output_prefix="py")

    with get_snowpark_session() as session:
        set_session_context(session, settings)

        # Silver keeps only business-usable customer rows and preserves the
        # curated schema expected by downstream joins.
        customer_silver_df = (
            session.table(settings.bronze_object("customer_bronze"))
            .filter(col("ACCOUNT_BALANCE").is_not_null())
            .select(
                "CUST_KEY",
                "CUSTOMER_NAME",
                "CUSTOMER_ADDRESS",
                "NATION_KEY",
                "NATION_NAME",
                "REGION_NAME",
                "PHONE",
                "ACCOUNT_BALANCE",
                "MARKET_SEGMENT",
                "CUSTOMER_COMMENT",
            )
        )
        customer_silver_df.write.mode("overwrite").save_as_table(settings.silver_object("customer_silver"))

        # Orders with invalid pricing are filtered out here so the Gold layer can
        # trust Silver as the curated source of truth.
        orders_silver_df = (
            session.table(settings.bronze_object("orders_bronze"))
            .filter(col("TOTAL_PRICE") >= 0)
            .select(
                "ORDER_KEY",
                "CUST_KEY",
                "ORDER_STATUS",
                "TOTAL_PRICE",
                "ORDER_DATE",
                "ORDER_PRIORITY",
                "CLERK",
                "SHIP_PRIORITY",
                "ORDER_COMMENT",
            )
        )
        orders_silver_df.write.mode("overwrite").save_as_table(settings.silver_object("orders_silver"))

        # Silver line items exclude non-positive quantities before any revenue
        # metrics are calculated.
        lineitem_silver_df = (
            session.table(settings.bronze_object("lineitem_bronze"))
            .filter(col("QUANTITY") > 0)
            .select(
                "ORDER_KEY",
                "PART_KEY",
                "SUPPLIER_KEY",
                "LINE_NUMBER",
                "QUANTITY",
                "EXTENDED_PRICE",
                "DISCOUNT",
                "TAX",
                "RETURN_FLAG",
                "LINE_STATUS",
                "SHIP_DATE",
                "COMMIT_DATE",
                "RECEIPT_DATE",
                "SHIP_INSTRUCT",
                "SHIP_MODE",
            )
        )
        lineitem_silver_df.write.mode("overwrite").save_as_table(settings.silver_object("lineitem_silver"))

        # Gold aggregates combine curated orders, customers, and line items into
        # a daily analytical table keyed by order date, region, and segment.
        order_daily_gold_df = (
            orders_silver_df.alias("o")
            .join(customer_silver_df.alias("c"), col("o.CUST_KEY") == col("c.CUST_KEY"))
            .join(lineitem_silver_df.alias("li"), col("o.ORDER_KEY") == col("li.ORDER_KEY"))
            .group_by(col("o.ORDER_DATE"), col("c.REGION_NAME"), col("c.MARKET_SEGMENT"))
            .agg(
                count_distinct(col("o.ORDER_KEY")).alias("ORDER_COUNT"),
                count_distinct(col("o.CUST_KEY")).alias("CUSTOMER_COUNT"),
                sum_(col("o.TOTAL_PRICE")).alias("GROSS_ORDER_VALUE"),
                sum_(col("li.EXTENDED_PRICE") * (1 - col("li.DISCOUNT"))).alias("NET_LINE_REVENUE"),
            )
            .select(
                col("O.ORDER_DATE").alias("ORDER_DATE"),
                col("C.REGION_NAME").alias("REGION_NAME"),
                col("C.MARKET_SEGMENT").alias("MARKET_SEGMENT"),
                col("ORDER_COUNT"),
                col("CUSTOMER_COUNT"),
                col("GROSS_ORDER_VALUE"),
                col("NET_LINE_REVENUE"),
            )
        )
        order_daily_gold_df.write.mode("overwrite").save_as_table(settings.gold_object("order_daily_gold"))

        # This Gold view highlights the highest-value customers for showcase
        # queries and dashboarding.
        run_sql(
            session,
            f"""
            CREATE OR REPLACE VIEW {settings.gold_object('high_value_customers_gold_v')} AS
            SELECT
              c.cust_key,
              c.customer_name,
              c.region_name,
              c.market_segment,
              SUM(o.total_price) AS lifetime_order_value,
              COUNT(DISTINCT o.order_key) AS total_orders
            FROM {settings.silver_object('customer_silver')} c
            JOIN {settings.silver_object('orders_silver')} o
              ON c.cust_key = o.cust_key
            GROUP BY 1, 2, 3, 4
            HAVING SUM(o.total_price) >= 750000
            """,
            "py gold high value customers view",
        )

        # The summary view is a lightweight parity check that reports row counts
        # across the medallion layers for the Python twin path.
        run_sql(
            session,
            f"""
            CREATE OR REPLACE VIEW {settings.gold_object('medallion_summary_gold_v')} AS
            SELECT
              '{settings.output_prefix}' AS output_prefix,
              (SELECT COUNT(*) FROM {settings.bronze_object('customer_bronze')}) AS bronze_customer_rows,
              (SELECT COUNT(*) FROM {settings.bronze_object('orders_bronze')}) AS bronze_order_rows,
              (SELECT COUNT(*) FROM {settings.silver_object('customer_silver')}) AS silver_customer_rows,
              (SELECT COUNT(*) FROM {settings.silver_object('orders_silver')}) AS silver_order_rows,
              (SELECT COUNT(*) FROM {settings.silver_object('mock_orders_silver')}) AS silver_mock_order_rows,
              (SELECT COUNT(*) FROM {settings.gold_object('order_daily_gold')}) AS gold_metric_rows
            """,
            "py gold summary view",
        )


if __name__ == "__main__":
    main()
