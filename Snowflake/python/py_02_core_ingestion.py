"""Snowpark Bronze ingestion for the Python pipeline.

Step 02 purpose:
1. Select the core sample tables from `SNOWFLAKE_SAMPLE_DATA.TPCH_SF1`.
2. Copy them into project-owned Bronze tables.
3. Preserve the raw business columns needed by the Silver and Gold steps.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from snowflake.snowpark.functions import col

from snowflake_project.connection import get_snowpark_session
from snowflake_project.project import load_project_settings, set_session_context


def main() -> None:
    settings = load_project_settings(output_prefix="py")

    with get_snowpark_session() as session:
        set_session_context(session, settings)

        # Join TPCH customer, nation, and region so Bronze starts with the same
        # enriched customer landing structure as the SQL pipeline.
        customer_df = (
            session.table("SNOWFLAKE_SAMPLE_DATA.TPCH_SF1.CUSTOMER").alias("c")
            .join(
                session.table("SNOWFLAKE_SAMPLE_DATA.TPCH_SF1.NATION").alias("n"),
                col("c.C_NATIONKEY") == col("n.N_NATIONKEY"),
            )
            .join(
                session.table("SNOWFLAKE_SAMPLE_DATA.TPCH_SF1.REGION").alias("r"),
                col("n.N_REGIONKEY") == col("r.R_REGIONKEY"),
            )
            .select(
                col("c.C_CUSTKEY").alias("CUST_KEY"),
                col("c.C_NAME").alias("CUSTOMER_NAME"),
                col("c.C_ADDRESS").alias("CUSTOMER_ADDRESS"),
                col("c.C_NATIONKEY").alias("NATION_KEY"),
                col("n.N_NAME").alias("NATION_NAME"),
                col("r.R_NAME").alias("REGION_NAME"),
                col("c.C_PHONE").alias("PHONE"),
                col("c.C_ACCTBAL").alias("ACCOUNT_BALANCE"),
                col("c.C_MKTSEGMENT").alias("MARKET_SEGMENT"),
                col("c.C_COMMENT").alias("CUSTOMER_COMMENT"),
            )
        )
        customer_df.write.mode("overwrite").save_as_table(settings.bronze_object("customer_bronze"))

        # Orders are copied from TPCH into the project-owned Bronze table without
        # business rules so Silver can demonstrate the curation step separately.
        orders_df = session.table("SNOWFLAKE_SAMPLE_DATA.TPCH_SF1.ORDERS").select(
            col("O_ORDERKEY").alias("ORDER_KEY"),
            col("O_CUSTKEY").alias("CUST_KEY"),
            col("O_ORDERSTATUS").alias("ORDER_STATUS"),
            col("O_TOTALPRICE").alias("TOTAL_PRICE"),
            col("O_ORDERDATE").alias("ORDER_DATE"),
            col("O_ORDERPRIORITY").alias("ORDER_PRIORITY"),
            col("O_CLERK").alias("CLERK"),
            col("O_SHIPPRIORITY").alias("SHIP_PRIORITY"),
            col("O_COMMENT").alias("ORDER_COMMENT"),
        )
        orders_df.write.mode("overwrite").save_as_table(settings.bronze_object("orders_bronze"))

        # Line items stay in Bronze at transaction grain because Gold later needs
        # them for revenue calculations.
        lineitem_df = session.table("SNOWFLAKE_SAMPLE_DATA.TPCH_SF1.LINEITEM").select(
            col("L_ORDERKEY").alias("ORDER_KEY"),
            col("L_PARTKEY").alias("PART_KEY"),
            col("L_SUPPKEY").alias("SUPPLIER_KEY"),
            col("L_LINENUMBER").alias("LINE_NUMBER"),
            col("L_QUANTITY").alias("QUANTITY"),
            col("L_EXTENDEDPRICE").alias("EXTENDED_PRICE"),
            col("L_DISCOUNT").alias("DISCOUNT"),
            col("L_TAX").alias("TAX"),
            col("L_RETURNFLAG").alias("RETURN_FLAG"),
            col("L_LINESTATUS").alias("LINE_STATUS"),
            col("L_SHIPDATE").alias("SHIP_DATE"),
            col("L_COMMITDATE").alias("COMMIT_DATE"),
            col("L_RECEIPTDATE").alias("RECEIPT_DATE"),
            col("L_SHIPINSTRUCT").alias("SHIP_INSTRUCT"),
            col("L_SHIPMODE").alias("SHIP_MODE"),
        )
        lineitem_df.write.mode("overwrite").save_as_table(settings.bronze_object("lineitem_bronze"))


if __name__ == "__main__":
    main()
