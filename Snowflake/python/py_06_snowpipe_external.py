"""Snowpark setup for the external S3 Snowpipe path.

Step 06 purpose:
1. Create the Bronze external stage that points at the trusted storage integration.
2. Create a Snowpipe that copies arriving S3 files into Bronze.
3. Create a Bronze stream and a Silver merge task for those Snowpipe rows.

Snowpark is used as the Python execution environment, while Snowflake SQL is
still used for account-level and DDL-heavy objects such as stages, pipes,
streams, and tasks.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from snowflake_project.connection import get_snowpark_session
from snowflake_project.project import load_project_settings, run_sql, set_session_context


def main() -> None:
    settings = load_project_settings(output_prefix="py")

    with get_snowpark_session() as session:
        set_session_context(session, settings)

        orders_s3_stage = settings.bronze_object("orders_s3_stage")
        orders_pipe = settings.bronze_object("orders_pipe")
        snowpipe_orders_bronze_table = settings.bronze_object("snowpipe_orders_bronze")
        snowpipe_orders_bronze_stream = settings.bronze_object("snowpipe_orders_bronze_stream")
        mock_orders_silver_table = settings.silver_object("mock_orders_silver")
        snowpipe_task = settings.silver_object("snowpipe_bronze_to_silver_task")

        # The external stage points at the Python twin S3 prefix so Snowpipe only
        # watches files uploaded under `.../py/`.
        run_sql(
            session,
            f"""
            CREATE OR REPLACE STAGE {orders_s3_stage}
            URL = '{settings.s3_stage_url}'
            STORAGE_INTEGRATION = {settings.aws_storage_integration}
            FILE_FORMAT = {settings.bronze_schema}.orders_csv_ff
            """,
            "py create external stage",
        )

        # The pipe handles raw file ingestion into Bronze. Auto-ingest means new
        # S3 notifications should trigger this COPY automatically after setup.
        run_sql(
            session,
            f"""
            CREATE OR REPLACE PIPE {orders_pipe}
            AUTO_INGEST = TRUE
            AWS_SNS_TOPIC = '{settings.aws_sns_topic_arn}'
            AS
            COPY INTO {snowpipe_orders_bronze_table}
            FROM (
              SELECT
                $1::NUMBER AS order_key,
                $2::NUMBER AS cust_key,
                $3::STRING AS order_status,
                $4::NUMBER(12,2) AS total_price,
                $5::DATE AS order_date,
                $6::STRING AS order_priority,
                $7::STRING AS clerk,
                $8::NUMBER AS ship_priority,
                $9::STRING AS order_comment,
                $10::TIMESTAMP_NTZ AS ingested_at,
                METADATA$FILENAME AS source_filename
              FROM @{orders_s3_stage}
            )
            FILE_FORMAT = (FORMAT_NAME = {settings.bronze_schema}.orders_csv_ff)
            """,
            "py create pipe",
        )

        # This stream captures just the new Snowpipe arrivals so the Silver task
        # can process incrementally.
        run_sql(
            session,
            f"CREATE OR REPLACE STREAM {snowpipe_orders_bronze_stream} ON TABLE {snowpipe_orders_bronze_table}",
            "py create snowpipe bronze stream",
        )

        # This task is the Bronze-to-Silver promotion path for Snowpipe-delivered
        # rows.
        run_sql(
            session,
            f"""
            CREATE OR REPLACE TASK {snowpipe_task}
            WAREHOUSE = {settings.project_warehouse}
            SCHEDULE = '1 MINUTE'
            WHEN SYSTEM$STREAM_HAS_DATA('{snowpipe_orders_bronze_stream}')
            AS
            MERGE INTO {mock_orders_silver_table} tgt
            USING (SELECT * FROM {snowpipe_orders_bronze_stream}) src
            ON tgt.order_key = src.order_key
            WHEN MATCHED THEN UPDATE SET
              tgt.cust_key = src.cust_key,
              tgt.order_status = src.order_status,
              tgt.total_price = src.total_price,
              tgt.order_date = src.order_date,
              tgt.order_priority = src.order_priority,
              tgt.clerk = src.clerk,
              tgt.ship_priority = src.ship_priority,
              tgt.order_comment = src.order_comment,
              tgt.ingested_at = src.ingested_at,
              tgt.source_filename = src.source_filename,
              tgt.load_method = 'SNOWPIPE'
            WHEN NOT MATCHED THEN INSERT (
              order_key, cust_key, order_status, total_price, order_date,
              order_priority, clerk, ship_priority, order_comment, ingested_at,
              source_filename, load_method
            )
            VALUES (
              src.order_key, src.cust_key, src.order_status, src.total_price, src.order_date,
              src.order_priority, src.clerk, src.ship_priority, src.order_comment, src.ingested_at,
              src.source_filename, 'SNOWPIPE'
            )
            """,
            "py create snowpipe task",
        )

        # Resume the task so newly ingested Snowpipe rows can promote to Silver automatically.
        run_sql(session, f"ALTER TASK {snowpipe_task} RESUME", "py resume snowpipe task")


if __name__ == "__main__":
    main()
