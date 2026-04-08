"""Snowpark validation for the Python Snowpipe path.

Step 07 purpose:
1. Prove the Python S3 stage, pipe, stream, and task exist.
2. Confirm Snowpipe has ingested at least one file into Bronze.
3. Confirm Bronze-to-Silver promotion has produced `load_method = 'SNOWPIPE'` rows.

This step mirrors the SQL validation helper but adds Python-side assertions so a
failed check stops immediately with a clear message.
"""

import json
import sys
from pathlib import Path
from typing import cast

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from snowflake_project.connection import get_snowpark_session
from snowflake_project.project import load_project_settings, run_sql, set_session_context


def require(condition: bool, message: str) -> None:
    """Raise a clear assertion-style error when a validation check fails."""
    if not condition:
        raise AssertionError(message)


def main() -> None:
    settings = load_project_settings(output_prefix="py")

    with get_snowpark_session() as session:
        set_session_context(session, settings)

        orders_s3_stage = settings.bronze_object("orders_s3_stage")
        orders_pipe = settings.bronze_object("orders_pipe")
        snowpipe_orders_bronze_table = settings.bronze_object("snowpipe_orders_bronze")
        snowpipe_orders_bronze_stream = settings.bronze_object("snowpipe_orders_bronze_stream")
        mock_orders_silver_table = settings.silver_object("mock_orders_silver")
        snowpipe_task_name = f"{settings.output_prefix.upper()}_SNOWPIPE_BRONZE_TO_SILVER_TASK"

        run_sql(session, f"ALTER PIPE {orders_pipe} REFRESH", "py validate refresh pipe")

        stage_files = session.sql(f"LIST @{orders_s3_stage}").collect()
        print(f"Stage files: {stage_files}")
        require(len(stage_files) > 0, "No files are visible through the Python Snowpipe external stage.")

        pipes = session.sql(f"SHOW PIPES IN SCHEMA {settings.bronze_schema}").collect()
        stages = session.sql(f"SHOW STAGES IN SCHEMA {settings.bronze_schema}").collect()
        streams = session.sql(f"SHOW STREAMS IN SCHEMA {settings.bronze_schema}").collect()
        tasks = session.sql(f"SHOW TASKS IN SCHEMA {settings.silver_schema}").collect()

        require(any(getattr(row, "name", "").upper() == f"{settings.output_prefix.upper()}_ORDERS_PIPE" for row in pipes),
                "Expected Python Snowpipe pipe was not found.")
        require(any(getattr(row, "name", "").upper() == f"{settings.output_prefix.upper()}_ORDERS_S3_STAGE" for row in stages),
                "Expected Python Snowpipe external stage was not found.")
        require(any(getattr(row, "name", "").upper() == f"{settings.output_prefix.upper()}_SNOWPIPE_ORDERS_BRONZE_STREAM" for row in streams),
                "Expected Python Snowpipe Bronze stream was not found.")
        require(any(getattr(row, "name", "").upper() == snowpipe_task_name for row in tasks),
                "Expected Python Snowpipe Bronze-to-Silver task was not found.")

        pipe_status_row = session.sql(f"SELECT SYSTEM$PIPE_STATUS('{orders_pipe}') AS pipe_status").collect()[0]
        pipe_status_raw = pipe_status_row["PIPE_STATUS"]
        require(isinstance(pipe_status_raw, str), "SYSTEM$PIPE_STATUS did not return a JSON string.")
        pipe_status = json.loads(cast(str, pipe_status_raw))
        print(f"Pipe status: {pipe_status}")
        require(pipe_status.get("executionState") == "RUNNING", "Python Snowpipe is not in RUNNING state.")
        require(pipe_status.get("lastIngestedFilePath"), "Python Snowpipe has not ingested a file yet.")

        bronze_count_raw = session.sql(
            f"SELECT COUNT(*) AS bronze_rows FROM {snowpipe_orders_bronze_table}"
        ).collect()[0]["BRONZE_ROWS"]
        require(isinstance(bronze_count_raw, int), "Bronze row count did not return an integer.")
        bronze_count = cast(int, bronze_count_raw)
        print(f"Bronze row count: {bronze_count}")
        require(bronze_count > 0, "Python Snowpipe Bronze table has zero rows.")

        bronze_sample = session.sql(
            f"SELECT * FROM {snowpipe_orders_bronze_table} ORDER BY ingested_at DESC LIMIT 20"
        ).collect()
        print(f"Bronze sample rows: {bronze_sample}")

        copy_history = session.sql(
            f"""
            SELECT *
            FROM TABLE(
              INFORMATION_SCHEMA.COPY_HISTORY(
                TABLE_NAME => '{snowpipe_orders_bronze_table}',
                START_TIME => DATEADD('day', -1, CURRENT_TIMESTAMP())
              )
            )
            ORDER BY LAST_LOAD_TIME DESC
            """
        ).collect()
        print(f"Copy history: {copy_history}")
        require(len(copy_history) > 0, "Python Snowpipe COPY_HISTORY returned no rows.")
        require(any(getattr(row, "STATUS", "") == "Loaded" for row in copy_history),
                "Python Snowpipe COPY_HISTORY does not show a loaded file.")

        bronze_stream_count_raw = session.sql(
            f"SELECT COUNT(*) AS bronze_stream_rows FROM {snowpipe_orders_bronze_stream}"
        ).collect()[0]["BRONZE_STREAM_ROWS"]
        require(isinstance(bronze_stream_count_raw, int), "Bronze stream row count did not return an integer.")
        bronze_stream_count = cast(int, bronze_stream_count_raw)
        print(f"Bronze stream row count: {bronze_stream_count}")

        task_history = session.sql(
            f"""
            SELECT NAME, STATE, QUERY_ID, SCHEDULED_TIME, COMPLETED_TIME
            FROM TABLE(INFORMATION_SCHEMA.TASK_HISTORY())
            WHERE NAME = '{snowpipe_task_name}'
            ORDER BY SCHEDULED_TIME DESC
            LIMIT 20
            """
        ).collect()
        print(f"Task history: {task_history}")
        require(len(task_history) > 0, "Python Snowpipe task history returned no rows.")

        silver_rows = session.sql(
            f"""
            SELECT *
            FROM {mock_orders_silver_table}
            WHERE load_method = 'SNOWPIPE'
            ORDER BY ingested_at DESC
            LIMIT 20
            """
        ).collect()
        print(f"Silver sample rows: {silver_rows}")
        require(len(silver_rows) > 0, "Python Silver table does not contain SNOWPIPE-promoted rows yet.")

        print("Python Snowpipe validation passed.")


if __name__ == "__main__":
    main()
