-- Step 07: run a full post-submission validation for the SQL Snowpipe path.
-- Process:
-- 1. Build the SQL-path object names from the current session variables.
-- 2. List the external stage contents to prove the uploaded S3 file is visible.
-- 3. Show the pipe status and refresh once so testing does not depend on SNS timing.
-- 4. Check Bronze row counts, sample Bronze rows, and recent copy history.
-- 5. Check the Snowpipe Silver task and sample the promoted Silver rows.
--
-- This file is intentionally written as plain SQL statements instead of a
-- scripting block so each check returns visible output in Snowsight or the
-- Snowflake extension.
--
-- Success guide:
-- - `LIST @stage` should show at least one `.csv.gz` file under the `sql/` prefix.
-- - `ALTER PIPE ... REFRESH` should complete without an authorization or path error.
-- - `SHOW PIPES/STAGES/STREAMS/TASKS` should include the `sql_` objects created in Step 06.
-- - `SYSTEM$PIPE_STATUS(...)` should show `RUNNING`, and after a successful load it should
--   include a non-null `lastIngestedFilePath` and `lastIngestedTimestamp`.
-- - Bronze row count should be greater than `0` after the uploaded file is ingested.
-- - Bronze sample rows should show the uploaded order data and a `source_filename`.
-- - `COPY_HISTORY` should show `STATUS = Loaded` with matching row counts and the SQL pipe name.
-- - Bronze stream row count may be `0` if the task has already consumed the stream, or greater
--   than `0` if rows are still waiting to be promoted.
-- - `TASK_HISTORY` should show recent runs for `SQL_SNOWPIPE_BRONZE_TO_SILVER_TASK` in a
--   successful state after Bronze receives data.
-- - Silver sample rows should show `load_method = 'SNOWPIPE'` for the promoted records.

SET bronze_schema = $project_database || '.BRONZE';
SET silver_schema = $project_database || '.SILVER';
SET orders_s3_stage = $bronze_schema || '.' || $output_prefix || '_orders_s3_stage';
SET orders_pipe = $bronze_schema || '.' || $output_prefix || '_orders_pipe';
SET snowpipe_orders_bronze_table = $bronze_schema || '.' || $output_prefix || '_snowpipe_orders_bronze';
SET snowpipe_orders_bronze_stream = $bronze_schema || '.' || $output_prefix || '_snowpipe_orders_bronze_stream';
SET mock_orders_silver_table = $silver_schema || '.' || $output_prefix || '_mock_orders_silver';
SET snowpipe_task_name = UPPER($output_prefix) || '_SNOWPIPE_BRONZE_TO_SILVER_TASK';
SET list_stage_sql = 'LIST @' || $orders_s3_stage;
SET refresh_pipe_sql = 'ALTER PIPE ' || $orders_pipe || ' REFRESH';
SET show_pipes_sql = 'SHOW PIPES IN SCHEMA ' || $bronze_schema;
SET show_stages_sql = 'SHOW STAGES IN SCHEMA ' || $bronze_schema;
SET show_streams_sql = 'SHOW STREAMS IN SCHEMA ' || $bronze_schema;
SET show_tasks_sql = 'SHOW TASKS IN SCHEMA ' || $silver_schema;
SET bronze_count_sql = 'SELECT COUNT(*) AS bronze_rows FROM ' || $snowpipe_orders_bronze_table;
SET bronze_sample_sql = 'SELECT * FROM ' || $snowpipe_orders_bronze_table || ' ORDER BY ingested_at DESC LIMIT 20';
SET bronze_stream_count_sql = 'SELECT COUNT(*) AS bronze_stream_rows FROM ' || $snowpipe_orders_bronze_stream;
SET silver_sample_sql =
  'SELECT * FROM ' || $mock_orders_silver_table ||
  ' WHERE load_method = ''SNOWPIPE'' ORDER BY ingested_at DESC LIMIT 20';

-- Confirm that the uploaded S3 files are visible through the external stage.
-- Success: at least one file appears, typically `orders_batch_*.csv.gz`.
EXECUTE IMMEDIATE $list_stage_sql;

-- Refresh once so this validation also works when SNS delivery is slightly delayed.
-- Success: the command finishes cleanly; it does not need to report that a new file was found.
EXECUTE IMMEDIATE $refresh_pipe_sql;

-- Show the core objects that Step 06 was expected to create.
EXECUTE IMMEDIATE $show_pipes_sql;
-- Success: the pipe list includes `sql_orders_pipe`.
EXECUTE IMMEDIATE $show_stages_sql;
-- Success: the stage list includes `sql_orders_s3_stage`.
EXECUTE IMMEDIATE $show_streams_sql;
-- Success: the stream list includes `sql_snowpipe_orders_bronze_stream`.
EXECUTE IMMEDIATE $show_tasks_sql;
-- Success: the task list includes `sql_snowpipe_bronze_to_silver_task`.

-- Inspect the pipe runtime state after the refresh request.
-- Success: `executionState` is `RUNNING`; after ingestion, `lastIngestedFilePath` is populated.
SELECT SYSTEM$PIPE_STATUS($orders_pipe) AS pipe_status;

-- Prove that rows landed in Bronze.
-- Success: `bronze_rows` is greater than `0`.
EXECUTE IMMEDIATE $bronze_count_sql;
-- Success: sample rows are returned and include order values plus `source_filename`.
EXECUTE IMMEDIATE $bronze_sample_sql;

-- Show the recent Snowpipe load history for the Bronze table.
-- Success: at least one row appears with `STATUS = Loaded`, the SQL pipe name, and matching row counts.
SELECT *
FROM TABLE(
  INFORMATION_SCHEMA.COPY_HISTORY(
    TABLE_NAME => $snowpipe_orders_bronze_table,
    START_TIME => DATEADD('day', -1, CURRENT_TIMESTAMP())
  )
)
ORDER BY LAST_LOAD_TIME DESC;

-- Show whether the Bronze stream still has rows waiting for the task.
-- Success: either `0` because the task already consumed the stream, or a positive number if rows are pending.
EXECUTE IMMEDIATE $bronze_stream_count_sql;

-- Inspect recent task runs so we can confirm Bronze-to-Silver promotion happened.
-- Success: recent task runs appear for `SQL_SNOWPIPE_BRONZE_TO_SILVER_TASK` after Bronze ingestion.
SELECT NAME, STATE, QUERY_ID, SCHEDULED_TIME, COMPLETED_TIME
FROM TABLE(INFORMATION_SCHEMA.TASK_HISTORY())
WHERE NAME = $snowpipe_task_name
ORDER BY SCHEDULED_TIME DESC
LIMIT 20;

-- Prove that the promoted rows are visible in Silver too.
-- Success: rows appear with `load_method = 'SNOWPIPE'`.
EXECUTE IMMEDIATE $silver_sample_sql;
