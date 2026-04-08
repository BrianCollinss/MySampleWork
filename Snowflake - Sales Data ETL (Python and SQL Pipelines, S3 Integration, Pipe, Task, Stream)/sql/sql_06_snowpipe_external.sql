-- Step 06: configure the external S3 ingestion path after AWS trust is ready.
-- Process:
-- 1. Create the Bronze external stage that points at the trusted storage integration.
-- 2. Create a Snowpipe that copies arriving S3 files into Bronze.
-- 3. Create a Bronze stream and a Silver merge task for those Snowpipe rows.
--
-- Prerequisite:
-- Run Step 04, then Step 05, then update AWS trust before running this file.
-- Dynamic SQL keeps the S3 stage, pipe, stream, and task names explicit and easy to inspect.
EXECUTE IMMEDIATE
$$
DECLARE
  bronze_schema STRING := $project_database || '.BRONZE';
  silver_schema STRING := $project_database || '.SILVER';
  prefix STRING := $output_prefix;
  stage_url STRING := RTRIM($aws_s3_bucket_url, '/') || '/' || prefix || '/';
  orders_csv_ff STRING := bronze_schema || '.orders_csv_ff';
  orders_s3_stage STRING := bronze_schema || '.' || prefix || '_orders_s3_stage';
  orders_pipe STRING := bronze_schema || '.' || prefix || '_orders_pipe';
  snowpipe_orders_bronze_table STRING := bronze_schema || '.' || prefix || '_snowpipe_orders_bronze';
  snowpipe_orders_bronze_stream STRING := bronze_schema || '.' || prefix || '_snowpipe_orders_bronze_stream';
  mock_orders_silver_table STRING := silver_schema || '.' || prefix || '_mock_orders_silver';
  snowpipe_task STRING := silver_schema || '.' || prefix || '_snowpipe_bronze_to_silver_task';
  create_stage_sql STRING;
  create_pipe_sql STRING;
  create_stream_sql STRING;
  create_task_sql STRING;
  resume_task_sql STRING;
BEGIN
  -- Create the Bronze external stage that reads files from the twin-specific S3 prefix.
  create_stage_sql := 'CREATE OR REPLACE STAGE ' || orders_s3_stage || ' '
    || 'URL = ''' || stage_url || ''' '
    || 'STORAGE_INTEGRATION = ' || $aws_storage_integration || ' '
    || 'FILE_FORMAT = ' || orders_csv_ff;
  EXECUTE IMMEDIATE create_stage_sql;

  -- Snowpipe copies raw arriving files into Bronze as soon as S3 notifications reach Snowflake.
  create_pipe_sql := 'CREATE OR REPLACE PIPE ' || orders_pipe || ' '
    || 'AUTO_INGEST = TRUE AWS_SNS_TOPIC = ''' || $aws_sns_topic_arn || ''' '
    || 'AS COPY INTO ' || snowpipe_orders_bronze_table || ' '
    || 'FROM (SELECT '
    || '$1::NUMBER AS order_key, '
    || '$2::NUMBER AS cust_key, '
    || '$3::STRING AS order_status, '
    || '$4::NUMBER(12,2) AS total_price, '
    || '$5::DATE AS order_date, '
    || '$6::STRING AS order_priority, '
    || '$7::STRING AS clerk, '
    || '$8::NUMBER AS ship_priority, '
    || '$9::STRING AS order_comment, '
    || '$10::TIMESTAMP_NTZ AS ingested_at, '
    || 'METADATA$FILENAME AS source_filename '
    || 'FROM @' || orders_s3_stage || ') '
    || 'FILE_FORMAT = (FORMAT_NAME = ' || orders_csv_ff || ')';
  EXECUTE IMMEDIATE create_pipe_sql;

  -- The stream isolates only the newly landed Snowpipe rows for downstream promotion.
  create_stream_sql := 'CREATE OR REPLACE STREAM ' || snowpipe_orders_bronze_stream || ' ON TABLE ' || snowpipe_orders_bronze_table;
  EXECUTE IMMEDIATE create_stream_sql;

  -- The Silver task merges Snowpipe arrivals into the serving table used by this external ingestion path.
  create_task_sql := 'CREATE OR REPLACE TASK ' || snowpipe_task || ' '
    || 'WAREHOUSE = ' || $project_warehouse || ' '
    || 'SCHEDULE = ''1 MINUTE'' '
    || 'WHEN SYSTEM$STREAM_HAS_DATA(''' || snowpipe_orders_bronze_stream || ''') '
    || 'AS MERGE INTO ' || mock_orders_silver_table || ' tgt '
    || 'USING (SELECT * FROM ' || snowpipe_orders_bronze_stream || ') src '
    || 'ON tgt.order_key = src.order_key '
    || 'WHEN MATCHED THEN UPDATE SET '
    || 'tgt.cust_key = src.cust_key, tgt.order_status = src.order_status, tgt.total_price = src.total_price, '
    || 'tgt.order_date = src.order_date, tgt.order_priority = src.order_priority, tgt.clerk = src.clerk, '
    || 'tgt.ship_priority = src.ship_priority, tgt.order_comment = src.order_comment, '
    || 'tgt.ingested_at = src.ingested_at, tgt.source_filename = src.source_filename, '
    || 'tgt.load_method = ''SNOWPIPE'' '
    || 'WHEN NOT MATCHED THEN INSERT (order_key, cust_key, order_status, total_price, order_date, '
    || 'order_priority, clerk, ship_priority, order_comment, ingested_at, source_filename, load_method) '
    || 'VALUES (src.order_key, src.cust_key, src.order_status, src.total_price, src.order_date, '
    || 'src.order_priority, src.clerk, src.ship_priority, src.order_comment, src.ingested_at, '
    || 'src.source_filename, ''SNOWPIPE'')';
  EXECUTE IMMEDIATE create_task_sql;

  -- Resume the task so Snowpipe-delivered Bronze rows can flow through to Silver automatically.
  resume_task_sql := 'ALTER TASK ' || snowpipe_task || ' RESUME';
  EXECUTE IMMEDIATE resume_task_sql;
END;
$$;
