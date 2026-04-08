-- Step 01: bootstrap the medallion foundation for one twin path.
-- Process:
-- 1. Read the runtime variables declared below.
-- 2. Create the warehouse and the Bronze, Silver, and Gold schemas.
-- 3. Provision shared file formats and stages.
-- 4. Create the base tables that later steps populate and transform.
CREATE WAREHOUSE IF NOT EXISTS IDENTIFIER($project_warehouse)
  WAREHOUSE_SIZE = 'XSMALL'
  AUTO_SUSPEND = 60
  AUTO_RESUME = TRUE
  INITIALLY_SUSPENDED = TRUE;
USE WAREHOUSE IDENTIFIER($project_warehouse);
CREATE DATABASE IF NOT EXISTS IDENTIFIER($project_database);

-- Use dynamic SQL so the output prefix can be reused without rewriting object names by hand.
EXECUTE IMMEDIATE
$$
DECLARE
  bronze_schema STRING := $project_database || '.BRONZE';
  silver_schema STRING := $project_database || '.SILVER';
  gold_schema STRING := $project_database || '.GOLD';
  prefix STRING := $output_prefix;
  orders_csv_ff STRING := bronze_schema || '.orders_csv_ff';
  customer_bronze_table STRING := bronze_schema || '.' || prefix || '_customer_bronze';
  orders_bronze_table STRING := bronze_schema || '.' || prefix || '_orders_bronze';
  lineitem_bronze_table STRING := bronze_schema || '.' || prefix || '_lineitem_bronze';
  snowpipe_orders_bronze_table STRING := bronze_schema || '.' || prefix || '_snowpipe_orders_bronze';
  customer_silver_table STRING := silver_schema || '.' || prefix || '_customer_silver';
  orders_silver_table STRING := silver_schema || '.' || prefix || '_orders_silver';
  lineitem_silver_table STRING := silver_schema || '.' || prefix || '_lineitem_silver';
  mock_orders_silver_table STRING := silver_schema || '.' || prefix || '_mock_orders_silver';
  order_daily_gold_table STRING := gold_schema || '.' || prefix || '_order_daily_gold';
BEGIN
  -- Create the three medallion schemas before any layer-specific objects.
  EXECUTE IMMEDIATE 'CREATE SCHEMA IF NOT EXISTS ' || bronze_schema;
  EXECUTE IMMEDIATE 'CREATE SCHEMA IF NOT EXISTS ' || silver_schema;
  EXECUTE IMMEDIATE 'CREATE SCHEMA IF NOT EXISTS ' || gold_schema;

  -- Define the CSV parsing rules once and reuse them for Snowpipe-based S3 loads.
  EXECUTE IMMEDIATE 'CREATE OR REPLACE FILE FORMAT ' || orders_csv_ff || ' '
    || 'TYPE = CSV SKIP_HEADER = 1 FIELD_OPTIONALLY_ENCLOSED_BY = ''"'' NULL_IF = (''NULL'', '''')';

  -- Bronze holds raw landed copies of source data with minimal shaping.
  EXECUTE IMMEDIATE 'CREATE OR REPLACE TABLE ' || customer_bronze_table || ' ('
    || 'cust_key NUMBER, customer_name STRING, customer_address STRING, nation_key NUMBER, nation_name STRING, '
    || 'region_name STRING, phone STRING, account_balance NUMBER(12,2), market_segment STRING, customer_comment STRING)';

  EXECUTE IMMEDIATE 'CREATE OR REPLACE TABLE ' || orders_bronze_table || ' ('
    || 'order_key NUMBER, cust_key NUMBER, order_status STRING, total_price NUMBER(12,2), '
    || 'order_date DATE, order_priority STRING, clerk STRING, ship_priority NUMBER, order_comment STRING)';

  EXECUTE IMMEDIATE 'CREATE OR REPLACE TABLE ' || lineitem_bronze_table || ' ('
    || 'order_key NUMBER, part_key NUMBER, supplier_key NUMBER, line_number NUMBER, quantity NUMBER(12,2), '
    || 'extended_price NUMBER(12,2), discount NUMBER(12,2), tax NUMBER(12,2), return_flag STRING, '
    || 'line_status STRING, ship_date DATE, commit_date DATE, receipt_date DATE, ship_instruct STRING, ship_mode STRING)';

  EXECUTE IMMEDIATE 'CREATE OR REPLACE TABLE ' || snowpipe_orders_bronze_table || ' ('
    || 'order_key NUMBER, cust_key NUMBER, order_status STRING, total_price NUMBER(12,2), '
    || 'order_date DATE, order_priority STRING, clerk STRING, ship_priority NUMBER, '
    || 'order_comment STRING, ingested_at TIMESTAMP_NTZ, source_filename STRING)';

  -- Silver tables store cleaned, business-ready records promoted out of Bronze.
  EXECUTE IMMEDIATE 'CREATE OR REPLACE TABLE ' || customer_silver_table || ' ('
    || 'cust_key NUMBER, customer_name STRING, customer_address STRING, nation_key NUMBER, nation_name STRING, '
    || 'region_name STRING, phone STRING, account_balance NUMBER(12,2), market_segment STRING, customer_comment STRING)';

  EXECUTE IMMEDIATE 'CREATE OR REPLACE TABLE ' || orders_silver_table || ' ('
    || 'order_key NUMBER, cust_key NUMBER, order_status STRING, total_price NUMBER(12,2), '
    || 'order_date DATE, order_priority STRING, clerk STRING, ship_priority NUMBER, order_comment STRING)';

  EXECUTE IMMEDIATE 'CREATE OR REPLACE TABLE ' || lineitem_silver_table || ' ('
    || 'order_key NUMBER, part_key NUMBER, supplier_key NUMBER, line_number NUMBER, quantity NUMBER(12,2), '
    || 'extended_price NUMBER(12,2), discount NUMBER(12,2), tax NUMBER(12,2), return_flag STRING, '
    || 'line_status STRING, ship_date DATE, commit_date DATE, receipt_date DATE, ship_instruct STRING, ship_mode STRING)';

  EXECUTE IMMEDIATE 'CREATE OR REPLACE TABLE ' || mock_orders_silver_table || ' ('
    || 'order_key NUMBER, cust_key NUMBER, order_status STRING, total_price NUMBER(12,2), '
    || 'order_date DATE, order_priority STRING, clerk STRING, ship_priority NUMBER, '
    || 'order_comment STRING, ingested_at TIMESTAMP_NTZ, source_filename STRING, load_method STRING)';

  -- Gold starts with the analytics table that Step 02 refreshes from Silver.
  EXECUTE IMMEDIATE 'CREATE OR REPLACE TABLE ' || order_daily_gold_table || ' ('
    || 'order_date DATE, region_name STRING, market_segment STRING, order_count NUMBER, '
    || 'customer_count NUMBER, gross_order_value NUMBER(18,2), net_line_revenue NUMBER(18,2))';
END;
$$;
