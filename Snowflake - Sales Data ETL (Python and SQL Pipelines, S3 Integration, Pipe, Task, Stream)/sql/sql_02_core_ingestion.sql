-- Step 02: ingest read-only TPCH sample data into Bronze.
-- Process:
-- 1. Select the core sample tables from SNOWFLAKE_SAMPLE_DATA.TPCH_SF1.
-- 2. Copy them into project-owned Bronze tables.
-- 3. Preserve the raw business columns needed by the Silver and Gold steps.
-- Dynamic SQL keeps the target object names compact and debuggable.
EXECUTE IMMEDIATE
$$
DECLARE
  bronze_schema STRING := $project_database || '.BRONZE';
  prefix STRING := $output_prefix;
  customer_bronze_table STRING := bronze_schema || '.' || prefix || '_customer_bronze';
  orders_bronze_table STRING := bronze_schema || '.' || prefix || '_orders_bronze';
  lineitem_bronze_table STRING := bronze_schema || '.' || prefix || '_lineitem_bronze';
  customer_insert_sql STRING;
  orders_insert_sql STRING;
  lineitem_insert_sql STRING;
BEGIN
  -- Load customer geography and segmentation attributes into Bronze.
  customer_insert_sql := 'INSERT OVERWRITE INTO ' || customer_bronze_table || ' '
    || 'SELECT c.c_custkey AS cust_key, c.c_name AS customer_name, c.c_address AS customer_address, '
    || 'c.c_nationkey AS nation_key, n.n_name AS nation_name, r.r_name AS region_name, '
    || 'c.c_phone AS phone, c.c_acctbal AS account_balance, c.c_mktsegment AS market_segment, '
    || 'c.c_comment AS customer_comment '
    || 'FROM SNOWFLAKE_SAMPLE_DATA.TPCH_SF1.CUSTOMER c '
    || 'JOIN SNOWFLAKE_SAMPLE_DATA.TPCH_SF1.NATION n ON c.c_nationkey = n.n_nationkey '
    || 'JOIN SNOWFLAKE_SAMPLE_DATA.TPCH_SF1.REGION r ON n.n_regionkey = r.r_regionkey';
  EXECUTE IMMEDIATE customer_insert_sql;

  -- Load the order header table into Bronze exactly once per refresh.
  orders_insert_sql := 'INSERT OVERWRITE INTO ' || orders_bronze_table || ' '
    || 'SELECT o_orderkey AS order_key, o_custkey AS cust_key, o_orderstatus AS order_status, '
    || 'o_totalprice AS total_price, o_orderdate AS order_date, o_orderpriority AS order_priority, '
    || 'o_clerk AS clerk, o_shippriority AS ship_priority, o_comment AS order_comment '
    || 'FROM SNOWFLAKE_SAMPLE_DATA.TPCH_SF1.ORDERS';
  EXECUTE IMMEDIATE orders_insert_sql;

  -- Load the detailed line items that the Gold revenue metric depends on later.
  lineitem_insert_sql := 'INSERT OVERWRITE INTO ' || lineitem_bronze_table || ' '
    || 'SELECT l_orderkey AS order_key, l_partkey AS part_key, l_suppkey AS supplier_key, '
    || 'l_linenumber AS line_number, l_quantity AS quantity, l_extendedprice AS extended_price, '
    || 'l_discount AS discount, l_tax AS tax, l_returnflag AS return_flag, l_linestatus AS line_status, '
    || 'l_shipdate AS ship_date, l_commitdate AS commit_date, l_receiptdate AS receipt_date, '
    || 'l_shipinstruct AS ship_instruct, l_shipmode AS ship_mode '
    || 'FROM SNOWFLAKE_SAMPLE_DATA.TPCH_SF1.LINEITEM';
  EXECUTE IMMEDIATE lineitem_insert_sql;
END;
$$;
