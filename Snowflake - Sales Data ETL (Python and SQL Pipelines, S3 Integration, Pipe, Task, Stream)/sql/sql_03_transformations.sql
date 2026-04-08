-- Step 03: promote Bronze data into Silver and Gold.
-- Process:
-- 1. Apply lightweight quality filters while moving tables from Bronze to Silver.
-- 2. Build the Gold daily metric table from the Silver layer.
-- 3. Publish Gold views that summarize pipeline health and high-value customers.
-- Keep the object names parameterized so the generated statements stay compact and readable.
EXECUTE IMMEDIATE
$$
DECLARE
  bronze_schema STRING := $project_database || '.BRONZE';
  silver_schema STRING := $project_database || '.SILVER';
  gold_schema STRING := $project_database || '.GOLD';
  prefix STRING := $output_prefix;
  customer_bronze_table STRING := bronze_schema || '.' || prefix || '_customer_bronze';
  orders_bronze_table STRING := bronze_schema || '.' || prefix || '_orders_bronze';
  lineitem_bronze_table STRING := bronze_schema || '.' || prefix || '_lineitem_bronze';
  customer_silver_table STRING := silver_schema || '.' || prefix || '_customer_silver';
  orders_silver_table STRING := silver_schema || '.' || prefix || '_orders_silver';
  lineitem_silver_table STRING := silver_schema || '.' || prefix || '_lineitem_silver';
  mock_orders_silver_table STRING := silver_schema || '.' || prefix || '_mock_orders_silver';
  order_daily_gold_table STRING := gold_schema || '.' || prefix || '_order_daily_gold';
  high_value_customers_view STRING := gold_schema || '.' || prefix || '_high_value_customers_gold_v';
  medallion_summary_view STRING := gold_schema || '.' || prefix || '_medallion_summary_gold_v';
  customer_silver_sql STRING;
  orders_silver_sql STRING;
  lineitem_silver_sql STRING;
  order_daily_gold_sql STRING;
  high_value_customers_sql STRING;
  medallion_summary_sql STRING;
BEGIN
  -- Keep only customer rows that contain usable financial context.
  customer_silver_sql := 'INSERT OVERWRITE INTO ' || customer_silver_table || ' '
    || 'SELECT cust_key, customer_name, customer_address, nation_key, nation_name, region_name, '
    || 'phone, account_balance, market_segment, customer_comment '
    || 'FROM ' || customer_bronze_table || ' '
    || 'WHERE account_balance IS NOT NULL';
  EXECUTE IMMEDIATE customer_silver_sql;

  -- Retain non-negative orders before the analytics layer consumes them.
  orders_silver_sql := 'INSERT OVERWRITE INTO ' || orders_silver_table || ' '
    || 'SELECT order_key, cust_key, order_status, total_price, order_date, order_priority, clerk, '
    || 'ship_priority, order_comment '
    || 'FROM ' || orders_bronze_table || ' '
    || 'WHERE total_price >= 0';
  EXECUTE IMMEDIATE orders_silver_sql;

  -- Discard zero-quantity lines so downstream revenue calculations remain meaningful.
  lineitem_silver_sql := 'INSERT OVERWRITE INTO ' || lineitem_silver_table || ' '
    || 'SELECT order_key, part_key, supplier_key, line_number, quantity, extended_price, discount, tax, '
    || 'return_flag, line_status, ship_date, commit_date, receipt_date, ship_instruct, ship_mode '
    || 'FROM ' || lineitem_bronze_table || ' '
    || 'WHERE quantity > 0';
  EXECUTE IMMEDIATE lineitem_silver_sql;

  -- Aggregate the Silver layer into a Gold daily fact table grouped by date, region, and segment.
  order_daily_gold_sql := 'INSERT OVERWRITE INTO ' || order_daily_gold_table || ' '
    || 'SELECT o.order_date, c.region_name, c.market_segment, '
    || 'COUNT(DISTINCT o.order_key) AS order_count, '
    || 'COUNT(DISTINCT o.cust_key) AS customer_count, '
    || 'SUM(o.total_price) AS gross_order_value, '
    || 'SUM(li.extended_price * (1 - li.discount)) AS net_line_revenue '
    || 'FROM ' || orders_silver_table || ' o '
    || 'JOIN ' || customer_silver_table || ' c ON o.cust_key = c.cust_key '
    || 'JOIN ' || lineitem_silver_table || ' li ON o.order_key = li.order_key '
    || 'GROUP BY 1, 2, 3';
  EXECUTE IMMEDIATE order_daily_gold_sql;

  -- Expose a reusable Gold view for customer-level order value analysis.
  high_value_customers_sql := 'CREATE OR REPLACE VIEW ' || high_value_customers_view || ' AS '
    || 'SELECT c.cust_key, c.customer_name, c.region_name, c.market_segment, '
    || 'SUM(o.total_price) AS lifetime_order_value, COUNT(DISTINCT o.order_key) AS total_orders '
    || 'FROM ' || customer_silver_table || ' c '
    || 'JOIN ' || orders_silver_table || ' o ON c.cust_key = o.cust_key '
    || 'GROUP BY 1, 2, 3, 4 '
    || 'HAVING SUM(o.total_price) >= 750000';
  EXECUTE IMMEDIATE high_value_customers_sql;

  -- Publish a compact row-count summary so the two pipelines can be compared quickly.
  medallion_summary_sql := 'CREATE OR REPLACE VIEW ' || medallion_summary_view || ' AS '
    || 'SELECT '''
    || prefix
    || ''' AS output_prefix, '
    || '(SELECT COUNT(*) FROM ' || customer_bronze_table || ') AS bronze_customer_rows, '
    || '(SELECT COUNT(*) FROM ' || orders_bronze_table || ') AS bronze_order_rows, '
    || '(SELECT COUNT(*) FROM ' || customer_silver_table || ') AS silver_customer_rows, '
    || '(SELECT COUNT(*) FROM ' || orders_silver_table || ') AS silver_order_rows, '
    || '(SELECT COUNT(*) FROM ' || mock_orders_silver_table || ') AS silver_mock_order_rows, '
    || '(SELECT COUNT(*) FROM ' || order_daily_gold_table || ') AS gold_metric_rows';
  EXECUTE IMMEDIATE medallion_summary_sql;
END;
$$;
