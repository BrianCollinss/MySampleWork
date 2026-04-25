-- Validation checks for the Synapse views.
-- Batch checks should work after ADF ingestion and silver/gold scripts run.
-- Streaming-dependent checks are commented out until Event Hubs/Stream Analytics outputs exist.

USE retailanalyticsdemo;
GO

-- Expected: 10 customers from sample-data/raw/customers.csv.
SELECT COUNT(*) AS customers_count FROM silver.dim_customer;

-- Expected: 10 products from sample-data/raw/products.csv.
SELECT COUNT(*) AS products_count FROM silver.dim_product;

-- Expected: 8 orders from sample-data/raw/orders.csv.
SELECT COUNT(*) AS orders_count FROM silver.fact_orders;

-- Expected: 8 order items from sample-data/raw/order_items.csv.
SELECT COUNT(*) AS order_items_count FROM silver.fact_order_items;

-- Expected: up to 10 customer rows, sorted by highest lifetime_revenue first.
SELECT TOP 10 * FROM gold.gold_customer_360 ORDER BY lifetime_revenue DESC;

-- Expected: up to 10 product rows, sorted by highest revenue first.
SELECT TOP 10 * FROM gold.gold_product_performance ORDER BY revenue DESC;

-- Expected: campaign rows from the batch campaign dimension.
SELECT TOP 10 * FROM gold.gold_campaign_performance ORDER BY campaign_id;

-- Expected: recent 5-minute funnel windows if Stream Analytics has written output files; empty is OK before streaming output exists.
-- Requires gold/streaming/funnel_realtime_5min/*.csv.
-- Run this after Stream Analytics has written output files.
-- SELECT TOP 10 * FROM gold.gold_funnel_realtime_5min ORDER BY window_end_utc DESC;
