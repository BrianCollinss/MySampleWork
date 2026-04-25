-- Creates gold reporting views for analytics queries.
-- Run after the silver dimension and fact views have been created.

USE retailanalyticsdemo;
GO

-- Customer 360: combines customer profile and submitted order history.
-- This view is batch-safe and does not require optional raw streaming event files.
CREATE OR ALTER VIEW gold.gold_customer_360 AS
SELECT
    c.customer_id,
    c.first_name,
    c.last_name,
    c.email,
    c.loyalty_tier,
    c.city,
    c.state,
    c.country,
    COUNT(DISTINCT o.order_id) AS total_orders,
    COALESCE(SUM(o.total_amount), 0) AS lifetime_revenue,
    MAX(o.order_date) AS last_order_date
FROM silver.dim_customer c
LEFT JOIN silver.fact_orders o
    ON c.customer_id = o.customer_id
    AND o.order_status = 'Submitted'
GROUP BY
    c.customer_id, c.first_name, c.last_name, c.email, c.loyalty_tier, c.city, c.state, c.country;
GO

-- Daily sales summary: aggregates submitted order revenue by order date.
CREATE OR ALTER VIEW gold.gold_daily_sales_summary AS
SELECT
    order_date,
    COUNT(DISTINCT order_id) AS orders_count,
    COUNT(DISTINCT customer_id) AS purchasing_customers,
    SUM(total_amount) AS gross_sales
FROM silver.fact_orders
WHERE order_status = 'Submitted'
GROUP BY order_date;
GO

-- Product performance: calculates units, revenue, and margin by product.
CREATE OR ALTER VIEW gold.gold_product_performance AS
SELECT
    p.product_id,
    p.product_name,
    p.category,
    p.brand,
    SUM(oi.quantity) AS units_sold,
    SUM(oi.line_amount) AS revenue,
    SUM(oi.line_amount - (oi.quantity * p.unit_cost)) AS gross_margin
FROM silver.fact_order_items oi
INNER JOIN silver.dim_product p
    ON oi.product_id = p.product_id
GROUP BY
    p.product_id, p.product_name, p.category, p.brand;
GO

-- Campaign performance: batch-safe campaign summary.
-- Raw streaming event attribution is optional and not required for this script.
CREATE OR ALTER VIEW gold.gold_campaign_performance AS
SELECT
    c.campaign_id,
    c.campaign_name,
    c.channel,
    c.budget,
    c.start_date,
    c.end_date,
    c.target_audience
FROM silver.dim_campaign c
;
GO

-- Real-time funnel: reads 5-minute Stream Analytics aggregate CSV outputs.
CREATE OR ALTER VIEW gold.gold_funnel_realtime_5min AS
SELECT
    window_end_utc,
    product_views,
    add_to_carts,
    checkout_started,
    orders_submitted,
    CASE WHEN product_views = 0 THEN 0 ELSE CAST(add_to_carts AS decimal(10,4)) / product_views END AS view_to_cart_rate,
    CASE WHEN add_to_carts = 0 THEN 0 ELSE CAST(orders_submitted AS decimal(10,4)) / add_to_carts END AS cart_to_order_rate
FROM
    OPENROWSET(
        BULK 'streaming/funnel_realtime_5min/*.csv',
        DATA_SOURCE = 'ds_gold_retail',
        FORMAT = 'CSV',
        PARSER_VERSION = '2.0',
        HEADER_ROW = TRUE
    ) WITH (
        window_end_utc datetime2,
        product_views int,
        add_to_carts int,
        checkout_started int,
        orders_submitted int,
        email_clicks int
    ) AS src;
GO
