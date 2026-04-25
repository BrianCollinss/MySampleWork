-- Creates silver fact views over batch CSV files and optional streaming raw events.
-- Run after 01_setup.sql and 02_silver_dim.sql.

USE retailanalyticsdemo;
GO

-- Order fact: one row per order with parsed dates and order totals.
CREATE OR ALTER VIEW silver.fact_orders AS
SELECT
    order_id,
    customer_id,
    CAST(order_date AS date) AS order_date,
    order_status,
    payment_method,
    shipping_country,
    shipping_state,
    CAST(total_amount AS decimal(12,2)) AS total_amount
FROM
    OPENROWSET(
        BULK 'ecommerce/orders/orders.csv',
        DATA_SOURCE = 'ds_bronze_retail',
        FORMAT = 'CSV',
        PARSER_VERSION = '2.0',
        HEADER_ROW = TRUE
    ) WITH (
        order_id varchar(20),
        customer_id varchar(20),
        order_date varchar(20),
        order_status varchar(50),
        payment_method varchar(50),
        shipping_country varchar(100),
        shipping_state varchar(50),
        total_amount varchar(20)
    ) AS src;
GO

-- Order item fact: one row per product line item with parsed quantity and amounts.
CREATE OR ALTER VIEW silver.fact_order_items AS
SELECT
    order_item_id,
    order_id,
    product_id,
    CAST(quantity AS int) AS quantity,
    CAST(unit_price AS decimal(10,2)) AS unit_price,
    CAST(line_amount AS decimal(12,2)) AS line_amount
FROM
    OPENROWSET(
        BULK 'ecommerce/order_items/order_items.csv',
        DATA_SOURCE = 'ds_bronze_retail',
        FORMAT = 'CSV',
        PARSER_VERSION = '2.0',
        HEADER_ROW = TRUE
    ) WITH (
        order_item_id varchar(20),
        order_id varchar(20),
        product_id varchar(20),
        quantity varchar(20),
        unit_price varchar(20),
        line_amount varchar(20)
    ) AS src;
GO

-- Campaign engagement fact: reads optional raw JSON event files from streaming output.
-- This view can be created before files exist, but querying it requires matching JSON files in ADLS.
CREATE OR ALTER VIEW silver.fact_campaign_engagement AS
SELECT
    JSON_VALUE(doc, '$.event_id') AS event_id,
    TRY_CAST(JSON_VALUE(doc, '$.event_time') AS datetime2) AS event_time,
    JSON_VALUE(doc, '$.event_type') AS event_type,
    JSON_VALUE(doc, '$.customer_id') AS customer_id,
    JSON_VALUE(doc, '$.campaign_id') AS campaign_id,
    JSON_VALUE(doc, '$.channel') AS channel,
    JSON_VALUE(doc, '$.product_id') AS product_id,
    TRY_CAST(JSON_VALUE(doc, '$.event_value') AS decimal(12,2)) AS event_value
FROM
    OPENROWSET(
        BULK 'streaming/raw-events/*.json',
        DATA_SOURCE = 'ds_bronze_retail',
        FORMAT = 'CSV',
        FIELDTERMINATOR = '0x0b',
        FIELDQUOTE = '0x0b'
    ) WITH (
        doc varchar(max)
    ) AS src
WHERE JSON_VALUE(doc, '$.campaign_id') IS NOT NULL;
GO
