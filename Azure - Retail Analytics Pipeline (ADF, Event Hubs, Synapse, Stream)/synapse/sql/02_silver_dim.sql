-- Creates silver dimension views over CSV files landed by ADF in ADLS Gen2.
-- Run after 01_setup.sql and after the raw CSVs have been copied into bronze/ecommerce.

USE retailanalyticsdemo;
GO

-- Customer dimension: cleans signup date and converts marketing opt-in into a flag.
CREATE OR ALTER VIEW silver.dim_customer AS
SELECT
    customer_id,
    first_name,
    last_name,
    email,
    CAST(signup_date AS date) AS signup_date,
    loyalty_tier,
    city,
    state,
    country,
    CASE WHEN marketing_opt_in = 'Y' THEN 1 ELSE 0 END AS marketing_opt_in_flag
FROM
    OPENROWSET(
        BULK 'ecommerce/customers/customers.csv',
        DATA_SOURCE = 'ds_bronze_retail',
        FORMAT = 'CSV',
        PARSER_VERSION = '2.0',
        HEADER_ROW = TRUE
    ) WITH (
        customer_id varchar(20),
        first_name varchar(100),
        last_name varchar(100),
        email varchar(255),
        signup_date varchar(20),
        loyalty_tier varchar(20),
        city varchar(100),
        state varchar(50),
        country varchar(100),
        marketing_opt_in varchar(5)
    ) AS src;
GO

-- Product dimension: parses price/cost fields and active status.
CREATE OR ALTER VIEW silver.dim_product AS
SELECT
    product_id,
    product_name,
    category,
    brand,
    CAST(unit_price AS decimal(10,2)) AS unit_price,
    CAST(unit_cost AS decimal(10,2)) AS unit_cost,
    CASE WHEN is_active = 'Y' THEN 1 ELSE 0 END AS is_active_flag
FROM
    OPENROWSET(
        BULK 'ecommerce/products/products.csv',
        DATA_SOURCE = 'ds_bronze_retail',
        FORMAT = 'CSV',
        PARSER_VERSION = '2.0',
        HEADER_ROW = TRUE
    ) WITH (
        product_id varchar(20),
        product_name varchar(200),
        category varchar(100),
        brand varchar(100),
        unit_price varchar(20),
        unit_cost varchar(20),
        is_active varchar(5)
    ) AS src;
GO

-- Campaign dimension: parses campaign dates and budget from the source CSV.
CREATE OR ALTER VIEW silver.dim_campaign AS
SELECT
    campaign_id,
    campaign_name,
    channel,
    CAST(start_date AS date) AS start_date,
    CAST(end_date AS date) AS end_date,
    CAST(budget AS decimal(12,2)) AS budget,
    target_audience
FROM
    OPENROWSET(
        BULK 'ecommerce/campaigns/campaigns.csv',
        DATA_SOURCE = 'ds_bronze_retail',
        FORMAT = 'CSV',
        PARSER_VERSION = '2.0',
        HEADER_ROW = TRUE
    ) WITH (
        campaign_id varchar(20),
        campaign_name varchar(200),
        channel varchar(50),
        start_date varchar(20),
        end_date varchar(20),
        budget varchar(20),
        target_audience varchar(200)
    ) AS src;
GO

-- Date dimension: generates 2026 calendar dates without recursive CTE syntax.
CREATE OR ALTER VIEW silver.dim_date AS
WITH numbers AS (
    SELECT TOP (365)
        ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) - 1 AS day_offset
    FROM sys.all_objects
), dates AS (
    SELECT DATEADD(day, day_offset, CAST('2026-01-01' AS date)) AS calendar_date
    FROM numbers
)
SELECT
    CAST(FORMAT(calendar_date, 'yyyyMMdd') AS int) AS date_key,
    calendar_date,
    YEAR(calendar_date) AS calendar_year,
    MONTH(calendar_date) AS calendar_month,
    DATENAME(month, calendar_date) AS month_name,
    DATEPART(quarter, calendar_date) AS calendar_quarter,
    DATENAME(weekday, calendar_date) AS weekday_name
FROM dates;
GO
