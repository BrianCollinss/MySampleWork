-- Stream Analytics query for 5-minute ecommerce funnel metrics.
-- Input alias: customer-events-input, sourced from Event Hubs.
-- Output alias: funnel-realtime-output, usually written to ADLS Gen2 as CSV.

-- Count key funnel actions within each 5-minute tumbling window.
SELECT
    System.Timestamp() AS window_end_utc,
    SUM(CASE WHEN event_type = 'product_view' THEN 1 ELSE 0 END) AS product_views,
    SUM(CASE WHEN event_type = 'add_to_cart' THEN 1 ELSE 0 END) AS add_to_carts,
    SUM(CASE WHEN event_type = 'checkout_started' THEN 1 ELSE 0 END) AS checkout_started,
    SUM(CASE WHEN event_type = 'order_submitted' THEN 1 ELSE 0 END) AS orders_submitted,
    SUM(CASE WHEN event_type = 'email_clicked' THEN 1 ELSE 0 END) AS email_clicks
INTO [funnel-realtime-output]
FROM [customer-events-input] TIMESTAMP BY event_time
GROUP BY TumblingWindow(minute, 1);
