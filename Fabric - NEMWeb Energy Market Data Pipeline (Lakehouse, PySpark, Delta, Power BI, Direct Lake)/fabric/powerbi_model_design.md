# Power BI Model Design

Use Direct Lake over Gold tables where possible. Keep Power BI transformations minimal and push shaping into Gold tables.

Core facts are region 5-minute, region 30-minute, region daily, price spikes, generation mix, interconnector flows, dashboard KPIs, and data freshness.

Dimensions are region, date, time, price band, and fuel type where available.
