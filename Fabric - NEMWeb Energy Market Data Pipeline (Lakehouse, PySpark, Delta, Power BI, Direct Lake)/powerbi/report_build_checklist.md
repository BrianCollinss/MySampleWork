# Report Build Checklist

- Use the included `powerbi/powerbi.pbix` as the current Power BI Desktop
  working file.
- Create a Direct Lake semantic model from Fabric Lakehouse Gold tables when a
  Fabric-enabled workspace is available.
- Use Import mode from local/Fabric Gold outputs when Direct Lake is unavailable.
- Mark the date table.
- Create relationships.
- Add DAX measures.
- Configure region, time range, and interval granularity slicers.
- Configure refresh or Direct Lake behaviour.
- Validate latest values against source files.
- Add screenshots to `screenshots/` when report pages are materially updated.
- Publish the report when Fabric licensing and workspace access are available.
- Add data freshness and operations page checks.

The PBIX is a work in progress. More report pages and visuals will be added as
the Gold tables and semantic model mature. The current PBIX was created in Power
BI Desktop because the available Fabric licence was nearing expiry.
