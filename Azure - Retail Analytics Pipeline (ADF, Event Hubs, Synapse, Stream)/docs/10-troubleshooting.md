# Troubleshooting

## Event Hubs Connection Issues

- Check that the connection string includes send permissions.
- Confirm the hub name matches the config file.
- Verify outbound network access from the local machine.

## JSON Schema Mismatch

- Ensure event payload fields match `streaming/event_schema.json`.
- Keep event timestamps in ISO 8601 format.
- Confirm numeric fields are not being sent as strings unless the query expects that.

## Stream Analytics Errors

- Validate the query before starting the job.
- Check input alias names and output aliases carefully.
- Confirm the job is reading JSON and using event time correctly.

## Synapse External Table Problems

- Check storage path placeholders and file formats.
- Confirm the workspace identity has ADLS read access.
- Validate that CSV headers align with expected column names.

## ADF Ingestion Failures

- Review linked service authentication.
- Confirm dataset file names exactly match the sample data.
- Check destination folder permissions in ADLS.

## Cost Mistakes

- Stop Stream Analytics after testing.
- Avoid leaving test resources idle for days.
- Use finite producer runs rather than open-ended loops.

## Manual vs Repo Reminder

- Azure connectivity and permissions must be configured manually.
- Repo assets provide the structure, logic, and examples, not full automated deployment.
