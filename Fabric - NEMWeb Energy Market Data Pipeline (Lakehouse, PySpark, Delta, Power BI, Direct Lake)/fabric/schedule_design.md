# Schedule Design

Run current-report ingestion every 5 minutes in production. Use a 15-minute schedule during development to reduce noise and avoid unnecessary polling.

Create a separate daily backfill job for archive reports once the current pipeline is stable.

Retain raw ZIP files long enough for audit and replay. Review storage after 30 to 90 days and compact Delta tables as a future maintenance step.

Use respectful polling: filter by lookback, cap `MAX_ZIPS_PER_RUN`, and retry with backoff.
