# Runbook

## Goal

Run the batch and streaming flows end to end.

## Start-Up Sequence

1. Confirm sample CSV files exist.
2. Run ADF ingestion pipelines.
3. Execute Synapse SQL silver and gold scripts.
4. Start Stream Analytics job.
5. Run `streaming/producer.py` to send streaming events.
6. Query gold models in Synapse.

## What Is Done Locally

- Activate Python environment
- Run producer script (`streaming/producer.py`)

## What Is Done In Azure Portal

- Verify resource health
- View Event Hubs metrics
- Start or stop the Stream Analytics job if not using the service UI directly

## What Is Done In Service UI

- Trigger ADF master pipeline
- Run Synapse scripts or pipeline
- Query outputs in Synapse Studio

## What Is Provided In Repo

- Batch and streaming artefacts
- SQL and event scripts
- Architecture diagrams

## Outputs To Capture

- Daily sales summary
- Product performance
- Campaign performance
- Customer 360 sample rows
- 5-minute funnel metrics

## Shut Down Steps

- Stop Stream Analytics
- Stop local producer
- Delete or pause resources if no longer needed
- Remove any temporary uploaded files not needed for screenshots
