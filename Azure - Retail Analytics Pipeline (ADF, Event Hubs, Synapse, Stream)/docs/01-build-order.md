# Build Order

This is the critical path for building the project with minimal wasted spend.

## Goal

Stand up the smallest viable end-to-end flow first, then layer on reporting and streaming.

## Recommended Order

1. Prepare the local repository and Python environment.
2. Create Azure storage, Synapse workspace, Data Factory, Event Hubs, and Stream Analytics resources.
3. Upload or stage sample CSV files.
4. Build and test ADF batch ingestion into `bronze`.
5. Configure Synapse serverless external access and run dimension/fact SQL.
6. Validate gold reporting queries.
7. Configure Event Hubs send access and confirm connectivity from the local machine.
8. Deploy the Stream Analytics funnel query and output sink.
9. Run the producer to generate streaming events.
10. Capture screenshots and shut services down.

## What Is Done Locally

- Clone or open this repository
- Create the Python virtual environment
- Generate or validate sample data
- Run event producer scripts

## What Is Done In Azure Portal

- Provision the Azure resources listed in `docs/03-azure-portal-tasks.md`

## What Is Done In Service UI

- Configure each service using its dedicated task file:
- `docs/04-data-factory-tasks.md`
- `docs/05-synapse-tasks.md`
- `docs/06-event-hubs-tasks.md`
- `docs/07-stream-analytics-tasks.md`

## What Is Provided In Repo

- Sample source data
- Streaming tools
- SQL scripts
- Example pipeline definitions
- Architecture and runbook documentation

## What Can Be Automated Later

- Resource deployment with Bicep or Terraform
- Pipeline deployment with ARM templates or CI/CD
- SQL deployment through release workflows

## Cost-Safe Build Sequence

- Do not start Stream Analytics until Event Hubs connectivity has been tested.
- Do not create extra Synapse compute because this project uses serverless SQL.
- Keep Event Hubs throughput at the minimum practical level.
- Shut down or delete short-lived resources after each run.
