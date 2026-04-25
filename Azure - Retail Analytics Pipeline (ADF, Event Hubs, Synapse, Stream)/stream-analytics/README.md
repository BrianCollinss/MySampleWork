# Stream Analytics

This service handles near real-time event aggregation for the funnel output.

## Query Output

The provided query calculates 5-minute tumbling window counts for:

- product views
- add to cart actions
- checkout starts
- submitted orders
- email clicks

## What Is Done Locally

Event generation is covered in `streaming/README.md` and `docs/07-stream-analytics-tasks.md`.

## What Is Done In Azure Portal

Resource provisioning is covered in `docs/03-azure-portal-tasks.md`.

## What Is Done In Service UI

Detailed input, output, and query setup is covered in `docs/07-stream-analytics-tasks.md`.

## What Is Provided In Repo

- Stream Analytics query
- Schema-aligned event examples

## What Can Be Automated Later

- Job deployment and configuration templates
