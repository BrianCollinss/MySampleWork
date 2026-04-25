# Synapse Pipelines

Synapse pipelines are optional in this project, but useful for showing orchestration inside the analytics workspace.

## Recommended Use

- Run `02_silver_dim.sql` first
- Run `03_silver_facts.sql` second
- Run `04_gold.sql` third
- Finish with `05_validation.sql`

## Why Not Use Synapse Pipelines For Ingestion

ADF is already the ingestion service in this design. Using Synapse pipelines for the same job would blur responsibilities and make the story harder to explain.

## What Is Done Locally

- Review the execution order and dependencies

## What Is Done In Azure Portal

- Nothing specific beyond workspace creation

## What Is Done In Service UI

- Create SQL script activities and dependencies in Synapse Studio

## What Is Provided In Repo

- SQL artefacts and execution guidance

## What Can Be Automated Later

- Release-driven deployment of pipeline JSON and SQL assets
