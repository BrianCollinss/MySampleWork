# Azure Portal Tasks

This file lists the resource provisioning tasks that must be performed manually in Azure.

## What Is Done Locally

- Review naming conventions from the repository
- Prepare sample data and config files

## What Is Done In Azure Portal

- Create a resource group such as `rg-retail-analytics-demo`
- Create `adls` storage with hierarchical namespace enabled
- Create Azure Data Factory
- Create Synapse workspace linked to the storage account
- Create Event Hubs namespace and one event hub
- Create Stream Analytics job
- Optionally create dashboards or workbook views for screenshots

## Recommended Naming Pattern

- Resource group: `rg-retail-analytics-demo`
- Storage account: `stretailanalyticsdemo`
- Data Factory: `adf-retail-analytics-demo`
- Synapse workspace: `syn-retail-analytics-demo`
- Event Hubs namespace: `evh-retail-analytics-demo`
- Event hub: `eh-customer-events`
- Stream Analytics job: `asa-retail-funnel-demo`

## Event Hubs Namespace Settings

Create an **Event Hubs Namespace** with these settings:

- Pricing tier: `Basic`
- Throughput units: `1`
- Region: use the same region as the resource group, such as `Australia East`
- Namespace name: `evh-retail-analytics-demo`

After the namespace is created, create one event hub inside it named `eh-customer-events`.

## What Is Done In Service UI

- Service UI setup is covered in the dedicated service task files

## What Is Provided In Repo

- Example structures, SQL, and queries
- Documentation for each service area

## What Can Be Automated Later

- IaC for all resource deployment
- Role assignment automation
- Pipeline and query deployment
