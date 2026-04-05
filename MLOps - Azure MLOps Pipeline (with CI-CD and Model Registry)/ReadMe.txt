project/
│
├── src/
│   ├── train.py
│   ├── evaluate.py
│   ├── register.py
│   ├── deploy.py
│   ├── model_card.py
│   └── __init__.py
│
├── components/
│   ├── train.yml
│   ├── evaluate.yml
│   ├── register.yml
│   ├── deploy.yml
│   └── model_card.yml
│
├── pipeline/
│   └── pipeline.yml          # train + eval + register (+ optional metadata)
│
├── scripts/
│   ├── submit_pipeline.py    # submits training pipeline
│   ├── deploy_latest.py      # deploys latest model to staging
│   └── promote_traffic.py    # staging → production
│
├── data/                     # optional local samples
│
└── .github/
    └── workflows/
        ├── train.yml         # CI: train/eval/register
        ├── deploy.yml        # CD: deploy latest to staging
        └── promote.yml       # CD: promote staging → production

-----------------------------------------------------------------------

High-Level Architecture Diagram

┌──────────────────────────┐
│      Synapse Pipeline    │
│  (Spark Notebook runs)   │
└──────────────┬───────────┘
               │ writes
               ▼
┌──────────────────────────┐
│        ADLS Gen2         │
│    raw → processed data  │
└──────────────┬───────────┘
               │ consumed by
               ▼
┌──────────────────────────┐
│   Azure ML Training Job  │
│   pipeline/pipeline.yml  │
└──────────────┬───────────┘
               │ triggers
               ▼
┌──────────────────────────────────────────────┐
│  TRAIN → EVAL → REGISTER (model versioning)  │
│  train.py / evaluate.py / register.py        │
└──────────────┬───────────────────────────────┘
               │ produces
               ▼
┌──────────────────────────┐
│    Model Registry        │
│   (versioned models)     │
└──────────────┬───────────┘
               │ used by
               ▼
┌──────────────────────────┐
│   Deployment Pipeline    │
│   deploy.yml / deploy.py │
└──────────────┬───────────┘
               │ promotes
               ▼
┌──────────────────────────┐
│   Staging → Production   │
│   promote_traffic.py     │
└──────────────┬───────────┘
               │ generates
               ▼
┌──────────────────────────┐
│   Model Card Generator   │
│   model_card.py          │
└──────────────────────────┘

-----------------------------------------------------------------------

Detailed Stage-by-Stage File Execution Diagram

Stage 0 — Data Prep (Synapse)

Synapse Pipeline
   └── runs Spark Notebook
         └── src: Synapse Notebook (not in repo)
               reads: raw data
               writes: processed data → ADLS

Stage 1 — Training Pipeline Submission (GitHub Actions)

.github/workflows/train.yml
   └── python scripts/submit_pipeline.py
         └── loads pipeline/pipeline.yml

Stage 2 — Azure ML Pipeline Execution

2.1 Training Step

pipeline/pipeline.yml
   └── jobs.train_job
         └── components/train.yml
               └── src/train.py

2.2 Evaluation Step

pipeline/pipeline.yml
   └── jobs.eval_job
         └── components/evaluate.yml
               └── src/evaluate.py

2.3 Registration Step

pipeline/pipeline.yml
   └── jobs.register_job
         └── components/register.yml
               └── src/register.py

Stage 3 — Deployment Pipeline (GitHub Actions)

.github/workflows/deploy.yml
   └── python scripts/deploy_latest.py
         └── calls src/deploy.py
               └── creates/updates staging deployment


Stage 4 — Staging → Production Promotion

.github/workflows/promote.yml
   └── python scripts/promote_traffic.py
         └── updates endpoint traffic
               staging → production

Stage 5 — Model Card Generation (Azure ML Component)

components/model_card.yml
   └── src/model_card.py
         └── calls Azure OpenAI
               generates model_card.md
			   
-----------------------------------------------------------------------

When to Use What

Scenario				Use Spark MLlib		Use sklearn/XGBoost
Distributed training	✔️					❌
Distributed scoring		✔️					❌
Batch inference			✔️					✔️
Real‑time inference		❌					✔️
Online endpoints		❌					✔️
Batch endpoints			✔️					✔️
	
-----------------------------------------------------------------------

Synapse → ADLS → Azure ML Pipeline
        ↓
Train Component
        ↓
Evaluate Component
        ↓
Register Component (versioned)
        ↓
Deploy to Staging (CI/CD)
        ↓
Promote to Production (CI/CD)
        ↓
Generate Model Card (LLM)

-----------------------------------------------------------------------

What’s Different in Spark Versions?

Area			sklearn version		Spark MLlib version
Data			pandas				Spark DataFrame
Features		manual arrays		VectorAssembler
Model			sklearn	MLlib 		PipelineModel
Save			joblib				model.write().save()
Eval			sklearn.metrics	R	egressionEvaluator
Deployment		real-time endpoint	batch endpoint / Spark job
Compute			CPU/GPU				Spark cluster
Environment		sklearn image		Spark image

-----------------------------------------------------------------------

Where Each Value Comes From

Value				Where You Get It							Azure ML Studio Location
					Endpoint → Consume							REST endpoint
api_key				Endpoint → Consume							Primary/Secondary key
payload				Endpoint → Consume OR Swagger OR score.py	Example request
deployment name		Endpoint → Deployments						Deployment list
model version		Models → Your Model							Version list

-----------------------------------------------------------------------

How to test the endpoint from CLI?

az ml online-endpoint invoke \
  --name house-price-endpoint \
  --request-file sample.json

-----------------------------------------------------------------------

How to test the endpoint from Python SDK?

from azure.ai.ml import MLClient
from azure.identity import DefaultAzureCredential

ml_client = MLClient(DefaultAzureCredential(), subscription, rg, ws)

response = ml_client.online_endpoints.invoke(
    endpoint_name="house-price-endpoint",
    request_file="sample.json"
)

print(response)

-----------------------------------------------------------------------

Before start: 

1. Give the Azure ML workspace (or compute) Storage Blob Data Reader on the storage account that hosts raw and processed (Role: Storage Blob Data Reader).

2. Create the Service Principal: 
Run this in Azure Cloud Shell or locally with Azure CLI installed:
az ad sp create-for-rbac \
  --name "github-aml-sp" \
  --role contributor \
  --scopes /subscriptions/<SUBSCRIPTION_ID>/resourceGroups/<RESOURCE_GROUP>/providers/Microsoft.MachineLearningServices/workspaces/<WORKSPACE_NAME> \
  --sdk-auth

This outputs JSON like:
{
  "clientId": "xxxx",
  "clientSecret": "xxxx",
  "subscriptionId": "xxxx",
  "tenantId": "xxxx"
}

3. Give the Service Principal Access to ADLS Gen2:

Go to your Storage Account
Left menu → Access Control (IAM)
Click Add → Add role assignment
Role: Storage Blob Data Reader
Assign access to the service principal you just created
This gives GitHub Actions → Azure ML → ADLS read access.

4. Add the Secrets to GitHub:

Go to Settings
Select Secrets and variables → Actions
Click New repository secret
Add:
AZURE_TENANT_ID = <tenantId>
AZURE_CLIENT_ID = <clientId>
AZURE_CLIENT_SECRET = <clientSecret>
DefaultAzureCredential() automatically picks up these.

-----------------------------------------------------------------------

Steps: 

1. RAW data uploaded into ADLS (raw container) -> in a Blob in a storage account
2. Synapse notebook that reads RAW file → writes PROCESSED file
3. PROCESSED data stored in ADLS (processed container in a storage account)
4. Azure ML workspace with read access to the storage account
5. Pipeline submission script that points to the processed ADLS path

-----------------------------------------------------------------------

Steps:

1. Synapse Pipeline runs

Executes your Spark notebook
Reads RAW data from ADLS
Cleans/transforms it
Writes PROCESSED data to ADLS

2. Azure ML Pipeline is triggered

You run submit_pipeline.py
Azure ML reads the PROCESSED ADLS path
Runs your training component
Logs metrics via MLflow
Saves model artifacts

3. Model is ready for evaluation or deployment

You can add evaluation
You can add model registration
You can add deployment
Everything is now aligned with modern Azure architecture.

-----------------------------------------------------------------------

Run: python scripts/submit_pipeline.py