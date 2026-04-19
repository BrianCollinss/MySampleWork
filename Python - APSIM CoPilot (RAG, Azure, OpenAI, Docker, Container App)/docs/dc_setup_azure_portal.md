# Azure Portal Setup

This guide assumes you already have an Azure subscription and want a minimal-cost public demo deployment.

This version of the project uses the `OpenAI API` for chat and embeddings, not Azure OpenAI. Azure Portal is still used for the Azure infrastructure around the app.

Use one resource group so everything is easy to find and delete later.

## Naming Suggestion

Use a short prefix, for example `apsimcopilot`.

Example resource names:

- Resource group: `rg-apsimcopilot-dev`
- Azure AI Search: `srch-apsimcopilot-dev`
- Storage account: `stapsimcopilotdev`
- Key Vault: `kv-apsimcopilot-dev`
- Application Insights: `appi-apsimcopilot-dev`
- Container Apps environment: `cae-apsimcopilot-dev`
- Container App: `ca-apsimcopilot-dev`
- Container Registry: `acrapsimcopilotdev`

## 1. Create a Resource Group

1. In the Azure Portal, search for `Resource groups`.
2. Select `Create`.
3. Choose the `Basics` tab.
4. Set these fields:
   - Subscription: your subscription
   - Resource group: `rg-apsimcopilot-dev`
   - Region: choose one region and keep related services close where possible
5. Select `Review + create`.
6. Select `Create`.

## 2. Create Azure AI Search

1. In the Azure Portal, search for `Azure AI Search`.
2. Select `Create`.
3. Choose the `Basics` tab.
4. Set these fields:
   - Subscription: your subscription
   - Resource group: `rg-apsimcopilot-dev`
   - Service name: `srch-apsimcopilot-dev`
   - Location: choose a region near the app
   - Pricing tier: choose a low-cost tier appropriate for a demo
5. Select `Review + create`.
6. Select `Create`.

### Record Azure AI Search Values

After deployment:

1. Open the Azure AI Search service.
2. Open `Overview` and copy the `Url`. This becomes `AZURE_SEARCH_ENDPOINT`.
3. Open `Settings` > `Keys`.
4. Copy an `Admin key`. This becomes `AZURE_SEARCH_API_KEY`.

## 3. Create a Storage Account and Blob Container

1. In the Azure Portal, search for `Storage accounts`.
2. Select `Create`.
3. Choose the `Basics` tab.
4. Set these fields:
   - Subscription: your subscription
   - Resource group: `rg-apsimcopilot-dev`
   - Storage account name: `stapsimcopilotdev`
   - Region: choose a nearby region
   - Performance: `Standard`
   - Redundancy: `Locally-redundant storage (LRS)` for a minimal-cost demo
5. Select `Review + create`.
6. Select `Create`.

### Create the Blob Container

1. After deployment, open the storage account.
2. In the left menu, select `Containers`.
3. Select `+ Container`.
4. Enter the name `apsim-copilot`.
5. Leave public access disabled unless you explicitly need public blobs.
6. Select `Create`.

### Record Storage Values

1. Open the storage account.
2. In the left menu, open `Access keys`.
3. Copy a `Connection string`.
4. This becomes `AZURE_STORAGE_CONNECTION_STRING`.
5. The container name `apsim-copilot` becomes `AZURE_STORAGE_CONTAINER_NAME`.

## 4. Create Key Vault

1. In the Azure Portal, search for `Key vaults`.
2. Select `Create`.
3. Choose the `Basics` tab.
4. Set these fields:
   - Subscription: your subscription
   - Resource group: `rg-apsimcopilot-dev`
   - Key vault name: `kv-apsimcopilot-dev`
   - Region: choose a nearby region
   - Pricing tier: `Standard`
5. Select `Review + create`.
6. Select `Create`.

### Add Secrets to Key Vault

1. Open the Key Vault.
2. Select `Secrets`.
3. Select `Generate/Import`.
4. Create one secret for each sensitive value, for example:
   - `openai-api-key`
   - `azure-search-api-key`
   - `azure-storage-connection-string`
   - `applicationinsights-connection-string`
5. Paste the secret value and save it.

## 5. Create Application Insights

1. In the Azure Portal, search for `Application Insights`.
2. Select `Create`.
3. Choose the `Basics` tab.
4. Set these fields:
   - Subscription: your subscription
   - Resource group: `rg-apsimcopilot-dev`
   - Name: `appi-apsimcopilot-dev`
   - Region: choose a nearby region
   - Workspace-based mode: keep the default if required by the portal
5. Select `Review + create`.
6. Select `Create`.

### Record Application Insights Value

1. Open the Application Insights resource.
2. In `Overview`, copy the `Connection String`.
3. This becomes `APPLICATIONINSIGHTS_CONNECTION_STRING`.

## 6. Create a Container Apps Environment

Azure Portal usually expects you to complete the full `Create Container App` wizard, including the `Container` tab. If you only want to prepare the environment first, use one of these two approaches:

### Option A: Create the environment during Container App creation later

This is the simplest path.

1. Do not try to create the Container App yet.
2. First create and push your container image.
3. Then return to `Container Apps` and create the app.
4. On the `Basics` tab, choose `Create new` for `Container Apps environment`.
5. Enter `cae-apsimcopilot-dev` as the environment name.
6. Continue to the `Container` tab and finish the app creation in one pass.

### Option B: Create the environment as part of a temporary app draft

If you want the environment created before your real image is ready:

1. In the Azure Portal, search for `Container Apps`.
2. Select `Create` and choose `Container App`.
3. On the `Basics` tab, set:
   - Subscription: your subscription
   - Resource group: `rg-apsimcopilot-dev`
   - Container app name: a temporary name such as `ca-apsimcopilot-temp`
   - Region: choose a supported Container Apps region
   - Container Apps environment: select `Create new`
4. In the create-environment pane:
   - Environment name: `cae-apsimcopilot-dev`
   - Leave workload profile at the lowest suitable option for a demo
5. On the `Container` tab, provide a temporary placeholder image that the portal will accept. For example, use a public demo image if needed. Verify the portal accepts the image before continuing.
6. Finish creating the temporary app.
7. After the environment exists, you can delete the temporary app and keep the environment, or simply reuse the same app and update its image later.

For this demo, Option A is usually cleaner.

## 7. Create a Container Registry

1. In the Azure Portal, search for `Container registries`.
2. Select `Create`.
3. Choose the `Basics` tab.
4. Set these fields:
   - Subscription: your subscription
   - Resource group: `rg-apsimcopilot-dev`
   - Registry name: `acrapsimcopilotdev`
   - Location: choose a nearby region
   - SKU: `Basic`
5. Select `Review + create`.
6. Select `Create`.

## 8. Create the Container App

After your image is pushed, return to `Container Apps` and create the app. This is the point where you should complete the full wizard, including the `Container` tab.

1. In the Azure Portal, search for `Container Apps`.
2. Select `Create` and choose `Container App`.
3. Choose the `Basics` tab.
4. Set these fields:
   - Subscription: your subscription
   - Resource group: `rg-apsimcopilot-dev`
   - Container app name: `ca-apsimcopilot-dev`
   - Region: same region as the environment
   - Container Apps environment: `cae-apsimcopilot-dev`
5. Select `Next: Container`.
6. Set these fields:
   - Image source: your registry choice
   - Image: your pushed APSIM Copilot image
   - CPU and memory: start small for demo usage
7. Set the container port to `8501`.
8. Select `Next: Ingress`.
9. Enable `Ingress`.
10. Set:
    - Ingress traffic: `Accepting traffic from anywhere`
    - Type: `HTTP`
    - Target port: `8501`
11. Continue through the remaining tabs.
12. Select `Review + create`.
13. Select `Create`.

## 9. Set Environment Variables and Secrets in Container Apps

Recommended approach: use Key Vault plus a Container App managed identity.

### Enable managed identity and Key Vault access

1. Open the Container App.
2. Open `Identity`.
3. Enable the system-assigned managed identity.
4. Open the Key Vault.
5. Grant the Container App identity the `Key Vault Secrets User` role.

1. Open the Container App.
2. You can skip storing app secrets directly in Container Apps when using Key Vault.

Then:

1. Open `Containers`, `Revision management`, or `Environment variables`. Verify the label in the portal if it varies.
2. Add environment variables:
   - `APP_NAME=APSIM Copilot`
   - `AZURE_KEY_VAULT_URL=<your-key-vault-url>`
   - `OPENAI_CHAT_MODEL=gpt-5.4-mini` or your chosen OpenAI chat model
   - `OPENAI_EMBEDDING_MODEL=text-embedding-3-small` or your chosen OpenAI embeddings model
   - `AZURE_SEARCH_ENDPOINT=<your search endpoint>`
   - `AZURE_SEARCH_INDEX_NAME=apsim-docs`
   - `AZURE_SEARCH_VECTOR_DIMENSIONS=1536` or your actual embedding size
   - `AZURE_STORAGE_CONTAINER_NAME=apsim-copilot`
   - `SAVE_UPLOADS_TO_BLOB=true` if you want uploads saved
   - `OPENAI_API_KEY_SECRET_NAME=openai-api-key`
   - `AZURE_SEARCH_API_KEY_SECRET_NAME=azure-search-api-key`
   - `AZURE_STORAGE_CONNECTION_STRING_SECRET_NAME=azure-storage-connection-string`
   - `APPLICATIONINSIGHTS_CONNECTION_STRING_SECRET_NAME=applicationinsights-connection-string`
3. Save and let Container Apps create a new revision if prompted.

### Alternate approach: store secrets directly in Container Apps

If you do not want to use Key Vault references yet:

1. Open the Container App.
2. In the left menu, open `Secrets`.
3. Add secrets for:
   - `openai-api-key`
   - `azure-search-api-key`
   - `azure-storage-connection-string`
   - `applicationinsights-connection-string`
4. Bind them to:
   - `OPENAI_API_KEY`
   - `AZURE_SEARCH_API_KEY`
   - `AZURE_STORAGE_CONNECTION_STRING`
   - `APPLICATIONINSIGHTS_CONNECTION_STRING`
4. Save and let Container Apps create a new revision if prompted.

## 10. Verify Public Ingress and Open the App

1. Open the Container App.
2. In `Overview`, find the application URL.
3. Open it in a browser.
4. Confirm the Streamlit UI loads and shows three tabs:
   - Ask APSIM
   - Explain `.apsimx`
   - Summarise CSV

## 11. Create and Populate the Search Index

Run these locally after the Azure AI Search service is ready:

```powershell
python scripts/sc_01_create_search_index.py
python scripts/sc_03_upload_docs_to_blob.py --source-dir sample_data/docs
python scripts/sc_04_index_documents.py --source-dir sample_data/docs --topic apsim
```

## How to Link the Services Together

### OpenAI API

- Open the OpenAI platform dashboard outside Azure.
- Create an API key.
- Copy it into `OPENAI_API_KEY`.
- Choose a chat model and put it into `OPENAI_CHAT_MODEL`.
- Choose an embeddings model and put it into `OPENAI_EMBEDDING_MODEL`.
- For a lightweight first version, `gpt-5.4-mini` plus `text-embedding-3-small` is a sensible pairing. Verify current model availability and pricing in the OpenAI dashboard.

### Azure AI Search

- Open the Azure AI Search resource.
- In `Overview`, copy the service URL into `AZURE_SEARCH_ENDPOINT`.
- In `Keys`, copy an admin key into `AZURE_SEARCH_API_KEY`.
- Use the index name you created, for example `apsim-docs`, as `AZURE_SEARCH_INDEX_NAME`.

### Storage Account

- Open the storage account.
- Open `Access keys`.
- Copy one connection string into `AZURE_STORAGE_CONNECTION_STRING`.
- Use the blob container name, for example `apsim-copilot`, as `AZURE_STORAGE_CONTAINER_NAME`.

### Key Vault

- Open the Key Vault.
- Open `Secrets`.
- Record the secret names you created, for example `openai-api-key`.
- When editing Container Apps secrets or integrations, use those same secret values or secret references.

### Application Insights

- Open the Application Insights resource.
- Copy the connection string from `Overview`.
- Put it into `APPLICATIONINSIGHTS_CONNECTION_STRING`.

### Put the Values into Local `.env`

1. Copy `.env.example` to `.env`.
2. Paste the OpenAI and Azure values into the matching keys.
3. Save the file.
4. Restart Streamlit after editing `.env`.

### Put the Values into Container Apps

1. Open the Container App.
2. Add secret values under `Secrets`.
3. Add plain environment variables under the environment variables section.
4. Bind secret-backed environment variables to the stored secrets.
5. Save and wait for the new revision to become healthy.

## Troubleshooting Common Issues

### The app loads but answers fail

Check that the Container App has all required environment variables and that secret-backed variables are bound correctly.

### The container starts then crashes

Verify the target port is `8501` and the container command is running Streamlit with `--server.port=8501 --server.address=0.0.0.0`.

### Search answers are empty or weak

Verify the search index exists, contains documents, and uses the same vector dimension as your embeddings model.

### Blob uploads fail

Verify the storage connection string and container name.

### Application Insights shows no data

Verify `APPLICATIONINSIGHTS_CONNECTION_STRING` is set. Telemetry is best effort in this demo and logging still works without it.
