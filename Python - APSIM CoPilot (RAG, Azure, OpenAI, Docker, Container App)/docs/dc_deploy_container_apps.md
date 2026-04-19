# Deploy to Azure Container Apps

This guide covers two paths:

- Path A: build locally, push to Azure Container Registry, deploy to Azure Container Apps
- Path B: use a public container registry

The app listens on port `8501`.

## Path A: Build Locally and Push to Azure Container Registry

### 1. Prerequisites

- Docker Desktop installed and running
- Azure CLI installed
- You already created:
  - Resource group
  - Azure Container Registry
  - Container Apps environment

### 2. Log In to Azure

```powershell
az login
az account set --subscription "<your-subscription-name-or-id>"
```

### 3. Log In to Azure Container Registry

```powershell
az acr login --name acrapsimcopilotdev
```

### 4. Build the Image

From the project root:

```powershell
docker buildx build --platform linux/amd64 --provenance=false -t acrapsimcopilotdev.azurecr.io/apsim-copilot:latest --push .
```

### 5. Push the Image

The `buildx` command above already pushes the image, so no separate `docker push`
step is required.

### Optional: Use the included Windows batch script

If you prefer a repeatable local script, set `AZ_SUBSCRIPTION`, `ACR_NAME`, and
`BUILD_PLATFORM`
in your `.env` and run:

```powershell
.\bt_build_push_acr.bat
```

### 6. Deploy the Container App

You can create the Container App in the portal, use Azure CLI directly, or use
the included Windows batch script.

#### Option A: Use the included Windows batch script

Set these values in `.env` first:

- `AZ_SUBSCRIPTION`
- `AZURE_RESOURCE_GROUP`
- `AZURE_LOCATION`
- `ACR_NAME`
- `IMAGE_NAME`
- `IMAGE_TAG`
- `CONTAINER_APP_NAME`
- `CONTAINER_APP_ENVIRONMENT`
- `LOG_ANALYTICS_WORKSPACE_NAME`

If `IMAGE_TAG=USEDATE`, the build script generates a timestamped tag and saves
it to `.last_image_tag`. The deploy script reads that same file so both steps
use the same image tag.

Optional:

- `CONTAINER_APP_RESOURCE_GROUP`
- `CONTAINER_APP_INGRESS`
- `CONTAINER_APP_TARGET_PORT`
- `CONTAINER_APP_CPU`
- `CONTAINER_APP_MEMORY`
- `ACR_PULL_IDENTITY_RESOURCE_ID`
- `ACR_PULL_IDENTITY_NAME`
- `ACR_PULL_IDENTITY_RESOURCE_GROUP`
- `ACR_USERNAME`
- `ACR_PASSWORD`

`ACR_USERNAME` and `ACR_PASSWORD` are optional only when the Container App
already has another valid way to pull from your private registry, such as:

- the ACR admin user configured elsewhere
- a managed identity with the `AcrPull` role on the registry
- a public image that needs no registry authentication

For a first deployment of a brand-new Container App, the cleanest private-ACR
path is usually a user-assigned managed identity. In that case, set either:

- `ACR_PULL_IDENTITY_RESOURCE_ID`, or
- `ACR_PULL_IDENTITY_NAME` plus `ACR_PULL_IDENTITY_RESOURCE_GROUP`

The batch script will create the app with:

- `--user-assigned <identity-resource-id>`
- `--registry-identity <identity-resource-id>`

So you do not need to create the app first and then attach the identity in the
portal.

`CONTAINER_APP_RESOURCE_GROUP` is now optional when `AZURE_RESOURCE_GROUP` is
set. The script uses `AZURE_RESOURCE_GROUP` as the shared default for the app
resource group and the pull-identity resource group.

Then run:

```powershell
.\bt_deploy_container_app.bat
```

This script:

- logs into Azure
- selects the configured subscription
- registers `Microsoft.OperationalInsights` when needed
- creates the Log Analytics workspace when needed
- creates the Container Apps environment when needed
- creates the Container App if it does not exist
- can create the app already wired to a user-assigned pull identity
- updates the image if it already exists
- applies runtime environment variables
- configures direct Container Apps secrets when direct secret values are present in `.env`

#### Option B: Use Azure CLI directly

You can create the Container App manually with:

```powershell
az containerapp create `
  --name ca-apsimcopilot-dev `
  --resource-group rg-apsimcopilot-dev `
  --environment cae-apsimcopilot-dev `
  --image acrapsimcopilotdev.azurecr.io/apsim-copilot:latest `
  --target-port 8501 `
  --ingress external `
  --registry-server acrapsimcopilotdev.azurecr.io `
  --cpu 0.5 `
  --memory 1.0Gi
```

If you want to create the app with a user-assigned managed identity for ACR
pulls from the start, first get the identity resource ID:

```powershell
az identity show `
  --name id-apsimcopilot-pull `
  --resource-group <identity-resource-group> `
  --query id `
  --output tsv
```

Then create the app with:

```powershell
az containerapp create `
  --name ca-apsimcopilot-dev `
  --resource-group rg-apsimcopilot-dev `
  --environment cae-apsimcopilot-dev `
  --image acrapsimcopilotdev.azurecr.io/apsim-copilot:latest `
  --target-port 8501 `
  --ingress external `
  --registry-server acrapsimcopilotdev.azurecr.io `
  --user-assigned <identity-resource-id> `
  --registry-identity <identity-resource-id> `
  --cpu 0.5 `
  --memory 1.0Gi
```

That identity must already have `AcrPull` on the registry.

### 7. Add Secrets and Environment Variables

You can use the Azure Portal for the clearest first-time setup, or let the
batch script apply them from `.env`.

#### Option A: Use the batch script

The same script below handles both section 6 and section 7:

```powershell
.\bt_deploy_container_app.bat
```

It supports two modes:

1. Key Vault mode  
   Triggered when `AZURE_KEY_VAULT_URL` is set. The script writes normal
   environment variables such as:
   - `AZURE_KEY_VAULT_URL`
   - `OPENAI_API_KEY_SECRET_NAME`
   - `AZURE_SEARCH_API_KEY_SECRET_NAME`
   - `AZURE_STORAGE_CONNECTION_STRING_SECRET_NAME`
   - `APPLICATIONINSIGHTS_CONNECTION_STRING_SECRET_NAME`

2. Direct secret mode  
   Triggered when direct values such as `OPENAI_API_KEY` or
   `AZURE_SEARCH_API_KEY` are present in `.env`. The script stores them as
   Container Apps secrets and binds them to the corresponding runtime
   environment variables.

#### Option B: Use the Azure Portal manually

Use the Azure Portal for the clearest first-time setup:

1. Open the Container App.
2. Add secrets under `Secrets`.
3. Add environment variables under the container configuration section.
4. Save and allow a new revision.

Use the values listed in [dc_setup_azure_portal.md](dc_setup_azure_portal.md).

## Path B: Use a Public Registry

If you want to use Docker Hub or another public registry:

### 1. Build the Image

```powershell
docker buildx build --platform linux/amd64 --provenance=false -t <your-public-registry>/apsim-copilot:latest --push .
```

### 2. Push the Image

The `buildx` command above already pushes the image, so no separate `docker push`
step is required.

### 3. Create or Update the Container App

In the Azure Portal:

1. Open `Container Apps`.
2. Select `Create`, or open the existing app and edit the container image.
3. Set the image to `<your-public-registry>/apsim-copilot:latest`.
4. Set target port to `8501`.
5. Enable external ingress.
6. Save the revision.

If the registry is private, add registry credentials in Container Apps.

## Update an Existing Deployment

Each time you change the code:

```powershell
docker buildx build --platform linux/amd64 --provenance=false -t acrapsimcopilotdev.azurecr.io/apsim-copilot:latest --push .
```

Then restart or update the Container App revision so the latest image is pulled.

## Troubleshooting Invalid Container OS

If Azure Container Apps says:

```text
Selected tag uses an invalid operating system ''.
```

the pushed tag usually contains a manifest Azure cannot interpret cleanly, often
because the image was published without an explicit Linux platform or with extra
provenance attestations.

Use:

```powershell
docker buildx build --platform linux/amd64 --provenance=false -t <image> --push .
```

or rerun:

```powershell
.\bt_build_push_acr.bat
```

That pushes a plain Linux image manifest that Azure Container Apps accepts.

## Verify the Deployment

1. Open the Container App in the Azure Portal.
2. Open the application URL.
3. Confirm:
   - the page loads
   - the sidebar shows configured services
   - each tab opens
4. Upload a sample `.apsimx` and a sample CSV.
5. Ask a retrieval-backed APSIM question after indexing documents.

## Container Apps Settings Checklist

- Image points to the correct registry and tag
- Ingress is external
- Target port is `8501`
- CPU and memory are set
- Secrets are stored
- Environment variables are mapped
- Revision is healthy
