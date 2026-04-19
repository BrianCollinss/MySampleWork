# Local Setup

## Prerequisites

- Python 3.11
- PowerShell or another terminal
- Conda installed, such as Miniconda or Anaconda
- An OpenAI API key, either directly or stored in Azure Key Vault
- Azure resources already created, or placeholder values while testing non-Azure parts

## 1. Open the Project Folder

Open a terminal in the project root:

```powershell
cd "c:\Users\brcol\My Drive\Documents\!!!Resume\Sample Work\Python - APSIM Intelligence Assistant"
```

## 2. Create a Conda Environment

```powershell
conda env create -f environment.yml
```

Activate it:

```powershell
conda activate apsim-copilot
```

If Conda activation is not available in the shell yet, initialize Conda for PowerShell first:

```powershell
conda init powershell
```

Then close and reopen PowerShell, and run:

```powershell
conda activate apsim-copilot
```

## 3. Install Dependencies

The Conda environment file already installs the Python dependencies from `requirements.txt`, so there is no extra install command after the environment is created unless you later change dependencies.

Because the APSIM website fetcher can render dynamic pages, install the
Playwright browser once in the active environment:

```powershell
python -m playwright install chromium
```

## 4. Create the Local Environment File

Copy the example file:

```powershell
Copy-Item .env.example .env
```

Open `.env` and set the non-secret values for:

- `OPENAI_CHAT_MODEL`
- `OPENAI_EMBEDDING_MODEL`
- `AZURE_SEARCH_ENDPOINT`
- `AZURE_SEARCH_INDEX_NAME`
- `AZURE_STORAGE_CONTAINER_NAME`

For secrets, choose one of these approaches:

### Option A: Direct values in `.env`

Set:

- `OPENAI_API_KEY`
- `AZURE_SEARCH_API_KEY`
- `AZURE_STORAGE_CONNECTION_STRING`
- `APPLICATIONINSIGHTS_CONNECTION_STRING` (optional)

### Option B: Azure Key Vault for local development

1. Add these values to `.env` instead of pasting secrets:
   - `AZURE_KEY_VAULT_URL`
   - `OPENAI_API_KEY_SECRET_NAME`
   - `AZURE_SEARCH_API_KEY_SECRET_NAME`
   - `AZURE_STORAGE_CONNECTION_STRING_SECRET_NAME`
   - `APPLICATIONINSIGHTS_CONNECTION_STRING_SECRET_NAME` (optional)
2. Sign in locally:

```powershell
az login
```

3. Make sure your signed-in identity can read secrets from the vault.

With this setup, the app and helper scripts will try direct env vars first and
then fall back to Key Vault.

If you do not want uploaded files saved to Blob Storage during local testing, leave:

```env
SAVE_UPLOADS_TO_BLOB=false
```

## 5. Run the App Locally

```powershell
python -m streamlit run app/main.py
```

Open the local URL shown in the terminal, usually:

```text
http://localhost:8501
```

## 6. Ingest APSIM Documents

Place your APSIM docs in `sample_data` using the suggested folder structure, then:

```powershell
python scripts/sc_01_create_search_index.py
python scripts/sc_03_upload_docs_to_blob.py --source-dir sample_data/docs
python scripts/sc_04_index_documents.py --source-dir sample_data/docs --topic apsim
```

## 7. Run Tests

Unit tests:

```powershell
pytest tests
```

Connectivity smoke test:

```powershell
python scripts/sc_06_smoke_test.py
```

## 8. Local Troubleshooting

### Streamlit starts but the Q&A tab fails

Check that the required OpenAI and Azure AI Search values are configured, either
directly in `.env` or through Key Vault, and restart Streamlit after updating
`.env`.

### Embeddings fail

Verify `OPENAI_EMBEDDING_MODEL` is a valid embeddings model name.

### Search returns no useful answers

Verify that:

- the index exists
- documents were uploaded successfully
- `AZURE_SEARCH_INDEX_NAME` matches the created index
- the vector dimension matches your embeddings deployment

### Blob upload errors

Check `AZURE_STORAGE_CONNECTION_STRING` and confirm the container name in `AZURE_STORAGE_CONTAINER_NAME`.
