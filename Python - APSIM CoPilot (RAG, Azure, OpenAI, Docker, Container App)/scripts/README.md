# Scripts Folder

This folder contains helper scripts for setup, ingestion, and verification.

## Purpose

These scripts keep operational tasks out of the Streamlit app itself so indexing, deployment, and environment checks can be run independently.

## Current Contents

- `sc_01_create_search_index.py`: creates or updates the Azure AI Search index
- `sc_02_fetch_apsim_docs.py`: fetches and saves APSIM documentation pages from the public APSIM sites
- `sc_03_upload_docs_to_blob.py`: uploads local docs into Azure Blob Storage
- `sc_04_index_documents.py`: chunks local docs, creates embeddings, and uploads them into Azure AI Search
- `sc_05_refresh_docs_index.py`: runs fetch + index creation + optional blob upload + indexing as one refresh step
- `sc_06_smoke_test.py`: checks environment configuration and basic service connectivity

## Script Order

The scripts are usually run in this order when setting up or refreshing the
retrieval corpus.

### Path A: Manual Local Docs Flow

Use this path when you already have local files under `sample_data/docs`.

1. `sc_01_create_search_index.py`
2. `sc_03_upload_docs_to_blob.py` (optional)
3. `sc_04_index_documents.py`
4. `sc_06_smoke_test.py` (optional verification)

Example:

```powershell
conda activate apsim-copilot
python scripts/sc_01_create_search_index.py
python scripts/sc_03_upload_docs_to_blob.py --source-dir sample_data/docs
python scripts/sc_04_index_documents.py --topic apsim
python scripts/sc_06_smoke_test.py
```

### Path B: Automatic APSIM Website Sync

Use this path when you want to pull content from the APSIM documentation sites
without manually saving pages yourself.

1. `sc_01_create_search_index.py`
2. `sc_02_fetch_apsim_docs.py`
3. `sc_03_upload_docs_to_blob.py` (optional)
4. `sc_04_index_documents.py`
5. `sc_06_smoke_test.py` (optional verification)

Example:

```powershell
conda activate apsim-copilot
python scripts/sc_02_fetch_apsim_docs.py
python scripts/sc_01_create_search_index.py
python scripts/sc_03_upload_docs_to_blob.py --source-dir sample_data/docs/fetched --prefix docs/fetched
python scripts/sc_04_index_documents.py --topic apsim
python scripts/sc_06_smoke_test.py
```

`sc_04_index_documents.py` now defaults to `sample_data/docs` and indexes all
supported files recursively under that root, including subfolders such as
`sample_data/docs/fetched`.

### Path C: One-Command Refresh

Use this when you want the automatic APSIM website sync and indexing flow in a
single command.

1. `sc_05_refresh_docs_index.py`
2. `sc_06_smoke_test.py` (optional verification)

Example:

```powershell
conda activate apsim-copilot
python scripts/sc_05_refresh_docs_index.py
python scripts/sc_06_smoke_test.py
```

## Typical Usage

Run scripts from the project root with the Conda environment activated:

```powershell
conda activate apsim-copilot
python scripts/sc_05_refresh_docs_index.py
```

## Dynamic APSIM Docs Pages

Some `docs.apsim.info` pages render their content client-side and only return a
`Loading` shell to plain HTTP fetches. The fetcher handles this with Playwright.

Before using `sc_02_fetch_apsim_docs.py` for the first time, run:

```powershell
conda activate apsim-copilot
python -m playwright install chromium
```

## Scope Note

These scripts are designed to demonstrate an end-to-end RAG workflow around APSIM content. They are intentionally pragmatic and small, not a fully hardened production ingestion framework.
