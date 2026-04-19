# APSIM Document Ingestion

This app expects you to provide your own APSIM documentation and examples. The retrieval quality depends heavily on this content.

You have two practical ingestion paths:

1. manually place curated docs into `sample_data/docs`
2. automatically fetch APSIM documentation pages with the included sync scripts

## Recommended Local Folder Structure

Use a structure like this under `sample_data` or another local folder:

```text
sample_data/
  docs/
    manuals/
      soil_water.md
      manager_rules.md
    examples/
      wheat_example.apsimx
      maize_rotation.apsimx
    notes/
      output_variables.txt
```

You do not need this exact structure. The scripts recurse through subfolders.

## Supported File Types for Indexing

The included indexer currently reads:

- `.md`
- `.txt`
- `.apsimx`
- `.json`

For PDFs or Word documents, convert them to markdown or plain text first for the cleanest first version.

## Step 1: Upload APSIM Docs to Blob Storage

This is optional for retrieval itself if you index directly from local files, but it is useful for keeping the source content in Azure Blob Storage.

Run:

```powershell
python scripts/sc_03_upload_docs_to_blob.py --source-dir sample_data/docs --prefix docs
```

This uploads every file under `sample_data/docs` into the blob container defined by:

- `AZURE_STORAGE_CONNECTION_STRING`
- `AZURE_STORAGE_CONTAINER_NAME`

## Automatic Sync from APSIM Websites

The project includes two scripts that let you refresh documentation from:

- `https://apsimnextgeneration.netlify.app/`
- `https://docs.apsim.info/`

### Fetch APSIM Docs into the Local Folder

Run:

```powershell
python scripts/sc_02_fetch_apsim_docs.py
```

By default this writes fetched pages into:

```text
sample_data/docs/fetched/
```

Useful options:

```powershell
python scripts/sc_02_fetch_apsim_docs.py --max-pages 40
python scripts/sc_02_fetch_apsim_docs.py --output-dir sample_data/docs/fetched --max-pages 250
```

The fetcher:

1. starts from the two APSIM documentation sites
2. tries to read sitemap URLs when available
3. falls back to internal-link crawling on those domains
4. detects client-rendered pages such as `docs.apsim.info` leaf pages
5. uses Playwright to render those pages when needed
6. extracts readable text from each page
7. clears the previous fetched output folder
8. saves each page as a local markdown file with source metadata

Before using the dynamic-page fallback for the first time, install Chromium for
Playwright in your active environment:

```powershell
python -m playwright install chromium
```

The default seed set includes:

- APSIM Next Generation home, usage, and model documentation pages
- APSIM Docs home, validation, tutorials, and models pages

That broader seed set helps the crawler reach deeper pages such as:

- `https://docs.apsim.info/validation/AgPasture`
- `https://docs.apsim.info/tutorials/...`
- `https://docs.apsim.info/models/...`

### Refresh the Whole Search Corpus in One Command

Run:

```powershell
python scripts/sc_05_refresh_docs_index.py
```

This script:

1. fetches APSIM docs into `sample_data/docs/fetched`
2. creates or updates the Azure AI Search index
3. optionally uploads the fetched docs to Blob Storage
4. chunks and indexes the fetched docs into Azure AI Search

If you want to skip blob upload:

```powershell
python scripts/sc_05_refresh_docs_index.py --skip-blob-upload
```

If you want a smaller refresh:

```powershell
python scripts/sc_05_refresh_docs_index.py --max-pages 40
```

## Step 2: Create the Azure AI Search Index

Run:

```powershell
python scripts/sc_01_create_search_index.py
```

This creates or updates an index with fields:

- `id`
- `title`
- `source_path`
- `source_type`
- `topic`
- `chunk_id`
- `content`
- `content_vector`

## Search Index Design

### Fields

- `id`: string key
- `title`: document title or file stem
- `source_path`: original file path
- `source_type`: for example `documentation` or `example`
- `topic`: a simple topic label such as `apsim`, `soil`, `management`
- `chunk_id`: chunk identifier within the source document
- `content`: chunk text used for retrieval and prompting
- `content_vector`: embedding vector for vector search

### Vector Dimension

Set `AZURE_SEARCH_VECTOR_DIMENSIONS` to match your embeddings model.

Examples:

- `1536` for many small embedding deployments
- verify the exact dimension for your OpenAI embedding model before creating the index

If your embedding model uses a different vector length, update:

- `.env`
- Container App environment variables
- the `--dimensions` argument when running `scripts/sc_01_create_search_index.py`

Example:

```powershell
python scripts/sc_01_create_search_index.py --dimensions 1536
```

## Step 3: Chunk Documents

The included indexer uses a simple chunking strategy:

- default chunk size: 1200 characters
- overlap: 150 characters

That logic lives in `scripts/sc_04_index_documents.py`.

Why this is good enough for the demo:

- small
- readable
- easy to explain
- no external framework required

## Step 4: Populate the Search Index

Run:

```powershell
python scripts/sc_04_index_documents.py --topic apsim
```

This script:

1. walks the local folder recursively
2. reads supported files
3. splits them into chunks
4. creates embeddings with the OpenAI API
5. uploads chunk documents into Azure AI Search

For the auto-sync path, `sc_05_refresh_docs_index.py` already performs this step for
the fetched APSIM website content.

By default, `sc_04_index_documents.py` uses `sample_data/docs` as its root folder and
indexes all supported files recursively under that folder, including nested
subfolders such as `fetched`, `manuals`, `examples`, and `notes`.

## What Metadata to Store

At minimum, keep:

- `title`
- `source_path`
- `source_type`
- `topic`
- `chunk_id`

These values are displayed in the UI and used for source citations.

## How to Test Retrieval

After indexing:

1. Run the app locally or in Container Apps.
2. Open the `Ask APSIM` tab.
3. Ask a question that should match uploaded docs, for example:
   - `What report variables are commonly used for soil water outputs?`
   - `How is a manager script represented in APSIM examples?`
4. Check that:
   - the answer includes grounded wording
   - the answer cites sources
   - the retrieved source expander shows matching chunks

## Retrieval Troubleshooting

### Index created but no answers appear

Check:

- `AZURE_SEARCH_INDEX_NAME`
- `AZURE_SEARCH_ENDPOINT`
- `AZURE_SEARCH_API_KEY`
- the index actually contains documents

### Search works but relevance is weak

Improve:

- source document quality
- topic labels
- chunk size and overlap
- document cleanliness before indexing
- how many APSIM website pages you fetch during refresh

### Upload to search fails

Check that the vector dimension in the index matches the length returned by your embedding deployment.
