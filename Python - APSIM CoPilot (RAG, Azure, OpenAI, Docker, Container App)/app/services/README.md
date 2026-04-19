# Services Folder

This folder contains the service layer for APSIM CoPilot.

## Purpose

Each service module wraps one focused concern so the Streamlit UI stays small and readable.

This layer is intentionally thin. It shows integration seams clearly, but it does not try to hide every external dependency behind elaborate abstractions.

## Current Contents

- `oa_openai_service.py`: OpenAI API chat and embedding calls
- `az_ai_search_service.py`: Azure AI Search retrieval
- `az_blob_service.py`: Azure Blob Storage uploads
- `az_key_vault_service.py`: Azure Key Vault secret resolution
- `oa_prompt_builder.py`: prompt templates and prompt-construction helpers

## Related Modules Outside This Folder

- `app/models/mo_apsimx_parser.py`: minimal heuristic APSIMX parsing
- `app/models/mo_csv_summary_service.py`: minimal CSV analysis helpers
- `app/models/mo_search_models.py`: lightweight shared data models for retrieval and prompting
