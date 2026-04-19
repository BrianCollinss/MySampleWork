# App Folder

This folder contains the APSIM CoPilot application code.

## Purpose

The `app` package holds the Streamlit entrypoint, runtime configuration, UI helpers, service wrappers, and small domain models used by the demo.

The emphasis here is clarity and deployment completeness rather than heavy domain-specific modelling logic.

## Key Files

- `main.py`: Streamlit entrypoint and tab layout
- `config.py`: runtime configuration and deployment-mode detection
- `logging_config.py`: logging and optional telemetry wiring

## Subfolders

- `models/`: lightweight parser and summary models
- `services/`: OpenAI, Azure AI Search, Blob Storage, Key Vault, and prompt helpers
- `ui/`: reusable Streamlit UI helpers
- `utils/`: placeholder package for future shared utilities
