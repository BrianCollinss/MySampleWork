# Tests Folder

This folder contains unit tests for APSIM CoPilot.

## Purpose

The tests focus on the parts of the demo that are easiest to verify locally without needing live APSIM execution or live Azure dependencies.

## Current Coverage

- APSIMX parsing
- CSV summary helpers
- prompt generation
- env template drift protection

## Run Tests

```powershell
conda activate apsim-copilot
python -m pytest tests
```
