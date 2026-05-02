# Python Package Deployment

This project keeps reusable Python code in `src/nem_fabric`. Fabric Pipelines do
not automatically upload or install local source code when they run notebooks.
Each notebook runtime must be able to import `nem_fabric` before code such as
this can work:

```python
from nem_fabric.mmsdm_parser import parse_zip_bytes
```

## Option 1: Fabric Environment Library

Package `nem_fabric` as a Python wheel and upload it to a Fabric Environment
item.

Recommended flow:

1. Add or maintain packaging metadata such as `pyproject.toml`.
2. Build a wheel locally.
3. Create a Fabric Environment item in a Fabric-enabled workspace.
4. Upload the wheel as a custom library.
5. Publish the Environment.
6. Attach the Environment to each notebook or Spark job.
7. Run notebooks from the Fabric Pipeline.

Benefits:

- Clean production pattern.
- Versioned Python package deployment.
- Notebooks stay smaller.
- Local tests and Fabric runtime use the same package.

Limitations:

- Requires Fabric Environment support in the tenant.
- Requires rebuilding and re-uploading the wheel after source changes.
- Not available while the workspace is limited to Power BI Pro only.

## Option 2: Lakehouse Files Source Library

This project currently uses this option.

Upload the source package folder directly from the repo:

```text
src/nem_fabric
```

to this Lakehouse Files destination:

```text
Files/libs/nem_fabric
```

The Fabric notebook runtime sees that as:

```text
/lakehouse/default/Files/libs/nem_fabric
```

The notebooks add the parent folder to `sys.path`, not the package folder
itself:

```text
/lakehouse/default/Files/libs
```

That parent folder must contain the `nem_fabric` directory.

## Notebook Library Path Parameter

The notebooks use this environment variable:

```text
FABRIC_NOTEBOOK_LIB_PATH
```

Default value:

```text
/lakehouse/default/Files/libs
```

Use the default when the uploaded package is located at:

```text
/lakehouse/default/Files/libs/nem_fabric
```

Override `FABRIC_NOTEBOOK_LIB_PATH` only if the parent folder is different.

Example:

```text
FABRIC_NOTEBOOK_LIB_PATH=/lakehouse/default/Files/custom_python
```

That requires the package to be uploaded to:

```text
/lakehouse/default/Files/custom_python/nem_fabric
```

## Notebook Bootstrap Cell

Each notebook that imports `nem_fabric` includes this bootstrap pattern:

```python
import os
import sys

fabric_lib_path = os.getenv("FABRIC_NOTEBOOK_LIB_PATH", "/lakehouse/default/Files/libs")
if fabric_lib_path not in sys.path:
    sys.path.insert(0, fabric_lib_path)
```

After that, imports work:

```python
from nem_fabric.mmsdm_parser import parse_zip_bytes
```

## When to Re-Upload

Re-upload `src/nem_fabric` whenever files under that folder change.

Examples:

- Parser changes in `src/nem_fabric/mmsdm_parser.py`.
- Client changes in `src/nem_fabric/nemweb_client.py`.
- Config-loading changes in `src/nem_fabric/config.py`.
- Schema or quality-rule changes used by notebooks.

Notebook-only changes do not require re-uploading `src/nem_fabric`.
