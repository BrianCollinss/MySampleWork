ENV_NAME=snowflake-project

.PHONY: setup setup-update activate bootstrap ingest transform integration-check snowpipe validate

setup:
	conda env create -f environment.yml

setup-update:
	conda env update -n $(ENV_NAME) -f environment.yml

activate:
	conda activate $(ENV_NAME)

bootstrap:
	python python/py_01_bootstrap.py

ingest:
	python python/py_02_core_ingestion.py

transform:
	python python/py_03_transformations.py

integration-check:
	python python/py_05_storage_integration_check.py

snowpipe:
	python python/py_06_snowpipe_external.py

validate:
	python python/py_07_validate_snowpipe_objects.py
