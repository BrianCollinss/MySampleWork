ENV_NAME=churn-analysis

.PHONY: help setup pipeline notebook activate-help

help:
	@echo "Available targets:"
	@echo "  make setup         - Create the Conda environment"
	@echo "  make pipeline      - Run the data preparation pipeline"
	@echo "  make notebook      - Launch Jupyter Lab in the Conda environment"
	@echo "  make activate-help - Show the PowerShell Conda activation fix"

setup:
	conda env create -f environment.yml

pipeline:
	conda run -n $(ENV_NAME) python scripts/run_data_pipeline.py

notebook:
	conda run -n $(ENV_NAME) jupyter lab

activate-help:
	@echo "If 'conda activate $(ENV_NAME)' fails in PowerShell, run:"
	@echo "  conda init powershell"
	@echo "Then close and reopen PowerShell and run:"
	@echo "  conda activate $(ENV_NAME)"
