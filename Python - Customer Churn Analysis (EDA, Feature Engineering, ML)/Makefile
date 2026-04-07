ENV_NAME=churn-analysis

.PHONY: help setup notebook lab activate-help

help:
	@echo "Available targets:"
	@echo "  make setup         - Create the Conda environment"
	@echo "  make notebook      - Launch Jupyter Notebook in the Conda environment"
	@echo "  make lab           - Launch Jupyter Lab in the Conda environment"
	@echo "  make activate-help - Show the PowerShell Conda activation fix"

setup:
	conda env create -f environment.yml

notebook:
	conda run -n $(ENV_NAME) jupyter notebook

lab:
	conda run -n $(ENV_NAME) jupyter lab

activate-help:
	@echo "If 'conda activate $(ENV_NAME)' fails in PowerShell, run:"
	@echo "  conda init powershell"
	@echo "Then close and reopen PowerShell and run:"
	@echo "  conda activate $(ENV_NAME)"
