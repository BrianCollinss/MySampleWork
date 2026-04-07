.PHONY: help install lint test clean

help: ## Show this help message
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  %-15s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

install: ## Install dependencies
	conda env create -f environments/conda.yml
	conda activate azure-mlops-env && pre-commit install

lint: ## Run linting
	pre-commit run --all-files

test: ## Run tests
	pytest

clean: ## Clean up generated files
	rm -rf outputs/
	rm -rf .coverage
	rm -rf htmlcov/
	rm -rf __pycache__/
	rm -rf .pytest_cache/
	find . -type f -name desktop.ini -exec rm -f {} +
