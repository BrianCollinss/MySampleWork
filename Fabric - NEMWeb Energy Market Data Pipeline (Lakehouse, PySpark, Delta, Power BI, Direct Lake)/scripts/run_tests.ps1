# Stop at the first failing command so CI/local runs do not hide earlier errors.
$ErrorActionPreference = "Stop"

# Use the project Conda environment defined in environment.yml.
conda activate fabric-nem-dashboard

# Validate parser/client behaviour before style checks.
pytest

# Ruff checks import/style issues; Black enforces deterministic formatting.
ruff check src tests scripts
black --check src tests scripts
