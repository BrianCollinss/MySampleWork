# Create the local Conda environment used for parser tests and smoke tests.
conda env create -f environment.yml

# Activate the environment so the kernel registration uses the right Python.
conda activate fabric-nem-dashboard

# Register a named Jupyter kernel for VS Code and Fabric-style notebook editing.
python -m ipykernel install --user --name fabric-nem-dashboard --display-name "Python (fabric-nem-dashboard)"
