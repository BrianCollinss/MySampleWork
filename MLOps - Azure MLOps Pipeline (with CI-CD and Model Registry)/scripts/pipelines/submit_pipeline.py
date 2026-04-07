"""
Azure ML Pipeline Submission and Model Registration Orchestrator

This script submits and monitors an Azure Machine Learning pipeline job,
captures its outputs, and triggers a downstream model registration step.

Workflow
1. Define core configuration parameters such as model name, target column,
   and input dataset location in Azure Data Lake Storage Gen2.
2. Authenticate to the Azure ML workspace using a helper configuration.
3. Submit the pipeline defined in pipeline/pipeline.yml.
4. Stream pipeline execution logs to the console for real-time monitoring.
5. Retrieve the completed pipeline job metadata and validate its status.
6. Extract output artefact locations such as the trained model and
   evaluation results.
7. Persist these output paths to a JSON file for use in downstream steps.
8. Execute the model registration script using the pipeline outputs.

Purpose
Acts as an orchestration layer in an MLOps workflow by coordinating
pipeline execution, artefact extraction, and model registration. It
enables automated ML training pipelines to integrate with CI/CD
processes and model lifecycle management.

Inputs
- pipeline/pipeline.yml: Azure ML pipeline definition
- processed_path: cleaned dataset stored in ADLS Gen2
- target_column: prediction target used during model training
- model_name: name under which the model will be registered
- is_mlflow: flag indicating whether the model follows MLflow format

Outputs
- pipeline_outputs.json containing:
    - job_name
    - trained_model path
    - eval_results path
- Registered model artefacts produced by scripts/3.register.py
  stored under ./outputs/register

Dependencies
- Azure ML SDK (azure-ai-ml)
- Python standard libraries (json, subprocess, sys, pathlib)
- Custom workspace authentication helper: get_ml_client()

Typical Use Case
Executed as part of an automated ML pipeline or CI/CD workflow where
training pipelines run in Azure ML, and their outputs must be captured
and promoted into the model registry for deployment or evaluation.
"""

import json
import subprocess
import sys
from pathlib import Path

from azure.ai.ml.entities import PipelineJob
from utilities.utils import get_ml_client


def extract_output_uri(output_obj) -> str:
    """Safely extract a URI/path from an Azure ML job output object."""

    # Try common Azure ML output attributes
    for attr in ("uri", "path"):
        value = getattr(output_obj, attr, None)
        if value:
            return value

    # Handle case where output is returned as a dictionary
    if isinstance(output_obj, dict):
        for key in ("uri", "path"):
            value = output_obj.get(key)
            if value:
                return value

    # Fail clearly if URI cannot be resolved
    raise ValueError(f"Could not extract output URI from: {output_obj}")


def main():

    # Core pipeline configuration parameters
    target_column = "price"
    model_name = "house-price-regression"
    is_mlflow = "false"

    # Input dataset location in ADLS Gen2
    processed_path = (
        "abfss://processed@<storageaccount>.dfs.core.windows.net/house_prices_clean.csv"
    )

    # Create authenticated Azure ML client
    ml_client = get_ml_client()

    # Submit the Azure ML pipeline job defined in pipeline.yml
    pipeline_job = ml_client.jobs.create_or_update(
        PipelineJob(
            path="pipeline/pipeline.yml",
            inputs={
                "input_data": processed_path,
                "target_column": target_column,
                "model_name": model_name,
                "is_mlflow": is_mlflow,
            },
        )
    )

    # Log the job name for tracking
    print(f"Submitted pipeline job: {pipeline_job.name}")

    # Stream live logs from the pipeline execution
    ml_client.jobs.stream(pipeline_job.name)

    # Retrieve final job metadata after execution
    completed_job = ml_client.jobs.get(pipeline_job.name)

    print(f"Pipeline status: {completed_job.status}")

    # Fail fast if pipeline execution was unsuccessful
    if completed_job.status != "Completed":
        raise RuntimeError(
            f"Pipeline did not complete successfully. Status: {completed_job.status}"
        )

    # Extract output locations for trained model and evaluation results
    trained_model_path = extract_output_uri(completed_job.outputs["trained_model"])
    eval_results_path = extract_output_uri(completed_job.outputs["eval_results"])

    print(f"trained_model path: {trained_model_path}")
    print(f"eval_results path: {eval_results_path}")

    # Store pipeline outputs for downstream steps (e.g. CI/CD)
    output_payload = {
        "job_name": pipeline_job.name,
        "trained_model": trained_model_path,
        "eval_results": eval_results_path,
    }

    # Persist outputs to a JSON file
    with open("pipeline_outputs.json", "w", encoding="utf-8") as f:
        json.dump(output_payload, f, indent=2)

    # Create directory to store model registration outputs
    register_output_dir = "./outputs/register"
    Path(register_output_dir).mkdir(parents=True, exist_ok=True)

    # Run the model registration script with pipeline outputs
    subprocess.run(
        [
            sys.executable,
            "src/register.py",
            "--train_output_dir",
            trained_model_path,
            "--eval_output_dir",
            eval_results_path,
            "--model_name",
            model_name,
            "--is_mlflow",
            is_mlflow,
            "--register_output_dir",
            register_output_dir,
        ],
        check=True,
    )


if __name__ == "__main__":
    # Entry point for script execution
    main()
