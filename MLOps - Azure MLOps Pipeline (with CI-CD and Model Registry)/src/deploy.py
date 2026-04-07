"""
Azure ML Online Deployment Script

This script deploys the latest registered version of a model to an Azure
Machine Learning managed online endpoint. It supports both MLflow-based and
custom model deployments, with optional creation of the endpoint and traffic
routing to the new deployment.
"""

import argparse

from azure.ai.ml import MLClient
from azure.ai.ml.entities import (
    CodeConfiguration,
    ManagedOnlineDeployment,
    ManagedOnlineEndpoint,
)
from azure.core.exceptions import ResourceNotFoundError
from scripts.utilities.utils import get_ml_client


def sort_version(model) -> tuple[int, str]:
    """
    Sort versions numerically when possible, otherwise push non-numeric versions lower.
    """
    # Read model version as a string for safe comparison
    version_str = str(model.version)

    # Prefer numeric versions when available
    if version_str.isdigit():
        return int(version_str), version_str

    # Push non-numeric versions below numeric ones
    return -1, version_str


def get_latest_model_id(ml_client: MLClient, model_name: str) -> str:
    """
    Return the latest registered model in azureml:<name>:<version> format.
    """
    # Retrieve all registered versions for the requested model name
    models = list(ml_client.models.list(name=model_name))
    if not models:
        raise RuntimeError(f"No models found with name '{model_name}'")

    # Select the latest version using safe version sorting
    latest = max(models, key=sort_version)
    return f"azureml:{latest.name}:{latest.version}"


def ensure_endpoint(ml_client: MLClient, endpoint_name: str) -> None:
    """
    Ensure the managed online endpoint exists.
    """
    try:
        # Reuse the endpoint if it already exists
        ml_client.online_endpoints.get(endpoint_name)
        print(f"Using existing endpoint: {endpoint_name}")
    except ResourceNotFoundError:
        # Create a new managed online endpoint if missing
        endpoint = ManagedOnlineEndpoint(
            name=endpoint_name,
            auth_mode="key",
            description="House price regression endpoint",
        )
        ml_client.online_endpoints.begin_create_or_update(endpoint).result()
        print(f"Created endpoint: {endpoint_name}")


def main(args):
    # Create authenticated Azure ML client
    ml_client = get_ml_client()

    # Ensure the target endpoint exists before deployment
    ensure_endpoint(ml_client, args.endpoint_name)

    # Resolve the latest registered model version
    model_id = get_latest_model_id(ml_client, args.model_name)
    print(f"Using latest model: {model_id}")

    # Base deployment configuration shared by both MLflow and custom models
    deployment_kwargs = {
        "name": args.deployment_name,
        "endpoint_name": args.endpoint_name,
        "model": model_id,
        "instance_type": args.instance_type,
        "instance_count": int(args.instance_count),
    }

    # Add scoring code and environment for custom model deployments
    if args.is_mlflow.lower() != "true":
        if not args.code_path or not args.scoring_script or not args.environment:
            raise ValueError(
                "For non-MLflow deployment, --code_path, "
                "--scoring_script, and --environment are required."
            )

        deployment_kwargs["code_configuration"] = CodeConfiguration(
            code=args.code_path,
            scoring_script=args.scoring_script,
        )
        deployment_kwargs["environment"] = args.environment

    # Create the managed online deployment definition
    deployment = ManagedOnlineDeployment(**deployment_kwargs)

    # Deploy the model to the endpoint
    ml_client.online_deployments.begin_create_or_update(deployment).result()
    print(f"Deployed {model_id} to {args.deployment_name} on {args.endpoint_name}")

    # Optionally route all traffic to the new deployment
    if args.set_traffic.lower() == "true":
        endpoint = ml_client.online_endpoints.get(args.endpoint_name)
        endpoint.traffic = {args.deployment_name: 100}
        ml_client.online_endpoints.begin_create_or_update(endpoint).result()
        print(f"Set 100% traffic to deployment: {args.deployment_name}")


if __name__ == "__main__":
    # Parse command line arguments
    parser = argparse.ArgumentParser()
    parser.add_argument("--model_name", type=str, required=True)
    parser.add_argument("--endpoint_name", type=str, required=True)
    parser.add_argument("--deployment_name", type=str, required=True)
    parser.add_argument("--instance_type", type=str, default="Standard_DS3_v2")
    parser.add_argument("--instance_count", type=str, default="1")
    parser.add_argument("--set_traffic", type=str, default="false")
    parser.add_argument("--is_mlflow", type=str, default="false")
    parser.add_argument("--code_path", type=str, default=None)
    parser.add_argument("--scoring_script", type=str, default=None)
    parser.add_argument("--environment", type=str, default=None)
    args = parser.parse_args()

    # Execute deployment workflow
    main(args)
