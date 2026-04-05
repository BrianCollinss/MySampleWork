"""
Azure ML Deployment Promotion Script

This script promotes a target deployment within an Azure Machine Learning
managed online endpoint by updating endpoint traffic so that 100% of requests
are routed to the selected deployment.

Workflow:
1. Connect to the Azure ML workspace using environment-based configuration.
2. Retrieve the target online endpoint and inspect its current traffic split.
3. Validate that the source and target deployment names are different.
4. Warn if either deployment is not currently present in the endpoint traffic map.
5. Update the endpoint traffic configuration so the target deployment receives all live traffic.
6. Apply the updated endpoint configuration in Azure ML.

Purpose:
Provides a controlled promotion step in the MLOps release process, allowing a
validated deployment such as production to take over live traffic from another
deployment such as staging.

Inputs:
- endpoint_name: Azure ML online endpoint to update
- from_deployment: current source deployment being promoted from
- to_deployment: target deployment that will receive 100% traffic

Outputs:
- updated Azure ML endpoint traffic configuration
- console logs showing previous and new traffic assignments
"""

import argparse

from utils import get_ml_client


def main(args):
    # Create authenticated Azure ML client
    ml_client = get_ml_client()

    # Retrieve the target online endpoint
    endpoint = ml_client.online_endpoints.get(args.endpoint_name)

    # Read current traffic allocation, defaulting to an empty map
    current_traffic = endpoint.traffic or {}
    print(f"Current traffic: {current_traffic}")

    # Ensure source and target deployments are not the same
    if args.to_deployment == args.from_deployment:
        raise ValueError("--from_deployment and --to_deployment must be different")

    # Warn if the target deployment is not already present in traffic routing
    if args.to_deployment not in current_traffic:
        print(
            f"Warning: '{args.to_deployment}' is not currently in the endpoint traffic map. "
            "Proceeding anyway."
        )

    # Warn if the source deployment is not currently receiving traffic
    if args.from_deployment not in current_traffic:
        print(
            f"Warning: '{args.from_deployment}' is not currently in the endpoint traffic map."
        )

    # Route all endpoint traffic to the target deployment
    endpoint.traffic = {args.to_deployment: 100}
    ml_client.online_endpoints.begin_create_or_update(endpoint).result()

    # Print promotion result
    print(f"Promoted deployment '{args.to_deployment}' on endpoint '{args.endpoint_name}'")
    print(f"New traffic: {{'{args.to_deployment}': 100}}")


if __name__ == "__main__":
    # Parse command line arguments
    parser = argparse.ArgumentParser()
    parser.add_argument("--endpoint_name", type=str, required=True)
    parser.add_argument("--from_deployment", type=str, required=True)
    parser.add_argument("--to_deployment", type=str, required=True)
    args = parser.parse_args()

    # Execute promotion workflow
    main(args)