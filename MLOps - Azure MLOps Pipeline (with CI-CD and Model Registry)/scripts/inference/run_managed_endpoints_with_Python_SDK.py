# How to test the endpoint from Python SDK

import argparse

from azure.ai.ml import MLClient
from azure.identity import DefaultAzureCredential


def main(args):
    # Initialize ML client with Azure credentials
    ml_client = MLClient(
        DefaultAzureCredential(),
        args.subscription,
        args.resource_group,
        args.workspace,
    )

    # Invoke the online endpoint
    response = ml_client.online_endpoints.invoke(
        endpoint_name=args.endpoint_name,
        request_file=args.request_file,
    )

    print(response)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--subscription", type=str, required=True,
                        help="Azure subscription ID")
    parser.add_argument("--resource_group", type=str, required=True,
                        help="Azure resource group")
    parser.add_argument("--workspace", type=str, required=True,
                        help="Azure ML workspace name")
    parser.add_argument("--endpoint_name", type=str, required=True,
                        help="Name of the online endpoint")
    parser.add_argument("--request_file", type=str, required=True,
                        help="Path to request JSON file")
    args = parser.parse_args()
    main(args)
