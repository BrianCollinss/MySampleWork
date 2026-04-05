# How to test the endpoint from Python SDK

from azure.ai.ml import MLClient
from azure.identity import DefaultAzureCredential

ml_client = MLClient(DefaultAzureCredential(), subscription, rg, ws)

response = ml_client.online_endpoints.invoke(
    endpoint_name="house-price-endpoint",
    request_file="sample.json"
)

print(response)
