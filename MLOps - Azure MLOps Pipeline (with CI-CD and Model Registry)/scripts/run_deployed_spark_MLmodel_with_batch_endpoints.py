# Trigger batch endpoint via SDK

from azure.ai.ml import MLClient
from azure.identity import DefaultAzureCredential

from utils import get_ml_client

ml_client = get_ml_client()

job = ml_client.batch_endpoints.invoke(
    endpoint_name="spark-batch-endpoint",
    deployment_name="spark-batch-deployment",
    inputs={"input_data": input_path}
)

print(job)


# curl -X POST \
#   -H "Authorization: Bearer $TOKEN" \
#   -H "Content-Type: application/json" \
#   -d '{"input_data": "abfss://..."}' \
#   https://<region>.api.azureml.ms/batch-endpoints/v1.0/.../invoke
