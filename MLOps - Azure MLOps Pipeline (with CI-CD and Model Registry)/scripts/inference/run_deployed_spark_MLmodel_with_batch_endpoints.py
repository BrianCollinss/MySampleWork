# Trigger batch endpoint via SDK

import argparse

from utilities.utils import get_ml_client


def main(args):
    ml_client = get_ml_client()

    job = ml_client.batch_endpoints.invoke(
        endpoint_name="spark-batch-endpoint",
        deployment_name="spark-batch-deployment",
        inputs={"input_data": args.input_path},
    )

    print(job)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--input_path", type=str, required=True,
                        help="Path to input data for batch endpoint")
    args = parser.parse_args()
    main(args)


# curl -X POST \
#   -H "Authorization: Bearer $TOKEN" \
#   -H "Content-Type: application/json" \
#   -d '{"input_data": "abfss://..."}' \
#   https://<region>.api.azureml.ms/batch-endpoints/v1.0/.../invoke
