from azure.ai.ml.entities import PipelineJob
from scripts.utilities.utils import get_ml_client

# ML autnetication
ml_client = get_ml_client()

processed_path = (
    "abfss://processed@<storageaccount>.dfs.core.windows.net/house_prices_clean.csv"
)

pipeline_job = ml_client.jobs.create_or_update(
    PipelineJob(
        path="scripts/pipelines/pipeline_spark.yml",
        inputs={"input_data": processed_path, "target_column": "price"},
    )
)

print("Spark pipeline submitted:", pipeline_job.name)
