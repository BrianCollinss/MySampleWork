from pyspark.ml import PipelineModel
from pyspark.sql import SparkSession

# Get or create Spark session (automatic in Databricks, explicit elsewhere)
spark = SparkSession.builder.getOrCreate()

# Note: dbutils is provided automatically in Databricks notebooks
model = PipelineModel.load(dbutils.widgets.get("model_path"))  # noqa: F821
df = spark.read.parquet(dbutils.widgets.get("input_path"))  # noqa: F821
preds = model.transform(df)
preds.write.mode("overwrite").parquet(dbutils.widgets.get("output_path"))  # noqa: F821
