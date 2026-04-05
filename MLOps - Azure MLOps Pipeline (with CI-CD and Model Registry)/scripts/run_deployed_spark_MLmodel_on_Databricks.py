from pyspark.ml import PipelineModel

model = PipelineModel.load(dbutils.widgets.get("model_path"))
df = spark.read.parquet(dbutils.widgets.get("input_path"))
preds = model.transform(df)
preds.write.mode("overwrite").parquet(dbutils.widgets.get("output_path"))
