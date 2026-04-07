import argparse

from pyspark.ml import PipelineModel
from pyspark.sql import SparkSession


def main(args):
    spark = SparkSession.builder.getOrCreate()

    model = PipelineModel.load(args.model_dir)
    df = spark.read.csv(args.input_data, header=True, inferSchema=True)

    preds = model.transform(df)
    preds.write.mode("overwrite").csv(args.output_dir)


if __name__ == "__main__":
    p = argparse.ArgumentParser()
    p.add_argument("--model_dir")
    p.add_argument("--input_data")
    p.add_argument("--output_dir")
    args = p.parse_args()
    main(args)
