"""
Spark Model Evaluation Script for Azure ML Pipeline

This script evaluates a trained Spark MLlib regression model using a supplied
dataset and logs the resulting performance metrics to MLflow. It is intended
to run as the distributed evaluation stage of an Azure Machine Learning
pipeline.

Workflow:
1. Create or obtain an existing Spark session.
2. Load the trained Spark pipeline model from the model output directory.
3. Retrieve the stored MLflow run ID associated with the training step.
4. Load the evaluation dataset using Spark DataFrame APIs.
5. Generate predictions with the trained Spark model.
6. Compute regression performance metrics such as R² and RMSE.
7. Log metrics to the existing MLflow run and save evaluation outputs for
   downstream validation, reporting or model registration steps.

Purpose:
Provides a scalable evaluation component for distributed machine learning
workflows, ensuring model performance is tracked in MLflow and persisted as
structured output artefacts within the Azure ML pipeline.

Inputs:
- train_output_dir: directory containing the trained Spark model artefacts
- input_data: dataset used for model evaluation
- target_column: column representing the prediction target
- eval_output_dir: directory where evaluation outputs are written

Outputs:
- logged MLflow evaluation metrics
- saved metric files and summary outputs in the evaluation directory
"""

import argparse
import os

import mlflow
import mlflow.pyspark.ml
from pyspark.sql import SparkSession
from pyspark.ml import PipelineModel
from pyspark.ml.evaluation import RegressionEvaluator

from utils import get_stored_run_id, log_stats_to_csv, save_metric_outputs


def main(args):
    # Create or retrieve the active Spark session
    spark = SparkSession.builder.getOrCreate()

    # Enable MLflow autologging for Spark ML pipelines
    mlflow.pyspark.ml.autolog()

    # Load the trained Spark pipeline model
    model = PipelineModel.load(args.train_output_dir)

    # Retrieve MLflow run ID associated with the training stage
    run_id = get_stored_run_id(args.train_output_dir)

    # Load evaluation dataset into a Spark DataFrame
    df = spark.read.csv(args.input_data, header=True, inferSchema=True)

    # Generate predictions using the trained pipeline
    preds = model.transform(df)

    # Define evaluator for R² metric
    evaluator_r2 = RegressionEvaluator(
        labelCol=args.target_column,
        predictionCol="prediction",
        metricName="r2"
    )

    # Define evaluator for RMSE metric
    evaluator_rmse = RegressionEvaluator(
        labelCol=args.target_column,
        predictionCol="prediction",
        metricName="rmse"
    )

    # Compute evaluation metrics
    metrics = {
        "eval_r2": evaluator_r2.evaluate(preds),
        "eval_rmse": evaluator_rmse.evaluate(preds)
    }

    # Ensure evaluation output directory exists
    os.makedirs(args.eval_output_dir, exist_ok=True)

    # Log evaluation metrics to the original MLflow run
    with mlflow.start_run(run_id=run_id):
        mlflow.log_metrics(metrics)

    # Persist evaluation metrics for downstream pipeline stages
    log_stats_to_csv(args.eval_output_dir, run_id, metrics)
    save_metric_outputs(args.eval_output_dir, metrics)

    # Print evaluation summary
    print("Spark evaluation complete")
    print(f"Run ID: {run_id}")
    print(f"eval_r2: {metrics['eval_r2']}")
    print(f"eval_rmse: {metrics['eval_rmse']}")
    print(f"Outputs saved to: {args.eval_output_dir}")


if __name__ == "__main__":
    # Parse arguments supplied by the Azure ML pipeline
    parser = argparse.ArgumentParser()
    parser.add_argument("--train_output_dir", type=str, required=True)
    parser.add_argument("--input_data", type=str, required=True)
    parser.add_argument("--target_column", type=str, required=True)
    parser.add_argument("--eval_output_dir", type=str, required=True)
    args = parser.parse_args()

    # Execute evaluation workflow
    main(args)