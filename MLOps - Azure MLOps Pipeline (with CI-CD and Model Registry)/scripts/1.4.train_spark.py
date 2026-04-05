"""
Distributed Spark ML Training Script

This script trains a regression model using Apache Spark MLlib as part of an
Azure Machine Learning pipeline. It reads a dataset from a specified location,
constructs a Spark ML pipeline and trains a linear regression model using
distributed Spark compute.

Workflow:
1. Create or obtain an existing Spark session.
2. Load the dataset from the provided path using Spark DataFrame APIs.
3. Automatically construct feature vectors using VectorAssembler.
4. Train a Linear Regression model within a Spark ML Pipeline.
5. Enable MLflow autologging to capture parameters, metrics and artefacts.
6. Save the trained Spark model and MLflow run ID to the specified output
   directory for downstream pipeline steps.

Purpose:
Provides a scalable training component for large datasets using Spark MLlib,
enabling distributed model training within an Azure ML MLOps pipeline.

Inputs:
- input_data: path to the dataset used for training
- target_column: column used as the prediction target
- train_output_dir: directory where the trained Spark model is stored

Outputs:
- trained Spark ML pipeline model
- MLflow run metadata used for experiment tracking
"""

import argparse
import os

import mlflow
import mlflow.pyspark.ml
from pyspark.ml import Pipeline
from pyspark.ml.feature import VectorAssembler
from pyspark.ml.regression import LinearRegression
from pyspark.sql import SparkSession

from utils import save_run_id


def main(args):
    # Create or retrieve the active Spark session
    spark = SparkSession.builder.getOrCreate()

    # Enable MLflow autologging for Spark ML pipelines
    mlflow.pyspark.ml.autolog()

    # Load dataset into a Spark DataFrame
    df = spark.read.csv(args.input_data, header=True, inferSchema=True)

    # Automatically select all columns except the target as features
    feature_cols = [c for c in df.columns if c != args.target_column]

    # Assemble feature columns into a single vector required by Spark ML
    assembler = VectorAssembler(
        inputCols=feature_cols,
        outputCol="features"
    )

    # Define the regression model
    lr = LinearRegression(
        featuresCol="features",
        labelCol=args.target_column
    )

    # Create a Spark ML pipeline combining feature assembly and model training
    pipeline = Pipeline(stages=[assembler, lr])

    # Start MLflow run for experiment tracking
    with mlflow.start_run() as run:
        # Train the pipeline model on the dataset
        model = pipeline.fit(df)

        # Ensure output directory exists
        os.makedirs(args.train_output_dir, exist_ok=True)

        # Save MLflow run ID for downstream pipeline components
        save_run_id(args.train_output_dir, run.info.run_id)

        # Persist the trained Spark pipeline model
        model.write().overwrite().save(args.train_output_dir)

        # Log completion information
        print(f"Training complete. MLflow run_id: {run.info.run_id}")
        print(f"Model saved to: {args.train_output_dir}")


if __name__ == "__main__":
    # Parse arguments supplied by the Azure ML pipeline
    parser = argparse.ArgumentParser()
    parser.add_argument("--input_data", type=str, required=True)
    parser.add_argument("--target_column", type=str, required=True)
    parser.add_argument("--train_output_dir", type=str, required=True)
    args = parser.parse_args()

    # Execute training workflow
    main(args)