"""
Model Evaluation Script for Azure ML Pipeline

This script evaluates a trained regression model using a supplied dataset and
logs the resulting performance metrics to MLflow. It is intended to run as the
evaluation stage of an Azure Machine Learning pipeline.

Workflow:
1. Load the trained model artefact from the model output directory.
2. Retrieve the stored MLflow run ID associated with the training step.
3. Load the evaluation dataset and separate features from the target column.
4. Generate predictions using the trained model.
5. Compute evaluation metrics such as mean squared error and R² score.
6. Log metrics back to the original MLflow run and save evaluation outputs
   to the specified directory for downstream use.

Purpose:
Provides a standardised evaluation step that measures model performance,
tracks results in MLflow and writes structured metric outputs for reporting,
validation or model registration decisions.

Inputs:
- train_output_dir: directory containing the trained model artefacts
- input_data: dataset used for model evaluation
- target_column: column representing the prediction target
- eval_output_dir: directory where evaluation outputs are written

Outputs:
- logged MLflow evaluation metrics
- saved metric files and summary outputs in the evaluation directory
"""

import argparse
import os

import joblib
import mlflow
import pandas as pd
from sklearn.metrics import mean_squared_error, r2_score

from utils import get_stored_run_id, log_stats_to_csv, save_metric_outputs


def main(args):
    # Ensure evaluation output directory exists
    os.makedirs(args.output_dir, exist_ok=True)

    # Load trained model artefact from training step
    model = joblib.load(os.path.join(args.train_output_dir, "model.pkl"))

    # Retrieve MLflow run ID associated with training
    run_id = get_stored_run_id(args.train_output_dir)

    # Load evaluation dataset
    df = pd.read_csv(args.input_data)

    # Separate features and target variable
    X = df.drop(args.target_column, axis=1)
    y = df[args.target_column]

    # Generate predictions using the trained model
    preds = model.predict(X)

    # Compute evaluation metrics
    metrics = {
        "eval_mse": mean_squared_error(y, preds),
        "eval_r2": r2_score(y, preds)
    }

    # Log evaluation metrics to the same MLflow run as training
    with mlflow.start_run(run_id=run_id):
        mlflow.log_metrics(metrics)

    # Save evaluation metrics to structured output files
    log_stats_to_csv(args.eval_output_dir, run_id, metrics)
    save_metric_outputs(args.eval_output_dir, metrics)

    # Print evaluation summary
    print("Evaluation complete")
    print(f"Run ID: {run_id}")
    print(f"eval_mse: {metrics['eval_mse']}")
    print(f"eval_r2: {metrics['eval_r2']}")
    print(f"Evaluation outputs saved to: {args.eval_output_dir}")


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