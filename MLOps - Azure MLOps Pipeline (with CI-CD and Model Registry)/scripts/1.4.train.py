"""
Model Training Script for Azure ML Pipeline

This script trains a regression model using a prepared dataset and logs the
training process with MLflow. It is designed to be executed as part of an
Azure Machine Learning pipeline component.

Workflow:
1. Load the input dataset from the provided file path.
2. Split the data into training and validation sets.
3. Train a Linear Regression model using scikit-learn.
4. Enable MLflow autologging to capture parameters, metrics and artefacts.
5. Save the trained model and the MLflow run ID to the specified output
   directory for use by downstream pipeline stages.

Purpose:
Provides the training stage of the ML workflow, producing a trained model
artefact that can later be evaluated, registered or deployed.

Inputs:
- input_data: path to the processed dataset
- target_column: column used as the prediction target
- train_output_dir: directory where the trained model and run metadata are stored

Outputs:
- model.pkl: serialized trained model
- run_id metadata used for tracking the MLflow experiment run
"""

import argparse
import os

import joblib
import mlflow
import mlflow.sklearn
import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split

from utils import save_run_id


def main(args):
    # Enable MLflow autologging for parameters, metrics and model artefacts
    mlflow.autolog()

    # Load processed training dataset
    df = pd.read_csv(args.input_data)

    # Separate features and target variable
    X = df.drop(args.target_column, axis=1)
    y = df[args.target_column]

    # Split dataset into training and validation sets
    X_train, X_val, y_train, y_val = train_test_split(
        X, y, test_size=0.2, random_state=42
    )

    # Start an MLflow run to track the experiment
    with mlflow.start_run() as run:
        # Initialise and train a Linear Regression model
        model = LinearRegression()
        model.fit(X_train, y_train)

        # Ensure output directory exists
        os.makedirs(args.train_output_dir, exist_ok=True)

        # Save MLflow run ID for downstream pipeline stages
        save_run_id(args.train_output_dir, run.info.run_id)

        # Persist the trained model as a serialized file
        joblib.dump(model, os.path.join(args.train_output_dir, "model.pkl"))

        # Log completion details
        print(f"Training complete. MLflow run_id: {run.info.run_id}")
        print(f"Model saved to: {args.train_output_dir}")


if __name__ == "__main__":
    # Parse command line arguments provided by the pipeline
    parser = argparse.ArgumentParser()
    parser.add_argument("--input_data", type=str, required=True)
    parser.add_argument("--target_column", type=str, default="price")
    parser.add_argument("--train_output_dir", type=str, required=True)
    args = parser.parse_args()

    # Execute training workflow
    main(args)