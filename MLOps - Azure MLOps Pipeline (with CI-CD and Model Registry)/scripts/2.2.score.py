"""
Azure ML Inference Scoring Script

This script provides the inference entry point used by an Azure Machine
Learning online endpoint. It loads the trained model during container
initialisation and exposes a prediction function that processes incoming
requests and returns model predictions.

Workflow:
1. During container start-up, the init() function loads the trained model
   from the directory specified by the AZUREML_MODEL_DIR environment variable.
2. The run() function receives incoming request payloads in JSON format.
3. The payload is converted into a pandas DataFrame compatible with the
   trained model's expected feature structure.
4. The model generates predictions which are returned as a JSON response.
5. Basic error handling ensures failures are returned as structured messages.

Purpose:
Provides the runtime scoring logic used by the Azure ML managed online
deployment to generate predictions from incoming API requests.

Inputs:
- raw_data: JSON payload containing feature data for prediction

Outputs:
- JSON response containing model predictions or an error message
"""


import json
import os

import joblib
import pandas as pd

model = None


def init():
    global model
    model_dir = os.getenv("AZUREML_MODEL_DIR")
    model_path = os.path.join(model_dir, "model.pkl")
    model = joblib.load(model_path)


def run(raw_data):
    try:
        payload = json.loads(raw_data)

        if isinstance(payload, dict) and "data" in payload:
            df = pd.DataFrame(payload["data"])
        elif isinstance(payload, dict):
            df = pd.DataFrame([payload])
        else:
            df = pd.DataFrame(payload)

        predictions = model.predict(df)
        return {"predictions": predictions.tolist()}
    except Exception as exc:
        return {"error": str(exc)}