import os
import pandas as pd

from azure.ai.ml import MLClient
from azure.identity import DefaultAzureCredential


def get_ml_client() -> MLClient:
    return MLClient(
        DefaultAzureCredential(),
        subscription_id=os.environ["AZURE_SUBSCRIPTION_ID"],
        resource_group_name=os.environ["AZURE_RESOURCE_GROUP"],
        workspace_name=os.environ["AZURE_ML_WORKSPACE"],
    )
    
    
def save_run_id(output_dir, run_id):
    """Saves the active run_id to a text file for downstream components."""
    
    os.makedirs(output_dir, exist_ok=True)
    with open(os.path.join(output_dir, "run_id.txt"), "w") as f:
        f.write(run_id)


def get_stored_run_id(train_output_dir):
    """Reads the run_id from the text file in the model directory."""
    
    run_id_path = os.path.join(train_output_dir, "run_id.txt")
    if not os.path.exists(run_id_path):
        raise FileNotFoundError(f"No run_id.txt found at {train_output_dir}")
    with open(run_id_path, "r") as f:
        return f.read().strip()


def log_stats_to_csv(output_dir, run_id, metrics):
    """Appends run metadata and metrics to a persistent CSV file."""
    
    os.makedirs(output_dir, exist_ok=True)
    csv_path = os.path.join(output_dir, "run_history.csv")
    
    data = {
        "run_id": [run_id],
        "timestamp": [pd.Timestamp.now()]
    }
    data.update({k: [v] for k, v in metrics.items()})
    new_row = pd.DataFrame(data)

    if not os.path.exists(csv_path):
        new_row.to_csv(csv_path, index=False)
    else:
        new_row.to_csv(csv_path, mode='a', header=False, index=False)


def save_metric_outputs(output_dir, metrics):
    """Saves individual metrics to files for Azure ML pipeline string outputs."""
    
    os.makedirs(output_dir, exist_ok=True)
    for name, value in metrics.items():
        with open(os.path.join(output_dir, f"{name}.txt"), "w") as f:
            f.write(str(value))


def read_metric_file(output_dir, metric_name):
    """Read a metric value from a <metric_name>.txt file inside the specified output directory."""    
    
    metric_path = os.path.join(output_dir, f"{metric_name}.txt")
    if not os.path.exists(metric_path):
        raise FileNotFoundError(f"No {metric_name}.txt found at {output_dir}")
    with open(metric_path, "r") as f:
        return f.read().strip()            