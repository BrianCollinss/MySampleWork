import argparse
import os
import json
from datetime import datetime
import openai

def main(args):
    # Load metadata
    with open(args.metadata_path, "r") as f:
        meta = json.load(f)

    openai.api_type = "azure"
    openai.api_base = args.openai_endpoint
    openai.api_version = "2023-05-15"
    openai.api_key = args.openai_api_key

    prompt = f"""
You are an expert ML engineer. Generate a concise, clear model card in Markdown.

Model name: {meta.get('model_name')}
Model version: {meta.get('model_version')}
Run ID: {meta.get('run_id')}
Data path: {meta.get('data_path')}
Eval R2: {meta.get('eval_r2')}
Eval MSE: {meta.get('eval_mse')}
Git commit: {meta.get('git_commit')}
Timestamp: {datetime.utcnow().isoformat()}Z

Include sections:
- Overview
- Intended use
- Data
- Training
- Evaluation
- Limitations
- Version & lineage
"""

    response = openai.ChatCompletion.create(
        engine=args.openai_deployment,
        messages=[{"role": "user", "content": prompt}],
        temperature=0.2,
    )

    content = response["choices"][0]["message"]["content"]

    os.makedirs(args.output_dir, exist_ok=True)
    out_path = os.path.join(args.output_dir, "model_card.md")
    with open(out_path, "w") as f:
        f.write(content)

    print(f"Model card written to {out_path}")

if __name__ == "__main__":
    p = argparse.ArgumentParser()
    p.add_argument("--metadata_path")
    p.add_argument("--openai_endpoint")
    p.add_argument("--openai_deployment")
    p.add_argument("--openai_api_key")
    p.add_argument("--output_dir")
    args = p.parse_args()
    main(args)
