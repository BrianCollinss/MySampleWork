import argparse
import json

import requests


def main(args):
    headers = {
        "Authorization": f"Bearer {args.api_key}",
        "Content-Type": "application/json",
    }

    payload = json.loads(args.payload)

    response = requests.post(args.url, json=payload, headers=headers)
    print(response.json())


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--url", type=str, required=True,
                        help="Endpoint URL")
    parser.add_argument("--api_key", type=str, required=True,
                        help="API key for authentication")
    parser.add_argument("--payload", type=str, required=True,
                        help="JSON payload as string")
    args = parser.parse_args()
    main(args)
