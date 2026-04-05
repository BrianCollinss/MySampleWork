import requests

url = "https://australiaeast.inference.ml.azure.com/score"
api_key = "YOUR_PRIMARY_KEY"

headers = {
    "Authorization": f"Bearer {api_key}",
    "Content-Type": "application/json"
}

payload = {
    "data": [[1200, 3, 2]]
}

response = requests.post(url, json=payload, headers=headers)
print(response.json())
