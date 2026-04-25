# Event Hubs Tasks

## Purpose

Event Hubs receives locally generated customer interaction events.

## What Is Done Locally

- Create `streaming/config.json` from `streaming/config.example.json`
- Open the existing Event Hubs namespace `evh-retail-analytics-demo`
- Go to **Shared access policies**
- Create or select a policy with **Send** permission, such as `send-events-policy`
- Copy the policy's **Connection string-primary key**
- Paste it into `streaming/config.json` as `event_hub_connection_string`
- Keep `event_hub_name` as `eh-customer-events`

Example:

```json
{
  "event_hub_connection_string": "Endpoint=sb://evh-retail-analytics-demo.servicebus.windows.net/;SharedAccessKeyName=send-events-policy;SharedAccessKey=<key>",
  "event_hub_name": "eh-customer-events",
  "send_batch_size": 50,
  "default_delay_seconds": 0.5,
  "default_iterations": 100,
  "seed": 42
}
```

- Do not commit the real connection string if the repo will be public
- Test producer connectivity with:

```powershell
python streaming/producer.py --config streaming/config.json --iterations 10 --delay 0.2
```

- Expected result: the terminal prints `Sent ...` lines and Event Hubs metrics show incoming messages

## What Is Done In Azure Portal

- Create or verify a shared access policy with send rights for local testing

## What Is Done In Service UI

- Review throughput and consumer metrics
- Confirm Stream Analytics is attached as a consumer

## What Is Provided In Repo

- Event schema
- Deterministic event replay
- Sample events

## What Can Be Automated Later

- Namespace and policy deployment via IaC
- Environment-based secret injection
