# Stream Analytics Tasks

## Purpose

Stream Analytics creates the near real-time 5-minute funnel output.

## What Is Done Locally

- Review the query in `stream-analytics/queries/funnel_realtime.sql`
- Confirm `streaming/config.json` can send events to `eh-customer-events`
- From the repo root, generate test traffic with:

```powershell
python streaming/producer.py --config streaming/config.json --iterations 50 --delay 0.5
```

## What Is Done In Azure Portal

- Use the existing Stream Analytics job: `asa-retail-funnel-demo`

## What Is Done In Service UI

- Open the Stream Analytics job
- Go to **Inputs** > **Add stream input** > **Event Hub**
- Configure the input:

```text
Input alias: customer-events-input
Event Hubs namespace: evh-retail-analytics-demo
Event hub name: eh-customer-events
Event hub policy name: Create new, or use an existing policy with Listen permission
Consumer group: $Default
Event serialization format: JSON
Encoding: UTF-8
Event compression type: None
```

- Go to **Outputs** > **Add** > **Blob storage/ADLS Gen2**
- Configure the output:

```text
Output alias: funnel-realtime-output
Storage account: stretailanalyticsdemo
Container: retailanalyticsdemo
Path pattern: gold/streaming/funnel_realtime_5min
Event serialization format: CSV - comma (,)
Encoding: UTF-8
Write mode: Append, as results arrive
```

- Go to **Query**
- Paste the contents of `stream-analytics/queries/funnel_realtime.sql`
- Click **Test query** if sample input data is available
- Click **Save query**
- Start the job with **Now** as the output start time
- Run the local producer command again to send events while the job is running
- Confirm output files appear under:

```text
retailanalyticsdemo/gold/streaming/funnel_realtime_5min/
```

## Troubleshooting Output

- If **Input Events** increases but **Output Events** stays at `0`, the Event Hubs input is working.
- The query uses `TumblingWindow(minute, 5)`, so output is only written after a 5-minute window closes.
- For a quick test, send events for at least 5 minutes:

```powershell
python streaming/producer.py --config streaming/config.json --iterations 300 --delay 1
```

- If you only sent a short run, such as 30 events, wait for the current 5-minute window to close or run the longer command above.
- Keep the Stream Analytics job running while sending events.

## What Is Provided In Repo

- Query logic
- Event schema
- Demo guidance

## What Can Be Automated Later

- Job deployment with templates
- Query promotion via CI/CD
