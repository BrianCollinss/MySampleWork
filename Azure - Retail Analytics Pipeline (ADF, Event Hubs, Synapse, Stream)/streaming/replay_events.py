"""Replay a fixed JSON event file into Azure Event Hubs.

Use this when a run needs repeatable input events instead of newly generated ones.
"""

import argparse
import json
import time
from pathlib import Path

from azure.eventhub import EventData, EventHubProducerClient


def load_json(path: Path) -> dict | list:
    """Load a JSON config or event file from disk."""
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def main() -> None:
    """Read config/events, optionally limit the event list, and send events to Event Hubs."""
    parser = argparse.ArgumentParser(description="Deterministically replay sample retail events.")
    parser.add_argument("--config", type=Path, default=Path("config.json"))
    parser.add_argument("--events", type=Path, default=Path("sample_events.json"))
    parser.add_argument("--delay", type=float, default=0.25, help="Delay between events.")
    parser.add_argument("--limit", type=int, default=0, help="Optional max number of events to send.")
    args = parser.parse_args()

    config = load_json(args.config)
    events = load_json(args.events)
    if not isinstance(events, list):
        raise ValueError("Event file must contain a JSON array.")

    if args.limit > 0:
        events = events[: args.limit]

    producer = EventHubProducerClient.from_connection_string(
        conn_str=config["event_hub_connection_string"],
        eventhub_name=config["event_hub_name"],
    )

    with producer:
        for index, event in enumerate(events, start=1):
            batch = producer.create_batch()
            batch.add(EventData(json.dumps(event)))
            producer.send_batch(batch)
            print(f"[{index}/{len(events)}] replayed {event['event_type']} for {event['customer_id']}")
            time.sleep(args.delay)


if __name__ == "__main__":
    main()
