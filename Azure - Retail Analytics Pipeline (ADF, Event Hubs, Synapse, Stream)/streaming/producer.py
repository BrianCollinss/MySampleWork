"""Generate or load retail events and send them to Azure Event Hubs.

Use this for quick connectivity tests and finite event runs from a local machine.
"""

import argparse
import json
import time
from datetime import datetime, timedelta, timezone
from pathlib import Path
import random

from azure.eventhub import EventData, EventHubProducerClient


def load_config(config_path: Path) -> dict:
    """Read Event Hubs connection settings from a local JSON config file."""
    with config_path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def weighted_event_type(rng: random.Random) -> str:
    """Pick a realistic ecommerce event type using fixed probabilities."""
    choices = [
        ("product_view", 0.38),
        ("add_to_cart", 0.22),
        ("remove_from_cart", 0.08),
        ("checkout_started", 0.14),
        ("order_submitted", 0.08),
        ("email_clicked", 0.10),
    ]
    events, weights = zip(*choices)
    event = rng.choices(events, weights=weights, k=1)
    return event


def generate_events(iterations: int, seed: int) -> list[dict]:
    """Create a finite list of synthetic customer interaction events."""
    rng = random.Random(seed)
    base_time = datetime.now(timezone.utc)
    events = []

    for index in range(iterations):
        event_type = weighted_event_type(rng)
        quantity = rng.randint(1, 3) if event_type in {"add_to_cart", "remove_from_cart", "checkout_started", "order_submitted"} else None
        event_value = round(rng.uniform(9.99, 149.99), 2) if event_type != "email_clicked" else 0
        events.append(
            {
                "event_id": f"evt-live-{seed}-{index:05d}",
                "event_time": (base_time + timedelta(seconds=index * 2)).isoformat().replace("+00:00", "Z"),
                "event_type": event_type,
                "customer_id": f"C{rng.randint(1, 20):03d}",
                "session_id": f"SL{1000 + index:04d}",
                "product_id": f"P{rng.randint(1, 10):03d}",
                "campaign_id": rng.choice([None, "CMP001", "CMP002", "CMP003", "CMP004", "CMP005"]),
                "channel": rng.choice(["organic", "direct", "email", "paid_search", "paid_social", "affiliate"]),
                "device_type": rng.choice(["mobile", "desktop", "tablet"]),
                "order_id": f"O{3000 + index}" if event_type == "order_submitted" else None,
                "quantity": quantity,
                "event_value": event_value,
            }
        )

    return events


def send_events(config: dict, events: list[dict], delay_seconds: float) -> None:
    """Send events to the configured Event Hub, one EventData batch per event."""
    producer = EventHubProducerClient.from_connection_string(
        conn_str=config["event_hub_connection_string"],
        eventhub_name=config["event_hub_name"],
    )

    with producer:
        for event in events:
            batch = producer.create_batch()
            batch.add(EventData(json.dumps(event)))
            producer.send_batch(batch)
            print(f"Sent {event['event_id']} {event['event_type']}")
            if delay_seconds > 0:
                time.sleep(delay_seconds)


def main() -> None:
    """Parse CLI arguments, build or load events, and publish them to Event Hubs."""
    parser = argparse.ArgumentParser(description="Send retail events to Azure Event Hubs.")
    parser.add_argument("--config", type=Path, default=Path("config.json"), help="Path to config JSON.")
    parser.add_argument("--events", type=Path, default=None, help="Optional JSON file containing events.")
    parser.add_argument("--delay", type=float, default=None, help="Delay between messages in seconds.")
    parser.add_argument("--iterations", type=int, default=None, help="Generate a finite number of synthetic events.")
    args = parser.parse_args()

    config = load_config(args.config)
    delay_seconds = args.delay if args.delay is not None else config.get("default_delay_seconds", 1.0)
    iterations = args.iterations if args.iterations is not None else config.get("default_iterations", 60)

    if args.events is not None:
        with args.events.open("r", encoding="utf-8") as f:
            events = json.load(f)
    else:
        events = generate_events(iterations=iterations, seed=config.get("seed", 42))

    send_events(config, events, delay_seconds)


if __name__ == "__main__":
    main()
