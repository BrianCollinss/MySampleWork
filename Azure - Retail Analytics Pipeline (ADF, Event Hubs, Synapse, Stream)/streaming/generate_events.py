"""Generate deterministic retail event JSON files for streaming runs.

The output can be replayed later with replay_events.py for repeatable runs.
"""

import argparse
import json
import random
import uuid
from datetime import datetime, timedelta, timezone
from pathlib import Path


EVENT_TYPES = [
    ("product_view", 0.38),
    ("add_to_cart", 0.22),
    ("remove_from_cart", 0.08),
    ("checkout_started", 0.14),
    ("order_submitted", 0.08),
    ("email_clicked", 0.10),
]

CHANNELS = ["organic", "direct", "email", "paid_search", "paid_social", "affiliate"]
DEVICES = ["mobile", "desktop", "tablet"]


def weighted_choice(rng: random.Random) -> str:
    """Pick an event type from the configured probability distribution."""
    roll = rng.random()
    cumulative = 0.0
    for event_type, probability in EVENT_TYPES:
        cumulative += probability
        if roll <= cumulative:
            return event_type
    return EVENT_TYPES[-1][0]


def build_event(index: int, rng: random.Random, event_time: datetime) -> dict:
    """Build one synthetic customer interaction event."""
    customer_num = rng.randint(1, 20)
    product_num = rng.randint(1, 10)
    event_type = weighted_choice(rng)
    quantity = rng.randint(1, 3) if event_type in {"add_to_cart", "checkout_started", "order_submitted", "remove_from_cart"} else None
    price = round(rng.uniform(9.99, 149.99), 2)
    order_id = f"O{2000 + index}" if event_type == "order_submitted" else None

    return {
        "event_id": f"evt-{uuid.uuid4().hex[:12]}",
        "event_time": event_time.replace(tzinfo=timezone.utc).isoformat().replace("+00:00", "Z"),
        "event_type": event_type,
        "customer_id": f"C{customer_num:03d}",
        "session_id": f"S{1000 + index:04d}",
        "product_id": f"P{product_num:03d}",
        "campaign_id": rng.choice([None, "CMP001", "CMP002", "CMP003", "CMP004", "CMP005"]),
        "channel": rng.choice(CHANNELS),
        "device_type": rng.choice(DEVICES),
        "order_id": order_id,
        "quantity": quantity,
        "event_value": price if event_type != "email_clicked" else 0,
    }


def main() -> None:
    """Generate a JSON array of synthetic events and write it to disk."""
    parser = argparse.ArgumentParser(description="Generate realistic retail streaming events.")
    parser.add_argument("--count", type=int, default=100, help="Number of events to generate.")
    parser.add_argument("--seed", type=int, default=42, help="Seed for deterministic output.")
    parser.add_argument("--output", type=Path, default=Path("sample_events.generated.json"), help="Output JSON file path.")
    args = parser.parse_args()

    rng = random.Random(args.seed)
    base_time = datetime(2026, 4, 1, 9, 0, 0)
    events = []

    for index in range(args.count):
        event_time = base_time + timedelta(seconds=index * rng.randint(5, 18))
        events.append(build_event(index, rng, event_time))

    args.output.write_text(json.dumps(events, indent=2), encoding="utf-8")
    print(f"Wrote {len(events)} events to {args.output}")


if __name__ == "__main__":
    main()
