# Streaming

This folder contains the local Python tooling used to simulate customer interaction events.

## Files

- `producer.py`: primary script; generates synthetic events and sends them directly to Event Hubs
- `replay_events.py`: optional script for replaying a fixed JSON event file
- `generate_events.py`: optional script for creating a JSON event file to replay later
- `sample_events.json`: optional fixed event file for `replay_events.py`
- `event_schema.json`: expected payload contract
- `config.example.json`: placeholder configuration

## Which Script Should I Use?

Use `producer.py` for the normal event generation path.

```powershell
python producer.py --config config.json --iterations 300 --delay 1
```

Use `replay_events.py` only when you want the exact same prebuilt events every run.
It does not generate events; it reads an existing JSON event file and sends each event to Event Hubs.

```powershell
python replay_events.py --config config.json --events sample_events.json --delay 0.2
```

Use `generate_events.py` only if you want to create a new JSON event file for later replay.

## What Is Done Locally

- Complete environment setup in `docs/02-local-setup.md`
- Populate `streaming/config.json` using `docs/06-event-hubs-tasks.md`
- Run `producer.py` to send events from this folder

```powershell
python producer.py --config config.json --iterations 50 --delay 0.5
```

## What Is Done In Azure Portal

Covered in `docs/03-azure-portal-tasks.md` and `docs/06-event-hubs-tasks.md`.

## What Is Done In Service UI

Event Hubs and Stream Analytics setup are covered in `docs/06-event-hubs-tasks.md` and `docs/07-stream-analytics-tasks.md`.

## What Is Provided In Repo

- Event schema
- Producer scripts
- Deterministic sample event file

## What Can Be Automated Later

- Secret retrieval from Key Vault
- Scheduled event playback from CI or container jobs
