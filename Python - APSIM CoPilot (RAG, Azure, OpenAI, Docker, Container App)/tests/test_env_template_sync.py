"""Guardrail tests for keeping .env and .env.example aligned."""

from __future__ import annotations

from pathlib import Path


PROJECT_ROOT = Path(__file__).resolve().parents[1]


def _read_env_keys(path: Path) -> list[str]:
    """Return environment variable keys in file order, ignoring comments."""
    keys: list[str] = []
    for line in path.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#") or "=" not in stripped:
            continue
        key = stripped.split("=", 1)[0].strip()
        keys.append(key)
    return keys


def _read_glossary_keys(path: Path) -> list[str]:
    """Return glossary keys in file order from the Comment Glossary block."""
    lines = path.read_text(encoding="utf-8").splitlines()
    start_index: int | None = None
    for index, line in enumerate(lines):
        if line.strip() == "# Comment Glossary":
            start_index = index + 1
            break

    assert start_index is not None, f"{path.name} is missing a Comment Glossary section"

    glossary_keys: list[str] = []
    for line in lines[start_index:]:
        stripped = line.strip()
        if not stripped:
            continue
        if not stripped.startswith("# - "):
            continue
        entry = stripped.removeprefix("# - ")
        key, _, _ = entry.partition(":")
        glossary_keys.append(key.strip())
    return glossary_keys


def test_env_and_example_define_the_same_keys_in_the_same_order() -> None:
    """Prevent .env and .env.example from drifting apart over time."""
    env_keys = _read_env_keys(PROJECT_ROOT / ".env")
    example_keys = _read_env_keys(PROJECT_ROOT / ".env.example")
    assert env_keys == example_keys


def test_env_glossaries_match_declared_keys() -> None:
    """Require the Comment Glossary blocks to stay aligned with the env keys."""
    for filename in (".env", ".env.example"):
        path = PROJECT_ROOT / filename
        assert _read_glossary_keys(path) == _read_env_keys(path)
