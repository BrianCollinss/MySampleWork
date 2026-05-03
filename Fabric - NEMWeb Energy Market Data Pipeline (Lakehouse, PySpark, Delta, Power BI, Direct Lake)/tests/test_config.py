from __future__ import annotations

from pathlib import Path

import pytest

from nem_fabric import common_config as config


def test_load_yaml_falls_back_to_fabric_files_config(
    monkeypatch,
    tmp_path: Path,
) -> None:
    """Fabric notebooks load YAML from Lakehouse Files when repo files are absent."""

    monkeypatch.setattr(config, "PROJECT_ROOT", tmp_path / "missing_repo")
    monkeypatch.setattr(config, "FABRIC_FILES_ROOT", tmp_path / "Files")

    config_path = tmp_path / "Files" / "config" / "sources.yml"
    config_path.parent.mkdir(parents=True)
    config_path.write_text("sources:\n  - name: dispatch\n", encoding="utf-8")

    assert config.load_yaml("config/sources.yml") == {
        "sources": [{"name": "dispatch"}]
    }


def test_settings_validate_url_shape() -> None:
    """URL settings fail fast when an invalid value is supplied."""

    with pytest.raises(ValueError, match="nemweb_base_url"):
        config.Settings(nemweb_base_url="not-a-url")


def test_settings_validate_positive_integers() -> None:
    """Numeric limits must be positive."""

    with pytest.raises(ValueError, match="max_zips_per_run"):
        config.Settings(max_zips_per_run=0)
