"""Regression tests for readable physics-name validation diagnostics."""

from __future__ import annotations

from importlib.resources import files
import json
from pathlib import Path
from typing import Any

from click.testing import CliRunner
import pytest
import yaml

pytestmark = pytest.mark.api


def _load_sample_dict() -> dict[str, Any]:
    resource = files("supy").joinpath("sample_data", "sample_config.yml")
    if not resource.is_file():
        pytest.skip(f"Sample config not available at {resource}")
    return yaml.safe_load(resource.read_text(encoding="utf-8"))


def test_validate_unknown_physics_name_lists_readable_names(tmp_path: Path) -> None:
    """Readable physics-name errors list accepted agent-facing aliases."""
    from supy.cmd.validate_config import cli as validate_cli

    payload = _load_sample_dict()
    payload["model"]["physics"]["surface_conductance"] = "not_a_scheme"
    config_path = tmp_path / "bad_physics_name.yml"
    config_path.write_text(
        yaml.safe_dump(payload, sort_keys=False), encoding="utf-8"
    )

    runner = CliRunner()
    result = runner.invoke(
        validate_cli,
        [
            "--pipeline",
            "ABC",
            "--dry-run",
            "--format",
            "json",
            str(config_path),
        ],
    )

    assert result.exit_code != 0
    envelope = json.loads(result.stdout)
    messages = "\n".join(error.get("message", "") for error in envelope["errors"])
    assert "valid names" in messages
    assert "w16" in messages
    assert "ward" in messages
