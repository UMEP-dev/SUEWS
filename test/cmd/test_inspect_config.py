"""Tests for ``suews inspect``."""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from click.testing import CliRunner

pytestmark = pytest.mark.api


def _sample_yaml_path() -> Path:
    import supy

    return Path(supy.__file__).parent / "sample_data" / "sample_config.yml"


def test_inspect_sample_config_returns_surface_fractions_summing_to_one() -> None:
    """The bundled sample config has land-cover fractions summing to 1.0."""
    from supy.cmd.inspect_config import inspect_config_cmd

    sample = _sample_yaml_path()
    if not sample.exists():
        pytest.skip(f"Sample config not available at {sample}")

    runner = CliRunner()
    result = runner.invoke(
        inspect_config_cmd, [str(sample), "--format", "json"]
    )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] in {"success", "warning"}

    data = envelope["data"]
    assert data["config_path"] == str(sample)
    assert data["schema_version"], "schema_version must be reported"
    assert data["sites"], "at least one site must be reported"

    fractions = data["surface_cover_fraction"]
    assert set(fractions) == {"paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"}
    list_values = [v for v in fractions.values() if v is not None]
    total = sum(list_values)
    assert abs(total - 1.0) < 1e-6, (
        "Surface fractions must sum to 1.0; got %s -> %.6f" % (fractions, total)
    )


def test_inspect_text_format_prints_overview() -> None:
    """Text mode emits a human-readable overview."""
    from supy.cmd.inspect_config import inspect_config_cmd

    sample = _sample_yaml_path()
    if not sample.exists():
        pytest.skip(f"Sample config not available at {sample}")

    runner = CliRunner()
    result = runner.invoke(inspect_config_cmd, [str(sample)])
    assert result.exit_code == 0, result.output
    assert "Configuration:" in result.output
    assert "Sites" in result.output


def test_inspect_invalid_yaml_returns_error_envelope(tmp_path: Path) -> None:
    """A garbage YAML must return a structured error envelope."""
    from supy.cmd.inspect_config import inspect_config_cmd

    bad = tmp_path / "broken.yml"
    bad.write_text("this is: not\n- a valid: yaml\n  structure\n", encoding="utf-8")

    runner = CliRunner()
    result = runner.invoke(inspect_config_cmd, [str(bad), "--format", "json"])
    assert result.exit_code != 0
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert envelope["errors"]
