"""Tests for ``suews inspect``."""

from __future__ import annotations

from contextlib import contextmanager
from importlib.resources import as_file, files
import json
from pathlib import Path
import shutil
from typing import Iterator

from click.testing import CliRunner
import pytest
import yaml

pytestmark = pytest.mark.api


@contextmanager
def _sample_yaml_path() -> Iterator[Path]:
    """Yield the bundled sample config as a filesystem path for Click."""
    resource = files("supy").joinpath("sample_data", "sample_config.yml")
    with as_file(resource) as path_sample:
        yield path_sample


@contextmanager
def _sample_forcing_path() -> Iterator[Path]:
    """Yield the bundled sample forcing as a filesystem path."""
    resource = files("supy").joinpath("sample_data", "Kc_2012_data_60.txt")
    with as_file(resource) as path_forcing:
        yield path_forcing


def test_inspect_sample_config_returns_surface_fractions_summing_to_one() -> None:
    """The bundled sample config has land-cover fractions summing to 1.0."""
    from supy.cmd.inspect_config import inspect_config_cmd

    with _sample_yaml_path() as sample:
        if not sample.exists():
            pytest.skip(f"Sample config not available at {sample}")

        runner = CliRunner()
        result = runner.invoke(inspect_config_cmd, [str(sample), "--format", "json"])
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] in {"success", "warning"}

    data = envelope["data"]
    assert data["config_path"] == str(sample)
    assert data["schema_version"], "schema_version must be reported"
    assert data["sites"], "at least one site must be reported"
    assert data["forcing_summary"]["file"] == "Kc_2012_data_60.txt"
    assert data["forcing_summary"]["n_columns"] == 24
    assert data["forcing_summary"]["columns"][0] == "iy"

    fractions = data["surface_cover_fraction"]
    assert set(fractions) == {
        "paved",
        "bldgs",
        "evetr",
        "dectr",
        "grass",
        "bsoil",
        "water",
    }
    list_values = [v for v in fractions.values() if v is not None]
    total = sum(list_values)
    assert abs(total - 1.0) < 1e-6, (
        "Surface fractions must sum to 1.0; got %s -> %.6f" % (fractions, total)
    )


def test_inspect_list_forcing_reports_each_file(tmp_path: Path) -> None:
    """A valid multi-file forcing config reports per-file header summaries."""
    from supy.cmd.inspect_config import inspect_config_cmd

    with _sample_yaml_path() as sample, _sample_forcing_path() as forcing:
        if not sample.exists() or not forcing.exists():
            pytest.skip("Sample config or forcing file not available")

        payload = yaml.safe_load(sample.read_text(encoding="utf-8"))
        list_forcing = ["forcing_a.txt", "forcing_b.txt"]
        for forcing_name in list_forcing:
            shutil.copyfile(forcing, tmp_path / forcing_name)
        payload["model"]["control"]["forcing"] = {
            "file": {"value": list_forcing}
        }

    path_config = tmp_path / "config.yml"
    path_config.write_text(
        yaml.safe_dump(payload, sort_keys=False), encoding="utf-8"
    )

    runner = CliRunner()
    result = runner.invoke(inspect_config_cmd, [str(path_config), "--format", "json"])
    assert result.exit_code == 0, result.output

    summary = json.loads(result.stdout)["data"]["forcing_summary"]
    assert summary["file"] == list_forcing
    assert summary["n_columns"] == 24
    assert summary["columns"][0] == "iy"
    assert [item["n_columns"] for item in summary["files"]] == [24, 24]


def test_inspect_top_level_command_metadata_is_not_duplicated() -> None:
    """The unified ``suews inspect`` path records the command once."""
    from supy.cmd.suews_cli import cli

    with _sample_yaml_path() as sample:
        if not sample.exists():
            pytest.skip(f"Sample config not available at {sample}")

        runner = CliRunner()
        result = runner.invoke(cli, ["inspect", str(sample), "--format", "json"])
    assert result.exit_code == 0, result.output
    command = json.loads(result.stdout)["meta"]["command"]
    assert "inspect inspect" not in command


def test_inspect_text_format_prints_overview() -> None:
    """Text mode emits a human-readable overview."""
    from supy.cmd.inspect_config import inspect_config_cmd

    with _sample_yaml_path() as sample:
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
