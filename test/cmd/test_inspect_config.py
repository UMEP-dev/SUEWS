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

    physics = data["physics"]
    assert physics["net_radiation"]["code"] == 3
    assert physics["net_radiation"]["selected"] == "ldown_air"
    assert "ldown_air" in physics["net_radiation"]["accepted_names"]
    assert physics["emissions"]["selected"] == "J11"
    assert "canonical_name" not in physics["emissions"]
    assert physics["storage_heat"]["selected"] == "ohm"
    assert physics["roughness_length_heat"]["selected"] == "K09"
    assert physics["stability"]["selected"] == "CN98"
    assert physics["surface_conductance"]["selected"] == "W16"
    assert physics["surface_conductance"]["canonical_name"] == "ward"
    assert "ohm_inc_qf" not in physics
    storage_ohm = physics["storage_heat"]["ohm"]
    assert storage_ohm["include_qf_selected"] == "exclude"
    assert storage_ohm["include_qf"] is False
    assert physics["leaf_area_index"]["internal_key"] == "laimethod"
    assert physics["leaf_area_index"]["selected"] == "modelled"
    assert physics["snow"]["internal_key"] == "snow_use"
    assert physics["snow"]["selected"] == "disabled"
    assert physics["stebbs"]["parameter_source"]["internal_key"] == "parameters"
    assert physics["stebbs"]["parameter_source"]["selected"] == "default"

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


def test_inspect_only_reports_ohm_qf_axis_when_ohm_is_selected() -> None:
    """OHMIncQF stays hidden unless storage_heat is actually OHM."""
    from supy.cmd.inspect_config import _physics_summary

    physics = _physics_summary(
        {
            "physics": {
                "storage_heat": {"value": 5},
                "ohm_inc_qf": {"value": 0},
            }
        }
    )

    assert physics["storage_heat"]["selected"] == "ehc"
    assert "ohm" not in physics["storage_heat"]


def test_inspect_reports_ohm_qf_include_branch() -> None:
    """The OHM-owned QF axis reports the true/include case directly."""
    from supy.cmd.inspect_config import _physics_summary

    physics = _physics_summary(
        {
            "physics": {
                "storage_heat": {"value": 1},
                "ohm_inc_qf": {"value": 1},
            }
        }
    )

    ohm = physics["storage_heat"]["ohm"]
    assert ohm["include_qf"] is True
    assert ohm["include_qf_code"] == 1
    assert ohm["include_qf_selected"] == "include"


def test_inspect_handles_missing_physics_block() -> None:
    """The new physics envelope key remains stable when physics is absent."""
    from supy.cmd.inspect_config import _physics_summary

    assert _physics_summary({}) == {}


def test_inspect_unwraps_stebbs_enabled_refvalue() -> None:
    """STEBBS enabled must unwrap RefValue-like dicts before bool conversion."""
    from supy.cmd.inspect_config import _physics_summary

    physics = _physics_summary(
        {
            "physics": {
                "stebbs": {
                    "enabled": {"value": False, "ref": {"DOI": "10.test/ref"}},
                },
            }
        }
    )

    assert physics["stebbs"]["enabled"] is False


def test_inspect_labels_smd_numeric_compatibility_code() -> None:
    """SMD code 2 remains readable as the unified observed source choice."""
    from supy.cmd.inspect_config import _physics_summary

    physics = _physics_summary(
        {
            "physics": {
                "soil_moisture_deficit": {"value": 2},
            }
        }
    )

    smd = physics["soil_moisture_deficit"]
    assert smd["code"] == 2
    assert smd["selected"] == "observed"
    assert smd["selected_name_is_lossy"] is True
    assert smd["accepted_names"] == ["modelled", "observed"]


def test_inspect_distinguishes_smd_observed_codes() -> None:
    """SMD observed codes share a public name but code 2 is marked lossy."""
    from supy.cmd.inspect_config import _physics_summary

    physics = _physics_summary(
        {
            "physics": {
                "soil_moisture_deficit": {"value": 1},
            }
        }
    )

    smd = physics["soil_moisture_deficit"]
    assert smd["code"] == 1
    assert smd["selected"] == "observed"
    assert "selected_name_is_lossy" not in smd


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
    assert "Physics selectors:" in result.output
    assert "leaf_area_index:" in result.output
    assert "storage_heat.ohm.include_qf:" in result.output


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
