"""Scenario: cli-yaml-benchmark-run."""

from pathlib import Path
import shutil

from click.testing import CliRunner
import pytest

from supy.cmd.SUEWS import SUEWS as suews_run_cmd

from helpers import FIXTURES, assert_non_empty_files


pytestmark = [pytest.mark.api, pytest.mark.e2e]


def _copy_benchmark_short(workspace: Path) -> Path:
    """Copy the short benchmark YAML and its forcing file into a scenario workspace."""
    workspace.mkdir(parents=True, exist_ok=True)
    forcing_dir = workspace / "forcing"
    forcing_dir.mkdir()

    source_yaml = FIXTURES / "benchmark1" / "benchmark1_short.yml"
    source_forcing = FIXTURES / "benchmark1" / "forcing" / "Kc1_2011_data_5_short.txt"

    target_yaml = workspace / source_yaml.name
    shutil.copy2(source_yaml, target_yaml)
    shutil.copy2(source_forcing, forcing_dir / source_forcing.name)
    return target_yaml


def test_cli_yaml_benchmark_run_creates_outputs_without_legacy_warning(tmp_path, monkeypatch):
    """Scenario: cli-yaml-benchmark-run.

    Persona: command-line user starting from a YAML benchmark.
    Starting condition: a copied short benchmark YAML and forcing file sit in a clean workspace.
    User action: run `suews-run benchmark1_short.yml`.
    Expected warning/error: no namelist deprecation warning.
    Expected result: successful CLI output and non-empty result artefacts.
    """
    workspace = tmp_path / "workspace"
    config_path = _copy_benchmark_short(workspace)
    runner = CliRunner()
    monkeypatch.chdir(workspace)

    result = runner.invoke(suews_run_cmd, [config_path.name], catch_exceptions=False)

    assert result.exit_code == 0, result.output
    assert "YAML configuration" in result.output
    assert "SUEWS run successfully done" in result.output
    assert "DEPRECATION WARNING" not in result.output

    artefacts = [
        path
        for path in workspace.iterdir()
        if path.is_file() and path.name != config_path.name
    ]
    assert_non_empty_files(artefacts)
