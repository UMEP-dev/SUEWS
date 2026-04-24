"""Scenario-level end-to-end tests for SUEWS user and maintainer workflows."""

from pathlib import Path
import shutil

from click.testing import CliRunner
import pytest
import yaml

from supy import SUEWSSimulation
from supy.cmd.SUEWS import SUEWS as suews_run_cmd
from supy.cmd.table_converter import convert_table_cmd
from supy.cmd.validate_config import cli as validate_cmd
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.schema import CURRENT_SCHEMA_VERSION
from supy.util.converter.yaml_upgrade import upgrade_yaml


pytestmark = [pytest.mark.api, pytest.mark.e2e]

REPO_ROOT = Path(__file__).resolve().parents[2]
FIXTURES = REPO_ROOT / "test" / "fixtures"


def _as_dataframe(output):
    """Return the dataframe inside a SUEWSOutput-like object."""
    if hasattr(output, "to_dataframe"):
        return output.to_dataframe()
    if hasattr(output, "df"):
        return output.df
    return output


def _assert_non_empty_files(paths):
    assert paths, "Expected at least one output file"
    for path in paths:
        output_path = Path(path)
        assert output_path.exists(), f"Expected output file to exist: {output_path}"
        assert output_path.stat().st_size > 0, f"Expected non-empty output file: {output_path}"


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


def _write_validation_problem(path: Path) -> None:
    path.write_text(
        """
name: e2e validation repair
description: A deliberately flawed config for the validation scenario
model:
  control:
    tstep: 300
    forcing_file:
      value: missing_forcing.txt
    output_file:
      format: txt
      freq: 3600
      groups: ["SUEWS"]
    custom_control_param: kept for user review
  physics:
    emissionsmethod:
      value: 2
    storageheatmethod:
      value: 1
    diagmethod:
      value: 2
    cp:
      value: 1005
    custom_physics_param:
      value: 999
sites:
  - name: ScenarioSite
    gridiv: 1
    properties:
      alb:
        value: 0.15
      custom_site_property:
        value: 123
""".strip(),
        encoding="utf-8",
    )


def test_python_user_short_run_saves_outputs(tmp_path):
    """Scenario: python-user-short-run.

    Persona: urban climate researcher using the Python API.
    Starting condition: bundled sample data are available from the installed package.
    User action: create a sample simulation, run a short deterministic window, save results.
    Expected warning/error: none.
    Expected result: SUEWS output variables, final state, and non-empty saved files.
    """
    sim = SUEWSSimulation.from_sample_data()

    output = sim.run(end_date=sim.forcing.index[23])
    df_output = _as_dataframe(output)
    saved_paths = sim.save(tmp_path)

    assert sim.state_final is not None
    assert not sim.state_final.empty
    assert not df_output.empty
    assert "SUEWS" in set(df_output.columns.get_level_values("group"))
    assert {"QH", "QE"} <= set(df_output.columns.get_level_values("var"))
    _assert_non_empty_files(saved_paths)


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
    _assert_non_empty_files(artefacts)


def test_cli_missing_forcing_fails_without_outputs(tmp_path, monkeypatch):
    """Scenario: cli-missing-forcing-failure.

    Persona: command-line user with an incomplete YAML file.
    Starting condition: the config is syntactically valid but contains no forcing data.
    User action: run `suews-run missing_forcing.yml`.
    Expected warning/error: clear "No forcing data" failure.
    Expected result: non-zero exit and no misleading output artefacts.
    """
    config_path = tmp_path / "missing_forcing.yml"
    config_path.write_text(
        """
name: missing forcing scenario
model:
  control:
    tstep: 3600
sites:
  - name: TestSite
    gridiv: 1
""".strip(),
        encoding="utf-8",
    )
    runner = CliRunner()
    monkeypatch.chdir(tmp_path)

    result = runner.invoke(suews_run_cmd, [config_path.name], catch_exceptions=False)

    assert result.exit_code != 0
    assert "No forcing data" in result.output
    artefacts = [path for path in tmp_path.iterdir() if path.name != config_path.name]
    assert artefacts == []


def test_legacy_table_conversion_output_can_run(tmp_path):
    """Scenario: legacy-table-migration-run.

    Persona: user migrating an old namelist table setup.
    Starting condition: legacy `RunControl.nml` tables are available.
    User action: convert to YAML, load the YAML, attach tiny forcing, run SUEWS.
    Expected warning/error: none.
    Expected result: converted YAML parses and produces QH/QE output variables.
    """
    input_file = FIXTURES / "data_test" / "AVL_1_LDN1" / "RunControl.nml"
    forcing_file = FIXTURES / "benchmark1" / "forcing" / "Kc1_2011_data_5_tiny.txt"
    output_file = tmp_path / "converted.yml"

    result = CliRunner().invoke(
        convert_table_cmd,
        ["--input", str(input_file), "--output", str(output_file)],
        catch_exceptions=False,
    )

    assert result.exit_code == 0, result.output
    assert output_file.exists()

    sim = SUEWSSimulation(str(output_file))
    sim.update_forcing(str(forcing_file))
    df_output = _as_dataframe(sim.run())

    assert not df_output.empty
    assert {"QH", "QE"} <= set(df_output.columns.get_level_values("var"))


def test_validation_repair_report_is_meaningful_and_repeatable(tmp_path, monkeypatch):
    """Scenario: validation-repair-report.

    Persona: maintainer checking YAML drift before release.
    Starting condition: YAML contains missing, renamed, and extra parameters.
    User action: run `suews-validate --pipeline A --forcing off` twice.
    Expected warning/error: report names the parameters needing action.
    Expected result: updated YAML and report are non-empty and not truncated on repeat.
    """
    config_path = tmp_path / "validation_problem.yml"
    _write_validation_problem(config_path)
    runner = CliRunner()
    monkeypatch.chdir(tmp_path)

    first = runner.invoke(
        validate_cmd,
        ["--pipeline", "A", "--forcing", "off", config_path.name],
        catch_exceptions=False,
    )
    report_path = tmp_path / "report_validation_problem.txt"
    updated_path = tmp_path / "updatedA_validation_problem.yml"

    assert first.exit_code in (0, 1), first.output
    _assert_non_empty_files([report_path, updated_path])
    first_sizes = {
        report_path.name: report_path.stat().st_size,
        updated_path.name: updated_path.stat().st_size,
    }

    report_text = report_path.read_text(encoding="utf-8", errors="replace")
    updated_text = updated_path.read_text(encoding="utf-8", errors="replace")

    assert "# SUEWS Validation Report" in report_text
    assert "net_radiation" in report_text
    assert "diagmethod changed to roughness_sublayer" in report_text
    assert "cp changed to rho_cp" in report_text
    assert "custom_physics_param" in report_text
    assert "roughness_sublayer" in updated_text
    assert "rho_cp" in updated_text

    second = runner.invoke(
        validate_cmd,
        ["--pipeline", "A", "--forcing", "off", config_path.name],
        catch_exceptions=False,
    )

    assert second.exit_code in (0, 1), second.output
    for name, size_before in first_sizes.items():
        output = tmp_path / name
        assert output.exists(), f"{name} disappeared after repeat validation"
        assert output.stat().st_size >= size_before, f"{name} was truncated"


def test_release_yaml_upgrade_preserves_migration_intent(tmp_path):
    """Scenario: release-yaml-upgrade.

    Persona: maintainer guarding release fixture compatibility.
    Starting condition: a vendored release YAML predates current schema cleanup.
    User action: upgrade it with its release tag and parse it under the current schema.
    Expected warning/error: none.
    Expected result: current schema stamp and explicit field migration.
    """
    source = FIXTURES / "release_configs" / "2025.10.15.yml"
    upgraded = tmp_path / "upgraded.yml"

    upgrade_yaml(input_path=source, output_path=upgraded, from_ver="2025.10.15")
    SUEWSConfig.from_yaml(str(upgraded))

    payload = yaml.safe_load(upgraded.read_text(encoding="utf-8"))
    assert payload["schema_version"] == CURRENT_SCHEMA_VERSION

    building_archetype = payload["sites"][0]["properties"]["building_archetype"]
    stebbs = payload["sites"][0]["properties"]["stebbs"]
    assert "Wallx1" not in building_archetype
    assert "wall_outer_heat_capacity_fraction" in building_archetype
    assert "DHWVesselEmissivity" not in stebbs
    assert "MinimumVolumeOfDHWinUse" not in stebbs
