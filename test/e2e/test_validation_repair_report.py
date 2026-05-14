"""Scenario: validation-repair-report."""

from pathlib import Path

from click.testing import CliRunner
import pytest

from supy.cmd.validate_config import cli as validate_cmd

from helpers import assert_non_empty_files


pytestmark = [pytest.mark.api, pytest.mark.e2e]


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
    assert_non_empty_files([report_path, updated_path])
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
