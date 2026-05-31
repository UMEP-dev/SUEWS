"""Scenario: cli-missing-forcing-failure."""

from click.testing import CliRunner
import pytest

from supy.cmd.SUEWS import SUEWS as suews_run_cmd

pytestmark = [pytest.mark.api, pytest.mark.e2e]


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
