"""CLI coverage for forcing and output schema artefacts."""

from __future__ import annotations

import json

from click.testing import CliRunner
import pytest

from supy.cmd.schema_cli import cli

pytestmark = pytest.mark.api


def test_schema_cli_emits_forcing_catalogue_envelope() -> None:
    result = CliRunner().invoke(
        cli,
        [
            "--kind",
            "forcing",
            "--artifact",
            "catalogue",
            "--format",
            "json",
        ],
    )

    assert result.exit_code == 0, result.output
    envelope = json.loads(result.output)
    assert envelope["status"] == "success"
    assert envelope["data"]["kind"] == "forcing"
    assert len(envelope["data"]["variables"]) == 25


def test_schema_cli_default_remains_configuration_schema() -> None:
    result = CliRunner().invoke(cli, ["--format", "json"])

    assert result.exit_code == 0, result.output
    envelope = json.loads(result.output)
    assert "SUEWS Configuration Schema" in envelope["data"]["title"]


def test_config_catalogue_reports_a_user_error() -> None:
    result = CliRunner().invoke(
        cli,
        ["--kind", "config", "--artifact", "catalogue", "--format", "json"],
    )

    assert result.exit_code == 2
    envelope = json.loads(result.output)
    assert envelope["status"] == "error"
    assert "not supported" in envelope["errors"][0]["message"]
