"""Tests for ``suews init`` (gh#1362).

Covers:
- JSON envelope success on the simple-urban template.
- Default text output mode.
- Refusal to overwrite an existing config in the target directory.
- Reserved-but-unshipped templates return a structured error envelope.
- Acceptance criterion: the scaffolded YAML parses under ``suews validate``.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest
import yaml
from click.testing import CliRunner

pytestmark = pytest.mark.api


def test_init_simple_urban_json(tmp_path: Path) -> None:
    """Successful scaffold produces a config file and a parseable envelope."""
    from supy.cmd.init_case import init_case_cmd

    out_dir = tmp_path / "case01"

    runner = CliRunner()
    result = runner.invoke(
        init_case_cmd,
        [str(out_dir), "--template", "simple-urban", "--format", "json"],
    )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "success"

    data = envelope["data"]
    assert data["template"] == "simple-urban"
    assert data["target_dir"] == str(out_dir)
    assert data["files_created"], "files_created list must be non-empty"
    assert data["schema_version"], "schema_version must be reported"
    assert data["next_steps"], "next_steps must be non-empty"
    assert data["next_steps"][-1] == f"suews run {out_dir / 'sample_config.yml'}"
    assert "--output" not in data["next_steps"][-1]

    # Config file must exist on disk and look like a SUEWS YAML.
    path_yaml = out_dir / "sample_config.yml"
    assert path_yaml.exists()
    parsed = yaml.safe_load(path_yaml.read_text(encoding="utf-8"))
    assert "schema_version" in parsed
    assert "model" in parsed


def test_init_text_format_succeeds(tmp_path: Path) -> None:
    """Default text mode prints a human-readable summary."""
    from supy.cmd.init_case import init_case_cmd

    out_dir = tmp_path / "case02"

    runner = CliRunner()
    result = runner.invoke(init_case_cmd, [str(out_dir)])
    assert result.exit_code == 0, result.output
    assert "Initialised" in result.output
    assert f"suews run {out_dir}/sample_config.yml" in result.output
    assert "--output" not in result.output
    assert (out_dir / "sample_config.yml").exists()


def test_init_unshipped_template_returns_structured_error(tmp_path: Path) -> None:
    """Reserved-but-unshipped templates must return a clear envelope error."""
    from supy.cmd.init_case import init_case_cmd

    out_dir = tmp_path / "case03"

    runner = CliRunner()
    result = runner.invoke(
        init_case_cmd,
        [str(out_dir), "--template", "multi-site", "--format", "json"],
    )
    assert result.exit_code != 0
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert envelope["errors"]
    msg = envelope["errors"][0]["message"]
    assert "not yet shipped" in msg.lower() or "reserved" in msg.lower()


def test_init_refuses_overwrite(tmp_path: Path) -> None:
    """A second init into the same directory must refuse to clobber."""
    from supy.cmd.init_case import init_case_cmd

    out_dir = tmp_path / "case04"

    runner = CliRunner()
    result_first = runner.invoke(
        init_case_cmd,
        [str(out_dir), "--template", "simple-urban", "--format", "json"],
    )
    assert result_first.exit_code == 0, result_first.output

    result_second = runner.invoke(
        init_case_cmd,
        [str(out_dir), "--template", "simple-urban", "--format", "json"],
    )
    assert result_second.exit_code != 0
    envelope = json.loads(result_second.stdout)
    assert envelope["status"] == "error"


def test_init_companion_forcing_copied(tmp_path: Path) -> None:
    """Bundled forcing file is copied so the case runs out of the box."""
    from supy.cmd.init_case import init_case_cmd

    out_dir = tmp_path / "case05"

    runner = CliRunner()
    result = runner.invoke(init_case_cmd, [str(out_dir)])
    assert result.exit_code == 0, result.output
    assert (out_dir / "Kc_2012_data_60.txt").exists()


def test_init_output_validates_via_dispatcher(tmp_path: Path) -> None:
    """Acceptance: ``suews validate`` accepts the scaffolded YAML.

    Drives the validate subcommand the same way users will -- through the
    unified dispatcher group -- so the chain mirrors the spec in gh#1362.
    """
    from supy.cmd.init_case import init_case_cmd
    from supy.cmd.suews_cli import cli as dispatcher

    out_dir = tmp_path / "case06"
    runner = CliRunner()
    result_init = runner.invoke(init_case_cmd, [str(out_dir), "--format", "json"])
    assert result_init.exit_code == 0, result_init.output

    path_yaml = out_dir / "sample_config.yml"
    assert path_yaml.exists()

    # Use dry-run pipeline C: read-only schema validation, no side files.
    result_validate = CliRunner().invoke(
        dispatcher,
        ["validate", "--pipeline", "C", "--dry-run", str(path_yaml)],
    )
    assert result_validate.exit_code == 0, (
        "suews validate (dry-run pipeline C) failed on scaffolded config:\n"
        f"{result_validate.output}"
    )
