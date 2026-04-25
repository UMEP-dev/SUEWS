"""Tests for ``suews init``."""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from click.testing import CliRunner

pytestmark = pytest.mark.api


def test_init_simple_urban_json(tmp_path: Path) -> None:
    """Successful scaffold produces a config file and a parseable envelope."""
    from supy.cmd.init_case import init_case_cmd

    out_dir = tmp_path / "case01"

    runner = CliRunner()
    result = runner.invoke(
        init_case_cmd,
        [
            "--template",
            "simple-urban",
            "-o",
            str(out_dir),
            "--format",
            "json",
        ],
    )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "success"

    data = envelope["data"]
    assert data["template"] == "simple-urban"
    assert data["output_dir"] == str(out_dir)
    assert data["files_created"], "files_created list must be non-empty"
    assert data["schema_version"], "schema_version must be reported"
    assert data["next_steps"], "next_steps must be non-empty"

    # Config file must exist on disk.
    path_yaml = out_dir / "sample_config.yml"
    assert path_yaml.exists()
    text = path_yaml.read_text(encoding="utf-8")
    assert "schema_version" in text


def test_init_unshipped_template_returns_structured_error(tmp_path: Path) -> None:
    """Reserved-but-unshipped templates must return a clear envelope error."""
    from supy.cmd.init_case import init_case_cmd

    out_dir = tmp_path / "case02"

    runner = CliRunner()
    result = runner.invoke(
        init_case_cmd,
        [
            "--template",
            "multi-site",
            "-o",
            str(out_dir),
            "--format",
            "json",
        ],
    )
    assert result.exit_code != 0
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert envelope["errors"]
    msg = envelope["errors"][0]["message"]
    assert "not yet shipped" in msg.lower() or "reserved" in msg.lower()


def test_init_text_format_succeeds(tmp_path: Path) -> None:
    """Default text mode prints a human-readable summary."""
    from supy.cmd.init_case import init_case_cmd

    out_dir = tmp_path / "case03"

    runner = CliRunner()
    result = runner.invoke(
        init_case_cmd,
        ["--template", "simple-urban", "-o", str(out_dir)],
    )
    assert result.exit_code == 0, result.output
    assert "Initialised" in result.output or "schema_version" in result.output.lower()
    assert (out_dir / "sample_config.yml").exists()


def test_init_refuses_overwrite(tmp_path: Path) -> None:
    """A second init into the same directory must refuse to clobber."""
    from supy.cmd.init_case import init_case_cmd

    out_dir = tmp_path / "case04"

    runner = CliRunner()
    result_first = runner.invoke(
        init_case_cmd,
        ["--template", "simple-urban", "-o", str(out_dir), "--format", "json"],
    )
    assert result_first.exit_code == 0, result_first.output

    result_second = runner.invoke(
        init_case_cmd,
        ["--template", "simple-urban", "-o", str(out_dir), "--format", "json"],
    )
    assert result_second.exit_code != 0
    envelope = json.loads(result_second.stdout)
    assert envelope["status"] == "error"
