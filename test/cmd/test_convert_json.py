"""Tests for ``suews convert --format json``."""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from click.testing import CliRunner

pytestmark = pytest.mark.api


def test_convert_json_yaml_upgrade(tmp_path: Path) -> None:
    """A no-op YAML upgrade in JSON mode emits a success envelope."""
    from supy.cmd.table_converter import convert_table_cmd

    # Use the bundled sample config — already at the current schema, so an
    # upgrade is essentially a re-emission. This exercises the success path.
    import supy

    sample = Path(supy.__file__).parent / "sample_data" / "sample_config.yml"
    if not sample.exists():
        pytest.skip(f"Sample config not available at {sample}")

    out = tmp_path / "out.yml"

    runner = CliRunner()
    result = runner.invoke(
        convert_table_cmd,
        ["-i", str(sample), "-o", str(out), "--format", "json"],
    )
    assert result.exit_code == 0, result.output

    # The yaml_upgrade utility prints progress lines to stderr; filter to stdout.
    envelope = json.loads(result.stdout)
    assert envelope["status"] in {"success", "warning"}
    assert envelope["data"]["input_type"] == "yaml"
    assert envelope["data"]["output"] == str(out)
    assert out.exists()


def test_convert_json_missing_args() -> None:
    """Missing --input / --output should produce a structured error envelope."""
    from supy.cmd.table_converter import convert_table_cmd

    runner = CliRunner()
    result = runner.invoke(convert_table_cmd, ["--format", "json"])
    assert result.exit_code != 0
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert envelope["errors"], "expected at least one error"
