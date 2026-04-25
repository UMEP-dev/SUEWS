"""Tests for ``suews run --format json`` and the ``provenance.json`` sidecar."""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from click.testing import CliRunner

pytestmark = pytest.mark.api


def _sample_yaml_path() -> Path:
    """Locate the bundled sample_config.yml."""
    import supy

    return Path(supy.__file__).parent / "sample_data" / "sample_config.yml"


@pytest.mark.smoke
def test_run_json_emits_envelope_and_writes_provenance(tmp_path: Path) -> None:
    """End-to-end: a JSON-mode run must produce a parseable envelope and a
    ``provenance.json`` sidecar in the output directory."""
    from supy.cmd.SUEWS import SUEWS

    sample_yaml = _sample_yaml_path()
    if not sample_yaml.exists():
        pytest.skip(f"Sample config not available at {sample_yaml}")

    runner = CliRunner()
    out_dir = tmp_path / "run01"
    result = runner.invoke(
        SUEWS,
        [str(sample_yaml), "--format", "json", "--output", str(out_dir)],
    )
    assert result.exit_code == 0, result.output

    # The envelope itself should be the only payload on stdout.
    envelope = json.loads(result.output)
    assert envelope["status"] == "success"
    data = envelope["data"]
    assert "provenance_path" in data
    assert "config_hash_sha256" in data
    assert data["n_steps"] > 0

    # Provenance sidecar is present and well-formed.
    path_prov = Path(data["provenance_path"])
    assert path_prov.exists(), f"provenance.json missing at {path_prov}"
    prov = json.loads(path_prov.read_text())
    for key in (
        "command",
        "config_path",
        "config_hash_sha256",
        "output_dir",
        "output_files",
        "period_start",
        "period_end",
        "n_steps",
        "started_at",
        "ended_at",
        "schema_version",
        "suews_version",
        "supy_version",
        "git_commit",
    ):
        assert key in prov, f"provenance.json missing key {key!r}"


def test_run_json_namelist_rejected(tmp_path: Path) -> None:
    """Namelist + --format json must error cleanly with a structured envelope."""
    from supy.cmd.SUEWS import SUEWS

    nml = tmp_path / "RunControl.nml"
    nml.write_text("&RunControl\n/\n", encoding="utf-8")

    runner = CliRunner()
    result = runner.invoke(SUEWS, [str(nml), "--format", "json"])
    assert result.exit_code != 0
    envelope = json.loads(result.output)
    assert envelope["status"] == "error"
    assert any(
        "json" in (e.get("message", "").lower() if isinstance(e, dict) else str(e).lower())
        for e in envelope["errors"]
    )
