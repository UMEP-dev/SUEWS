"""Contract-level UX baseline tests for the SUEWS validator across all pipeline modes.

Locks the user-facing CLI and output-artefact contracts across all seven
``--pipeline`` combinations (``A``, ``B``, ``C``, ``AB``, ``AC``, ``BC``,
``ABC``), scientific fix policies (``suggest``, ``apply``, ``off``), output
formats (``table``, ``json``), bare vs ``RefValue`` style preservation, and
failure-stopping banners so structural redesign waves preserve external UX.
"""

from __future__ import annotations

from importlib.resources import files
import json
from pathlib import Path
from typing import Any

from click.testing import CliRunner
import pytest
import yaml

from supy.cmd.suews_cli import cli as suews_cli

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-ERROR"),
]


def _copy_sample_case(dst: Path) -> Path:
    """Copy packaged sample YAML and sibling forcing data into *dst*."""
    sample_dir = files("supy").joinpath("sample_data")
    for item in sample_dir.iterdir():
        if item.is_file():
            (dst / item.name).write_bytes(item.read_bytes())
    return dst / "sample_config.yml"


def _load_yaml(path: Path) -> dict[str, Any]:
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def _write_yaml(path: Path, payload: dict[str, Any]) -> None:
    path.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")


@pytest.mark.parametrize("pipeline", ["A", "B", "C", "AB", "AC", "BC", "ABC"])
def test_all_seven_pipelines_succeed_and_emit_canonical_artefacts(
    tmp_path: Path, pipeline: str
) -> None:
    """Every pipeline combination passes on the sample config and emits canonical files."""
    config_path = _copy_sample_case(tmp_path)
    runner = CliRunner()

    result = runner.invoke(
        suews_cli,
        [
            "validate",
            "-p",
            pipeline,
            "--forcing",
            "off",
            "--format",
            "json",
            str(config_path),
        ],
    )

    assert result.exit_code == 0, result.output
    envelope = json.loads(result.output)
    assert envelope["errors"] == []
    assert envelope["data"]["phases_run"] == list(pipeline)

    report_txt = tmp_path / "report_sample_config.txt"
    report_json = tmp_path / "report_sample_config.json"
    assert report_txt.is_file()
    assert report_json.is_file()

    text = report_txt.read_text(encoding="utf-8")
    assert "Validation stopped at:" not in text

    sidecar = json.loads(report_json.read_text(encoding="utf-8"))
    assert [phase["phase"] for phase in sidecar["phases"]] == list(pipeline)
    assert all(phase["status"] != "FAILED" for phase in sidecar["phases"])
    assert envelope["data"]["validation_report"] == sidecar

    updated_candidates = {
        "A": tmp_path / "updatedA_sample_config.yml",
        "B": tmp_path / "updatedB_sample_config.yml",
    }
    expected_yaml = updated_candidates.get(
        pipeline, tmp_path / "updated_sample_config.yml"
    )
    assert expected_yaml.is_file()


@pytest.mark.parametrize("science_fixes", ["suggest", "apply", "off"])
def test_abc_science_fixes_policy_preserves_bare_and_ref_metadata(
    tmp_path: Path, science_fixes: str
) -> None:
    """ABC honours suggest/apply/off while preserving bare values and RefValue ref metadata."""
    config_path = _copy_sample_case(tmp_path)
    payload = _load_yaml(config_path)
    land_cover = payload["sites"][0]["properties"]["land_cover"]
    # Keep paved as RefValue with custom ref metadata, bldgs as bare scalar.
    ref_meta = {"desc": "GIS survey 2026", "ID": "LC-SURVEY"}
    paved_val = land_cover["paved"]["sfr"]["value"] - 0.00005
    bldgs_val = land_cover["bldgs"]["sfr"]["value"]
    land_cover["paved"]["sfr"] = {"value": paved_val, "ref": dict(ref_meta)}
    land_cover["bldgs"]["sfr"] = bldgs_val
    original_sum = sum(
        (s["sfr"]["value"] if isinstance(s["sfr"], dict) else s["sfr"])
        for s in land_cover.values()
        if "sfr" in s
    )
    _write_yaml(config_path, payload)

    runner = CliRunner()
    result = runner.invoke(
        suews_cli,
        [
            "validate",
            "-p",
            "ABC",
            "--forcing",
            "off",
            "--science-fixes",
            science_fixes,
            "--format",
            "json",
            str(config_path),
        ],
    )

    assert result.exit_code == 0, result.output
    updated = _load_yaml(tmp_path / "updated_sample_config.yml")
    updated_lc = updated["sites"][0]["properties"]["land_cover"]
    assert isinstance(updated_lc["bldgs"]["sfr"], (int, float))
    assert updated_lc["paved"]["sfr"]["ref"] == ref_meta

    updated_sum = sum(
        (s["sfr"]["value"] if isinstance(s["sfr"], dict) else s["sfr"])
        for s in updated_lc.values()
        if "sfr" in s
    )
    expected_sum = 1.0 if science_fixes == "apply" else original_sum
    assert updated_sum == pytest.approx(expected_sum, abs=1e-10)

    report_text = (tmp_path / "report_sample_config.txt").read_text(encoding="utf-8")
    sidecar = json.loads(
        (tmp_path / "report_sample_config.json").read_text(encoding="utf-8")
    )
    b_issues = sidecar["phases"][1]["issues"]
    if science_fixes == "suggest":
        assert "## SUGGESTED UPDATES" in report_text
        assert any(issue["severity"] == "SUGGESTION" for issue in b_issues)
    elif science_fixes == "apply":
        assert "## APPLIED UPDATES" in report_text
        assert any(issue["severity"] == "APPLIED_FIX" for issue in b_issues)
    else:
        assert "## SUGGESTED UPDATES" not in report_text
        assert not any(
            issue["severity"] in {"SUGGESTION", "APPLIED_FIX"} for issue in b_issues
        )


@pytest.mark.parametrize(
    ("failed_phase", "expected_stage"),
    [
        ("A", "Completeness Check"),
        ("B", "Scientific Validation"),
        ("C", "Model Compatibility"),
    ],
)
def test_abc_failure_stopping_phase_and_exit_code(
    tmp_path: Path, failed_phase: str, expected_stage: str
) -> None:
    """ABC stops at the first failing phase, sets exit code 1, and records stopping banner."""
    config_path = _copy_sample_case(tmp_path)
    payload = _load_yaml(config_path)
    if failed_phase == "A":
        payload["model"]["physics"].pop("net_radiation")
    elif failed_phase == "B":
        payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] = -0.2
    else:
        payload["sites"][0]["properties"]["unexpected_phase_c_key"] = 123
    _write_yaml(config_path, payload)

    runner = CliRunner()
    result = runner.invoke(
        suews_cli,
        ["validate", "-p", "ABC", "--forcing", "off", "--format", "json", str(config_path)],
    )

    assert result.exit_code == 1, result.output
    envelope = json.loads(result.output)
    assert envelope["status"] == "error"
    assert len(envelope["errors"]) >= 1

    report_text = (tmp_path / "report_sample_config.txt").read_text(encoding="utf-8")
    assert f"Validation stopped at: {expected_stage}" in report_text
    assert "## ACTION NEEDED" in report_text

    sidecar = json.loads(
        (tmp_path / "report_sample_config.json").read_text(encoding="utf-8")
    )
    expected_phases = {"A": ["A"], "B": ["A", "B"], "C": ["A", "B", "C"]}[failed_phase]
    assert [phase["phase"] for phase in sidecar["phases"]] == expected_phases
    assert sidecar["phases"][-1]["status"] == "FAILED"
