"""End-to-end tests for the validator pipeline's structured JSON output.

Each phase (A, B, C) writes a ``<report>.json`` sidecar next to the
existing ``<report>.txt``. The sidecar conforms to ``PhaseReport.to_dict()``
from ``report_schema.py`` and is the canonical machine-readable
representation of the phase's findings.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

import supy as sp
from supy.data_model.validation.pipeline.orchestrator import (
    run_phase_a,
    run_phase_b,
    run_phase_c,
)
from supy.data_model.validation.pipeline.report_schema import PhaseReport

pytestmark = pytest.mark.api


@pytest.fixture
def sample_config_path() -> Path:
    return Path(sp.__file__).parent / "sample_data" / "sample_config.yml"


def test_phase_a_emits_json_sidecar(tmp_path, sample_config_path):
    """Phase A writes a JSON sidecar alongside its text report."""
    uptodate_file = tmp_path / "uptodate.yml"
    report_file = tmp_path / "report.txt"

    report = run_phase_a(
        user_yaml_file=str(sample_config_path),
        standard_yaml_file=str(sample_config_path),
        uptodate_file=str(uptodate_file),
        report_file=str(report_file),
        silent=True,
        forcing="off",
    )

    assert isinstance(report, PhaseReport)
    assert report.phase == "A"

    json_path = report_file.with_suffix(".json")
    assert json_path.exists(), "Phase A should write a JSON sidecar"

    payload = json.loads(json_path.read_text())
    assert payload["phase"] == "A"
    assert payload["status"] in {"PASSED", "WARNING", "FAILED"}
    assert isinstance(payload["issues"], list)
    for issue in payload["issues"]:
        assert issue["phase"] == "A"
        assert "severity" in issue
        assert "code" in issue
        # Phase A codes always start with "A."
        assert issue["code"].startswith("A.")


def test_phase_b_emits_json_sidecar(tmp_path, sample_config_path):
    """Phase B writes a JSON sidecar alongside its text report."""
    science_yaml = tmp_path / "science.yml"
    science_report = tmp_path / "science_report.txt"

    report = run_phase_b(
        user_yaml_file=str(sample_config_path),
        uptodate_file=str(sample_config_path),  # Skip Phase A
        standard_yaml_file=str(sample_config_path),
        science_yaml_file=str(science_yaml),
        science_report_file=str(science_report),
        phase_a_report_file=str(tmp_path / "phase_a_report.txt"),
        phase_a_performed=False,
        silent=True,
    )

    assert isinstance(report, PhaseReport)
    assert report.phase == "B"

    json_path = science_report.with_suffix(".json")
    assert json_path.exists(), "Phase B should write a JSON sidecar"

    payload = json.loads(json_path.read_text())
    assert payload["phase"] == "B"
    assert payload["status"] in {"PASSED", "WARNING", "FAILED"}
    assert isinstance(payload["issues"], list)
    # Every issue must carry the canonical fields
    for issue in payload["issues"]:
        assert issue["phase"] == "B"
        assert "severity" in issue
        assert "code" in issue
        assert "message" in issue


def test_phase_c_emits_json_for_passing_config(tmp_path, sample_config_path):
    """Phase C writes a JSON sidecar even on the success path."""
    pydantic_yaml = tmp_path / "pydantic.yml"
    pydantic_report = tmp_path / "pydantic_report.txt"

    report = run_phase_c(
        input_yaml_file=str(sample_config_path),
        pydantic_yaml_file=str(pydantic_yaml),
        pydantic_report_file=str(pydantic_report),
        phases_run=["C"],
        silent=True,
    )

    assert isinstance(report, PhaseReport)
    assert report.phase == "C"
    json_path = pydantic_report.with_suffix(".json")
    assert json_path.exists()
    payload = json.loads(json_path.read_text())
    assert payload["phase"] == "C"
    # Sample config should pass; status is PASSED unless there are
    # legitimate warnings in the sample.
    assert payload["status"] in {"PASSED", "WARNING"}


def test_phase_c_emits_structured_pydantic_errors_for_bad_config(tmp_path):
    """A YAML that fails Pydantic produces ``C.PYDANTIC.*`` issues."""
    bad_yaml = tmp_path / "bad.yml"
    # ``tstep`` must be an integer; supplying a string forces a Pydantic error.
    bad_yaml.write_text(
        "model:\n"
        "  control:\n"
        "    tstep: not_an_int\n"
        "    forcing_file: forcing.txt\n"
        "sites: []\n"
    )
    pydantic_yaml = tmp_path / "pydantic.yml"
    pydantic_report = tmp_path / "pydantic_report.txt"

    report = run_phase_c(
        input_yaml_file=str(bad_yaml),
        pydantic_yaml_file=str(pydantic_yaml),
        pydantic_report_file=str(pydantic_report),
        phases_run=["C"],
        silent=True,
    )

    assert report.has_errors, "A malformed config must produce errors"
    payload = json.loads(pydantic_report.with_suffix(".json").read_text())
    assert payload["phase"] == "C"
    assert payload["status"] == "FAILED"
    # At least one issue should be a structured Pydantic error.
    pydantic_codes = [i["code"] for i in payload["issues"] if i["code"].startswith("C.PYDANTIC.")]
    assert pydantic_codes, "Expected at least one C.PYDANTIC.* issue"


def test_phase_b_text_report_unchanged(tmp_path, sample_config_path):
    """Confirm the text report still exists and is non-empty (no regression)."""
    science_yaml = tmp_path / "science.yml"
    science_report = tmp_path / "science_report.txt"

    run_phase_b(
        user_yaml_file=str(sample_config_path),
        uptodate_file=str(sample_config_path),
        standard_yaml_file=str(sample_config_path),
        science_yaml_file=str(science_yaml),
        science_report_file=str(science_report),
        phase_a_report_file=str(tmp_path / "phase_a_report.txt"),
        phase_a_performed=False,
        silent=True,
    )

    assert science_report.exists()
    text = science_report.read_text()
    assert len(text) > 0
    # The text report still uses the historical section markers.
    # Downstream tooling that greps these strings must continue to work.
    assert "SUEWS" in text or "Phase B" in text or "Science" in text


def test_pipeline_writes_json_sidecars_alongside_text_reports(tmp_path, sample_config_path):
    """Running A then B then C produces a JSON sidecar for every text report.

    This is the end-to-end machine-format contract: any tool that wants the
    structured per-phase result reads ``<text_report_path>.json`` rather than
    parsing the human-readable form.
    """
    uptodate_file = tmp_path / "uptodate.yml"
    report_file = tmp_path / "report.txt"
    science_yaml = tmp_path / "science.yml"
    science_report = tmp_path / "science_report.txt"
    pydantic_yaml = tmp_path / "pydantic.yml"
    pydantic_report = tmp_path / "pydantic_report.txt"

    a_report = run_phase_a(
        user_yaml_file=str(sample_config_path),
        standard_yaml_file=str(sample_config_path),
        uptodate_file=str(uptodate_file),
        report_file=str(report_file),
        silent=True,
        forcing="off",
    )
    b_report = run_phase_b(
        user_yaml_file=str(sample_config_path),
        uptodate_file=str(uptodate_file),
        standard_yaml_file=str(sample_config_path),
        science_yaml_file=str(science_yaml),
        science_report_file=str(science_report),
        phase_a_report_file=str(report_file),
        phase_a_performed=True,
        silent=True,
    )
    c_report = run_phase_c(
        input_yaml_file=str(sample_config_path),
        pydantic_yaml_file=str(pydantic_yaml),
        pydantic_report_file=str(pydantic_report),
        phases_run=["A", "B", "C"],
        silent=True,
    )

    # Every phase should have advertised its JSON sidecar via PhaseReport.
    for r, expected in [
        (a_report, report_file),
        (b_report, science_report),
        (c_report, pydantic_report),
    ]:
        assert r.json_report_path is not None, f"{r.phase}: missing json_report_path"
        assert Path(r.json_report_path) == expected.with_suffix(".json")
        assert Path(r.json_report_path).exists()

    # Each sidecar carries the same canonical schema.
    for r in (a_report, b_report, c_report):
        payload = json.loads(Path(r.json_report_path).read_text())
        assert set(payload.keys()) == {
            "phase", "status", "issues", "yaml_in", "yaml_out",
            "text_report_path", "json_report_path", "extra",
        }
        assert payload["phase"] == r.phase
