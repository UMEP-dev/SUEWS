"""Tests for the canonical validator report schema."""

from __future__ import annotations

import json

import pytest

from supy.data_model.validation.pipeline.report_schema import (
    Issue,
    JSONReportWriter,
    PhaseReport,
    SEVERITY_ERROR,
    SEVERITY_WARNING,
    ValidationReport,
)

pytestmark = pytest.mark.api


def test_issue_round_trips_through_dict():
    issue = Issue(
        phase="B",
        severity=SEVERITY_ERROR,
        code="B.PHYSICS.LAIMAX",
        message="laimax must exceed laimin",
        yaml_path="sites.0.properties.land_cover.evetr.lai.laimax",
        site_gridid=1,
        category="PHYSICS",
        suggested_value=5.0,
    )
    payload = issue.to_dict()
    assert payload["severity"] == "ERROR"
    assert payload["code"] == "B.PHYSICS.LAIMAX"
    assert payload["yaml_path"].endswith("laimax")
    # Round-trip through JSON to confirm it is fully serialisable.
    assert json.loads(json.dumps(payload)) == payload


def test_issue_rejects_invalid_phase_or_severity():
    with pytest.raises(ValueError):
        Issue(phase="Z", severity=SEVERITY_ERROR, code="X", message="m")
    with pytest.raises(ValueError):
        Issue(phase="A", severity="OOPS", code="X", message="m")


def test_phase_report_status_derives_from_issues():
    err = Issue(phase="A", severity=SEVERITY_ERROR, code="A.X", message="bad")
    warn = Issue(phase="A", severity=SEVERITY_WARNING, code="A.Y", message="hmm")

    failed = PhaseReport(phase="A", issues=[err, warn])
    assert failed.has_errors is True
    assert failed.status == "FAILED"

    warned = PhaseReport(phase="A", issues=[warn])
    assert warned.has_errors is False
    assert warned.status == "WARNING"

    clean = PhaseReport(phase="A", issues=[])
    assert clean.status == "PASSED"


def test_phase_report_to_dict_is_json_safe():
    report = PhaseReport(
        phase="B",
        issues=[Issue(phase="B", severity=SEVERITY_WARNING, code="B.X", message="x")],
        yaml_in="/tmp/in.yml",
        yaml_out="/tmp/out.yml",
        text_report_path="/tmp/report.txt",
        json_report_path="/tmp/report.json",
    )
    payload = report.to_dict()
    assert payload["phase"] == "B"
    assert payload["status"] == "WARNING"
    assert len(payload["issues"]) == 1
    assert json.loads(json.dumps(payload)) == payload


def test_validation_report_aggregates_overall_status():
    a = PhaseReport(phase="A", issues=[])
    b = PhaseReport(
        phase="B",
        issues=[Issue(phase="B", severity=SEVERITY_WARNING, code="B.W", message="w")],
    )
    report = ValidationReport(phases=[a, b])
    assert report.overall_status == "WARNING"

    c = PhaseReport(
        phase="C",
        issues=[Issue(phase="C", severity=SEVERITY_ERROR, code="C.E", message="e")],
    )
    report.phases.append(c)
    assert report.overall_status == "FAILED"

    clean = ValidationReport(phases=[
        PhaseReport(phase="A", issues=[]),
        PhaseReport(phase="B", issues=[]),
    ])
    assert clean.overall_status == "PASSED"


def test_json_report_writer_emits_utf8_with_trailing_newline(tmp_path):
    writer = JSONReportWriter()
    report = PhaseReport(
        phase="A",
        issues=[Issue(phase="A", severity=SEVERITY_ERROR, code="A.X", message="é")],
    )
    out = tmp_path / "report.json"
    writer.write(out, report)

    text = out.read_text(encoding="utf-8")
    assert text.endswith("\n")
    payload = json.loads(text)
    assert payload["issues"][0]["message"] == "é"
