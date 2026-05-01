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
    run_phase_b,
)
from supy.data_model.validation.pipeline.report_schema import PhaseReport

pytestmark = pytest.mark.api


@pytest.fixture
def sample_config_path() -> Path:
    return Path(sp.__file__).parent / "sample_data" / "sample_config.yml"


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
