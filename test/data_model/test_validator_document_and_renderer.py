"""Unit tests for ConfigDocument, pure TextReportRenderer, and single-pass Phase C."""

from __future__ import annotations

from importlib.resources import files
import json
from pathlib import Path
from types import MappingProxyType

import pytest
import yaml

from supy.data_model.core.config import SUEWSConfig
from supy.data_model.validation.pipeline import orchestrator
from supy.data_model.validation.pipeline.document import (
    ConfigDocument,
    ConfigPatch,
    delete_path,
    format_pydantic_loc,
    is_ref_only,
    is_ref_wrapper,
    read_path,
    read_scalar,
    site_gridid_map,
    unwrap_entry,
    write_path,
    write_scalar,
)
from supy.data_model.validation.pipeline.phase_c import validate_phase_c
from supy.data_model.validation.pipeline.renderers import render_text_report
from supy.data_model.validation.pipeline.report_schema import (
    SEVERITY_APPLIED_FIX,
    SEVERITY_ERROR,
    SEVERITY_INFO,
    SEVERITY_SUGGESTION,
    SEVERITY_WARNING,
    Issue,
    PhaseReport,
    ValidationReport,
)

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-ERROR"),
]


def _copy_sample_case(dst: Path) -> Path:
    sample_dir = files("supy").joinpath("sample_data")
    for item in sample_dir.iterdir():
        if item.is_file():
            (dst / item.name).write_bytes(item.read_bytes())
    return dst / "sample_config.yml"


def test_document_ref_and_bare_scalar_helpers() -> None:
    """RefValue, bare, ref-only, and frozen MappingProxyType values are handled uniformly."""
    ref_entry = {"value": 0.4, "ref": {"desc": "survey"}}
    ref_only = {"ref": {"desc": "placeholder"}}
    frozen = MappingProxyType({"value": 0.6, "ref": {"ID": "F1"}})

    assert is_ref_wrapper(ref_entry)
    assert not is_ref_wrapper(ref_only)
    assert is_ref_only(ref_only)
    assert not is_ref_only(ref_entry)

    assert unwrap_entry(ref_entry) == 0.4
    assert unwrap_entry(ref_only, default=-1) == -1
    assert unwrap_entry({}, default=-1) == -1
    assert unwrap_entry(0.25) == 0.25

    container = {"bare": 0.2, "wrapped": dict(ref_entry), "frozen": frozen}
    assert read_scalar(container, "bare") == 0.2
    assert read_scalar(container, "wrapped") == 0.4
    assert read_scalar(container, "frozen") == 0.6
    assert read_scalar(container, "missing", 99) == 99

    write_scalar(container, "bare", 0.3)
    write_scalar(container, "wrapped", 0.5)
    write_scalar(container, "frozen", 0.7)
    write_scalar(container, "new_wrapped", 1.0)
    write_scalar(container, "new_bare", 2.0, default_wrapped=False)

    assert container["bare"] == 0.3
    assert container["wrapped"] == {"value": 0.5, "ref": {"desc": "survey"}}
    assert container["frozen"] == {"value": 0.7, "ref": {"ID": "F1"}}
    assert container["new_wrapped"] == {"value": 1.0}
    assert container["new_bare"] == 2.0


def test_document_path_and_patch_application_preserves_style() -> None:
    """ConfigDocument.apply_patches mutates a clone and preserves bare vs RefValue style."""
    original = {
        "sites": [
            {
                "gridiv": {"value": 42},
                "properties": {
                    "land_cover": {
                        "paved": {"sfr": {"value": 0.5, "ref": {"desc": "GIS"}}},
                        "bldgs": {"sfr": 0.5},
                    },
                    "stebbs": {
                        "alpha": {"value": 1.0, "ref": {"ID": "S1"}},
                        "beta": 2.0,
                        "ref_only": {"ref": {"desc": "keep me"}},
                        "keep_key": 9.9,
                    },
                },
                "initial_states": {"snowalb": {"value": 0.5}},
            }
        ]
    }
    doc = ConfigDocument.from_dict(original, normalise=False)
    assert doc.gridid_map() == {0: 42}
    assert format_pydantic_loc(("sites", 0, "properties", "lat"), doc.gridid_map()) == (
        "sites.42.properties.lat"
    )
    assert doc.read("sites[0].properties.land_cover.paved.sfr") == 0.5
    assert read_path(doc.data, "sites[0].properties.land_cover.bldgs.sfr") == 0.5

    patches = [
        ConfigPatch(
            path="sites[0].properties.land_cover.paved.sfr",
            action="set",
            new_value=0.6,
        ),
        ConfigPatch(
            path="sites[0].properties.land_cover.bldgs.sfr",
            action="set",
            new_value=0.4,
        ),
        ConfigPatch(
            path="sites[0].initial_states.snowalb",
            action="delete",
        ),
        ConfigPatch(
            path="sites[0].properties.stebbs",
            action="nullify_tree",
            skip_keys=("keep_key",),
        ),
    ]
    patched = doc.apply_patches(patches)

    # Original document is untouched.
    assert doc.read("sites[0].properties.land_cover.paved.sfr") == 0.5
    assert "snowalb" in doc.data["sites"][0]["initial_states"]

    # Patched document preserved RefValue metadata, bare floats, ref-only, and skip_keys.
    assert patched.data["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] == {
        "value": 0.6,
        "ref": {"desc": "GIS"},
    }
    assert patched.data["sites"][0]["properties"]["land_cover"]["bldgs"]["sfr"] == 0.4
    assert "snowalb" not in patched.data["sites"][0]["initial_states"]
    stebbs = patched.data["sites"][0]["properties"]["stebbs"]
    assert stebbs["alpha"] == {"value": None, "ref": {"ID": "S1"}}
    assert stebbs["beta"] is None
    assert stebbs["ref_only"] == {"ref": {"desc": "keep me"}}
    assert stebbs["keep_key"] == 9.9

    # Standalone helpers round-trip cleanly too.
    assert write_path(patched.data, "sites[0].properties.lat", 51.5)
    assert read_path(patched.data, "sites[0].properties.lat") == 51.5
    assert delete_path(patched.data, "sites[0].properties.lat")
    assert read_path(patched.data, "sites[0].properties.lat") is None
    assert site_gridid_map(patched.data) == {0: 42}


def test_render_text_report_emits_all_sections_and_stopping_stage() -> None:
    """Pure TextReportRenderer emits canonical headers directly from ValidationReport."""
    report = ValidationReport(
        phases=[
            PhaseReport(
                phase="A",
                issues=[
                    Issue(
                        phase="A",
                        severity=SEVERITY_APPLIED_FIX,
                        code="A.RENAMED_PARAM",
                        message="Renamed soildepth -> soil_depth",
                        yaml_path="soil_depth",
                        applied_fix=True,
                    ),
                    Issue(
                        phase="A",
                        severity=SEVERITY_WARNING,
                        code="A.MISSING_PARAM",
                        message="Missing parameter: sites[0].properties.optional_field",
                        yaml_path="sites[0].properties.optional_field",
                    ),
                ],
            ),
            PhaseReport(
                phase="B",
                issues=[
                    Issue(
                        phase="B",
                        severity=SEVERITY_WARNING,
                        code="LAND_COVER.EVETR",
                        message="evetr fraction is 0",
                        yaml_path="land_cover.evetr",
                        site_gridid=1,
                    ),
                    Issue(
                        phase="B",
                        severity=SEVERITY_SUGGESTION,
                        code="SCIENCE.PAVED_SFR",
                        message="Auto-correct paved.sfr",
                        yaml_path="paved.sfr",
                        site_gridid=1,
                    ),
                ],
            ),
            PhaseReport(
                phase="C",
                issues=[
                    Issue(
                        phase="C",
                        severity=SEVERITY_ERROR,
                        code="C.PYDANTIC.EXTRA_FORBIDDEN",
                        message="Extra inputs are not permitted",
                        yaml_path="sites.1.properties.bogus",
                    ),
                    Issue(
                        phase="C",
                        severity=SEVERITY_INFO,
                        code="C.DEFAULT_APPLIED.SCHEMA_VERSION",
                        message="schema_version found missing in user YAML",
                        yaml_path="schema_version",
                    ),
                ],
            ),
        ]
    )

    text = render_text_report(report, mode="public", science_fixes="suggest")
    assert "# Mode: Public" in text
    assert "# Validation stopped at: Model Compatibility" in text
    assert "## ACTION NEEDED" in text
    assert "sites.1.properties.bogus" in text
    assert "## REVIEW ADVISED" in text
    assert "sites[0].properties.optional_field" in text
    assert "## SUGGESTED UPDATES" in text
    assert "## APPLIED UPDATES" in text
    assert "- Updated (1) renamed parameter(s) to current standards:" in text
    assert "-- soildepth changed to soil_depth" in text
    assert "## INFO" in text
    assert "- Updated (1) renamed parameter(s):" in text
    assert "schema_version found missing" in text
    assert text.isascii()


def test_phase_c_runs_single_pydantic_pass_and_emits_schema_version_default(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Single-pass Phase C validates once and detects defaults via model_fields_set."""
    config_path = _copy_sample_case(tmp_path)
    payload = yaml.safe_load(config_path.read_text(encoding="utf-8"))
    payload.pop("schema_version", None)
    config_path.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")

    calls = 0
    original = SUEWSConfig._validate_raw_mapping.__func__

    @classmethod
    def _counting_validate(cls, config_data):
        nonlocal calls
        calls += 1
        return original(cls, config_data)

    monkeypatch.setattr(SUEWSConfig, "_validate_raw_mapping", _counting_validate)

    out_yaml = tmp_path / "updatedC.yml"
    out_report = tmp_path / "reportC.txt"
    phase_report = orchestrator.run_phase_c(
        str(config_path), str(out_yaml), str(out_report), mode="public", silent=True
    )

    assert calls == 1
    assert not phase_report.has_errors
    assert any(
        issue.code == "C.DEFAULT_APPLIED.SCHEMA_VERSION"
        and issue.severity == SEVERITY_INFO
        for issue in phase_report.issues
    )
    report_text = out_report.read_text(encoding="utf-8")
    assert "## INFO" in report_text
    assert "schema_version" in report_text


def test_abc_pipeline_renders_report_without_txt_scraping(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """run_pipeline renders consolidated reports from ValidationReport without .txt scraping."""
    config_path = _copy_sample_case(tmp_path)

    def _forbid_scrape(*_args, **_kwargs):
        raise AssertionError("Live pipeline must not scrape intermediate .txt reports")

    monkeypatch.setattr(
        orchestrator, "extract_no_action_messages_from_report", _forbid_scrape
    )

    result = orchestrator.run_pipeline(
        str(config_path), pipeline="ABC", mode="public", forcing="off"
    )
    assert not result.has_errors
    assert result.report_path.is_file()
    sidecar = json.loads(
        result.report_path.with_suffix(".json").read_text(encoding="utf-8")
    )
    assert [p["phase"] for p in sidecar["phases"]] == ["A", "B", "C"]
    # Also verify validate_phase_c does not fail with C.PIPELINE.UNKNOWN on bad input.
    bad_path = tmp_path / "bad.yml"
    bad_path.write_text("model: null\nsites: []\n", encoding="utf-8")
    bad_res = validate_phase_c(str(bad_path))
    assert bad_res.has_errors
    assert all(i.code != "C.PIPELINE.UNKNOWN" for i in bad_res.issues)
