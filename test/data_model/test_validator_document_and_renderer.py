"""Unit tests for ConfigDocument, pure TextReportRenderer, and single-pass Phase C."""

from __future__ import annotations

from importlib.resources import files
import json
from pathlib import Path
import subprocess
from types import MappingProxyType

from conftest import resolve_suews_cli_command
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
    nullify_tree_in_place,
    parse_path_tokens,
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
    pytest.mark.unit,
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


def test_document_hardening_ref_metadata_immutability_and_path_validation() -> None:
    """Indexed ref-only writes, wrapped-list/section refs, caller values, and frozen dicts."""
    # Indexed write preserves ref-only metadata.
    indexed = {"items": [{"ref": {"source": "measurement"}}]}
    assert write_path(indexed, "items[0]", 2)
    assert indexed == {"items": [{"ref": {"source": "measurement"}, "value": 2}]}

    # Caller-owned mutable patch values are never mutated by subsequent patches.
    payload = [1, 2]
    doc = ConfigDocument.from_dict({}, normalise=False)
    patched = doc.apply_patches(
        [
            ConfigPatch("x", "set", payload, default_wrapped=False),
            ConfigPatch("x", "nullify_tree"),
        ]
    )
    assert payload == [1, 2]
    assert patched.data["x"] == [None, None]

    # Nullifying wrapped lists and section-level ref blocks preserves all ref metadata.
    tree = {
        "ref": {"desc": "section provenance"},
        "wrapped_list": {
            "value": [
                {"value": 1, "ref": {"source": "obs"}},
                {"ref": {"source": "placeholder"}},
            ],
            "ref": {"source": "array"},
        },
    }
    nullify_tree_in_place(tree)
    assert tree["ref"] == {"desc": "section provenance"}
    assert tree["wrapped_list"]["ref"] == {"source": "array"}
    assert tree["wrapped_list"]["value"] == [
        {"value": None, "ref": {"source": "obs"}},
        {"ref": {"source": "placeholder"}},
    ]

    # ConfigDocument.from_dict thaws nested MappingProxyType and read() returns subtrees.
    frozen_doc = ConfigDocument.from_dict(
        {"model": MappingProxyType({"physics": MappingProxyType({"snow_use": 0})})},
        normalise=False,
    )
    assert frozen_doc.read("model.physics") == {"snow_use": 0}

    # Malformed paths raise ValueError; failed write_path creates no partial parents.
    with pytest.raises(ValueError, match="Malformed YAML path"):
        parse_path_tokens("a..b")
    with pytest.raises(ValueError, match="Malformed YAML path"):
        parse_path_tokens("a[0")
    partial: dict = {}
    assert write_path(partial, "a.b[0]", 1) is False
    assert partial == {}

    # Failed patch targets and unsupported actions raise ValueError.
    with pytest.raises(ValueError, match="Failed to apply 'set'"):
        frozen_doc.apply_patches([ConfigPatch("sites[99].x", "set", 1)])
    with pytest.raises(ValueError, match="Unsupported patch action"):
        frozen_doc.apply_patches([ConfigPatch("model.physics", "bogus")])  # type: ignore[arg-type]

    # Non-RefValue mappings carrying a 'value' key alongside other keys stay intact.
    non_ref = {"value": 1, "units": "K"}
    assert not is_ref_wrapper(non_ref)
    assert unwrap_entry(non_ref) == non_ref


def _installed_suews_validate_cmd() -> list[str]:
    """Return the ``suews validate`` CLI command for wheel or source-lane runs."""
    return [*resolve_suews_cli_command("suews"), "validate"]


def test_phase_c_extra_forbidden_emits_drift_hint_in_single_pass(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """extra_forbidden emits C.SCHEMA_DRIFT_HINT without running Pydantic twice."""
    config_path = _copy_sample_case(tmp_path)
    payload = yaml.safe_load(config_path.read_text(encoding="utf-8"))
    payload["schema_version"] = "2026.1"
    payload["sites"][0]["properties"]["unexpected_drifted_key"] = 1
    config_path.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")

    calls = 0
    original = SUEWSConfig._validate_raw_mapping.__func__

    @classmethod
    def _counting_validate(cls, config_data):
        nonlocal calls
        calls += 1
        return original(cls, config_data)

    monkeypatch.setattr(SUEWSConfig, "_validate_raw_mapping", _counting_validate)

    result = validate_phase_c(str(config_path))
    assert calls == 1
    assert result.has_errors
    hint_issues = [i for i in result.issues if i.code == "C.SCHEMA_DRIFT_HINT"]
    assert len(hint_issues) == 1
    assert hint_issues[0].severity == SEVERITY_INFO
    assert "Detected schema version: 2026.1" in hint_issues[0].message
    assert "suews-convert -i <old.yml> -o <new.yml>" in hint_issues[0].message


@pytest.mark.e2e
@pytest.mark.parametrize(
    ("schema_mode", "pipeline", "expected_snippet", "forbidden_snippet"),
    [
        (
            "current",
            "C",
            "already declares the current schema version",
            "suews-convert",
        ),
        (
            "older",
            "C",
            "Detected schema version: 2026.1",
            "-f <release-tag>",
        ),
        (
            "unsigned",
            "C",
            "No schema_version field in YAML",
            None,
        ),
        (
            "older",
            "BC",
            "suews-convert -i <old.yml> -o <new.yml>",
            "-f <release-tag>",
        ),
        (
            "current",
            "ABC",
            "already declares the current schema version",
            "suews-convert",
        ),
    ],
)
def test_installed_cli_phase_c_drift_hint_outer_boundary(
    tmp_path: Path,
    schema_mode: str,
    pipeline: str,
    expected_snippet: str,
    forbidden_snippet: str | None,
) -> None:
    """Installed CLI preserves _drift_hint in text/JSON reports and exits 1 on Phase C failure."""
    config_path = _copy_sample_case(tmp_path)
    payload = yaml.safe_load(config_path.read_text(encoding="utf-8"))
    if schema_mode == "older":
        payload["schema_version"] = "2026.1"
    elif schema_mode == "unsigned":
        payload.pop("schema_version", None)
    payload["sites"][0]["properties"]["unexpected_drifted_key"] = 123
    config_path.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")

    proc = subprocess.run(
        [
            *_installed_suews_validate_cmd(),
            "-p",
            pipeline,
            "--forcing",
            "off",
            "--format",
            "json",
            str(config_path),
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=180,
    )

    assert proc.returncode == 1, proc.stdout + proc.stderr
    envelope = json.loads(proc.stdout)
    assert envelope["status"] == "error"
    evidence_dir = Path(envelope["data"]["evidence_dir"])
    assert evidence_dir.is_dir()
    assert list(tmp_path.glob(".suews-validate-*")) == [evidence_dir]

    report_txt = (tmp_path / "report_sample_config.txt").read_text(encoding="utf-8")
    report_json = json.loads(
        (tmp_path / "report_sample_config.json").read_text(encoding="utf-8")
    )
    assert "Validation stopped at: Model Compatibility" in report_txt
    assert "## ACTION NEEDED" in report_txt
    assert "## INFO" in report_txt
    assert expected_snippet in report_txt
    if schema_mode == "unsigned":
        assert "suews-convert -i <old.yml> -o <new.yml> -f <release-tag>" in report_txt
    if forbidden_snippet is not None:
        assert forbidden_snippet not in report_txt

    c_phase = report_json["phases"][-1]
    assert c_phase["phase"] == "C"
    assert c_phase["status"] == "FAILED"
    drift_issues = [i for i in c_phase["issues"] if i["code"] == "C.SCHEMA_DRIFT_HINT"]
    assert len(drift_issues) == 1
    assert drift_issues[0]["severity"] == SEVERITY_INFO
    assert expected_snippet in drift_issues[0]["message"]
    if schema_mode == "unsigned":
        assert (
            "suews-convert -i <old.yml> -o <new.yml> -f <release-tag>"
            in drift_issues[0]["message"]
        )
    if forbidden_snippet is not None:
        assert forbidden_snippet not in drift_issues[0]["message"]


def test_applied_updates_renderer_preserves_old_and_new_values() -> None:
    """Consolidated APPLIED UPDATES text preserves both old/new values and the reason."""
    from supy.data_model.validation.pipeline.phase_b import (
        ScientificAdjustment,
        _adjustment_to_issue,
    )

    adj = ScientificAdjustment(
        parameter="initial_states.paved.temperature",
        old_value=10.0,
        new_value=14.5,
        reason="Initialised from CRU TS4.06 mean air temperature",
        site_index=0,
        site_gridid=1,
    )
    issue = _adjustment_to_issue(adj)
    report = ValidationReport(
        phases=[PhaseReport(phase="B", issues=[issue])]
    )
    rendered = render_text_report(report, mode="public", science_fixes="apply")

    assert "## APPLIED UPDATES" in rendered
    assert "10.0 -> 14.5" in rendered
    assert "Initialised from CRU TS4.06 mean air temperature" in rendered
