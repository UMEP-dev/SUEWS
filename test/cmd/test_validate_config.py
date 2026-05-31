"""Tests for ``suews validate`` — Envelope shape + invalid-input regressions.

Acceptance for gh#1360:
- ``--format json`` output validates against the canonical SUEWS Envelope.
- Validation errors surface a ``schema_path`` and a ``hint``.
- Per ``.claude/rules/tests/patterns.md`` (Validation Edge Cases): every
  tightened input contract carries explicit regressions for negative
  values, the ``-999`` sentinel, and ``NaN`` — and at least one
  regression per public path. Here that's the CLI (``suews validate``)
  and the library function ``validate_single_file()``.

The fixtures all start from the bundled sample config and inject one
breaking change per parametrised case so the regression target is
unambiguous.
"""

from __future__ import annotations

from contextlib import contextmanager
import copy
from importlib.resources import as_file, files
import json
import math
from pathlib import Path
from typing import Any, Callable, Dict, Iterator

from click.testing import CliRunner
import pytest
import yaml

pytestmark = pytest.mark.api


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@contextmanager
def _sample_yaml_path() -> Iterator[Path]:
    """Yield the bundled sample config as a filesystem path for Click."""
    resource = files("supy").joinpath("sample_data", "sample_config.yml")
    with as_file(resource) as path_sample:
        yield path_sample


def _load_sample_dict() -> Dict[str, Any]:
    resource = files("supy").joinpath("sample_data", "sample_config.yml")
    if not resource.is_file():
        pytest.skip(f"Sample config not available at {resource}")
    return yaml.safe_load(resource.read_text(encoding="utf-8"))


def _write_yaml(path: Path, payload: Dict[str, Any]) -> None:
    path.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")


def _set_first_paved_sfr(payload: Dict[str, Any], value: float) -> None:
    """Override the first site's ``land_cover.paved.sfr`` to ``value``.

    The sample config nests this as ``{"value": 0.43}``; we keep the
    RefValue wrapper so the change is structurally valid (only the
    numeric content is invalid).
    """
    sites = payload.get("sites") or []
    if not sites:
        pytest.skip("Sample config has no sites — fixture cannot be built")
    paved = sites[0]["properties"]["land_cover"]["paved"]
    if isinstance(paved.get("sfr"), dict) and "value" in paved["sfr"]:
        paved["sfr"]["value"] = value
    else:
        paved["sfr"] = {"value": value}


# ---------------------------------------------------------------------------
# Envelope shape — happy path
# ---------------------------------------------------------------------------


def test_validate_sample_config_emits_envelope() -> None:
    """``suews validate <sample> --format json`` emits the canonical envelope."""
    from supy.cmd.validate_config import cli as validate_cli

    with _sample_yaml_path() as sample:
        if not sample.exists():
            pytest.skip(f"Sample config not available at {sample}")

        runner = CliRunner()
        result = runner.invoke(
            validate_cli,
            [
                "--pipeline",
                "ABC",
                "--dry-run",
                "--format",
                "json",
                str(sample),
            ],
        )
    assert result.exit_code in {0, 1}, result.output
    envelope = json.loads(result.stdout)
    assert set(envelope.keys()) == {"status", "data", "errors", "warnings", "meta"}
    assert envelope["status"] in {"success", "warning", "error"}
    meta = envelope["meta"]
    for key in (
        "schema_version",
        "suews_version",
        "supy_version",
        "git_commit",
        "command",
        "started_at",
        "ended_at",
    ):
        assert key in meta, f"meta key {key!r} missing from envelope"


def test_validate_pipeline_c_json_accepts_multiple_files() -> None:
    """Pipeline C dry-run JSON mode validates more than one file."""
    from supy.cmd.validate_config import cli as validate_cli

    with _sample_yaml_path() as sample:
        if not sample.exists():
            pytest.skip(f"Sample config not available at {sample}")

        runner = CliRunner()
        result = runner.invoke(
            validate_cli,
            [
                "--pipeline",
                "C",
                "--dry-run",
                "--format",
                "json",
                str(sample),
                str(sample),
            ],
        )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "success"
    assert len(envelope["data"]["files"]) == 2


# ---------------------------------------------------------------------------
# Report consolidation regressions — gh#1466 / gh#1458
# ---------------------------------------------------------------------------


def _copy_sample_data(dst: Path) -> Path:
    """Copy the bundled sample config and its forcing file into ``dst``.

    The forcing file must sit beside the config so Phase B forcing
    validation resolves it and the science checks run end-to-end. Returns
    the path to the copied ``sample_config.yml``.
    """
    sample_dir = files("supy").joinpath("sample_data")
    if not sample_dir.joinpath("sample_config.yml").is_file():
        pytest.skip("Sample data not available")
    for resource in sample_dir.iterdir():
        if resource.is_file():
            (dst / resource.name).write_bytes(resource.read_bytes())
    return dst / "sample_config.yml"


def test_missing_schema_version_keeps_review_and_suggestion_sections(
    tmp_path: Path,
) -> None:
    """A YAML missing ``schema_version`` must still surface the Phase B
    ``REVIEW ADVISED`` / ``SUGGESTED UPDATES`` sections — gh#1466 / gh#1458.

    Root cause: Phase C's INFO consolidation short-circuited to an
    INFO-only report whenever it detected *any* default value. A missing
    ``schema_version`` is the common trigger, so the upstream phase
    content carried in ``no_action_messages`` (Phase A renames, Phase B
    review/suggestion sections) was silently dropped — the report
    collapsed to a single ``## INFO`` line about the missing field.
    """
    from supy.cmd.validate_config import cli as validate_cli

    config_path = _copy_sample_data(tmp_path)

    # Strip the ``schema_version`` line textually so the rest of the
    # bundled config is byte-for-byte unchanged (no YAML round-trip).
    original = config_path.read_text(encoding="utf-8")
    stripped = "".join(
        line
        for line in original.splitlines(keepends=True)
        if not line.startswith("schema_version")
    )
    assert stripped != original, "fixture must contain a schema_version line"
    config_path.write_text(stripped, encoding="utf-8")

    runner = CliRunner()
    result = runner.invoke(validate_cli, ["--mode", "dev", str(config_path)])
    assert result.exit_code in {0, 1}, result.output

    report_path = tmp_path / "report_sample_config.txt"
    assert report_path.exists(), result.output
    report = report_path.read_text(encoding="utf-8")

    # The missing-schema_version INFO is expected to be reported ...
    assert "schema_version" in report, report
    # ... but it must NOT have replaced the upstream phase sections.
    assert "## REVIEW ADVISED" in report, report
    assert "## SUGGESTED UPDATES" in report, report


# ---------------------------------------------------------------------------
# Validation edge cases — CLI path
# ---------------------------------------------------------------------------


def _mutate_negative(payload: Dict[str, Any]) -> None:
    _set_first_paved_sfr(payload, -0.1)


def _mutate_sentinel(payload: Dict[str, Any]) -> None:
    _set_first_paved_sfr(payload, -999.0)


def _mutate_nan(payload: Dict[str, Any]) -> None:
    _set_first_paved_sfr(payload, float("nan"))


@pytest.mark.parametrize(
    "case_name, mutator",
    [
        ("negative", _mutate_negative),
        ("sentinel_minus_999", _mutate_sentinel),
        ("nan", _mutate_nan),
    ],
)
def test_validate_invalid_sfr_returns_error_envelope(
    tmp_path: Path,
    case_name: str,
    mutator: Callable[[Dict[str, Any]], None],
) -> None:
    """Each invalid-input pattern must produce a structured error envelope.

    Per ``patterns.md`` (Validation Edge Cases): cover negative, ``-999``
    sentinel, and ``NaN`` — separately, with one regression per pattern.
    """
    from supy.cmd.validate_config import cli as validate_cli

    payload = _load_sample_dict()
    mutator(payload)
    config_path = tmp_path / f"sample_{case_name}.yml"
    _write_yaml(config_path, payload)

    runner = CliRunner()
    result = runner.invoke(
        validate_cli,
        [
            "--pipeline",
            "ABC",
            "--dry-run",
            "--format",
            "json",
            str(config_path),
        ],
    )
    # Exit non-zero on invalid; envelope still parsable on stdout.
    assert result.exit_code != 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert envelope["errors"], "expected non-empty errors array"
    # At least one error must surface a schema path and a hint.
    assert any(
        err.get("schema_path") and err.get("hint") for err in envelope["errors"]
    ), envelope["errors"]


# ---------------------------------------------------------------------------
# Validation edge cases — library function path
# ---------------------------------------------------------------------------


def test_validate_single_file_rejects_negative_sfr(tmp_path: Path) -> None:
    """``validate_single_file`` (the library API) must reject negative ``sfr``.

    This is the second public path the patterns rule asks us to cover —
    if the modern CLI ever stops emitting envelopes, the underlying
    validator still has its own regression here.
    """
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.schema.publisher import generate_json_schema

    payload = _load_sample_dict()
    _mutate_negative(payload)
    config_path = tmp_path / "sample_negative.yml"
    _write_yaml(config_path, payload)

    schema = generate_json_schema()
    is_valid, errors = validate_single_file(config_path, schema, show_details=True)

    assert not is_valid
    assert errors, "expected at least one error for a negative sfr"


# ---------------------------------------------------------------------------
# Critical-physics structural-presence — gh#1409
# ---------------------------------------------------------------------------


def _minimal_paved_only_config(schema_version: str) -> Dict[str, Any]:
    """Build a minimal YAML that passes schema and Pydantic but lacks the
    required ``model.physics`` keys.

    Uses ``land_cover.paved.sfr=1.0`` so the Pydantic ``model_validator``
    does not reject the payload via site-level critical-null checks
    (which fire only when vegetated surfaces are active). This isolates
    the structural-presence gap closed in gh#1409.
    """
    return {
        "schema_version": schema_version,
        "name": "gh1409-regression",
        "description": "missing model.physics keys",
        "model": {
            "control": {
                "tstep": 300,
                "forcing_file": "forcing.txt",
                "output_file": "out.txt",
                "start_time": "2020-01-01",
                "end_time": "2020-01-31",
            },
            "physics": {},
        },
        "sites": [
            {
                "name": "regression",
                "gridiv": 1,
                "properties": {
                    "lat": {"value": 33.45},
                    "lng": {"value": -112.07},
                    "alt": {"value": 331},
                    "timezone": {"value": -7},
                    "surfacearea": {"value": 1.0},
                    "land_cover": {
                        "paved": {"sfr": {"value": 1.0}},
                        "bldgs": {"sfr": {"value": 0.0}},
                        "evetr": {"sfr": {"value": 0.0}},
                        "dectr": {"sfr": {"value": 0.0}},
                        "grass": {"sfr": {"value": 0.0}},
                        "bsoil": {"sfr": {"value": 0.0}},
                        "water": {"sfr": {"value": 0.0}},
                    },
                },
            }
        ],
    }


def test_validate_flags_missing_critical_physics_params(tmp_path: Path) -> None:
    """A YAML with ``model.physics: {}`` must fail with one finding per
    missing critical physics field.

    Regression for gh#1409: Pydantic auto-fills ``ModelPhysics`` enum
    defaults so ``model.physics: {}`` previously passed the dry-run
    JSON path with ``status="success"``. The MCP wrapper consumed the
    false-success envelope and stopped iterating. The structural-presence
    check in ``validate_single_file`` now flags every missing
    ``CRITICAL_PHYSICS_PARAMS`` entry as ``MISSING_REQUIRED_FIELD`` so
    the dry-run path agrees with the full pipeline's Phase A on
    user-facing semantics.
    """
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.schema.publisher import generate_json_schema
    from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION
    from supy.data_model.validation.pipeline.orchestrator import (
        CRITICAL_PHYSICS_PARAMS,
    )
    from supy.data_model.core.field_renames import STEBBS_PHYSICS_LEAF_RENAMES

    payload = _minimal_paved_only_config(CURRENT_SCHEMA_VERSION)
    config_path = tmp_path / "missing_physics.yml"
    _write_yaml(config_path, payload)

    schema = generate_json_schema()
    is_valid, errors = validate_single_file(config_path, schema, show_details=True)

    assert not is_valid

    flagged = {
        getattr(err, "field", None)
        for err in errors
        if getattr(err, "field", "").startswith("model.physics.")
    }
    # gh#1456: STEBBS critical leaves are reported under the nested
    # model.physics.stebbs.* path; the remaining physics params stay flat.
    stebbs_leaf_names = set(STEBBS_PHYSICS_LEAF_RENAMES.values())
    expected = {
        (
            f"model.physics.stebbs.{name}"
            if name in stebbs_leaf_names
            else f"model.physics.{name}"
        )
        for name in CRITICAL_PHYSICS_PARAMS
    }
    assert flagged == expected, (
        f"missing fields not flagged: {expected - flagged}; "
        f"unexpected fields flagged: {flagged - expected}"
    )


def test_validate_flat_stebbs_form_not_flagged_missing(tmp_path: Path) -> None:
    """gh#1456 regression: a current-target YAML in the legacy FLAT form.

    The relocated STEBBS leaves (``capacitance``/``setpoint``/``same_*``)
    may still be written flat under ``model.physics`` alongside a flat
    ``stebbs: {value: 1}`` master toggle. ``SUEWSConfig.from_yaml`` folds
    those flat keys to the nested ``stebbs`` block and accepts them, so
    the dry-run critical-physics check must too. Before the fix it looked
    for the leaves only inside ``physics["stebbs"]`` and false-reported
    them missing.
    """
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.schema.publisher import generate_json_schema
    from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION

    payload = _minimal_paved_only_config(CURRENT_SCHEMA_VERSION)
    # Populate every required family switch plus the relocated STEBBS leaves
    # in the FLAT form (directly under model.physics, with a flat master
    # toggle). This is a still-accepted shape that the loader folds.
    payload["model"]["physics"] = {
        "net_radiation": {"value": 3},
        "emissions": {"value": 2},
        "storage_heat": {"value": 1},
        "ohm_inc_qf": {"value": 0},
        "roughness_length_momentum": {"value": 2},
        "roughness_length_heat": {"value": 2},
        "stability": {"value": 3},
        "soil_moisture_deficit": {"value": 0},
        "water_use": {"value": 0},
        "roughness_sublayer": {"value": 1},
        "frontal_area_index": {"value": 0},
        "roughness_sublayer_level": {"value": 0},
        "surface_conductance": {"value": 1},
        "snow_use": {"value": 0},
        # Flat STEBBS master toggle + relocated leaves (legacy flat form).
        "stebbs": {"value": 1},
        "capacitance": {"value": 1},
        "setpoint": {"value": 0},
        "same_albedo_wall": {"value": 1},
        "same_albedo_roof": {"value": 1},
        "same_emissivity_wall": {"value": 1},
        "same_emissivity_roof": {"value": 1},
    }
    config_path = tmp_path / "flat_stebbs.yml"
    _write_yaml(config_path, payload)

    schema = generate_json_schema()
    _is_valid, errors = validate_single_file(config_path, schema, show_details=True)

    # The relocated leaves must NOT appear as missing critical physics
    # parameters now that the dry-run path folds the flat form first.
    missing_stebbs = [
        field
        for err in errors
        for field in [getattr(err, "field", "") or ""]
        if field.startswith("model.physics.stebbs.")
        and "missing" in (getattr(err, "message", "") or "").lower()
    ]
    assert not missing_stebbs, (
        f"flat-form STEBBS leaves false-flagged missing: {missing_stebbs}"
    )
    # And none of the required family switches should be missing either.
    missing_flat = [
        field
        for err in errors
        for field in [getattr(err, "field", "") or ""]
        if field.startswith("model.physics.")
        and not field.startswith("model.physics.stebbs.")
        and "required physics parameter is missing"
        in (getattr(err, "message", "") or "")
    ]
    assert not missing_flat, (
        f"flat-form physics switches false-flagged missing: {missing_flat}"
    )


def test_validate_full_pipeline_emits_json_envelope(tmp_path: Path) -> None:
    """The non-dry-run pipeline path must honour ``--format json`` and
    emit a canonical envelope on stdout (gh#1409 follow-up).

    Previously only ``--dry-run --format json`` produced JSON; the
    full pipeline branch wrote a report file and printed a status
    banner to console regardless of the format flag, so any consumer
    that expected JSON had to parse the report file or fall back to
    the dry-run path.
    """
    from supy.cmd.validate_config import cli as validate_cli

    # Reuse the paved-only YAML from the gh#1409 reproducer — bad
    # enough to fail Phase A so we exercise both the failure path and
    # the JSON emit.
    payload = _minimal_paved_only_config("2026.5.dev9")
    config_path = tmp_path / "full_pipeline.yml"
    _write_yaml(config_path, payload)

    runner = CliRunner()
    result = runner.invoke(
        validate_cli,
        [
            "--pipeline",
            "ABC",
            "--format",
            "json",
            str(config_path),
        ],
    )
    # Bad config → non-zero exit, but stdout must still parse as the
    # canonical envelope.
    assert result.exit_code != 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert set(envelope.keys()) == {"status", "data", "errors", "warnings", "meta"}

    data = envelope["data"]
    assert "validation_report" in data, (
        "full-pipeline JSON envelope must carry the structured ValidationReport"
    )
    assert data["validation_report"]["overall_status"] == "FAILED"
    assert "report_file" in data and "updated_yaml" in data
    # phases_run records which phases actually executed; for a Phase-A
    # failure the list contains just "A" because the pipeline bails
    # before B/C.
    assert isinstance(data["phases_run"], list)
    assert "A" in data["phases_run"]


def test_validate_full_pipeline_experimental_restriction_emits_json_envelope(
    tmp_path: Path,
) -> None:
    """Early public-mode restrictions must still honour ``--format json``.

    The restriction check runs before PhaseReport objects exist, so it
    needs its own envelope path rather than printing Rich text.
    """
    from supy.cmd.validate_config import cli as validate_cli
    from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION

    payload = _minimal_paved_only_config(CURRENT_SCHEMA_VERSION)
    payload["model"]["physics"] = {"stebbs": {"value": 1}}
    config_path = tmp_path / "experimental.yml"
    _write_yaml(config_path, payload)

    runner = CliRunner()
    result = runner.invoke(
        validate_cli,
        [
            "--pipeline",
            "ABC",
            "--format",
            "json",
            str(config_path),
        ],
    )

    assert result.exit_code == 1, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert envelope["errors"][0]["code"] == "experimental_features_restricted"
    # gh#1456: master toggle is now model.physics.stebbs.enabled; the
    # restriction message reflects the new nested surface.
    assert "STEBBS is enabled" in envelope["data"]["restrictions"][0]


def test_public_mode_restriction_honours_nested_stebbs_bool_strings(
    tmp_path: Path,
) -> None:
    from supy.cmd.validate_config import _experimental_features_restriction

    payload = {"model": {"physics": {"stebbs": {"enabled": {"value": "off"}}}}}
    config_path = tmp_path / "public-ok.yml"
    _write_yaml(config_path, payload)

    ok, restrictions, read_error = _experimental_features_restriction(
        str(config_path), "public"
    )

    assert ok is True
    assert restrictions == []
    assert read_error is None

    payload["model"]["physics"]["stebbs"]["enabled"]["value"] = "yes"
    _write_yaml(config_path, payload)
    ok, restrictions, read_error = _experimental_features_restriction(
        str(config_path), "public"
    )

    assert ok is False
    assert "STEBBS is enabled" in restrictions[0]
    assert read_error is None


def test_emit_pipeline_result_warning_status_matches_inner_report(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """Warning-only phase reports should surface as a warning envelope."""
    from supy.cmd.validate_config import _emit_pipeline_result
    from supy.data_model.validation.pipeline.report_schema import (
        Issue,
        PhaseReport,
        SEVERITY_WARNING,
    )

    phase = PhaseReport(
        phase="A",
        issues=[
            Issue(
                phase="A",
                severity=SEVERITY_WARNING,
                code="A.TEST.WARNING",
                message="warning message",
                yaml_path="model.physics",
            )
        ],
    )

    exit_code = _emit_pipeline_result(
        phases=[phase],
        report_path=tmp_path / "report.txt",
        yaml_path=tmp_path / "updated.yml",
        out_format="json",
        command="suews validate --format json",
        started_at="2026-05-15T00:00:00Z",
    )

    assert exit_code == 0
    envelope = json.loads(capsys.readouterr().out)
    assert envelope["status"] == "warning"
    assert envelope["errors"] == []
    assert envelope["warnings"][0]["code"] == "A.TEST.WARNING"
    assert envelope["data"]["validation_report"]["overall_status"] == "WARNING"


def test_validate_passes_when_critical_physics_present(tmp_path: Path) -> None:
    """The bundled sample config declares every critical physics key, so
    the structural-presence check must not fire.

    Guards against over-eager flagging that would break the happy path.
    """
    from supy.cmd.validate_config import validate_single_file
    from supy.data_model.schema.publisher import generate_json_schema

    schema = generate_json_schema()
    with _sample_yaml_path() as sample:
        if not sample.exists():
            pytest.skip(f"Sample config not available at {sample}")
        is_valid, errors = validate_single_file(sample, schema, show_details=True)

    physics_findings = [
        err
        for err in errors
        if getattr(err, "field", "").startswith("model.physics.")
    ]
    assert is_valid, f"sample config should be valid; got errors: {errors}"
    assert physics_findings == [], (
        "sample config should not trigger physics structural-presence findings"
    )


def test_validate_sidecar_includes_non_error_info_messages(tmp_path: Path) -> None:
    """gh#1467: the consolidated CLI JSON sidecar carries non-error issues.

    Dropping the optional ``sites[0].properties.h_std`` makes the run
    succeed (exit 0) while Phase A records a non-error ``A.MISSING_PARAM``
    warning. That informational message must appear in the JSON sidecar,
    not only in the text report. Anchored on the deterministic ``h_std``
    signal (the sample config's Phase B issues are environment-dependent).
    """
    from supy.cmd.validate_config import cli as validate_cli

    with _sample_yaml_path() as sample:
        data = yaml.safe_load(sample.read_text(encoding="utf-8"))
    data["sites"][0]["properties"].pop("h_std")

    config_path = tmp_path / "myconfig.yml"
    _write_yaml(config_path, data)

    runner = CliRunner()
    result = runner.invoke(validate_cli, ["--forcing", "off", str(config_path)])
    assert result.exit_code == 0, result.output

    sidecar = tmp_path / "report_myconfig.json"
    assert sidecar.exists(), "sidecar JSON not written"
    payload = json.loads(sidecar.read_text(encoding="utf-8"))

    # Consolidated sidecar is a multi-phase ValidationReport (gh#1467),
    # not a single Phase-C PhaseReport.
    assert "overall_status" in payload, payload.keys()
    assert isinstance(payload.get("phases"), list)
    assert {p["phase"] for p in payload["phases"]} >= {"A", "B", "C"}

    all_issues = [i for p in payload["phases"] for i in p["issues"]]
    assert any(
        i["code"] == "A.MISSING_PARAM"
        and "h_std" in (i.get("yaml_path") or "")
        and i["severity"] != "ERROR"
        for i in all_issues
    ), f"h_std informational issue missing from sidecar: {all_issues}"

    # gh#1467: the consolidated sidecar must not publish paths to files that
    # no longer exist. In a multi-phase run the intermediate temp reports and
    # updated*_ YAMLs are merged/deleted, so every path the sidecar advertises
    # must resolve, and report paths point at the surviving consolidated pair.
    assert "temp_report" not in sidecar.read_text(encoding="utf-8")
    for p in payload["phases"]:
        assert Path(p["text_report_path"]).name == "report_myconfig.txt"
        assert Path(p["json_report_path"]).name == "report_myconfig.json"
        for key in ("text_report_path", "json_report_path", "yaml_in", "yaml_out"):
            val = p.get(key)
            assert val is None or Path(val).exists(), (
                f"sidecar phase {p['phase']} {key}={val} does not exist"
            )


def test_validate_sidecar_is_validation_report_for_clean_config(tmp_path: Path) -> None:
    """The consolidated CLI sidecar is a multi-phase ValidationReport even
    for the bundled sample config (which already carries Phase B notes)."""
    from supy.cmd.validate_config import cli as validate_cli

    with _sample_yaml_path() as sample:
        config_path = tmp_path / "clean.yml"
        config_path.write_text(sample.read_text(encoding="utf-8"), encoding="utf-8")

    runner = CliRunner()
    result = runner.invoke(validate_cli, ["--forcing", "off", str(config_path)])
    assert result.exit_code == 0, result.output

    payload = json.loads((tmp_path / "report_clean.json").read_text(encoding="utf-8"))
    assert payload["overall_status"] in {"PASSED", "WARNING"}
    assert {p["phase"] for p in payload["phases"]} >= {"A", "B", "C"}
    # Each phase entry preserves the PhaseReport shape.
    for phase_entry in payload["phases"]:
        assert {"phase", "status", "issues"} <= set(phase_entry)


def test_validate_sidecar_keeps_info_alongside_errors(tmp_path: Path) -> None:
    """A failing config keeps Phase A informational issues beside Phase C
    errors in the same consolidated sidecar."""
    from supy.cmd.validate_config import cli as validate_cli

    with _sample_yaml_path() as sample:
        data = yaml.safe_load(sample.read_text(encoding="utf-8"))
    data["sites"][0]["properties"].pop("h_std")  # -> A.MISSING_PARAM (info)
    data["sites"][0]["properties"]["bogus_extra_param"] = 42  # -> C.PYDANTIC error

    config_path = tmp_path / "bad.yml"
    _write_yaml(config_path, data)

    runner = CliRunner()
    result = runner.invoke(validate_cli, ["--forcing", "off", str(config_path)])
    assert result.exit_code != 0, result.output

    payload = json.loads((tmp_path / "report_bad.json").read_text(encoding="utf-8"))
    assert payload["overall_status"] == "FAILED"
    all_issues = [i for p in payload["phases"] for i in p["issues"]]
    assert any(i["code"].startswith("C.PYDANTIC") for i in all_issues), all_issues
    assert any(i["code"] == "A.MISSING_PARAM" for i in all_issues), all_issues


def test_emit_pipeline_result_writes_consolidated_sidecar(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """gh#1467: `_emit_pipeline_result` writes the consolidated
    ValidationReport sidecar beside the report, in both output modes."""
    from supy.cmd.validate_config import _emit_pipeline_result
    from supy.data_model.validation.pipeline.report_schema import (
        Issue,
        PhaseReport,
        SEVERITY_INFO,
        SEVERITY_ERROR,
    )

    phases = [
        PhaseReport(
            phase="A",
            issues=[Issue(phase="A", severity=SEVERITY_INFO, code="A.INFO.NOTE",
                          message="informational note", yaml_path="model.physics")],
        ),
        PhaseReport(
            phase="C",
            issues=[Issue(phase="C", severity=SEVERITY_ERROR, code="C.PYDANTIC.X",
                          message="bad value", yaml_path="sites.1.properties.lat")],
        ),
    ]
    report_path = tmp_path / "report_x.txt"

    exit_code = _emit_pipeline_result(
        phases=phases,
        report_path=report_path,
        yaml_path=tmp_path / "updated_x.yml",
        out_format="table",
        command="suews validate",
        started_at="2026-05-30T00:00:00Z",
    )
    assert exit_code == 1  # has an ERROR

    sidecar = tmp_path / "report_x.json"
    assert sidecar.exists()
    payload = json.loads(sidecar.read_text(encoding="utf-8"))
    assert payload["overall_status"] == "FAILED"
    assert {p["phase"] for p in payload["phases"]} == {"A", "C"}
    info_codes = [i["code"] for p in payload["phases"] for i in p["issues"]]
    assert "A.INFO.NOTE" in info_codes  # non-error info survived
    assert "C.PYDANTIC.X" in info_codes
    capsys.readouterr()  # drain captured table output


def test_emit_pipeline_result_clears_missing_phase_yaml_paths(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    """gh#1467: a failed phase's missing yaml_out is cleared, not repointed to
    another phase's surviving file (and intermediate paths never dangle)."""
    from supy.cmd.validate_config import _emit_pipeline_result
    from supy.data_model.validation.pipeline.report_schema import (
        Issue,
        PhaseReport,
        SEVERITY_ERROR,
    )

    existing_yaml = tmp_path / "updated_x.yml"
    existing_yaml.write_text("name: x\n", encoding="utf-8")
    report_path = tmp_path / "report_x.txt"

    phases = [
        # Successful Phase A whose intermediate output was deleted on consolidation.
        PhaseReport(
            phase="A",
            issues=[],
            yaml_in=str(tmp_path / "missing_in.yml"),
            yaml_out=str(tmp_path / "updatedA_x.yml"),
        ),
        # Failed Phase B that never produced an output YAML.
        PhaseReport(
            phase="B",
            issues=[Issue(phase="B", severity=SEVERITY_ERROR, code="B.X", message="boom")],
            yaml_out=str(tmp_path / "updatedB_x.yml"),
        ),
    ]

    _emit_pipeline_result(
        phases=phases,
        report_path=report_path,
        yaml_path=existing_yaml,
        out_format="table",
        command="suews validate",
        started_at="2026-05-31T00:00:00Z",
    )
    capsys.readouterr()

    payload = json.loads((tmp_path / "report_x.json").read_text(encoding="utf-8"))
    by_phase = {p["phase"]: p for p in payload["phases"]}
    # Vanished intermediate/failed YAML paths are cleared, not repointed.
    assert by_phase["A"]["yaml_in"] is None
    assert by_phase["A"]["yaml_out"] is None
    assert by_phase["B"]["yaml_out"] is None
    # No phase falsely claims the surviving final YAML as its own output.
    assert all(p["yaml_out"] != str(existing_yaml) for p in payload["phases"])
