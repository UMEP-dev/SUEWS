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
    expected = {f"model.physics.{name}" for name in CRITICAL_PHYSICS_PARAMS}
    assert flagged == expected, (
        f"missing fields not flagged: {expected - flagged}; "
        f"unexpected fields flagged: {flagged - expected}"
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
    assert "STEBBS method" in envelope["data"]["restrictions"][0]


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
