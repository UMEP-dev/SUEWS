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

import copy
import json
import math
from pathlib import Path
from typing import Any, Callable, Dict

import pytest
import yaml
from click.testing import CliRunner

pytestmark = pytest.mark.api


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


def _sample_yaml_path() -> Path:
    import supy

    return Path(supy.__file__).parent / "sample_data" / "sample_config.yml"


def _load_sample_dict() -> Dict[str, Any]:
    sample = _sample_yaml_path()
    if not sample.exists():
        pytest.skip(f"Sample config not available at {sample}")
    with sample.open("r", encoding="utf-8") as handle:
        return yaml.safe_load(handle)


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

    sample = _sample_yaml_path()
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
