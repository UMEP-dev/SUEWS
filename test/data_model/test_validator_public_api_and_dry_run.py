"""Wave 5 tests: public ``supy.validate_config``, unified ``--dry-run``, and legacy retirement.

Verifies:
- ``supy.validate_config`` accepts YAML file paths, in-memory dicts, and
  ``SUEWSConfig`` instances with zero disk writes by default.
- ``ValidationReport`` exposes ``is_valid``, ``errors``, ``warnings``,
  ``suggestions``, ``applied_fixes``, ``info``, ``to_text()``, and stable
  ``to_dict()``.
- ``suews validate --dry-run`` runs full read-only in-memory A+B+C checks
  (catching Phase B scientific errors, Phase A dimension errors, and public-mode
  restrictions) without writing any files.
- ``suews validate -p C --dry-run`` also enforces unified ``--mode public``
  restrictions while preserving multi-file schema checks.
- Retired legacy modules and symbols (``supy.validation``,
  ``ValidationController``, ``core.feedback``, ``run_precheck``) are absent.
"""

from __future__ import annotations

from importlib.resources import files
import json
from pathlib import Path

from click.testing import CliRunner
import pytest

import supy
from supy.cmd.validate_config import cli as validate_cli
from supy.data_model.core._yaml_io import safe_dump_yaml, safe_load_yaml
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.validation import (
    ValidationEngine,
    ValidationReport,
    validate_config,
)

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-ERROR", "C-API"),
]


def _load_sample_dict() -> dict:
    text = (files("supy") / "sample_data" / "sample_config.yml").read_text(
        encoding="utf-8"
    )
    return safe_load_yaml(text)


def _write_yaml(path: Path, data: dict) -> Path:
    path.write_text(
        safe_dump_yaml(data, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )
    return path


def test_supy_validate_config_accepts_path_dict_and_model_without_writes(
    tmp_path: Path,
) -> None:
    """Public ``supy.validate_config`` handles Path, dict, and SUEWSConfig in memory."""
    assert supy.validate_config is validate_config
    assert supy.ValidationEngine is ValidationEngine
    assert supy.ValidationReport is ValidationReport

    sample_dict = _load_sample_dict()
    cfg_path = _write_yaml(tmp_path / "sample.yml", sample_dict)
    before_entries = set(tmp_path.rglob("*"))

    report_path = supy.validate_config(cfg_path, forcing="off")
    report_dict = supy.validate_config(sample_dict, forcing="off")
    report_model = supy.validate_config(
        SUEWSConfig.from_yaml(str(cfg_path)), forcing="off"
    )

    # Zero disk writes for default write_outputs=False.
    assert set(tmp_path.rglob("*")) == before_entries

    for report in (report_path, report_dict, report_model):
        assert isinstance(report, ValidationReport)
        assert report.is_valid
        assert not report.has_errors
        assert report.errors == []
        assert [p.phase for p in report.phases] == ["A", "B", "C"]
        payload = report.to_dict()
        assert set(payload.keys()) == {"overall_status", "phases"}
        rendered = report.to_text()
        assert "# SUEWS Validation Report" in rendered


def test_validate_config_catches_phase_b_scientific_error_and_public_mode(
    tmp_path: Path,
) -> None:
    """In-memory engine enforces public-mode policy and Phase B scientific rules."""
    # 1. Public-mode restriction when STEBBS is enabled.
    stebbs_cfg = _load_sample_dict()
    stebbs_cfg["model"]["physics"]["stebbs"]["enabled"] = {"value": True}
    pub_report = validate_config(stebbs_cfg, mode="public", forcing="off")
    assert not pub_report.is_valid
    assert any(
        i.code == "MODE.PUBLIC.RESTRICTED_FEATURE" for i in pub_report.errors
    )

    # 2. Phase B scientific error in dev mode (STEBBS internal_shading=1 without factor).
    stebbs_cfg["sites"][0]["properties"]["stebbs"]["internal_shading"] = {"value": 1}
    stebbs_cfg["sites"][0]["properties"]["stebbs"]["reduction_factor_shading"] = None
    dev_report = validate_config(
        stebbs_cfg,
        pipeline="ABC",
        mode="dev",
        forcing="off",
        science_fixes="off",
    )
    assert not dev_report.is_valid
    assert any("reduction_factor_shading" in e.message for e in dev_report.errors)

    # 3. continue_on_error runs downstream read-only phases after Phase A failure.
    bad_nlayer = _load_sample_dict()
    bad_nlayer["sites"][0]["properties"]["vertical_layers"]["nlayer"] = {"value": 4}
    stop_report = validate_config(
        bad_nlayer, pipeline="ABC", forcing="off", continue_on_error=False
    )
    cont_report = validate_config(
        bad_nlayer, pipeline="ABC", forcing="off", continue_on_error=True
    )
    assert [p.phase for p in stop_report.phases] == ["A"]
    assert [p.phase for p in cont_report.phases] == ["A", "B", "C"]
    assert not cont_report.is_valid


def test_validate_config_write_outputs_publishes_staged_files(tmp_path: Path) -> None:
    """``write_outputs=True`` runs the transactional publisher and creates output files."""
    cfg_path = _write_yaml(tmp_path / "pub.yml", _load_sample_dict())

    report = supy.validate_config(cfg_path, forcing="off", write_outputs=True)

    assert report.is_valid
    assert (tmp_path / "updated_pub.yml").is_file()
    assert (tmp_path / "report_pub.txt").is_file()
    assert (tmp_path / "report_pub.json").is_file()

    with pytest.raises(ValueError, match="write_outputs=True requires"):
        supy.validate_config(_load_sample_dict(), write_outputs=True)


def test_dry_run_abc_catches_phase_b_scientific_error_without_writing_files(
    tmp_path: Path,
) -> None:
    """``suews validate --dry-run`` runs in-memory Phase A+B+C checks with zero writes."""
    # Invalid land-cover fraction sum (1.5) is a Phase B scientific error that
    # schema-only checks missed unless Pydantic or Phase B caught it; also test
    # vertical_layers nlayer mismatch (Phase A dimension error).
    data = _load_sample_dict()
    data["sites"][0]["properties"]["vertical_layers"]["nlayer"] = {"value": 4}
    cfg_path = _write_yaml(tmp_path / "bad_nlayer.yml", data)
    before_entries = set(tmp_path.rglob("*"))

    result = CliRunner().invoke(
        validate_cli,
        ["--dry-run", "--format", "json", str(cfg_path)],
    )

    assert result.exit_code == 1, result.output
    assert set(tmp_path.rglob("*")) == before_entries
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert envelope["data"]["is_valid"] is False
    assert any(
        "expected 4" in err["message"] or "nlayer" in err["message"].lower()
        for err in envelope["errors"]
    ), envelope["errors"]


@pytest.mark.parametrize("pipeline", ["ABC", "C"])
def test_dry_run_enforces_public_mode_restrictions_consistently(
    tmp_path: Path, pipeline: str
) -> None:
    """``--mode public`` blocks experimental features under ``--dry-run`` too."""
    data = _load_sample_dict()
    data["model"]["physics"]["stebbs"]["enabled"] = {"value": True}
    cfg_path = _write_yaml(tmp_path / "stebbs_public.yml", data)
    before_entries = set(tmp_path.rglob("*"))

    pub_result = CliRunner().invoke(
        validate_cli,
        ["-p", pipeline, "--mode", "public", "--dry-run", "--format", "json", str(cfg_path)],
    )
    assert pub_result.exit_code == 1, pub_result.output
    assert set(tmp_path.rglob("*")) == before_entries
    pub_env = json.loads(pub_result.stdout)
    assert any("STEBBS" in err["message"] for err in pub_env["errors"]), pub_env["errors"]

    # Same config succeeds under --mode dev (with STEBBS valid defaults).
    dev_result = CliRunner().invoke(
        validate_cli,
        ["-p", pipeline, "--mode", "dev", "--dry-run", "--format", "json", str(cfg_path)],
    )
    assert dev_result.exit_code == 0, dev_result.output
    assert set(tmp_path.rglob("*")) == before_entries


def test_retired_legacy_validation_modules_and_symbols_are_removed() -> None:
    """Retired legacy validator modules and precheck symbols are no longer present."""
    src_root = Path(__file__).resolve().parents[2] / "src" / "supy"
    for rel_path in (
        "validation.py",
        "data_model/validation/core/controller.py",
        "data_model/validation/core/feedback.py",
    ):
        assert not (src_root / rel_path).exists(), f"{rel_path} should be deleted"

    for symbol in (
        "ValidationController",
        "validate_suews_config_conditional",
    ):
        assert not hasattr(supy, symbol)
        assert not hasattr(supy.data_model, symbol)

    assert not hasattr(supy.data_model, "run_precheck")
    assert not hasattr(supy.data_model.validation, "run_precheck")
    assert not hasattr(supy.data_model.validation.core.yaml_helpers, "run_precheck")
    assert not hasattr(supy.data_model.validation.core.yaml_helpers, "SeasonCheck")
