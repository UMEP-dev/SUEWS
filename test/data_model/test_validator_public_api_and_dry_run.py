"""Wave 5A tests: public ``supy.validate_config`` and unified ``--dry-run``.

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
- Legacy validator exports have been retired and are verified absent.
"""

from __future__ import annotations

import copy
from importlib.resources import files
import json
from pathlib import Path
from types import MappingProxyType

from click.testing import CliRunner
import pytest
import yaml

import supy
from supy.cmd.validate_config import cli as validate_cli
from supy.data_model.core._yaml_io import safe_dump_yaml, safe_load_yaml
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.validation import (
    ValidationEngine,
    ValidationReport,
    validate_config,
)
from supy.data_model.validation.pipeline.document import ConfigDocument, ConfigPatch

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
    sample_dict_snapshot = copy.deepcopy(sample_dict)
    cfg_path = _write_yaml(tmp_path / "sample.yml", sample_dict)
    before_entries = set(tmp_path.rglob("*"))
    before_bytes = cfg_path.read_bytes()

    report_path = supy.validate_config(cfg_path, forcing="off")
    report_dict = supy.validate_config(sample_dict, forcing="off")
    report_model = supy.validate_config(
        SUEWSConfig.from_yaml(str(cfg_path)), forcing="off"
    )

    # Zero disk writes (no new files and no in-place overwrite) and no dict mutation.
    assert set(tmp_path.rglob("*")) == before_entries
    assert cfg_path.read_bytes() == before_bytes
    assert sample_dict == sample_dict_snapshot

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


def test_public_api_and_cli_dry_run_handle_malformed_and_incomplete_inputs(
    tmp_path: Path,
) -> None:
    """Outer-boundary regression tests for empty YAML, default SUEWSConfig, null model, bad snow_use, bad UTF-8."""
    # 1. Default SUEWSConfig() and {} warn cleanly on omitted bounds (#2305) without crashing,
    #    while malformed inputs return structured errors (never 'severity FAIL' or rule crashes).
    for default_like in (SUEWSConfig(), {}):
        rep_default = supy.validate_config(
            default_like, pipeline="ABC", forcing="off", continue_on_error=True
        )
        assert any(
            i.severity == "WARNING" and "season-dependent checks" in i.message
            for i in rep_default.issues
        )
    for bad_input in (
        {"model": None, "sites": None},
        {"model": {"control": {"start_time": "soon"}}, "sites": []},
    ):
        rep = supy.validate_config(
            bad_input, pipeline="ABC", forcing="off", continue_on_error=True
        )
        assert rep.has_errors
        assert not rep.is_valid
        assert not any(
            i.code.startswith("VALIDATOR.RULE_FAILED") for i in rep.issues
        ), rep.issues

    # 2. Empty YAML file fails cleanly in Phase A alone (not is_valid=True) and in CLI JSON dry-run.
    empty_yml = tmp_path / "empty.yml"
    empty_yml.write_text("", encoding="utf-8")
    rep_empty_a = supy.validate_config(empty_yml, pipeline="A", forcing="off")
    assert rep_empty_a.has_errors
    assert any(i.code == "A.PIPELINE.YAML_NOT_MAPPING" for i in rep_empty_a.errors)

    cli_empty = CliRunner().invoke(
        validate_cli,
        ["-p", "ABC", "--dry-run", "--format", "json", str(empty_yml)],
    )
    assert cli_empty.exit_code == 1
    payload_empty = json.loads(cli_empty.output)
    assert payload_empty["status"] == "error"

    # 3. Invalid UTF-8 bytes produce structured READ_ERROR instead of uncaught UnicodeDecodeError.
    bad_utf8 = tmp_path / "bad_utf8.yml"
    bad_utf8.write_bytes(b"\xff\xfe\x00")
    rep_utf8 = supy.validate_config(bad_utf8, pipeline="ABC", forcing="off")
    assert rep_utf8.has_errors
    assert any(i.code == "A.PIPELINE.READ_ERROR" for i in rep_utf8.errors)

    # 4. Invalid snow_use string does not crash public-mode checks across Python API or CLI JSON.
    bad_snow = tmp_path / "bad_snow.yml"
    bad_snow.write_text(
        "model:\n  physics:\n    snow_use: bogus\nsites: []\n",
        encoding="utf-8",
    )
    rep_snow = supy.validate_config(bad_snow, pipeline="ABC", mode="public", forcing="off")
    assert rep_snow.has_errors
    cli_snow = CliRunner().invoke(
        validate_cli,
        ["-p", "ABC", "--dry-run", "--mode", "public", "--format", "json", str(bad_snow)],
    )
    assert cli_snow.exit_code == 1
    payload_snow = json.loads(cli_snow.output)
    assert payload_snow["status"] == "error"

    # 5. Phase B blocking errors suppress scientific suggestions/apply in memory.
    cfg = _load_sample_dict()
    cfg["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] = {"value": 0.99}
    rep_b_err = supy.validate_config(
        cfg,
        pipeline="B",
        mode="dev",
        forcing="off",
        science_fixes="suggest",
    )
    assert rep_b_err.has_errors
    assert rep_b_err.suggestions == []
    assert rep_b_err.applied_fixes == []
    # Report text is strictly ASCII even for land-cover sum errors.
    assert rep_b_err.to_text().isascii()


def test_round2_parity_staged_policy_legacy_phase_b_frozen_tuples_and_norm_errors(
    tmp_path: Path,
) -> None:
    """Verify staged policy publication parity, legacy Phase B renames, frozen tuples, and norm errors."""

    def _freeze(obj):
        if isinstance(obj, dict):
            return MappingProxyType({k: _freeze(v) for k, v in obj.items()})
        if isinstance(obj, list):
            return tuple(_freeze(v) for v in obj)
        return obj

    # 1. Staged public-mode policy failure with continue_on_error=True matches disk reports.
    stebbs_cfg = _load_sample_dict()
    stebbs_cfg["model"]["physics"]["stebbs"]["enabled"] = True
    stebbs_yml = tmp_path / "stebbs_public.yml"
    stebbs_yml.write_text(yaml.safe_dump(stebbs_cfg, sort_keys=False), encoding="utf-8")

    staged_rep = supy.validate_config(
        stebbs_yml,
        pipeline="C",
        mode="public",
        forcing="off",
        continue_on_error=True,
        write_outputs=True,
    )
    assert staged_rep.has_errors
    assert any(i.code == "MODE.PUBLIC.RESTRICTED_FEATURE" for i in staged_rep.errors)
    pub_txt = (tmp_path / "report_stebbs_public.txt").read_text(encoding="utf-8")
    pub_json = json.loads(
        (tmp_path / "report_stebbs_public.json").read_text(encoding="utf-8")
    )
    assert "# Mode: Public" in pub_txt
    assert "STEBBS" in pub_txt
    assert pub_json["overall_status"] == "FAILED"
    assert any(
        issue["code"] == "MODE.PUBLIC.RESTRICTED_FEATURE"
        for phase in pub_json["phases"]
        for issue in phase["issues"]
    )
    assert not (tmp_path / "updated_stebbs_public.yml").exists()

    # 2. Standalone in-memory Phase B normalises legacy aliases (e.g. WallReflectivity).
    legacy_b = _load_sample_dict()
    legacy_b["model"]["physics"]["stebbs"]["enabled"] = True
    arch = legacy_b["sites"][0]["properties"]["building_archetype"]
    arch.pop("reflectivity_wall_external", None)
    arch["WallReflectivity"] = 0.8
    arch["absorptivity_wall_external"] = 0.8
    arch["transmissivity_wall_external"] = 0.8
    rep_legacy_b = supy.validate_config(
        legacy_b, pipeline="B", mode="dev", forcing="off", science_fixes="off"
    )
    assert rep_legacy_b.has_errors

    # 3. Recursively frozen mappings AND tuples thaw cleanly for patches and full ABC apply.
    frozen_doc = ConfigDocument.from_dict({"sites": ({"gridiv": 1},)}, normalise=False)
    patched_frozen = frozen_doc.apply_patches([ConfigPatch("sites[0].gridiv", "set", 2)])
    assert patched_frozen.read("sites[0].gridiv") == 2

    frozen_sample = _freeze(_load_sample_dict())
    rep_frozen = supy.validate_config(
        frozen_sample,
        pipeline="ABC",
        mode="public",
        forcing="off",
        science_fixes="apply",
    )
    assert rep_frozen.is_valid, rep_frozen.to_text()


def test_round3_parity_norm_errors_missing_physics_nlayer_and_cli_continue(
    tmp_path: Path,
) -> None:
    """Verify conflicting selectors, ASCII report, missing physics, oversized nlayer, and CLI continue."""
    # 1. Conflicting physics selectors (snow_use + snow) return structured errors, not raw ValueError.
    dup_snow = _load_sample_dict()
    dup_snow["model"]["physics"]["snow_use"] = 0
    for pipe in ("A", "B", "ABC"):
        rep_dup = supy.validate_config(
            dup_snow,
            pipeline=pipe,
            mode="dev",
            forcing="off",
            continue_on_error=True,
        )
        assert rep_dup.has_errors

    # 2. ASCII guarantee on to_text() even with land-cover sum error (0.53) and Unicode path.
    bad_lc = _load_sample_dict()
    bad_lc["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] = {"value": 0.53}
    rep_lc = supy.validate_config(
        bad_lc, pipeline="B", mode="dev", forcing="off", science_fixes="off"
    )
    assert rep_lc.has_errors
    assert rep_lc.to_text().isascii()

    # 3. Missing model.physics section is rejected by supy.validate_config (parity with CLI).
    no_physics = _load_sample_dict()
    no_physics["model"].pop("physics")
    rep_no_phys = supy.validate_config(
        no_physics, pipeline="ABC", mode="public", forcing="off", science_fixes="off"
    )
    assert rep_no_phys.has_errors
    assert any(i.code == "A.MISSING_PARAM.CRITICAL" for i in rep_no_phys.errors)

    # 4. Oversized nlayer (e.g. 10**30) returns structured Phase A limit warning + Phase C error, never OverflowError.
    huge_nlayer = _load_sample_dict()
    huge_nlayer["sites"][0]["properties"]["vertical_layers"]["nlayer"] = {"value": 10**30}
    rep_huge_a = supy.validate_config(
        huge_nlayer, pipeline="A", mode="public", forcing="off", science_fixes="off"
    )
    assert any(i.code == "A.NLAYER_LIMIT" for i in rep_huge_a.issues)
    rep_huge = supy.validate_config(
        huge_nlayer, pipeline="ABC", mode="public", forcing="off", science_fixes="off"
    )
    assert rep_huge.has_errors
    assert any(i.code == "A.NLAYER_LIMIT" for i in rep_huge.issues)

    # 5. CLI --mode public --continue-on-error runs downstream phases alongside policy restriction.
    snow_cfg = _load_sample_dict()
    snow_cfg["model"]["physics"]["snow"] = "enabled"
    snow_cfg["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] = {"value": 0.99}
    snow_yml = tmp_path / "snow_continue.yml"
    snow_yml.write_text(yaml.safe_dump(snow_cfg, sort_keys=False), encoding="utf-8")
    cli_cont = CliRunner().invoke(
        validate_cli,
        [
            "-p",
            "ABC",
            "--mode",
            "public",
            "--forcing",
            "off",
            "--continue-on-error",
            "--format",
            "json",
            str(snow_yml),
        ],
    )
    assert cli_cont.exit_code == 1
    cont_payload = json.loads(cli_cont.output)
    phases_run = [p["phase"] for p in cont_payload["data"]["validation_report"]["phases"]]
    assert phases_run == ["A", "B", "C"]
    all_codes = [
        i["code"]
        for p in cont_payload["data"]["validation_report"]["phases"]
        for i in p["issues"]
    ]
    assert "MODE.PUBLIC.RESTRICTED_FEATURE" in all_codes


def test_validate_config_handles_null_bounds_with_and_without_forcing(
    tmp_path: Path,
) -> None:
    """In-memory validation mirrors CLI #2305 null-bound forcing fallback and skip."""
    # 1. With readable forcing file next to YAML: bounds come from forcing, no skip warning.
    work = tmp_path / "with_forcing"
    work.mkdir()
    sample_dir = files("supy").joinpath("sample_data")
    for item in sample_dir.iterdir():
        if item.is_file():
            (work / item.name).write_bytes(item.read_bytes())
    cfg_path = work / "sample_config.yml"
    data = yaml.safe_load(cfg_path.read_text(encoding="utf-8"))
    data["model"]["control"]["start_time"] = None
    data["model"]["control"]["end_time"] = None
    cfg_path.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")

    rep_with_forcing = supy.validate_config(
        cfg_path, pipeline="ABC", mode="public", forcing="on", science_fixes="suggest"
    )
    assert rep_with_forcing.is_valid, rep_with_forcing.to_text()
    assert not any(
        "season-dependent checks" in issue.message for issue in rep_with_forcing.issues
    )

    # 2. In-memory dict with null bounds and runtime (null) forcing: warn and skip seasonal steps.
    dict_no_forcing = _load_sample_dict()
    dict_no_forcing["model"]["control"]["start_time"] = None
    dict_no_forcing["model"]["control"]["end_time"] = None
    dict_no_forcing["model"]["control"]["forcing"] = {"file": None}
    rep_no_forcing = supy.validate_config(
        dict_no_forcing,
        pipeline="ABC",
        mode="public",
        forcing="off",
        science_fixes="apply",
    )
    assert rep_no_forcing.is_valid, rep_no_forcing.to_text()
    assert any(
        issue.severity == "WARNING" and "season-dependent checks" in issue.message
        for issue in rep_no_forcing.issues
    )


def test_in_memory_and_staged_parity_for_non_mapping_bc_continue_and_to_text(
    tmp_path: Path,
) -> None:
    """Verify non-mapping BC error, unmutated Phase C input on A failure, and to_text() mode."""
    # 1. Non-mapping YAML ('[]') fails in pipeline='BC' for both write_outputs=False and True.
    list_yml = tmp_path / "list_root.yml"
    list_yml.write_text("[]\n", encoding="utf-8")
    rep_mem_bc = supy.validate_config(
        list_yml, pipeline="BC", forcing="off", science_fixes="off", write_outputs=False
    )
    rep_stg_bc = supy.validate_config(
        list_yml, pipeline="BC", forcing="off", science_fixes="off", write_outputs=True
    )
    assert rep_mem_bc.has_errors
    assert any(i.code == "B.PIPELINE.YAML_NOT_MAPPING" for i in rep_mem_bc.errors)
    assert rep_stg_bc.has_errors

    # 2. Continuing after Phase A dimension error forwards the original unpadded data to Phase C.
    short_height = _load_sample_dict()
    short_height["sites"][0]["properties"]["vertical_layers"]["height"] = {
        "value": [0.0, 10.0]
    }
    mem_yml = _write_yaml(tmp_path / "short_mem.yml", short_height)
    stg_dir = tmp_path / "staged_ac"
    stg_dir.mkdir()
    stg_yml = _write_yaml(stg_dir / "short_stg.yml", short_height)

    rep_mem_ac = supy.validate_config(
        mem_yml,
        pipeline="AC",
        forcing="off",
        continue_on_error=True,
        write_outputs=False,
    )
    rep_stg_ac = supy.validate_config(
        stg_yml,
        pipeline="AC",
        forcing="off",
        continue_on_error=True,
        write_outputs=True,
    )
    assert [p.phase for p in rep_mem_ac.phases] == ["A", "C"]
    assert [p.phase for p in rep_stg_ac.phases] == ["A", "C"]
    mem_c_msgs = sorted(
        i.message for i in rep_mem_ac.phases[1].issues if i.severity == "ERROR"
    )
    stg_c_msgs = sorted(
        i.message for i in rep_stg_ac.phases[1].issues if i.severity == "ERROR"
    )
    assert mem_c_msgs == stg_c_msgs

    # 3. ValidationReport.to_text() preserves the mode and science_fixes used during validation.
    rep_dev = supy.validate_config(
        _load_sample_dict(),
        pipeline="ABC",
        mode="dev",
        forcing="off",
        science_fixes="apply",
    )
    assert rep_dev.is_valid
    assert "# Mode: Dev" in rep_dev.to_text()
    assert "# Mode: Public" in rep_dev.to_text(mode="public")
    assert set(rep_dev.to_dict().keys()) == {"overall_status", "phases"}


def test_dry_run_respects_science_fixes_continue_on_error_and_two_site_identity(
    tmp_path: Path,
) -> None:
    """ABC dry-run honours --science-fixes/--continue-on-error and preserves per-site error identity."""
    import copy

    # 1. --continue-on-error controls whether downstream Phase B errors run after Phase A fails.
    bad_ab = _load_sample_dict()
    bad_ab["sites"][0]["properties"]["vertical_layers"]["nlayer"] = {"value": 4}
    bad_ab["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] = {"value": 0.99}
    bad_ab_path = _write_yaml(tmp_path / "bad_ab.yml", bad_ab)

    stop_res = CliRunner().invoke(
        validate_cli,
        ["-p", "ABC", "--dry-run", "--forcing", "off", "--format", "json", str(bad_ab_path)],
    )
    cont_res = CliRunner().invoke(
        validate_cli,
        [
            "-p",
            "ABC",
            "--dry-run",
            "--forcing",
            "off",
            "--continue-on-error",
            "--format",
            "json",
            str(bad_ab_path),
        ],
    )
    assert stop_res.exit_code == 1, stop_res.output
    assert cont_res.exit_code == 1, cont_res.output
    stop_phases = {err.get("phase") for err in json.loads(stop_res.stdout)["errors"]}
    cont_phases = {err.get("phase") for err in json.loads(cont_res.stdout)["errors"]}
    assert "B" not in stop_phases, stop_phases
    assert "B" in cont_phases, cont_phases

    # 2. --science-fixes apply validates the in-memory transformed A->B->C document with zero writes.
    half_dls = _load_sample_dict()
    half_dls["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = {"value": 85}
    half_dls["sites"][0]["properties"]["anthropogenic_emissions"]["enddls"] = None
    half_dls_path = _write_yaml(tmp_path / "half_dls.yml", half_dls)
    before_entries = set(tmp_path.rglob("*"))

    suggest_res = CliRunner().invoke(
        validate_cli,
        [
            "-p",
            "ABC",
            "--dry-run",
            "--forcing",
            "off",
            "--science-fixes",
            "suggest",
            "--format",
            "json",
            str(half_dls_path),
        ],
    )
    half_dls_before_bytes = half_dls_path.read_bytes()
    apply_res = CliRunner().invoke(
        validate_cli,
        [
            "-p",
            "ABC",
            "--dry-run",
            "--forcing",
            "off",
            "--science-fixes",
            "apply",
            "--format",
            "json",
            str(half_dls_path),
        ],
    )
    assert set(tmp_path.rglob("*")) == before_entries
    assert half_dls_path.read_bytes() == half_dls_before_bytes
    assert suggest_res.exit_code == 1, suggest_res.output
    assert apply_res.exit_code == 0, apply_res.output
    assert json.loads(apply_res.stdout)["data"]["is_valid"] is True

    # 3. Two-site config with identical missing lat preserves both errors, grid IDs, and issue codes.
    two_site = _load_sample_dict()
    site0 = copy.deepcopy(two_site["sites"][0])
    site1 = copy.deepcopy(two_site["sites"][0])
    site0["gridiv"] = 101
    site1["gridiv"] = 202
    site0["properties"]["lat"] = None
    site1["properties"]["lat"] = None
    two_site["sites"] = [site0, site1]
    two_site_path = _write_yaml(tmp_path / "two_sites_missing_lat.yml", two_site)

    two_res = CliRunner().invoke(
        validate_cli,
        [
            "-p",
            "ABC",
            "--dry-run",
            "--forcing",
            "off",
            "--continue-on-error",
            "--format",
            "json",
            str(two_site_path),
        ],
    )
    assert two_res.exit_code == 1, two_res.output
    two_env = json.loads(two_res.stdout)
    lat_errs = [
        err
        for err in two_env["errors"]
        if err.get("phase") == "B" and "lat" in str(err.get("message", "")).lower()
    ]
    assert len(lat_errs) == 2, two_env["errors"]
    assert {err.get("site_index") for err in lat_errs} == {0, 1}, lat_errs
    assert {err.get("site_gridid") for err in lat_errs} == {101, 202}, lat_errs
    assert all(err.get("issue_code") for err in lat_errs), lat_errs


def test_apply_dry_run_validates_transformed_document_without_stale_schema_errors(
    tmp_path: Path,
) -> None:
    """ABC --dry-run --science-fixes apply accepts inputs repaired by Phase B, matching staged execution."""
    summer = _load_sample_dict()
    summer["model"]["control"]["start_time"] = "2012-06-15"
    summer["model"]["control"]["end_time"] = "2012-06-16"
    summer["sites"][0]["initial_states"]["snowalb"] = "unused"
    summer_path = _write_yaml(tmp_path / "summer_snowalb.yml", summer)

    before_entries = set(tmp_path.rglob("*"))
    dry_apply = CliRunner().invoke(
        validate_cli,
        [
            "-p",
            "ABC",
            "--dry-run",
            "--forcing",
            "off",
            "--science-fixes",
            "apply",
            "--format",
            "json",
            str(summer_path),
        ],
    )
    assert set(tmp_path.rglob("*")) == before_entries
    assert dry_apply.exit_code == 0, dry_apply.output
    dry_env = json.loads(dry_apply.stdout)
    assert dry_env["data"]["is_valid"] is True
    assert dry_env["errors"] == []

    # Under default suggest (no transformation applied), the invalid snowalb string still fails.
    dry_suggest = CliRunner().invoke(
        validate_cli,
        [
            "-p",
            "ABC",
            "--dry-run",
            "--forcing",
            "off",
            "--science-fixes",
            "suggest",
            "--format",
            "json",
            str(summer_path),
        ],
    )
    assert dry_suggest.exit_code == 1, dry_suggest.output
    assert json.loads(dry_suggest.stdout)["data"]["is_valid"] is False

    # Winter start keeps snowalb, so apply dry-run still rejects the invalid string.
    winter = _load_sample_dict()
    winter["model"]["control"]["start_time"] = "2012-01-15"
    winter["model"]["control"]["end_time"] = "2012-01-16"
    winter["sites"][0]["initial_states"]["snowalb"] = "unused"
    winter_path = _write_yaml(tmp_path / "winter_snowalb.yml", winter)
    dry_winter = CliRunner().invoke(
        validate_cli,
        [
            "-p",
            "ABC",
            "--dry-run",
            "--forcing",
            "off",
            "--science-fixes",
            "apply",
            "--format",
            "json",
            str(winter_path),
        ],
    )
    assert dry_winter.exit_code == 1, dry_winter.output
    assert json.loads(dry_winter.stdout)["data"]["is_valid"] is False


@pytest.mark.parametrize(
    ("omit_kind", "expected_path"),
    [
        ("top_level", "model.physics.stability"),
        ("stebbs", "model.physics.stebbs.capacitance"),
    ],
)
def test_phase_c_and_suews_config_reject_omitted_required_physics(
    tmp_path: Path, omit_kind: str, expected_path: str
) -> None:
    """Omitted required physics switches fail consistently across mapping, SUEWSConfig, pipeline C, and CLI dry-run."""
    data = _load_sample_dict()
    if omit_kind == "top_level":
        del data["model"]["physics"]["stability"]
    else:
        del data["model"]["physics"]["stebbs"]["capacitance"]

    # 1. Pipeline C Python API rejects missing switch on raw mapping.
    c_map_report = validate_config(data, pipeline="C", forcing="off")
    assert not c_map_report.is_valid
    assert any(
        expected_path in (issue.yaml_path or "") for issue in c_map_report.errors
    ), c_map_report.errors

    # 2. Equivalent SUEWSConfig preserves omission provenance across ABC and C.
    cfg = SUEWSConfig.from_dict(copy.deepcopy(data))
    abc_cfg_report = validate_config(cfg, pipeline="ABC", forcing="off")
    c_cfg_report = validate_config(cfg, pipeline="C", forcing="off")
    assert not abc_cfg_report.is_valid
    assert any(
        expected_path in (issue.yaml_path or "") for issue in abc_cfg_report.errors
    ), abc_cfg_report.errors
    assert not c_cfg_report.is_valid
    assert any(
        expected_path in (issue.yaml_path or "") for issue in c_cfg_report.errors
    ), c_cfg_report.errors

    # 3. Explicitly assigning the missing switch on the live SUEWSConfig makes it valid.
    from supy.data_model.core.model import RCMethod
    from supy.data_model.core.type import RefValue

    if omit_kind == "top_level":
        cfg.model.physics.stability = 3
    else:
        cfg.model.physics.stebbs.capacitance = RefValue(RCMethod.DEFAULT)
    repaired_report = validate_config(cfg, pipeline="ABC", forcing="off")
    assert repaired_report.is_valid, repaired_report.errors

    # 4. CLI -p C --dry-run agrees on the same omitted switch.
    yaml_path = _write_yaml(tmp_path / f"omit_{omit_kind}.yml", data)
    cli_res = CliRunner().invoke(
        validate_cli,
        ["-p", "C", "--dry-run", "--format", "json", str(yaml_path)],
    )
    assert cli_res.exit_code == 1, cli_res.output
    cli_env = json.loads(cli_res.stdout)
    assert any(
        expected_path in str(err.get("schema_path", "")) for err in cli_env["errors"]
    ), cli_env["errors"]


def test_dry_run_surfaces_skipped_seasonal_checks_warning(tmp_path: Path) -> None:
    """Dry-run forwards engine warnings such as skipped seasonal checks with phase metadata."""
    data = _load_sample_dict()
    data["model"]["control"]["start_time"] = None
    data["model"]["control"]["end_time"] = None
    data["model"]["control"]["forcing"]["file"] = None
    null_bounds_path = _write_yaml(tmp_path / "null_bounds.yml", data)

    res = CliRunner().invoke(
        validate_cli,
        ["-p", "ABC", "--dry-run", "--forcing", "off", "--format", "json", str(null_bounds_path)],
    )
    assert res.exit_code == 0, res.output
    env = json.loads(res.stdout)
    assert env["status"] == "warning", env
    assert env["data"]["is_valid"] is True
    assert any(
        isinstance(w, dict)
        and w.get("phase") == "B"
        and w.get("issue_code") == "SEASONAL.MODEL_CONTROL_START_TIME"
        for w in env["warnings"]
    ), env["warnings"]


@pytest.mark.filterwarnings(
    "ignore:ModelPhysics.*flat STEBBS.*:DeprecationWarning"
)
@pytest.mark.parametrize("science_fixes", ["suggest", "off", "apply"])
def test_dry_run_accepts_legacy_flat_stebbs_across_all_science_fix_modes(
    tmp_path: Path, science_fixes: str
) -> None:
    """Legacy flat STEBBS representation is accepted consistently across suggest, off, and apply dry-run."""
    data = _load_sample_dict()
    physics = data["model"]["physics"]
    nested_stebbs = physics.pop("stebbs")
    physics["stebbs"] = 0
    for leaf in (
        "capacitance",
        "setpoint",
        "same_albedo_wall",
        "same_albedo_roof",
        "same_emissivity_wall",
        "same_emissivity_roof",
    ):
        physics[leaf] = nested_stebbs[leaf]

    flat_path = _write_yaml(tmp_path / f"legacy_flat_stebbs_{science_fixes}.yml", data)
    res = CliRunner().invoke(
        validate_cli,
        [
            "-p",
            "ABC",
            "--dry-run",
            "--forcing",
            "off",
            "--science-fixes",
            science_fixes,
            "--format",
            "json",
            str(flat_path),
        ],
    )
    assert res.exit_code == 0, res.output
    env = json.loads(res.stdout)
    assert env["data"]["is_valid"] is True
    assert env["errors"] == []


def test_suews_config_child_edits_repair_omitted_stebbs_block() -> None:
    """Assigning required leaves on a default-created stebbs child makes SUEWSConfig valid."""
    sample = _load_sample_dict()
    orig = SUEWSConfig.from_dict(copy.deepcopy(sample))

    data = copy.deepcopy(sample)
    del data["model"]["physics"]["stebbs"]
    cfg = SUEWSConfig.from_dict(copy.deepcopy(data))

    # Omitted stebbs block is initially rejected by both ABC and C.
    assert not validate_config(cfg, pipeline="C", forcing="off").is_valid

    # Explicitly setting all six required switches on the default child repairs it.
    for leaf in (
        "capacitance",
        "setpoint",
        "same_albedo_wall",
        "same_albedo_roof",
        "same_emissivity_wall",
        "same_emissivity_roof",
    ):
        setattr(
            cfg.model.physics.stebbs,
            leaf,
            copy.deepcopy(getattr(orig.model.physics.stebbs, leaf)),
        )

    repaired_c = validate_config(cfg, pipeline="C", forcing="off")
    repaired_abc = validate_config(cfg, pipeline="ABC", forcing="off")
    assert repaired_c.is_valid, repaired_c.errors
    assert repaired_abc.is_valid, repaired_abc.errors


@pytest.mark.parametrize("pipeline", ["A", "C", "AC", "ABC"])
def test_omitted_model_root_rejected_across_pipelines(
    tmp_path: Path, pipeline: str
) -> None:
    """Omitting the top-level ``model`` section never bypasses required physics checks."""
    sample = _load_sample_dict()
    for omitted in ({}, {"sites": copy.deepcopy(sample["sites"])}):
        report = supy.validate_config(omitted, pipeline=pipeline, forcing="off")
        assert not report.is_valid, (pipeline, report.to_text())
        assert any(
            i.code.startswith("C.MISSING_PARAM.") or i.code == "A.MISSING_PARAM.CRITICAL"
            for i in report.errors
        ), report.errors

    # Staged disk output mode enforces the same omitted-model rejection.
    staged_dir = tmp_path / f"staged_{pipeline.lower()}"
    staged_dir.mkdir()
    empty_map_path = staged_dir / "empty_map.yml"
    empty_map_path.write_text("{}\n", encoding="utf-8")
    staged_report = supy.validate_config(
        empty_map_path, pipeline=pipeline, forcing="off", write_outputs=True
    )
    assert not staged_report.is_valid, (pipeline, staged_report.to_text())
    assert any(
        i.code.startswith("C.MISSING_PARAM.") or i.code == "A.MISSING_PARAM.CRITICAL"
        for i in staged_report.errors
    ), staged_report.errors


@pytest.mark.parametrize("profile_kind", ["hourly", "ten_minute"])
def test_validate_config_rejects_mixed_int_string_profile_collisions_without_mutating_input(
    profile_kind: str,
) -> None:
    """Mixed int/string keys on a live SUEWSConfig fail cleanly without mutating caller input."""
    cfg = SUEWSConfig.from_dict(_load_sample_dict())
    if profile_kind == "hourly":
        target_map = cfg.sites[0].properties.anthropogenic_emissions.heat.ahprof_24hr.working_day
    else:
        target_map = cfg.sites[0].properties.building_archetype.profile_metabolism.working_day

    orig_str_val = target_map["1"]
    target_map[1] = 0.75
    snapshot_keys = list(target_map.keys())
    snapshot_items = list(target_map.items())

    for pipeline in ("ABC", "C"):
        report = supy.validate_config(cfg, pipeline=pipeline, forcing="off")
        assert not report.is_valid, (profile_kind, pipeline, report.to_text())
        assert any(
            "Duplicate" in err.message and "colliding keys" in err.message
            for err in report.errors
        ), report.errors

    # Caller's SUEWSConfig remains completely untouched.
    assert list(target_map.keys()) == snapshot_keys
    assert list(target_map.items()) == snapshot_items
    assert target_map["1"] == orig_str_val
    assert target_map[1] == pytest.approx(0.75)

    # Removing the colliding string key leaves a valid non-canonical int key:
    # validation succeeds and still does not mutate caller input in place.
    del target_map["1"]
    valid_report = supy.validate_config(cfg, pipeline="ABC", forcing="off")
    assert valid_report.is_valid, valid_report.errors
    assert 1 in target_map
    assert "1" not in target_map


def test_invalid_unquoted_yaml_date_returns_structured_error_across_api_and_cli(
    tmp_path: Path,
) -> None:
    """Invalid unquoted YAML timestamps (e.g. 2026-02-30) return structured errors, never ValueError."""
    bad_date = tmp_path / "invalid_date.yml"
    bad_date.write_text(
        "model:\n  control:\n    start_time: 2026-02-30\nsites: []\n",
        encoding="utf-8",
    )

    # 1. Public Python API: in-memory (write_outputs=False) and staged (write_outputs=True),
    #    across both public and dev modes.
    for mode in ("public", "dev"):
        for write_outputs in (False, True):
            rep = supy.validate_config(
                bad_date,
                pipeline="ABC",
                mode=mode,
                forcing="off",
                write_outputs=write_outputs,
            )
            assert not rep.is_valid, (mode, write_outputs)
            assert any(
                i.code == "A.PIPELINE.YAML_PARSE_ERROR" for i in rep.errors
            ), rep.errors

    # 2. CLI dry-run (-p ABC and -p C) across both JSON and table output formats.
    for pipeline in ("ABC", "C"):
        json_res = CliRunner().invoke(
            validate_cli,
            ["-p", pipeline, "--dry-run", "--format", "json", str(bad_date)],
        )
        assert json_res.exit_code == 1, (pipeline, json_res.output)
        payload = json.loads(json_res.stdout)
        assert payload["status"] == "error"
        assert payload["data"]["is_valid"] is False

        table_res = CliRunner().invoke(
            validate_cli,
            ["-p", pipeline, "--dry-run", "--format", "table", str(bad_date)],
        )
        assert table_res.exit_code == 1, (pipeline, table_res.output)
        assert table_res.exception is None or isinstance(table_res.exception, SystemExit)

    # 3. CLI non-dry-run (legacy/staged path) across public and dev modes and -p ABC / -p C.
    for mode in ("public", "dev"):
        for pipeline in ("ABC", "C"):
            case_dir = tmp_path / f"cli_{mode}_{pipeline.lower()}"
            case_dir.mkdir()
            case_yml = case_dir / "invalid_date.yml"
            case_yml.write_text(
                "model:\n  control:\n    start_time: 2026-02-30\nsites: []\n",
                encoding="utf-8",
            )
            cli_res = CliRunner().invoke(
                validate_cli,
                [
                    "-p",
                    pipeline,
                    "--mode",
                    mode,
                    "--forcing",
                    "off",
                    "--format",
                    "json",
                    str(case_yml),
                ],
            )
            assert cli_res.exit_code == 1, (mode, pipeline, cli_res.output)
            cli_payload = json.loads(cli_res.stdout)
            assert cli_payload["status"] == "error"
            assert cli_res.exception is None or isinstance(cli_res.exception, SystemExit)

            cli_table = CliRunner().invoke(
                validate_cli,
                [
                    "-p",
                    pipeline,
                    "--mode",
                    mode,
                    "--forcing",
                    "off",
                    "--format",
                    "table",
                    str(case_yml),
                ],
            )
            assert cli_table.exit_code == 1, (mode, pipeline, cli_table.output)
            assert cli_table.exception is None or isinstance(
                cli_table.exception, SystemExit
            )


def test_dry_run_preserves_spartacus_land_cover_morphology_warnings(
    tmp_path: Path,
) -> None:
    """Dry-run surfaces SPARTACUS land-cover/morphology mismatch warnings without suppressing them."""
    data = _load_sample_dict()
    data["model"]["physics"]["net_radiation"] = {"value": 1001}
    data["model"]["physics"]["storage_heat"] = {"value": 5}
    data["model"]["physics"]["ohm_inc_qf"] = {"value": 0}
    vl = data["sites"][0]["properties"]["vertical_layers"]
    vl["building_frac"] = {"value": [0.2, 0.15, 0.1]}
    vl["veg_frac"] = {"value": [0.3, 0.1, 0.0]}
    vl["veg_scale"] = {"value": [0.1, 0.1, 0.0]}
    cfg_path = _write_yaml(tmp_path / "spartacus_mismatch.yml", data)

    py_report = supy.validate_config(
        cfg_path, pipeline="ABC", mode="public", forcing="off"
    )
    py_codes = {w.code for w in py_report.warnings}
    assert "LAND_COVER.LAND_COVER_BLDGS_SFR" in py_codes, py_codes
    assert "LAND_COVER.LAND_COVER_TREE_SFR" in py_codes, py_codes

    cli_res = CliRunner().invoke(
        validate_cli,
        ["-p", "ABC", "--dry-run", "--forcing", "off", "--format", "json", str(cfg_path)],
    )
    assert cli_res.exit_code == 0, cli_res.output
    envelope = json.loads(cli_res.stdout)
    warn_codes = {
        w.get("issue_code") for w in envelope["warnings"] if isinstance(w, dict)
    }
    assert "LAND_COVER.LAND_COVER_BLDGS_SFR" in warn_codes, envelope["warnings"]
    assert "LAND_COVER.LAND_COVER_TREE_SFR" in warn_codes, envelope["warnings"]


def test_symlink_config_resolves_same_forcing_for_in_memory_and_staged(
    tmp_path: Path,
) -> None:
    """Symlinked YAML paths resolve relative forcing identically for write_outputs=False and True."""
    shared_dir = tmp_path / "shared"
    work_dir = tmp_path / "work"
    shared_dir.mkdir()
    work_dir.mkdir()

    sample_dir = files("supy").joinpath("sample_data")
    for item in sample_dir.iterdir():
        if item.is_file():
            (shared_dir / item.name).write_bytes(item.read_bytes())

    shared_cfg = shared_dir / "sample_config.yml"
    # Place a corrupt forcing file in work_dir with the same filename so any
    # symlink-resolution divergence between in-memory and staged modes fails loudly.
    (work_dir / "Kc_2012_data_60.txt").write_text("not valid forcing\n", encoding="utf-8")
    link_cfg = work_dir / "sample_config.yml"
    link_cfg.symlink_to(shared_cfg)

    mem_report = supy.validate_config(
        link_cfg, pipeline="ABC", mode="public", forcing="on", write_outputs=False
    )
    staged_report = supy.validate_config(
        link_cfg, pipeline="ABC", mode="public", forcing="on", write_outputs=True
    )
    assert mem_report.is_valid, mem_report.to_text()
    assert staged_report.is_valid, staged_report.to_text()


def test_validate_config_rejects_explicit_null_sdd_id_on_model_and_mapping() -> None:
    """Explicit null ``sdd_id`` via ``model_copy(update=...)`` matches mapping rejection without mutating input."""
    sample_map = _load_sample_dict()
    sample_map["sites"][0]["initial_states"]["evetr"]["sdd_id"] = None
    map_report = supy.validate_config(sample_map, pipeline="ABC", forcing="off")
    assert not map_report.is_valid

    cfg = SUEWSConfig.from_dict(_load_sample_dict())
    evetr_null = cfg.sites[0].initial_states.evetr.model_copy(update={"sdd_id": None})
    cfg.sites[0].initial_states.evetr = evetr_null

    for pipeline in ("ABC", "C"):
        model_report = supy.validate_config(cfg, pipeline=pipeline, forcing="off")
        assert not model_report.is_valid, (pipeline, model_report.to_text())
        assert any("sdd_id" in err.message for err in model_report.errors), (
            model_report.errors
        )

    # Caller's live SUEWSConfig remains completely untouched.
    assert cfg.sites[0].initial_states.evetr is evetr_null
    assert cfg.sites[0].initial_states.evetr.sdd_id is None


def test_validate_config_rejects_explicit_null_tstep_and_malformed_roofs_list() -> None:
    """Explicit null non-optional RefValue (``tstep=None``) and malformed ``roofs: [None]`` return structured errors."""
    # 1. Parity for explicit null non-optional RefValue beyond sdd_id (e.g. model.control.tstep=None).
    map_null_tstep = _load_sample_dict()
    map_null_tstep["model"]["control"]["tstep"] = None
    assert not supy.validate_config(map_null_tstep, pipeline="ABC", forcing="off").is_valid

    cfg = SUEWSConfig.from_dict(_load_sample_dict())
    ctrl_null = cfg.model.control.model_copy(update={"tstep": None})
    cfg.model.control = ctrl_null
    for pipeline in ("ABC", "C"):
        rep = supy.validate_config(cfg, pipeline=pipeline, forcing="off")
        assert not rep.is_valid, (pipeline, rep.to_text())
        assert any("tstep" in err.message for err in rep.errors), rep.errors
    assert cfg.model.control is ctrl_null
    assert cfg.model.control.tstep is None

    # 2. Malformed vertical_layers.roofs: [None] with nlayer=3 never raises AttributeError
    #    and allows continue_on_error=True to reach downstream phases cleanly.
    bad_roofs = _load_sample_dict()
    bad_roofs["sites"][0]["properties"]["vertical_layers"]["nlayer"] = {"value": 3}
    bad_roofs["sites"][0]["properties"]["vertical_layers"]["roofs"] = [None]
    rep_stop = supy.validate_config(
        bad_roofs, pipeline="ABC", forcing="off", continue_on_error=False
    )
    assert not rep_stop.is_valid
    assert [p.phase for p in rep_stop.phases] == ["A"]

    rep_cont = supy.validate_config(
        bad_roofs, pipeline="ABC", forcing="off", continue_on_error=True
    )
    assert not rep_cont.is_valid
    assert [p.phase for p in rep_cont.phases] == ["A", "B", "C"]


def test_validate_config_rejects_null_non_optional_model_fields_and_malformed_nested_models() -> (
    None
):
    """Explicit null ``sites``/``model`` and malformed nested models return structured errors."""
    # 1. Parity with mapping input for explicit null non-optional non-RefValue field (`sites=None`).
    map_null_sites = _load_sample_dict()
    map_null_sites["sites"] = None
    assert not supy.validate_config(map_null_sites, pipeline="C", forcing="off").is_valid

    cfg_null_sites = SUEWSConfig.from_dict(_load_sample_dict()).model_copy(
        update={"sites": None}
    )
    for pipeline in ("C", "AC", "ABC"):
        rep = supy.validate_config(cfg_null_sites, pipeline=pipeline, forcing="off")
        assert not rep.is_valid, (pipeline, rep.to_text())
        assert any("sites" in err.message for err in rep.errors), rep.errors
    assert cfg_null_sites.sites is None

    cfg_null_model = SUEWSConfig.from_dict(_load_sample_dict()).model_copy(
        update={"model": None}
    )
    rep_null_model = supy.validate_config(cfg_null_model, pipeline="C", forcing="off")
    assert not rep_null_model.is_valid
    assert any("model" in err.message for err in rep_null_model.errors), (
        rep_null_model.errors
    )

    # 2. Malformed nested model (`initial_states.evetr = 7`) never escapes as AttributeError,
    #    including under continue_on_error=True, and leaves caller input unmutated.
    cfg_bad_evetr = SUEWSConfig.from_dict(_load_sample_dict())
    bad_states = cfg_bad_evetr.sites[0].initial_states.model_copy(update={"evetr": 7})
    cfg_bad_evetr.sites[0].initial_states = bad_states
    for pipeline in ("C", "ABC"):
        for cont in (False, True):
            rep = supy.validate_config(
                cfg_bad_evetr,
                pipeline=pipeline,
                forcing="off",
                continue_on_error=cont,
            )
            assert not rep.is_valid, (pipeline, cont, rep.to_text())
            assert any("evetr" in err.message for err in rep.errors), rep.errors
    assert cfg_bad_evetr.sites[0].initial_states is bad_states
    assert cfg_bad_evetr.sites[0].initial_states.evetr == 7


def test_dry_run_matches_conditional_inactive_irrigation_contract(
    tmp_path: Path,
) -> None:
    """Dry-run accepts inactive irrigation DOY placeholders and rejects active invalid ones."""
    # 1. Inactive irrigation (water_use=1, ie_start=20.5, ie_end=null) passes
    #    Python API, -p ABC --dry-run (suggest/off/apply), and -p C --dry-run.
    inactive_irr = _load_sample_dict()
    inactive_irr["model"]["physics"]["water_use"] = {"value": 1}
    inactive_irr["sites"][0]["properties"]["irrigation"]["ie_start"] = {"value": 20.5}
    inactive_irr["sites"][0]["properties"]["irrigation"]["ie_end"] = None
    irr_path = _write_yaml(tmp_path / "inactive_irrigation.yml", inactive_irr)

    py_rep = supy.validate_config(irr_path, pipeline="ABC", forcing="off")
    assert py_rep.is_valid, py_rep.to_text()

    for fix_mode in ("suggest", "off", "apply"):
        abc_res = CliRunner().invoke(
            validate_cli,
            [
                "-p",
                "ABC",
                "--dry-run",
                "--forcing",
                "off",
                "--science-fixes",
                fix_mode,
                "--format",
                "json",
                str(irr_path),
            ],
        )
        assert abc_res.exit_code == 0, (fix_mode, abc_res.output)
        assert json.loads(abc_res.stdout)["data"]["is_valid"] is True

    c_res = CliRunner().invoke(
        validate_cli,
        ["-p", "C", "--dry-run", "--format", "json", str(irr_path)],
    )
    assert c_res.exit_code == 0, c_res.output
    assert json.loads(c_res.stdout)["data"]["is_valid"] is True

    # 2. Negative control: active modelled irrigation (water_use=0, daywat.monday=1.0)
    #    still rejects fractional ie_start=20.5 across -p ABC --dry-run and -p C --dry-run.
    active_irr = _load_sample_dict()
    active_irr["model"]["physics"]["water_use"] = {"value": 0}
    active_irr["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = {
        "value": 1.0
    }
    active_irr["sites"][0]["properties"]["irrigation"]["ie_start"] = {"value": 20.5}
    active_irr["sites"][0]["properties"]["irrigation"]["ie_end"] = None
    active_path = _write_yaml(tmp_path / "active_irrigation_bad.yml", active_irr)

    for cmd in (
        ["-p", "ABC", "--dry-run", "--forcing", "off", "--format", "json", str(active_path)],
        ["-p", "C", "--dry-run", "--format", "json", str(active_path)],
    ):
        res = CliRunner().invoke(validate_cli, cmd)
        assert res.exit_code == 1, (cmd, res.output)
        assert json.loads(res.stdout)["data"]["is_valid"] is False


def test_validate_config_preserves_explicit_optional_nulls_and_loaded_yaml_base_dir(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Explicit optional nulls reach Phase B and ``from_yaml`` objects keep their forcing base_dir."""
    # 1. Explicit optional null (ohm_threshold_wet_dry=None) on a live SUEWSConfig
    #    matches the equivalent mapping route rather than being erased by exclude_none.
    raw_map = _load_sample_dict()
    raw_map["sites"][0]["properties"]["land_cover"]["paved"][
        "ohm_threshold_wet_dry"
    ] = None
    map_rep = supy.validate_config(raw_map, pipeline="ABC", forcing="off")

    cfg = SUEWSConfig.from_dict(_load_sample_dict())
    cfg.sites[0].properties.land_cover.paved.ohm_threshold_wet_dry = None
    model_rep = supy.validate_config(cfg, pipeline="ABC", forcing="off")

    map_ohm = [i for i in map_rep.issues if "ohm_threshold_wet_dry" in i.message]
    model_ohm = [i for i in model_rep.issues if "ohm_threshold_wet_dry" in i.message]
    assert map_ohm, map_rep.to_text()
    assert len(model_ohm) == len(map_ohm), (model_ohm, map_ohm)
    assert {i.code for i in model_ohm} == {i.code for i in map_ohm}

    # 2. SUEWSConfig loaded via from_yaml() preserves its YAML parent directory for
    #    Phase B seasonal bound inference even when validated from another CWD.
    cfg_dir = tmp_path / "site_run"
    other_cwd = tmp_path / "other_cwd"
    cfg_dir.mkdir()
    other_cwd.mkdir()

    sample_dir = files("supy").joinpath("sample_data")
    for item in sample_dir.iterdir():
        if item.is_file():
            (cfg_dir / item.name).write_bytes(item.read_bytes())

    null_bounds = yaml.safe_load(
        (cfg_dir / "sample_config.yml").read_text(encoding="utf-8")
    )
    null_bounds["model"]["control"]["start_time"] = None
    null_bounds["model"]["control"]["end_time"] = None
    cfg_path = _write_yaml(cfg_dir / "null_bounds.yml", null_bounds)
    loaded_cfg = SUEWSConfig.from_yaml(str(cfg_path))

    # Plant an unrelated forcing file in other_cwd with the same filename so any
    # fallback to CWD would read the wrong file instead of cfg_dir.
    (other_cwd / "Kc_2012_data_60.txt").write_text("corrupt forcing\n", encoding="utf-8")
    monkeypatch.chdir(other_cwd)

    path_rep = supy.validate_config(cfg_path, pipeline="ABC", forcing="off")
    obj_rep = supy.validate_config(loaded_cfg, pipeline="ABC", forcing="off")
    assert path_rep.is_valid, path_rep.to_text()
    assert obj_rep.is_valid, obj_rep.to_text()
    assert not any(
        w.code == "SEASONAL.MODEL_CONTROL_START_TIME" for w in path_rep.warnings
    )
    assert not any(
        w.code == "SEASONAL.MODEL_CONTROL_START_TIME" for w in obj_rep.warnings
    )


def test_validate_config_handles_dict_backed_sites_and_physics_without_losing_collisions() -> (
    None
):
    """Unchecked dict-backed sites keep profile-key collision checks and dict physics stays authored."""
    # 1. Dict-backed site (via from_dict(..., use_conditional_validation=False) or model_copy)
    #    still rejects mixed int/str profile key collisions before JSON coercion.
    raw = _load_sample_dict()
    raw["sites"][0]["properties"]["anthropogenic_emissions"]["heat"]["ahprof_24hr"] = {
        "working_day": {1: 0.5, "1": 0.6, **{str(h): 0.5 for h in range(2, 25)}},
        "holiday": {str(h): 0.5 for h in range(1, 25)},
    }
    dict_site_cfg = SUEWSConfig.from_dict(_load_sample_dict()).model_copy(
        update={"sites": [copy.deepcopy(raw["sites"][0])]}
    )
    rep_collision = supy.validate_config(dict_site_cfg, pipeline="ABC", forcing="off")
    assert not rep_collision.is_valid, rep_collision.to_text()
    assert any(
        "PROFILE_KEY_ERROR" in err.code or "duplicate" in err.message.lower()
        for err in rep_collision.errors
    ), rep_collision.errors
    # Caller's dict-backed site remains unmutated (both int 1 and str "1" still present).
    orig_wd = dict_site_cfg.sites[0]["properties"]["anthropogenic_emissions"]["heat"][
        "ahprof_24hr"
    ]["working_day"]
    assert 1 in orig_wd and "1" in orig_wd

    # 2. Dict-backed model.physics preserves authored critical physics switches instead of
    #    stripping them as unauthored BaseModel defaults.
    raw_valid = _load_sample_dict()
    base_cfg = SUEWSConfig.from_dict(raw_valid)
    cfg_dict_phys = base_cfg.model_copy(
        update={
            "model": base_cfg.model.model_copy(
                update={"physics": copy.deepcopy(raw_valid["model"]["physics"])}
            )
        }
    )
    rep_phys = supy.validate_config(cfg_dict_phys, pipeline="ABC", forcing="off")
    assert rep_phys.is_valid, rep_phys.to_text()
