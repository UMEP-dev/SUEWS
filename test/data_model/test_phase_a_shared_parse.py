"""Regression tests for shared YAML parsing and Phase A fast paths (gh#2099).

Covers unchanged inputs, annotation changes, sparse/missing inputs, legacy soil
conversion, duplicate-key handling, and malformed-input error paths across the
public CLI, ``run_phase_a``, ``annotate_missing_parameters``, and
``SUEWSConfig.from_yaml``.
"""

from __future__ import annotations

import copy
from importlib.resources import as_file, files
import json
from pathlib import Path

from click.testing import CliRunner
import pytest
import yaml

from supy._run_rust import config_to_engine_yaml  # ruff: ignore[import-private-name]
from supy.cmd.suews_cli import cli as suews_cli
from supy.cmd.validate_config import _experimental_features_restriction_with_data  # ruff: ignore[import-private-name]
from supy.data_model.core import SUEWSConfig
from supy.data_model.core._yaml_io import safe_dump_yaml, safe_load_yaml  # ruff: ignore[import-private-name]
from supy.data_model.validation.pipeline.orchestrator import run_phase_a
from supy.data_model.validation.pipeline.phase_a import (
    annotate_missing_parameters,
    handle_renamed_parameters,
    handle_renamed_parameters_in_parsed_yaml,
)

pytestmark = [
    pytest.mark.unit,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-API", "C-ERROR"),
]

_SAMPLE_RESOURCE = files("supy") / "sample_data" / "sample_config.yml"
_SOIL_FIXTURES = Path(__file__).parents[1] / "fixtures" / "soil_observation"
_LOADER_KEYS = frozenset({"_yaml_path", "_auto_generate_annotated", "_yaml_raw"})


@pytest.fixture(scope="module")
def sample_yaml_text() -> str:
    """Read immutable packaged sample YAML text once per module."""
    return _SAMPLE_RESOURCE.read_text(encoding="utf-8")


@pytest.fixture(scope="module")
def sample_yaml_dict(sample_yaml_text: str) -> dict:
    """Parse packaged sample YAML using pure-Python SafeLoader as reference."""
    return yaml.load(sample_yaml_text, Loader=yaml.SafeLoader)


def _strip_loader_keys(resolved: dict) -> dict:
    return {k: v for k, v in resolved.items() if k not in _LOADER_KEYS}


def _canonical_engine_dict(config: SUEWSConfig) -> dict:
    return _strip_loader_keys(
        yaml.load(config_to_engine_yaml(config), Loader=yaml.SafeLoader)
    )


def _invoke_phase_a_cli(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    source: Path,
    *,
    mode: str = "public",
    forcing: str = "off",
) -> tuple[int, dict]:
    runner = CliRunner()
    monkeypatch.chdir(tmp_path)
    result = runner.invoke(
        suews_cli,
        [
            "validate",
            "--pipeline",
            "A",
            "--mode",
            mode,
            "--forcing",
            forcing,
            "--format",
            "json",
            str(source),
        ],
    )
    envelope = json.loads(result.stdout)
    return result.exit_code, envelope


# ---------------------------------------------------------------------------
# 1. Shared YAML helper parity with pure-Python SafeLoader / SafeDumper
# ---------------------------------------------------------------------------


def test_safe_yaml_helpers_match_pure_python_reference(sample_yaml_text: str) -> None:
    """Shared safe YAML helpers match SafeLoader/SafeDumper semantics."""
    ref_loaded = yaml.load(sample_yaml_text, Loader=yaml.SafeLoader)
    fast_loaded = safe_load_yaml(sample_yaml_text)
    assert fast_loaded == ref_loaded

    ref_dumped = yaml.dump(
        ref_loaded,
        Dumper=yaml.SafeDumper,
        default_flow_style=False,
        sort_keys=False,
        allow_unicode=True,
    )
    fast_dumped = safe_dump_yaml(
        fast_loaded,
        default_flow_style=False,
        sort_keys=False,
        allow_unicode=True,
    )
    assert fast_dumped == ref_dumped


@pytest.mark.parametrize(
    "snippet",
    [
        "a: 1\na: 2\n",
        "m:\n  k: 1\n  k: 2\n",
        "a: yes\nb: no\nc: on\nd: off\ne: true\nf: false\n",
        "a: null\nb: ~\nc:\n",
        "d: 2012-01-01\ns: '2012-01-01'\n",
        "a: 1.0\nb: -999.0\nc: .inf\nd: -.inf\n",
    ],
    ids=["dup_root", "dup_nested", "yaml11_bools", "nulls", "dates", "floats"],
)
def test_safe_load_yaml_matches_safe_loader_on_edge_cases(snippet: str) -> None:
    """Scalar resolution and duplicate-key resolution match SafeLoader."""
    assert safe_load_yaml(snippet) == yaml.load(snippet, Loader=yaml.SafeLoader)


# ---------------------------------------------------------------------------
# 2. Unchanged files and non-mutation of shared parsed payload
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("customise", [False, True], ids=["unchanged", "custom"])
def test_cli_phase_a_shared_parse_matches_direct_phase_a(
    sample_yaml_dict: dict,
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    customise: bool,
) -> None:
    """Public CLI shared parse matches an independent direct Phase A run."""
    payload = copy.deepcopy(sample_yaml_dict)
    if customise:
        payload["sites"][0]["gridiv"] = 19
        cover = payload["sites"][0]["properties"]["land_cover"]
        cover["bldgs"]["bldgh"] = {"value": 21.0}
        cover["paved"]["emis"] = {"value": 0.89}

    cli_dir = tmp_path / "cli"
    direct_dir = tmp_path / "direct"
    cli_dir.mkdir()
    direct_dir.mkdir()

    yaml_text = safe_dump_yaml(payload, default_flow_style=False, sort_keys=False)
    cli_source = (cli_dir / "input.yml").resolve()
    cli_source.write_text(yaml_text, encoding="utf-8")
    direct_source = (direct_dir / "input.yml").resolve()
    direct_source.write_text(yaml_text, encoding="utf-8")

    exit_code, envelope = _invoke_phase_a_cli(cli_dir, monkeypatch, cli_source)
    assert exit_code == 0, envelope
    assert envelope["status"] in {"success", "warning"}
    cli_updated = Path(envelope["data"]["updated_yaml"]).resolve()
    cli_report = Path(envelope["data"]["report_file"]).resolve()
    assert cli_updated.is_file()
    assert cli_report.is_file()

    direct_updated = direct_dir / "updatedA_input.yml"
    direct_report = direct_dir / "report_input.txt"
    with as_file(_SAMPLE_RESOURCE) as standard_path:
        direct_phase_report = annotate_missing_parameters(
            user_file=str(direct_source),
            standard_file=str(standard_path),
            uptodate_file=str(direct_updated),
            report_file=str(direct_report),
            mode="public",
            phase="A",
            nlayer=3,
            forcing="off",
        )

    assert not direct_phase_report.has_errors
    assert cli_updated.read_text(encoding="utf-8") == direct_updated.read_text(
        encoding="utf-8"
    )
    assert cli_report.read_text(encoding="utf-8") == direct_report.read_text(
        encoding="utf-8"
    )
    assert _canonical_engine_dict(
        SUEWSConfig.from_yaml(str(cli_updated))
    ) == _canonical_engine_dict(SUEWSConfig.from_yaml(str(cli_source)))


def test_experimental_restriction_helper_does_not_mutate_parsed_dict(
    sample_yaml_text: str, tmp_path: Path
) -> None:
    """Shared parsed YAML returned by the public-mode gate stays unmutated."""
    source = tmp_path / "sample.yml"
    source.write_text(sample_yaml_text, encoding="utf-8")

    ok, restrictions, read_error, parsed = _experimental_features_restriction_with_data(
        str(source), "public"
    )
    assert ok is True
    assert restrictions == []
    assert read_error is None
    assert parsed == yaml.load(sample_yaml_text, Loader=yaml.SafeLoader)


# ---------------------------------------------------------------------------
# 3. Annotation changes: missing params, renames, folds, nlayer mismatch
# ---------------------------------------------------------------------------


def test_cli_phase_a_annotates_missing_optional_and_critical_params(
    sample_yaml_dict: dict, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Missing optional and critical parameters are detected and annotated."""
    # Optional nullable parameter -> warning, exit 0, annotated with null
    opt_dir = tmp_path / "opt"
    opt_dir.mkdir()
    opt_payload = copy.deepcopy(sample_yaml_dict)
    del opt_payload["model"]["control"]["start_time"]
    opt_source = (opt_dir / "opt.yml").resolve()
    opt_source.write_text(
        safe_dump_yaml(opt_payload, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )

    opt_code, opt_env = _invoke_phase_a_cli(opt_dir, monkeypatch, opt_source)
    assert opt_code == 0, opt_env
    opt_issues = opt_env["data"]["validation_report"]["phases"][0]["issues"]
    opt_codes = {issue["code"] for issue in opt_issues}
    assert "A.MISSING_PARAM" in opt_codes
    assert any(
        issue["code"] == "A.MISSING_PARAM"
        and issue.get("yaml_path") == "model.control.start_time"
        for issue in opt_issues
    )
    opt_updated = yaml.load(
        Path(opt_env["data"]["updated_yaml"]).read_text(encoding="utf-8"),
        Loader=yaml.SafeLoader,
    )
    # Omitted optional fields are reported as A.MISSING_PARAM but not padded with null.
    assert "start_time" not in opt_updated["model"]["control"]

    # Critical missing physics parameter -> error, exit 1, annotated with null
    crit_dir = tmp_path / "crit"
    crit_dir.mkdir()
    crit_payload = copy.deepcopy(sample_yaml_dict)
    del crit_payload["model"]["physics"]["net_radiation"]
    crit_source = (crit_dir / "crit.yml").resolve()
    crit_source.write_text(
        safe_dump_yaml(crit_payload, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )

    crit_code, crit_env = _invoke_phase_a_cli(crit_dir, monkeypatch, crit_source)
    assert crit_code == 1, crit_env
    crit_codes = {
        issue["code"]
        for issue in crit_env["data"]["validation_report"]["phases"][0]["issues"]
    }
    assert "A.MISSING_PARAM.CRITICAL" in crit_codes
    crit_updated = yaml.load(
        Path(crit_env["data"]["updated_yaml"]).read_text(encoding="utf-8"),
        Loader=yaml.SafeLoader,
    )
    assert "net_radiation" in crit_updated["model"]["physics"]


def test_cli_phase_a_handles_text_renames_and_compatibility_folds(
    sample_yaml_dict: dict, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Legacy renamed keys and legacy control blocks trigger re-parse and fold."""
    payload = copy.deepcopy(sample_yaml_dict)
    physics = payload["model"]["physics"]
    physics["diagmethod"] = physics.pop("roughness_sublayer")
    paved = payload["sites"][0]["properties"]["land_cover"]["paved"]
    paved["soildepth"] = paved.pop("soil_depth")
    control = payload["model"]["control"]
    legacy_forcing = control.pop("forcing")["file"]
    control["forcing_file"] = legacy_forcing

    source = (tmp_path / "legacy_renames.yml").resolve()
    source.write_text(
        safe_dump_yaml(payload, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )

    exit_code, envelope = _invoke_phase_a_cli(tmp_path, monkeypatch, source)
    assert exit_code == 0, envelope
    updated = yaml.load(
        Path(envelope["data"]["updated_yaml"]).read_text(encoding="utf-8"),
        Loader=yaml.SafeLoader,
    )
    assert "roughness_sublayer" in updated["model"]["physics"]
    assert "diagmethod" not in updated["model"]["physics"]
    assert "soil_depth" in updated["sites"][0]["properties"]["land_cover"]["paved"]
    assert "soildepth" not in updated["sites"][0]["properties"]["land_cover"]["paved"]
    assert updated["model"]["control"]["forcing"]["file"] == legacy_forcing
    assert "forcing_file" not in updated["model"]["control"]


def test_cli_phase_a_detects_nlayer_mismatch_from_shared_parse(
    sample_yaml_dict: dict, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Shared-parse nlayer detection pads short vertical-layer arrays."""
    payload = copy.deepcopy(sample_yaml_dict)
    vl = payload["sites"][0]["properties"]["vertical_layers"]
    vl["nlayer"] = {"value": 4}
    assert len(vl["veg_frac"]["value"]) == 3

    source = (tmp_path / "nlayer_mismatch.yml").resolve()
    source.write_text(
        safe_dump_yaml(payload, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )

    exit_code, envelope = _invoke_phase_a_cli(tmp_path, monkeypatch, source)
    assert exit_code == 1, envelope
    codes = {
        issue["code"]
        for issue in envelope["data"]["validation_report"]["phases"][0]["issues"]
    }
    assert "A.DIMENSION_MISMATCH" in codes
    updated = yaml.load(
        Path(envelope["data"]["updated_yaml"]).read_text(encoding="utf-8"),
        Loader=yaml.SafeLoader,
    )
    updated_vl = updated["sites"][0]["properties"]["vertical_layers"]
    assert len(updated_vl["veg_frac"]["value"]) == 4
    assert updated_vl["veg_frac"]["value"][-1] is None


def test_handle_renamed_parameters_edge_cases() -> None:
    """Fast line-level and parsed rename helpers preserve edge-case behaviour."""
    yaml_text = (
        "# cp: comment must stay untouched\n"
        "model:\n"
        "  physics:\n"
        "    diagmethod: 2  # inline: colon\n"
        "    cp: 1005   \n"
        "    smdmethod: 1\n"
        "items:\n"
        "  - soildepth: 150\n"
    )
    updated, replacements = handle_renamed_parameters(yaml_text, skip=("smdmethod",))
    assert replacements == [("diagmethod", "roughness_sublayer"), ("cp", "rho_cp")]
    assert "# cp: comment must stay untouched" in updated
    assert 'roughness_sublayer: 2  # inline: colon  #RENAMED IN STANDARD' in updated
    assert "smdmethod: 1" in updated
    assert "- soildepth: 150" in updated

    parsed = safe_load_yaml(updated)
    parsed_replacements = handle_renamed_parameters_in_parsed_yaml(
        parsed, skip=("smdmethod",)
    )
    assert parsed_replacements == [("soildepth", "soil_depth")]
    assert parsed["items"][0] == {"soil_depth": 150}
    assert parsed["model"]["physics"]["smdmethod"] == 1


# ---------------------------------------------------------------------------
# 4. Sparse / empty / missing inputs
# ---------------------------------------------------------------------------


def test_cli_phase_a_sparse_omitted_defaults_roundtrip(
    sample_yaml_dict: dict, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Sparse input omitting defaulted blocks survives CLI Phase A cleanly."""
    sparse = copy.deepcopy(sample_yaml_dict)
    paved = sparse["sites"][0]["properties"]["land_cover"]["paved"]
    del paved["emis"]
    del paved["alb"]
    paved.pop("soil_field_capacity", None)
    del sparse["sites"][0]["properties"]["lumps"]

    source = (tmp_path / "sparse.yml").resolve()
    source.write_text(
        safe_dump_yaml(sparse, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )

    exit_code, envelope = _invoke_phase_a_cli(tmp_path, monkeypatch, source)
    assert exit_code == 0, envelope
    published = Path(envelope["data"]["updated_yaml"]).resolve()
    assert _canonical_engine_dict(
        SUEWSConfig.from_yaml(str(published))
    ) == _canonical_engine_dict(SUEWSConfig.from_dict(copy.deepcopy(sparse)))


def test_cli_phase_a_handles_empty_yaml_without_sentinel_confusion(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """An empty YAML parses to None and matches direct Phase A byte-for-byte."""
    cli_dir = tmp_path / "cli"
    direct_dir = tmp_path / "direct"
    cli_dir.mkdir()
    direct_dir.mkdir()

    empty_cli = (cli_dir / "empty.yml").resolve()
    empty_cli.write_text("# comment only\n", encoding="utf-8")
    empty_direct = (direct_dir / "empty.yml").resolve()
    empty_direct.write_text("# comment only\n", encoding="utf-8")

    exit_code, envelope = _invoke_phase_a_cli(cli_dir, monkeypatch, empty_cli)
    assert exit_code == 0, envelope
    assert envelope["status"] == "warning"
    issues = envelope["data"]["validation_report"]["phases"][0]["issues"]
    missing_paths = {
        issue["yaml_path"] for issue in issues if issue["code"] == "A.MISSING_PARAM"
    }
    assert {"model", "sites"}.issubset(missing_paths)

    direct_updated = direct_dir / "updatedA_empty.yml"
    direct_report = direct_dir / "report_empty.txt"
    with as_file(_SAMPLE_RESOURCE) as standard_path:
        direct_phase_report = annotate_missing_parameters(
            user_file=str(empty_direct),
            standard_file=str(standard_path),
            uptodate_file=str(direct_updated),
            report_file=str(direct_report),
            mode="public",
            phase="A",
            detect_nlayer=True,
            forcing="off",
        )

    assert not direct_phase_report.has_errors
    assert Path(envelope["data"]["updated_yaml"]).read_text(
        encoding="utf-8"
    ) == direct_updated.read_text(encoding="utf-8")
    assert Path(envelope["data"]["report_file"]).read_text(
        encoding="utf-8"
    ) == direct_report.read_text(encoding="utf-8")


def test_direct_phase_a_missing_file_returns_file_not_found(tmp_path: Path) -> None:
    """Missing input file produces A.PIPELINE.FILE_NOT_FOUND without crashing."""
    missing = tmp_path / "does_not_exist.yml"
    with as_file(_SAMPLE_RESOURCE) as standard_path:
        report = annotate_missing_parameters(
            user_file=str(missing),
            standard_file=str(standard_path),
            uptodate_file=str(tmp_path / "out.yml"),
            report_file=str(tmp_path / "report.txt"),
            forcing="off",
        )
    assert report.has_errors
    assert report.issues[0].code == "A.PIPELINE.FILE_NOT_FOUND"


# ---------------------------------------------------------------------------
# 5. Legacy soil conversion and mixed soil contracts
# ---------------------------------------------------------------------------


def test_cli_phase_a_preserves_legacy_observed_soil_and_reports_migration(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Legacy observed soil YAML is reported for migration and never renamed."""
    legacy_text = (_SOIL_FIXTURES / "legacy_method1.yml").read_text(encoding="utf-8")
    source = (tmp_path / "legacy_soil.yml").resolve()
    source.write_text(legacy_text, encoding="utf-8")

    exit_code, envelope = _invoke_phase_a_cli(tmp_path, monkeypatch, source)
    assert exit_code == 1, envelope
    codes = {
        issue["code"]
        for issue in envelope["data"]["validation_report"]["phases"][0]["issues"]
    }
    assert "A.LEGACY_SOIL_MIGRATION" in codes

    report_text = Path(envelope["data"]["report_file"]).read_text(encoding="utf-8")
    assert "Legacy observed soil moisture" in report_text
    assert "soil_moisture_deficit changed to soil_moisture" not in report_text

    updated = yaml.load(
        Path(envelope["data"]["updated_yaml"]).read_text(encoding="utf-8"),
        Loader=yaml.SafeLoader,
    )
    assert "soil_moisture_deficit" in updated["model"]["physics"]
    assert "soil_moisture" not in updated["model"]["physics"]
    assert "depth" in updated["sites"][0]["properties"]["soil_observation"]


def test_cli_phase_a_reports_mixed_legacy_and_canonical_soil_contracts(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Mixed legacy and canonical soil inputs surface the conflict error."""
    mixed = yaml.load(
        (_SOIL_FIXTURES / "legacy_method1.yml").read_text(encoding="utf-8"),
        Loader=yaml.SafeLoader,
    )
    mixed["sites"][0]["properties"]["soil_observation"]["layer_top"] = 0
    source = (tmp_path / "mixed_soil.yml").resolve()
    source.write_text(
        safe_dump_yaml(mixed, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )

    exit_code, envelope = _invoke_phase_a_cli(tmp_path, monkeypatch, source)
    assert exit_code == 1, envelope
    report_text = Path(envelope["data"]["report_file"]).read_text(encoding="utf-8")
    assert "Both legacy and canonical soil-moisture inputs were supplied" in report_text
    assert "changed to soil_moisture" not in report_text


# ---------------------------------------------------------------------------
# 6. Duplicate-key handling (raw duplicates, post-rename duplicates, aliases)
# ---------------------------------------------------------------------------


def test_duplicate_keys_without_renames_match_safe_loader(
    sample_yaml_text: str, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Raw duplicate YAML keys resolve last-key-wins across shared parse & load."""
    dup_text = sample_yaml_text.replace(
        "    tstep: 300\n",
        "    tstep: 600\n    tstep: 300\n",
        1,
    )
    assert dup_text != sample_yaml_text
    source = (tmp_path / "dup_raw.yml").resolve()
    source.write_text(dup_text, encoding="utf-8")

    ref_dict = yaml.load(dup_text, Loader=yaml.SafeLoader)
    assert ref_dict["model"]["control"]["tstep"] == 300

    exit_code, envelope = _invoke_phase_a_cli(tmp_path, monkeypatch, source)
    assert exit_code == 0, envelope
    published = Path(envelope["data"]["updated_yaml"]).resolve()

    cfg_yaml = SUEWSConfig.from_yaml(str(source))
    cfg_pub = SUEWSConfig.from_yaml(str(published))
    assert cfg_yaml.model.control.tstep == 300
    assert cfg_pub.model.control.tstep == 300
    assert _canonical_engine_dict(cfg_pub) == _canonical_engine_dict(cfg_yaml)


def test_duplicate_key_created_by_text_rename_reparses_correctly(
    sample_yaml_text: str, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Renaming a legacy key onto an existing key triggers re-parse identically."""
    dup_rename_text = sample_yaml_text.replace(
        "    roughness_sublayer: variable\n",
        "    diagmethod: 0\n    roughness_sublayer: variable\n",
        1,
    )
    assert dup_rename_text != sample_yaml_text
    source = (tmp_path / "dup_rename.yml").resolve()
    source.write_text(dup_rename_text, encoding="utf-8")

    exit_code, envelope = _invoke_phase_a_cli(tmp_path, monkeypatch, source)
    assert exit_code == 0, envelope
    published = Path(envelope["data"]["updated_yaml"]).resolve()
    cfg_pub = SUEWSConfig.from_yaml(str(published))
    # Last key wins after diagmethod -> roughness_sublayer: variable (code 2)
    assert int(cfg_pub.model.physics.roughness_sublayer.value.value) == 2


def test_duplicate_legacy_stebbs_aliases_rejected_in_public_mode(
    sample_yaml_dict: dict, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Duplicate legacy STEBBS master aliases are rejected by the public gate."""
    payload = copy.deepcopy(sample_yaml_dict)
    physics = payload["model"]["physics"]
    physics.pop("stebbs", None)
    physics["stebbsmethod"] = {"value": 0}
    physics["stebbs_method"] = {"value": 1}

    source = (tmp_path / "dup_stebbs.yml").resolve()
    source.write_text(
        safe_dump_yaml(payload, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )

    exit_code, envelope = _invoke_phase_a_cli(tmp_path, monkeypatch, source)
    assert exit_code == 1, envelope
    assert envelope["status"] == "error"
    assert envelope["errors"][0]["code"] == "experimental_features_restricted"


# ---------------------------------------------------------------------------
# 7. Malformed-input error paths (public CLI, dev CLI, direct Phase A, loader)
# ---------------------------------------------------------------------------


def test_malformed_yaml_error_paths_across_cli_phase_a_and_loader(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Malformed YAML preserves exact error codes across every entry path."""
    bad = (tmp_path / "malformed.yml").resolve()
    bad.write_text("invalid: yaml: content: [unclosed\n", encoding="utf-8")

    # 1. Public CLI fails at the pre-Phase-A read gate
    pub_code, pub_env = _invoke_phase_a_cli(tmp_path, monkeypatch, bad, mode="public")
    assert pub_code == 1, pub_env
    assert pub_env["status"] == "error"
    assert pub_env["errors"][0]["code"] == "yaml_read_failed"

    # 2. Dev CLI bypasses the public gate and fails inside Phase A
    dev_code, dev_env = _invoke_phase_a_cli(tmp_path, monkeypatch, bad, mode="dev")
    assert dev_code == 1, dev_env
    assert dev_env["status"] == "error"
    dev_codes = {
        issue["code"]
        for issue in dev_env["data"]["validation_report"]["phases"][0]["issues"]
    }
    assert "A.PIPELINE.YAML_PARSE_ERROR" in dev_codes

    # 3. Direct run_phase_a and annotate_missing_parameters report YAML_PARSE_ERROR
    with as_file(_SAMPLE_RESOURCE) as standard_path:
        direct_run = run_phase_a(
            user_yaml_file=str(bad),
            standard_yaml_file=str(standard_path),
            uptodate_file=str(tmp_path / "direct_run_out.yml"),
            report_file=str(tmp_path / "direct_run_report.txt"),
            mode="dev",
            phase="A",
            silent=True,
            forcing="off",
        )
        direct_ann = annotate_missing_parameters(
            user_file=str(bad),
            standard_file=str(standard_path),
            uptodate_file=str(tmp_path / "direct_ann_out.yml"),
            report_file=str(tmp_path / "direct_ann_report.txt"),
            mode="dev",
            phase="A",
            forcing="off",
        )
    assert direct_run.has_errors
    assert any(i.code == "A.PIPELINE.YAML_PARSE_ERROR" for i in direct_run.issues)
    assert direct_ann.has_errors
    assert direct_ann.issues[0].code == "A.PIPELINE.YAML_PARSE_ERROR"

    # 4. SUEWSConfig.from_yaml raises yaml.YAMLError
    with pytest.raises(yaml.YAMLError):
        SUEWSConfig.from_yaml(str(bad))


@pytest.mark.parametrize("mode", ["public", "dev"])
def test_forcing_validation_preserves_absent_control(tmp_path, monkeypatch, mode):
    source = tmp_path / "no-control.yml"
    source.write_text("model: {}\n", encoding="utf-8")
    status, report = _invoke_phase_a_cli(
        tmp_path, monkeypatch, source, mode=mode, forcing="on"
    )
    assert status == 1
    assert any(
        "Forcing file path not found" in error["message"] for error in report["errors"]
    )


@pytest.mark.parametrize("content", ['a: "\\q"\n', "model:\t{}\n"])
def test_public_yaml_reader_preserves_error_location(tmp_path, content):
    source = tmp_path / "malformed.yml"
    source.write_text(content, encoding="utf-8")
    with source.open(encoding="utf-8") as stream, pytest.raises(yaml.YAMLError) as old:
        yaml.safe_load(stream)
    with pytest.raises(type(old.value)) as current:
        SUEWSConfig.from_yaml(source)
    assert str(current.value) == str(old.value)


def test_standard_template_cache_invalidates_on_same_length_preserved_mtime(tmp_path):
    """Replacing standard_file with same-length content and identical mtime misses cache."""
    import os

    from supy.data_model.validation.pipeline.phase_a import (
        _load_normalised_standard_data,
    )

    std_file = tmp_path / "standard.yml"
    content_a = "name: alpha\nmodel:\n  control:\n    tstep: 300\n"
    content_b = "name: bravo\nmodel:\n  control:\n    tstep: 600\n"
    content_bad = "name: [bad1\nmodel:\n  control:\n    tstep: 600\n"
    assert len(content_a.encode("utf-8")) == len(content_b.encode("utf-8"))
    assert len(content_a.encode("utf-8")) == len(content_bad.encode("utf-8"))

    std_file.write_text(content_a, encoding="utf-8")
    st_orig = std_file.stat()
    parsed_a = _load_normalised_standard_data(str(std_file))
    assert parsed_a["name"] == "alpha"
    assert parsed_a["model"]["control"]["tstep"] == 300

    # Same-length valid replacement with preserved (atime_ns, mtime_ns)
    std_file.write_text(content_b, encoding="utf-8")
    os.utime(std_file, ns=(st_orig.st_atime_ns, st_orig.st_mtime_ns))
    parsed_b = _load_normalised_standard_data(str(std_file))
    assert parsed_b["name"] == "bravo"
    assert parsed_b["model"]["control"]["tstep"] == 600

    # Same-length malformed replacement with preserved (atime_ns, mtime_ns)
    std_file.write_text(content_bad, encoding="utf-8")
    os.utime(std_file, ns=(st_orig.st_atime_ns, st_orig.st_mtime_ns))
    with pytest.raises(yaml.YAMLError) as exc_info:
        _load_normalised_standard_data(str(std_file))
    assert getattr(exc_info.value, "problem_mark", None) is not None
    assert exc_info.value.problem_mark.name == str(std_file)
