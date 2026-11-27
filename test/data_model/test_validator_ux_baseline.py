"""Contract-level UX baseline tests for the SUEWS validator across all pipeline modes.

Locks the user-facing CLI and output-artefact contracts across all seven
``--pipeline`` combinations (``A``, ``B``, ``C``, ``AB``, ``AC``, ``BC``,
``ABC``), scientific fix policies (``suggest``, ``apply``, ``off``), output
formats (``table``, ``json``), bare vs ``RefValue`` style preservation, and
failure-stopping banners so structural redesign waves preserve external UX.
"""

from __future__ import annotations

from importlib.resources import files
import json
import os
from pathlib import Path
import subprocess
from typing import Any

from click.testing import CliRunner
from conftest import resolve_suews_cli_command
import pytest
import yaml

from supy.cmd.suews_cli import cli as suews_cli
from supy.data_model.validation.pipeline import phase_b

pytestmark = [
    pytest.mark.unit,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-ERROR"),
]


def _copy_sample_case(dst: Path) -> Path:
    """Copy packaged sample YAML and sibling forcing data into *dst*."""
    sample_dir = files("supy").joinpath("sample_data")
    for item in sample_dir.iterdir():
        if item.is_file():
            (dst / item.name).write_bytes(item.read_bytes())
    return dst / "sample_config.yml"


def _load_yaml(path: Path) -> dict[str, Any]:
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def _write_yaml(path: Path, payload: dict[str, Any]) -> None:
    path.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")


@pytest.mark.parametrize("pipeline", ["A", "B", "C", "AB", "AC", "BC", "ABC"])
def test_all_seven_pipelines_succeed_and_emit_canonical_artefacts(
    tmp_path: Path, pipeline: str
) -> None:
    """Every pipeline combination passes on the sample config and emits canonical files."""
    config_path = _copy_sample_case(tmp_path)
    runner = CliRunner()

    result = runner.invoke(
        suews_cli,
        [
            "validate",
            "-p",
            pipeline,
            "--forcing",
            "off",
            "--format",
            "json",
            str(config_path),
        ],
    )

    assert result.exit_code == 0, result.output
    envelope = json.loads(result.output)
    assert envelope["errors"] == []
    assert envelope["data"]["phases_run"] == list(pipeline)

    report_txt = tmp_path / "report_sample_config.txt"
    report_json = tmp_path / "report_sample_config.json"
    assert report_txt.is_file()
    assert report_json.is_file()

    text = report_txt.read_text(encoding="utf-8")
    assert "Validation stopped at:" not in text

    sidecar = json.loads(report_json.read_text(encoding="utf-8"))
    assert [phase["phase"] for phase in sidecar["phases"]] == list(pipeline)
    assert all(phase["status"] != "FAILED" for phase in sidecar["phases"])
    assert envelope["data"]["validation_report"] == sidecar

    updated_candidates = {
        "A": tmp_path / "updatedA_sample_config.yml",
        "B": tmp_path / "updatedB_sample_config.yml",
    }
    expected_yaml = updated_candidates.get(
        pipeline, tmp_path / "updated_sample_config.yml"
    )
    assert expected_yaml.is_file()


@pytest.mark.parametrize("science_fixes", ["suggest", "apply", "off"])
def test_abc_science_fixes_policy_preserves_bare_and_ref_metadata(
    tmp_path: Path, science_fixes: str
) -> None:
    """ABC honours suggest/apply/off while preserving bare values and RefValue ref metadata."""
    config_path = _copy_sample_case(tmp_path)
    payload = _load_yaml(config_path)
    land_cover = payload["sites"][0]["properties"]["land_cover"]
    # Keep paved as RefValue with custom ref metadata, bldgs as bare scalar.
    ref_meta = {"desc": "GIS survey 2026", "ID": "LC-SURVEY"}
    paved_val = land_cover["paved"]["sfr"]["value"] - 0.00005
    bldgs_val = land_cover["bldgs"]["sfr"]["value"]
    land_cover["paved"]["sfr"] = {"value": paved_val, "ref": dict(ref_meta)}
    land_cover["bldgs"]["sfr"] = bldgs_val
    original_sum = sum(
        (s["sfr"]["value"] if isinstance(s["sfr"], dict) else s["sfr"])
        for s in land_cover.values()
        if "sfr" in s
    )
    _write_yaml(config_path, payload)

    runner = CliRunner()
    result = runner.invoke(
        suews_cli,
        [
            "validate",
            "-p",
            "ABC",
            "--forcing",
            "off",
            "--science-fixes",
            science_fixes,
            "--format",
            "json",
            str(config_path),
        ],
    )

    assert result.exit_code == 0, result.output
    updated = _load_yaml(tmp_path / "updated_sample_config.yml")
    updated_lc = updated["sites"][0]["properties"]["land_cover"]
    assert isinstance(updated_lc["bldgs"]["sfr"], (int, float))
    assert updated_lc["paved"]["sfr"]["ref"] == ref_meta

    updated_sum = sum(
        (s["sfr"]["value"] if isinstance(s["sfr"], dict) else s["sfr"])
        for s in updated_lc.values()
        if "sfr" in s
    )
    expected_sum = 1.0 if science_fixes == "apply" else original_sum
    assert updated_sum == pytest.approx(expected_sum, abs=1e-10)

    report_text = (tmp_path / "report_sample_config.txt").read_text(encoding="utf-8")
    sidecar = json.loads(
        (tmp_path / "report_sample_config.json").read_text(encoding="utf-8")
    )
    b_issues = sidecar["phases"][1]["issues"]
    if science_fixes == "suggest":
        assert "## SUGGESTED UPDATES" in report_text
        assert any(issue["severity"] == "SUGGESTION" for issue in b_issues)
    elif science_fixes == "apply":
        assert "## APPLIED UPDATES" in report_text
        assert any(issue["severity"] == "APPLIED_FIX" for issue in b_issues)
    else:
        assert "## SUGGESTED UPDATES" not in report_text
        assert not any(
            issue["severity"] in {"SUGGESTION", "APPLIED_FIX"} for issue in b_issues
        )


@pytest.mark.parametrize(
    ("failed_phase", "expected_stage"),
    [
        ("A", "Completeness Check"),
        ("B", "Scientific Validation"),
        ("C", "Model Compatibility"),
    ],
)
def test_abc_failure_stopping_phase_and_exit_code(
    tmp_path: Path, failed_phase: str, expected_stage: str
) -> None:
    """ABC stops at the first failing phase, sets exit code 1, and records stopping banner."""
    config_path = _copy_sample_case(tmp_path)
    payload = _load_yaml(config_path)
    if failed_phase == "A":
        payload["model"]["physics"].pop("net_radiation")
    elif failed_phase == "B":
        payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] = -0.2
    else:
        payload["sites"][0]["properties"]["unexpected_phase_c_key"] = 123
    _write_yaml(config_path, payload)

    runner = CliRunner()
    result = runner.invoke(
        suews_cli,
        ["validate", "-p", "ABC", "--forcing", "off", "--format", "json", str(config_path)],
    )

    assert result.exit_code == 1, result.output
    envelope = json.loads(result.output)
    assert envelope["status"] == "error"
    assert len(envelope["errors"]) >= 1

    report_text = (tmp_path / "report_sample_config.txt").read_text(encoding="utf-8")
    assert f"Validation stopped at: {expected_stage}" in report_text
    assert "## ACTION NEEDED" in report_text

    sidecar = json.loads(
        (tmp_path / "report_sample_config.json").read_text(encoding="utf-8")
    )
    expected_phases = {"A": ["A"], "B": ["A", "B"], "C": ["A", "B", "C"]}[failed_phase]
    assert [phase["phase"] for phase in sidecar["phases"]] == expected_phases
    assert sidecar["phases"][-1]["status"] == "FAILED"


def test_continue_on_error_runs_downstream_phases_without_applying_fixes(
    tmp_path: Path,
) -> None:
    """--continue-on-error suppresses mutating Phase B fixes after an A-only failure."""
    runner = CliRunner()

    # Positive control: with forcing checks off, Phase A succeeds and Phase B
    # applies an automatic surface-fraction normalisation fix.
    pos_dir = tmp_path / "positive_control"
    pos_dir.mkdir()
    pos_cfg = _copy_sample_case(pos_dir)
    pos_payload = _load_yaml(pos_cfg)
    paved_sfr = pos_payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]
    orig_paved = paved_sfr["value"] if isinstance(paved_sfr, dict) else paved_sfr
    perturbed_paved = round(float(orig_paved) + 5e-5, 8)
    if isinstance(paved_sfr, dict):
        paved_sfr["value"] = perturbed_paved
    else:
        pos_payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] = (
            perturbed_paved
        )
    _write_yaml(pos_cfg, pos_payload)

    pos_result = runner.invoke(
        suews_cli,
        [
            "validate",
            "-p",
            "AB",
            "--forcing",
            "off",
            "--science-fixes",
            "apply",
            "--format",
            "json",
            str(pos_cfg),
        ],
    )
    assert pos_result.exit_code == 0, pos_result.output
    pos_env = json.loads(pos_result.output)
    pos_b_issues = pos_env["data"]["validation_report"]["phases"][1]["issues"]
    assert any(issue["severity"] == "APPLIED_FIX" for issue in pos_b_issues)

    # A-only failure (missing forcing file) with valid B input and the same
    # applicable fraction fix: --continue-on-error must run B read-only, emit
    # zero APPLIED_FIX issues, and leave the published paved fraction unchanged.
    cont_dir = tmp_path / "continue_after_a_error"
    cont_dir.mkdir()
    cont_cfg = _copy_sample_case(cont_dir)
    cont_payload = _load_yaml(cont_cfg)
    cont_payload["model"]["control"]["forcing"]["file"] = {
        "value": str(cont_dir / "nonexistent_forcing.txt")
    }
    cont_paved = cont_payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]
    if isinstance(cont_paved, dict):
        cont_paved["value"] = perturbed_paved
    else:
        cont_payload["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] = (
            perturbed_paved
        )
    _write_yaml(cont_cfg, cont_payload)

    cont_result = runner.invoke(
        suews_cli,
        [
            "validate",
            "-p",
            "AB",
            "--forcing",
            "on",
            "--science-fixes",
            "apply",
            "--continue-on-error",
            "--format",
            "json",
            str(cont_cfg),
        ],
    )
    assert cont_result.exit_code == 1, cont_result.output
    cont_env = json.loads(cont_result.output)
    assert cont_env["status"] == "error"
    assert cont_env["data"]["phases_run"] == ["A", "B"]
    cont_phases = cont_env["data"]["validation_report"]["phases"]
    assert [phase["phase"] for phase in cont_phases] == ["A", "B"]
    assert cont_phases[0]["status"] == "FAILED"
    assert not any(
        issue["severity"] == "APPLIED_FIX" for issue in cont_phases[1]["issues"]
    )
    published_ab = _load_yaml(cont_dir / "updated_sample_config.yml")
    pub_paved = published_ab["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]
    pub_paved_val = pub_paved["value"] if isinstance(pub_paved, dict) else pub_paved
    assert pub_paved_val == pytest.approx(perturbed_paved)


def test_continue_on_error_ac_preserves_phase_a_failure_artefact(
    tmp_path: Path,
) -> None:
    """When A fails and C succeeds under --continue-on-error, A's placeholder YAML is kept."""
    config_path = _copy_sample_case(tmp_path)
    payload = _load_yaml(config_path)
    payload["model"]["physics"].pop("net_radiation")
    _write_yaml(config_path, payload)

    runner = CliRunner()
    result = runner.invoke(
        suews_cli,
        [
            "validate",
            "-p",
            "AC",
            "--forcing",
            "off",
            "--continue-on-error",
            "--format",
            "json",
            str(config_path),
        ],
    )

    assert result.exit_code == 1, result.output
    envelope = json.loads(result.output)
    assert envelope["status"] == "error"
    phases = envelope["data"]["validation_report"]["phases"]
    assert [phase["phase"] for phase in phases] == ["A", "C"]
    assert phases[0]["status"] == "FAILED"
    assert phases[1]["status"] == "PASSED"

    published_yaml = _load_yaml(tmp_path / "updated_sample_config.yml")
    assert "net_radiation" in published_yaml["model"]["physics"]
    net_rad = published_yaml["model"]["physics"]["net_radiation"]
    net_rad_val = net_rad["value"] if isinstance(net_rad, dict) else net_rad
    assert net_rad_val is None


def test_validate_geographic_parameters_wrapper_respects_model_year(
    tmp_path: Path,
) -> None:
    """phase_b.validate_geographic_parameters preserves leap/non-leap year DOY bounds."""
    config_path = _copy_sample_case(tmp_path)
    base_payload = _load_yaml(config_path)

    non_leap = _load_yaml(config_path)
    non_leap["model"]["control"]["start_time"] = "2023-01-01"
    non_leap["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = {
        "value": 366
    }
    non_leap_errors = [
        r for r in phase_b.validate_geographic_parameters(non_leap) if r.status == "ERROR"
    ]
    assert any("startdls" in r.parameter for r in non_leap_errors)

    leap = _load_yaml(config_path)
    leap["model"]["control"]["start_time"] = "2024-01-01"
    leap["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = {
        "value": 366
    }
    leap_errors = [
        r for r in phase_b.validate_geographic_parameters(leap) if r.status == "ERROR"
    ]
    assert not any("startdls" in r.parameter for r in leap_errors)

    invalid_start = base_payload
    invalid_start["model"]["control"]["start_time"] = "not-a-date"
    invalid_start["sites"][0]["properties"]["anthropogenic_emissions"]["startdls"] = {
        "value": 366
    }
    invalid_start_errors = [
        r
        for r in phase_b.validate_geographic_parameters(invalid_start)
        if r.status == "ERROR"
    ]
    assert not any("startdls" in r.parameter for r in invalid_start_errors)


def test_phase_b_apply_runs_adjustment_pipeline_once(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Phase B ``--science-fixes apply`` invokes the adjustment pipeline once, not twice."""
    config_path = _copy_sample_case(tmp_path)
    calls = 0
    original = phase_b.run_scientific_adjustment_pipeline

    def _counting_pipeline(*args, **kwargs):
        nonlocal calls
        calls += 1
        return original(*args, **kwargs)

    monkeypatch.setattr(phase_b, "run_scientific_adjustment_pipeline", _counting_pipeline)

    science_yaml = tmp_path / "science.yml"
    science_report = tmp_path / "science_report.txt"
    _, report = phase_b.run_science_check(
        uptodate_yaml_file=str(config_path),
        user_yaml_file=str(config_path),
        standard_yaml_file=str(config_path),
        science_yaml_file=str(science_yaml),
        science_report_file=str(science_report),
        phase_a_performed=False,
        science_fixes="apply",
    )

    assert not report.has_errors
    assert calls == 1


def _installed_suews_command() -> list[str]:
    """Return the ``suews validate`` CLI command for wheel or source-lane runs."""
    return [*resolve_suews_cli_command("suews"), "validate"]


@pytest.mark.e2e
def test_installed_cli_outer_boundary_json_exit_and_transactional_publication(
    tmp_path: Path,
) -> None:
    """Installed ``suews validate`` emits clean JSON stdout, exit codes, and clean publication."""
    config_path = _copy_sample_case(tmp_path)
    cmd = _installed_suews_command()

    ok_proc = subprocess.run(
        [
            *cmd,
            "-p",
            "ABC",
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
    assert ok_proc.returncode == 0, ok_proc.stdout + ok_proc.stderr
    ok_envelope = json.loads(ok_proc.stdout)
    assert ok_envelope["errors"] == []
    assert ok_envelope["data"]["phases_run"] == ["A", "B", "C"]
    assert ".suews-validate-" not in ok_proc.stdout
    assert not list(tmp_path.glob(".suews-validate-*"))

    report_txt = tmp_path / "report_sample_config.txt"
    report_json = tmp_path / "report_sample_config.json"
    updated_yml = tmp_path / "updated_sample_config.yml"
    assert report_txt.is_file()
    assert report_json.is_file()
    assert updated_yml.is_file()
    assert json.loads(report_json.read_text(encoding="utf-8")) == (
        ok_envelope["data"]["validation_report"]
    )

    # Non-ABC pipeline in table mode through the installed entry point returns
    # exit code 0, prints human-readable output, and removes its staging dir.
    table_dir = tmp_path / "table_phase_b"
    table_dir.mkdir()
    table_cfg = _copy_sample_case(table_dir)
    table_proc = subprocess.run(
        [
            *cmd,
            "-p",
            "B",
            "--forcing",
            "off",
            "--format",
            "table",
            str(table_cfg),
        ],
        cwd=table_dir,
        capture_output=True,
        text=True,
        timeout=180,
    )
    assert table_proc.returncode == 0, table_proc.stdout + table_proc.stderr
    assert table_proc.stdout.strip()
    assert not table_proc.stdout.lstrip().startswith("{")
    assert "report_sample_config.txt" in table_proc.stdout
    assert ".suews-validate-" not in table_proc.stdout
    assert not list(table_dir.glob(".suews-validate-*"))
    assert (table_dir / "report_sample_config.txt").is_file()
    assert (table_dir / "report_sample_config.json").is_file()

    # Failing Phase C run through the installed entry point returns exit code 1,
    # emits structured JSON on stdout, writes the stopping banner, and retains
    # only the reported evidence directory.
    failing_cfg = tmp_path / "failing_config.yml"
    payload = _load_yaml(config_path)
    payload["sites"][0]["properties"]["unexpected_outer_boundary_key"] = 123
    _write_yaml(failing_cfg, payload)

    fail_proc = subprocess.run(
        [
            *cmd,
            "-p",
            "ABC",
            "--forcing",
            "off",
            "--format",
            "json",
            str(failing_cfg),
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=180,
    )
    assert fail_proc.returncode == 1, fail_proc.stdout + fail_proc.stderr
    fail_envelope = json.loads(fail_proc.stdout)
    assert fail_envelope["status"] == "error"
    assert len(fail_envelope["errors"]) >= 1
    evidence_dir = Path(fail_envelope["data"]["evidence_dir"])
    assert evidence_dir.is_dir()
    assert list(tmp_path.glob(".suews-validate-*")) == [evidence_dir]

    fail_report = (tmp_path / "report_failing_config.txt").read_text(encoding="utf-8")
    assert "Validation stopped at: Model Compatibility" in fail_report
    assert "## ACTION NEEDED" in fail_report
    assert ".suews-validate-" not in fail_report


def test_non_abc_pipeline_rolls_back_prior_outputs_on_publish_failure(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Mid-publish failure on a newly unified non-ABC pipeline restores prior outputs."""
    from supy.data_model.validation.pipeline import orchestrator

    config_path = _copy_sample_case(tmp_path)
    prior_yaml = tmp_path / "updated_sample_config.yml"
    prior_txt = tmp_path / "report_sample_config.txt"
    prior_json = tmp_path / "report_sample_config.json"
    prior_yaml.write_text("# prior yaml\n", encoding="utf-8")
    prior_txt.write_text("# prior report\n", encoding="utf-8")
    prior_json.write_text('{"prior": true}\n', encoding="utf-8")

    real_replace = os.replace
    replace_calls = 0

    def _fail_second_publish(src, dst):
        nonlocal replace_calls
        if Path(dst) in {prior_yaml, prior_txt, prior_json}:
            replace_calls += 1
            if replace_calls == 2:
                raise OSError("simulated mid-publish failure")
        return real_replace(src, dst)

    monkeypatch.setattr(orchestrator.os, "replace", _fail_second_publish)

    with pytest.raises(OSError, match="simulated mid-publish failure"):
        orchestrator.run_pipeline(
            str(config_path),
            pipeline="AB",
            mode="public",
            forcing="off",
            science_fixes="off",
        )

    assert prior_yaml.read_text(encoding="utf-8") == "# prior yaml\n"
    assert prior_txt.read_text(encoding="utf-8") == "# prior report\n"
    assert prior_json.read_text(encoding="utf-8") == '{"prior": true}\n'


@pytest.mark.e2e
def test_standalone_phase_c_failure_does_not_publish_rejected_yaml(
    tmp_path: Path,
) -> None:
    """Standalone Phase C failure retains rejected YAML only in evidence, never canonical output."""
    import sys

    config_path = _copy_sample_case(tmp_path)
    payload = _load_yaml(config_path)
    payload["sites"][0]["properties"]["unexpected_standalone_c_key"] = 999
    _write_yaml(config_path, payload)

    proc = subprocess.run(
        [
            sys.executable,
            "-m",
            "supy.data_model.validation.pipeline.orchestrator",
            str(config_path),
            "--phase",
            "C",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=180,
    )
    assert proc.returncode == 1, proc.stdout + proc.stderr
    assert "Updated YAML:" not in proc.stdout
    assert not (tmp_path / "updated_sample_config.yml").exists()
    assert (tmp_path / "report_sample_config.txt").is_file()

    # Existing canonical output must not be replaced with rejected input either.
    prior_canonical = tmp_path / "updated_sample_config.yml"
    prior_canonical.write_text("# prior good canonical yaml\n", encoding="utf-8")
    proc2 = subprocess.run(
        [
            sys.executable,
            "-m",
            "supy.data_model.validation.pipeline.orchestrator",
            str(config_path),
            "--phase",
            "C",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=180,
    )
    assert proc2.returncode == 1, proc2.stdout + proc2.stderr
    assert "Updated YAML:" not in proc2.stdout
    assert (
        prior_canonical.read_text(encoding="utf-8") == "# prior good canonical yaml\n"
    )


def test_phase_b_compatibility_wrappers_unwrap_refvalue_gridiv(
    tmp_path: Path,
) -> None:
    """Compatibility wrappers unwrap RefValue gridiv so ValidationResult.to_dict() succeeds."""
    config_path = _copy_sample_case(tmp_path)
    geo_payload = _load_yaml(config_path)
    geo_payload["sites"][0]["gridiv"] = {"value": 42}
    geo_payload["sites"][0]["properties"]["lat"] = {"value": 999.0}
    geo_results = [
        r
        for r in phase_b.validate_geographic_parameters(geo_payload)
        if r.status == "ERROR"
    ]
    assert geo_results
    assert geo_results[0].site_gridid == 42
    assert geo_results[0].to_dict()["site_gridid"] == 42

    alb_payload = _load_yaml(config_path)
    alb_payload["sites"][0]["gridiv"] = {"value": 42}
    alb_payload["sites"][0]["properties"]["land_cover"]["grass"]["sfr"] = {"value": 0.2}
    alb_payload["sites"][0]["initial_states"]["grass"]["alb_id"] = {"value": None}
    alb_results = phase_b.check_missing_vegetation_albedo(alb_payload)
    assert alb_results
    assert alb_results[0].site_gridid == 42
    assert alb_results[0].to_dict()["site_gridid"] == 42
