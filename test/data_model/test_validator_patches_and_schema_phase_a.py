"""Wave 4 tests: pure scientific patch planners and schema-driven Phase A.

Verifies:
- Phase B scientific suggestion planning performs zero ``deepcopy`` calls and
  never mutates its input mapping.
- Phase B ``apply`` mode executes all planned patches in a single pass while
  preserving bare scalar vs ``RefValue`` representation and ``ref`` metadata.
- Phase A derives missing and extra parameters directly from ``SUEWSConfig``
  without reading ``sample_config.yml``.
- Phase A reports omitted optional fields without injecting speculative
  ``null`` placeholders into ``updated_<name>.yml``, while keeping critical
  missing physics placeholders intact.
- ``_NLAYER_DEPENDENT_ARRAY_RE`` covers bare arrays as well as ``RefValue``
  wrappers without over-matching fixed-length nested sub-arrays.
"""

from __future__ import annotations

from copy import deepcopy
from importlib.resources import files
import json
import os
from pathlib import Path
import subprocess
import sysconfig

import pytest
import yaml

from supy.data_model.core._yaml_io import safe_dump_yaml, safe_load_yaml
from supy.data_model.core.physics_families import flatten_physics_in_config
from supy.data_model.validation.pipeline import patches, phase_a, phase_b
from supy.data_model.validation.pipeline.document import ConfigDocument
from supy.data_model.validation.pipeline.patches import (
    plan_land_cover_fraction_patches,
    plan_model_dependent_nullification_patches,
    plan_rcmethod_patches,
    plan_setpointmethod_patches,
    plan_stebbsmethod_wwr_patches,
)

pytestmark = [
    pytest.mark.unit,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-ERROR"),
]


def _load_sample_dict() -> dict:
    text = (files("supy") / "sample_data" / "sample_config.yml").read_text(
        encoding="utf-8"
    )
    return safe_load_yaml(text)


def test_suggest_mode_performs_zero_deepcopy_and_does_not_mutate_input(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Planning Phase B suggestions avoids deep-copying and leaves input untouched."""
    data = _load_sample_dict()
    flatten_physics_in_config(data)
    # Trigger land-cover drift (within tolerance) and disabled-STEBBS nullification.
    data["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"] = 0.430002
    data["sites"][0]["properties"]["stebbs"]["temperature_air_indoor_initial"] = {
        "value": 21.0,
        "ref": {"DOI": "10.1234/example"},
    }
    snapshot = deepcopy(data)

    deepcopy_calls = 0
    real_deepcopy = deepcopy

    def _forbid_deepcopy(obj, *args, **kwargs):
        nonlocal deepcopy_calls
        deepcopy_calls += 1
        return real_deepcopy(obj, *args, **kwargs)

    monkeypatch.setattr(phase_b, "deepcopy", _forbid_deepcopy)
    monkeypatch.setattr(
        phase_b, "get_mean_monthly_air_temperature", lambda lat, lng, month: 12.5
    )
    monkeypatch.setattr(
        phase_b, "get_monthly_air_temperature_diffmax", lambda lat, lng: 14.0
    )
    monkeypatch.setattr(
        phase_b, "get_mean_annual_air_temperature", lambda lat, lng: 11.2
    )

    suggestions = phase_b.collect_science_suggestions(data, "2012-01-01", 2012)

    assert deepcopy_calls == 0
    assert data == snapshot
    assert len(suggestions) > 0


def test_apply_mode_single_pass_preserves_bare_refvalue_and_ref_metadata(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Applying planned patches preserves bare scalars and RefValue ``ref`` keys."""
    data = _load_sample_dict()
    flatten_physics_in_config(data)
    # Bare sfr on paved with tiny drift; RefValue+ref on bldgs (max sfr).
    lc = data["sites"][0]["properties"]["land_cover"]
    lc["paved"]["sfr"] = 0.430001
    lc["bldgs"]["sfr"] = {"value": 0.38, "ref": {"DOI": "10.1000/bldgs"}}
    # Bare temperature_ground on paved initial_states; RefValue+ref on bldgs.
    init = data["sites"][0]["initial_states"]
    init["paved"]["tsfc"] = 5.0
    init["bldgs"]["tsfc"] = {"value": 5.0, "ref": {"DOI": "10.1000/tsfc"}}

    monkeypatch.setattr(
        phase_b, "get_mean_monthly_air_temperature", lambda lat, lng, month: 14.2
    )
    monkeypatch.setattr(
        phase_b, "get_monthly_air_temperature_diffmax", lambda lat, lng: None
    )
    monkeypatch.setattr(
        phase_b, "get_mean_annual_air_temperature", lambda lat, lng: None
    )

    suggestions = phase_b.collect_science_suggestions(
        deepcopy(data), "2012-01-01", 2012
    )
    updated, adjustments = phase_b.apply_science_suggestions(
        deepcopy(data), suggestions, "2012-01-01", 2012
    )

    assert len(adjustments) > 0
    assert updated["sites"][0]["initial_states"]["paved"]["tsfc"] == pytest.approx(14.2)
    assert isinstance(updated["sites"][0]["initial_states"]["paved"]["tsfc"], float)
    assert updated["sites"][0]["initial_states"]["bldgs"]["tsfc"] == {
        "value": 14.2,
        "ref": {"DOI": "10.1000/tsfc"},
    }
    # Paved (max sfr) stayed bare float after drift adjustment.
    assert isinstance(
        updated["sites"][0]["properties"]["land_cover"]["paved"]["sfr"], float
    )
    assert updated["sites"][0]["properties"]["land_cover"]["bldgs"]["sfr"] == {
        "value": 0.38,
        "ref": {"DOI": "10.1000/bldgs"},
    }


def test_plan_patches_are_pure_and_apply_via_config_document() -> None:
    """Pure patch planners leave input untouched and apply cleanly via ConfigDocument."""
    cfg = {
        "model": {
            "physics": {
                "stebbs": {
                    "enabled": True,
                    "parameters": "default",
                    "capacitance": 0,
                    "setpoint": 0,
                },
                "emissions": 2,
            }
        },
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "land_cover": {
                        "paved": {"sfr": 0.600001},
                        "bldgs": {"sfr": {"value": 0.4, "ref": {"ID": "b"}}},
                    },
                    "building_archetype": {
                        "fraction_heat_capacity_wall_external": {
                            "value": 0.25,
                            "ref": {"ID": "w"},
                        },
                        "fraction_heat_capacity_roof_external": 0.3,
                        "profile_setpoint_temperature_heating_air": {
                            "working_day": {"1": 20.0},
                            "holiday": {"1": 18.0},
                        },
                    },
                },
            }
        ],
    }
    snapshot = deepcopy(cfg)

    plans = [
        plan_land_cover_fraction_patches(cfg),
        plan_rcmethod_patches(cfg),
        plan_setpointmethod_patches(cfg),
        plan_stebbsmethod_wwr_patches(cfg),
        plan_model_dependent_nullification_patches(cfg),
    ]
    assert cfg == snapshot

    all_patches = [p for plan in plans for p in plan.patches]
    assert all_patches

    doc = ConfigDocument.from_dict(cfg, normalise=False).apply_patches(all_patches)
    arch = doc.data["sites"][0]["properties"]["building_archetype"]
    assert arch["fraction_heat_capacity_wall_external"] == {
        "value": 0.5,
        "ref": {"ID": "w"},
    }
    assert arch["fraction_heat_capacity_roof_external"] == 0.5
    assert arch["profile_setpoint_temperature_heating_air"] == {
        "working_day": {"1": None},
        "holiday": {"1": None},
    }
    assert doc.data["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] == pytest.approx(
        0.6
    )


def test_schema_driven_phase_a_needs_no_sample_config_and_does_not_pad_optional_fields(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Phase A uses SUEWSConfig schema, reports omitted optionals, and skips null padding."""
    data = _load_sample_dict()
    del data["model"]["control"]["start_time"]
    del data["sites"][0]["properties"]["conductance"]["g_max"]
    data["sites"][0]["properties"]["bogus_extra_field"] = 99

    user_file = tmp_path / "user.yml"
    updated_file = tmp_path / "updated.yml"
    report_file = tmp_path / "report.txt"
    user_file.write_text(
        safe_dump_yaml(data, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )

    def _forbid_standard_load(*args, **kwargs):
        raise AssertionError("Phase A must not load sample_config.yml")

    monkeypatch.setattr(phase_a, "_load_normalised_standard_data", _forbid_standard_load)

    report = phase_a.annotate_missing_parameters(
        user_file=str(user_file),
        uptodate_file=str(updated_file),
        report_file=str(report_file),
        forcing="off",
        detect_nlayer=True,
    )

    assert not report.has_errors
    missing_paths = {
        i.yaml_path for i in report.issues if i.code == "A.MISSING_PARAM"
    }
    extra_paths = {i.yaml_path for i in report.issues if i.code == "A.EXTRA_PARAM"}
    assert "model.control.start_time" in missing_paths
    assert "sites[0].properties.conductance.g_max" in missing_paths
    assert "sites[0].properties.bogus_extra_field" in extra_paths

    updated = yaml.safe_load(updated_file.read_text(encoding="utf-8"))
    assert "start_time" not in updated["model"]["control"]
    assert "g_max" not in updated["sites"][0]["properties"]["conductance"]


def test_schema_driven_phase_a_pads_only_critical_missing_physics(
    tmp_path: Path,
) -> None:
    """Critical missing physics still gets null placeholders; optionals do not."""
    data = _load_sample_dict()
    del data["model"]["physics"]["net_radiation"]
    del data["model"]["control"]["start_time"]

    user_file = tmp_path / "crit.yml"
    updated_file = tmp_path / "updated_crit.yml"
    report_file = tmp_path / "report_crit.txt"
    user_file.write_text(
        safe_dump_yaml(data, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )

    report = phase_a.annotate_missing_parameters(
        user_file=str(user_file),
        uptodate_file=str(updated_file),
        report_file=str(report_file),
        forcing="off",
        detect_nlayer=True,
    )

    assert report.has_errors
    assert any(
        i.code == "A.MISSING_PARAM.CRITICAL"
        and i.yaml_path == "model.physics.net_radiation"
        for i in report.issues
    )
    updated = yaml.safe_load(updated_file.read_text(encoding="utf-8"))
    assert updated["model"]["physics"]["net_radiation"] == {"value": None}
    assert "start_time" not in updated["model"]["control"]


def test_schema_finders_clean_on_sample_and_bare_nlayer_regex() -> None:
    """Sample config yields zero schema diffs; nlayer regex matches bare and wrapped arrays."""
    sample = _load_sample_dict()
    assert phase_a.find_missing_parameters_from_schema(sample) == []
    assert phase_a.find_extra_parameters_from_schema(sample) == []

    regex = phase_a._NLAYER_DEPENDENT_ARRAY_RE
    assert regex.match("sites[0].properties.vertical_layers.height")
    assert regex.match("sites[0].properties.vertical_layers.height.value")
    assert regex.match("sites[2].properties.vertical_layers.veg_frac")
    assert regex.match("sites[1].properties.vertical_layers.roofs")
    assert regex.match("sites[1].initial_states.walls")
    assert regex.match(
        "sites[0].properties.vertical_layers.roofs[0].thermal_layers.dz"
    )
    assert not regex.match("sites[0].properties.vertical_layers.nlayer")
    assert not regex.match("sites[0].properties.pipe_capacity")


def test_get_allowed_nested_sections_uses_schema_graph() -> None:
    """Cached schema graph returns sorted extra-allowing nested sections without sharing mutable state."""
    first = phase_a.get_allowed_nested_sections_in_properties()
    second = phase_a.get_allowed_nested_sections_in_properties()
    assert first == second == sorted(first)
    assert {"stebbs", "irrigation", "snow", "spartacus", "lumps", "building_archetype"} <= set(
        first
    )
    first.append("mutated_caller_copy")
    assert "mutated_caller_copy" not in phase_a.get_allowed_nested_sections_in_properties()


def test_patches_and_phase_a_hardening_regressions(monkeypatch) -> None:
    """Selective apply_science_suggestions, profile-only CO2, ref-only setpoints, integer root keys."""
    # Empty suggestions applies nothing and leaves input untouched.
    data = _load_sample_dict()
    flatten_physics_in_config(data)
    monkeypatch.setattr(
        phase_b, "get_mean_monthly_air_temperature", lambda lat, lng, month: 14.2
    )
    updated_empty, adjs_empty = phase_b.apply_science_suggestions(
        data, [], "2012-01-01", 2012
    )
    assert adjs_empty == []
    assert updated_empty == data

    # Selective application applies only the chosen suggestion.
    suggestions = phase_b.collect_science_suggestions(data, "2012-01-01", 2012)
    paved_only = [s for s in suggestions if s.parameter == "initial_states.paved"]
    assert len(paved_only) == 1
    updated_sel, adjs_sel = phase_b.apply_science_suggestions(
        data, paved_only, "2012-01-01", 2012
    )
    assert len(adjs_sel) == 1
    assert updated_sel["sites"][0]["initial_states"]["paved"]["tsfc"]["value"] == pytest.approx(14.2)
    assert (
        updated_sel["sites"][0]["initial_states"]["bldgs"]["tsfc"]["value"]
        == data["sites"][0]["initial_states"]["bldgs"]["tsfc"]["value"]
    )

    # Disabled CO2 block with ONLY populated hourly profiles is still nullified.
    co2_profile_only = {
        "model": {"physics": {"emissions": {"value": 2}, "stebbs": {"enabled": False}}},
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "anthropogenic_emissions": {
                        "co2": {
                            "profile": {
                                "working_day": {"1": 2.0},
                                "holiday": {"1": {"value": 1.0, "ref": {"desc": "h"}}},
                            }
                        }
                    }
                },
            }
        ],
    }
    co2_plan = patches.plan_model_dependent_nullification_patches(co2_profile_only)
    assert len(co2_plan.patches) == 1
    applied_co2 = co2_plan.apply_to_dict(co2_profile_only)
    prof = applied_co2["sites"][0]["properties"]["anthropogenic_emissions"]["co2"]["profile"]
    assert prof["working_day"]["1"] is None
    assert prof["holiday"]["1"] == {"value": None, "ref": {"desc": "h"}}

    # Ref-only / wrapped-null setpoint profiles emit zero spurious patches.
    setpoint_noop = {
        "model": {"physics": {"stebbs": {"enabled": True, "setpoint": {"value": 0}}}},
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "building_archetype": {
                        "profile_setpoint_temperature_heating_air": {
                            "working_day": {"ref": {"desc": "placeholder"}},
                            "holiday": {"1": {"value": None, "ref": {"desc": "null"}}},
                        }
                    }
                },
            }
        ],
    }
    assert patches.plan_setpointmethod_patches(setpoint_noop).patches == []

    # Non-string YAML root keys are stringified cleanly in extra-parameter discovery.
    extras = phase_a.find_extra_parameters_from_schema({1: "unexpected"}, _normalised=True)
    assert extras == ["1"]

    # Missing structural roots ('model', 'sites') are still surfaced on {}.
    missing_empty = phase_a.find_missing_parameters_from_schema({}, _normalised=True)
    missing_paths = {p for p, _, _ in missing_empty}
    assert {"model", "sites"} <= missing_paths


def _installed_suews_validate_cmd() -> list[str]:
    """Return the installed ``suews validate`` console entry point command."""
    executable = Path(sysconfig.get_path("scripts")) / (
        "suews" + (".exe" if os.name == "nt" else "")
    )
    assert executable.is_file(), f"Missing installed command: {executable}"
    return [str(executable), "validate"]


def _copy_sample_case(dst: Path) -> Path:
    sample_dir = files("supy").joinpath("sample_data")
    for item in sample_dir.iterdir():
        if item.is_file():
            (dst / item.name).write_bytes(item.read_bytes())
    return dst / "sample_config.yml"


@pytest.mark.e2e
def test_installed_cli_outer_boundary_omitted_optionals_refvalue_apply_and_error_exit(
    tmp_path: Path,
) -> None:
    """Installed CLI keeps omitted optionals omitted, preserves RefValue metadata, and exits 1 on error."""
    cmd = _installed_suews_validate_cmd()
    config_path = _copy_sample_case(tmp_path)
    payload = safe_load_yaml(config_path.read_text(encoding="utf-8"))
    # Omit optional field and introduce a tiny land-cover sum drift with mixed RefValue/bare.
    payload["model"]["control"].pop("start_time", None)
    ref_meta = {"desc": "Survey 2026", "ID": "LC-01"}
    lc = payload["sites"][0]["properties"]["land_cover"]
    lc["paved"]["sfr"] = {"value": lc["paved"]["sfr"]["value"] - 0.00004, "ref": ref_meta}
    lc["bldgs"]["sfr"] = lc["bldgs"]["sfr"]["value"]
    config_path.write_text(
        safe_dump_yaml(payload, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )

    # 1. Suggest mode: omitted optional stays omitted, drift is NOT applied.
    suggest_proc = subprocess.run(
        [
            *cmd,
            "-p",
            "ABC",
            "--forcing",
            "off",
            "--science-fixes",
            "suggest",
            "--format",
            "json",
            str(config_path),
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=180,
    )
    assert suggest_proc.returncode == 0, suggest_proc.stdout + suggest_proc.stderr
    suggest_env = json.loads(suggest_proc.stdout)
    assert suggest_env["errors"] == []
    assert not list(tmp_path.glob(".suews-validate-*"))

    updated_suggest = safe_load_yaml(
        (tmp_path / "updated_sample_config.yml").read_text(encoding="utf-8")
    )
    assert "start_time" not in updated_suggest["model"]["control"]
    assert updated_suggest["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["ref"] == ref_meta
    assert isinstance(
        updated_suggest["sites"][0]["properties"]["land_cover"]["bldgs"]["sfr"],
        (int, float),
    )

    # 2. Apply mode: omitted optional still omitted, drift normalised, RefValue/bare preserved.
    apply_proc = subprocess.run(
        [
            *cmd,
            "-p",
            "ABC",
            "--forcing",
            "off",
            "--science-fixes",
            "apply",
            "--format",
            "json",
            str(config_path),
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=180,
    )
    assert apply_proc.returncode == 0, apply_proc.stdout + apply_proc.stderr
    apply_env = json.loads(apply_proc.stdout)
    assert apply_env["errors"] == []
    assert not list(tmp_path.glob(".suews-validate-*"))

    updated_apply = safe_load_yaml(
        (tmp_path / "updated_sample_config.yml").read_text(encoding="utf-8")
    )
    assert "start_time" not in updated_apply["model"]["control"]
    applied_lc = updated_apply["sites"][0]["properties"]["land_cover"]
    assert applied_lc["paved"]["sfr"]["ref"] == ref_meta
    assert isinstance(applied_lc["bldgs"]["sfr"], (int, float))
    applied_sum = sum(
        (s["sfr"]["value"] if isinstance(s["sfr"], dict) else s["sfr"])
        for s in applied_lc.values()
        if "sfr" in s
    )
    assert applied_sum == pytest.approx(1.0, abs=1e-10)

    # 3. Error exit on missing critical physics: exit 1, critical placeholder present,
    #    optional field stays omitted, evidence directory recorded in JSON envelope.
    bad_cfg = tmp_path / "missing_crit.yml"
    bad_payload = deepcopy(payload)
    bad_payload["model"]["physics"].pop("net_radiation", None)
    bad_cfg.write_text(
        safe_dump_yaml(bad_payload, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )
    fail_proc = subprocess.run(
        [
            *cmd,
            "-p",
            "ABC",
            "--forcing",
            "off",
            "--format",
            "json",
            str(bad_cfg),
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=180,
    )
    assert fail_proc.returncode == 1, fail_proc.stdout + fail_proc.stderr
    fail_env = json.loads(fail_proc.stdout)
    assert fail_env["status"] == "error"
    evidence_dir = Path(fail_env["data"]["evidence_dir"])
    assert evidence_dir.is_dir()
    assert list(tmp_path.glob(".suews-validate-*")) == [evidence_dir]
    updated_bad = safe_load_yaml(
        (tmp_path / "updated_missing_crit.yml").read_text(encoding="utf-8")
    )
    assert updated_bad["model"]["physics"]["net_radiation"] == {"value": None}
    assert "start_time" not in updated_bad["model"]["control"]


@pytest.mark.e2e
def test_installed_cli_outer_boundary_transactional_rollback_on_publish_failure(
    tmp_path: Path,
) -> None:
    """Publication failure rolls back previously published outputs cleanly through the installed CLI."""
    cmd = _installed_suews_validate_cmd()
    config_path = _copy_sample_case(tmp_path)

    # Establish a valid baseline publication first.
    first = subprocess.run(
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
    assert first.returncode == 0, first.stdout + first.stderr

    targets = [
        tmp_path / "updated_sample_config.yml",
        tmp_path / "report_sample_config.txt",
        tmp_path / "report_sample_config.json",
    ]
    baseline_bytes = {p: p.read_bytes() for p in targets}

    # Inject a publication failure on the final atomic replace via sitecustomize.py.
    hooks_dir = tmp_path / "hooks"
    hooks_dir.mkdir()
    (hooks_dir / "sitecustomize.py").write_text(
        (
            "from pathlib import Path\n"
            "from supy.data_model.validation.pipeline import orchestrator as eng\n"
            "_orig_replace = eng.os.replace\n"
            "def _fail_final(src, dst):\n"
            "    if Path(src).name == 'publish_2':\n"
            "        raise OSError('injected final publish failure')\n"
            "    return _orig_replace(src, dst)\n"
            "eng.os.replace = _fail_final\n"
        ),
        encoding="utf-8",
    )
    env = {
        **os.environ,
        "PYTHONPATH": str(hooks_dir) + os.pathsep + os.environ.get("PYTHONPATH", ""),
    }

    second = subprocess.run(
        [
            *cmd,
            "-p",
            "ABC",
            "--forcing",
            "off",
            "--science-fixes",
            "apply",
            "--format",
            "json",
            str(config_path),
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=180,
        env=env,
    )
    assert second.returncode != 0
    assert {p: p.read_bytes() for p in targets} == baseline_bytes
    second_env = json.loads(second.stdout)
    assert second_env["status"] == "error"
    evidence_dir = Path(second_env["data"]["evidence_dir"])
    assert evidence_dir.is_dir()
    assert list(tmp_path.glob(".suews-validate-*")) == [evidence_dir]


@pytest.mark.e2e
@pytest.mark.parametrize("bad_nlayer", [0, -1, 16, 100000])
def test_phase_a_rejects_out_of_range_nlayer_as_error(
    tmp_path: Path, bad_nlayer: int
) -> None:
    """Standalone Phase A rejects out-of-range nlayer (<= 0, > 15, huge) as an ERROR."""
    cmd = _installed_suews_validate_cmd()
    config_path = _copy_sample_case(tmp_path)
    payload = safe_load_yaml(config_path.read_text(encoding="utf-8"))
    payload["sites"][0]["properties"]["vertical_layers"]["nlayer"] = {
        "value": bad_nlayer
    }
    config_path.write_text(
        safe_dump_yaml(payload, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )

    proc = subprocess.run(
        [
            *cmd,
            "-p",
            "A",
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
    env = json.loads(proc.stdout)
    assert env["status"] == "error"
    phase_a_rep = env["data"]["validation_report"]["phases"][0]
    assert phase_a_rep["status"] == "FAILED"
    nlayer_issues = [i for i in phase_a_rep["issues"] if i["code"] == "A.NLAYER_LIMIT"]
    assert len(nlayer_issues) == 1
    assert nlayer_issues[0]["severity"] == "ERROR"


def test_patch_plan_apply_to_dict_copies_once_and_preserves_input(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """PatchPlan.apply_to_dict performs a single thaw_copy without mutating the caller's dict."""
    from supy.data_model.validation.pipeline import patches as patches_mod
    from supy.data_model.validation.pipeline.document import ConfigPatch

    copy_calls = 0
    real_thaw_copy = patches_mod.thaw_copy

    def _counting_thaw(obj):
        nonlocal copy_calls
        copy_calls += 1
        return real_thaw_copy(obj)

    monkeypatch.setattr(patches_mod, "thaw_copy", _counting_thaw)

    source = {"sites": [{"properties": {"lat": {"value": 51.5}}}]}
    snapshot = deepcopy(source)

    empty_plan = patches_mod.PatchPlan()
    out_empty = empty_plan.apply_to_dict(source)
    assert copy_calls == 1
    assert out_empty == snapshot
    assert out_empty is not source
    assert source == snapshot

    copy_calls = 0
    plan = patches_mod.PatchPlan(
        patches=[
            ConfigPatch(
                path="sites[0].properties.lat",
                action="set",
                new_value=52.0,
            )
        ]
    )
    out = plan.apply_to_dict(source)
    assert copy_calls == 1
    assert out["sites"][0]["properties"]["lat"] == {"value": 52.0}
    assert source == snapshot

    # Delete patch parity between apply_to_dict and apply_to_document:
    from supy.data_model.validation.pipeline.document import ConfigDocument

    # 1. Missing parent raises ValueError in both entry points.
    missing_parent = patches_mod.PatchPlan(
        patches=[ConfigPatch(path="missing.child", action="delete")]
    )
    with pytest.raises(ValueError, match="parent container not found"):
        missing_parent.apply_to_dict({})
    with pytest.raises(ValueError, match="parent container not found"):
        missing_parent.apply_to_document(ConfigDocument({}))

    # 2. Unsupported non-mapping target (list index) raises ValueError in both.
    list_target = patches_mod.PatchPlan(
        patches=[ConfigPatch(path="sites[0]", action="delete")]
    )
    with pytest.raises(ValueError, match="mapping key"):
        list_target.apply_to_dict(source)
    with pytest.raises(ValueError, match="mapping key"):
        list_target.apply_to_document(ConfigDocument(deepcopy(source)))

    # 3. Missing leaf under an existing mapping parent is idempotent in both.
    missing_leaf = patches_mod.PatchPlan(
        patches=[ConfigPatch(path="sites[0].properties.absent", action="delete")]
    )
    assert missing_leaf.apply_to_dict(source) == snapshot
    assert missing_leaf.apply_to_document(ConfigDocument(deepcopy(source))).data == snapshot

    # 4. Existing leaf is deleted identically in both.
    existing_leaf = patches_mod.PatchPlan(
        patches=[ConfigPatch(path="sites[0].properties.lat", action="delete")]
    )
    assert existing_leaf.apply_to_dict(source) == {"sites": [{"properties": {}}]}
    assert existing_leaf.apply_to_document(ConfigDocument(deepcopy(source))).data == {
        "sites": [{"properties": {}}]
    }

    # 5. Selective apply_science_suggestions also performs a single thaw_copy.
    from supy.data_model.validation.pipeline import phase_b as phase_b_mod

    pb_copy_calls = 0
    real_pb_thaw = phase_b_mod.thaw_copy

    def _counting_pb_thaw(obj):
        nonlocal pb_copy_calls
        pb_copy_calls += 1
        return real_pb_thaw(obj)

    monkeypatch.setattr(phase_b_mod, "thaw_copy", _counting_pb_thaw)
    suggestion = phase_b_mod.ScienceSuggestion(
        parameter="lat",
        message="Update lat",
        old_value=51.5,
        suggested_value=52.0,
        site_index=0,
        site_gridid=1,
        patches=(
            ConfigPatch(
                path="sites[0].properties.lat",
                action="set",
                new_value=52.0,
            ),
        ),
    )
    updated, applied = phase_b_mod.apply_science_suggestions(
        source, [suggestion], None, None
    )
    assert pb_copy_calls == 1
    assert len(applied) == 1
    assert updated["sites"][0]["properties"]["lat"] == {"value": 52.0}
    assert source == snapshot
