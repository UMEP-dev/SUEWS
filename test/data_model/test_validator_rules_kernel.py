"""Unit and contract tests for Wave 3 shared Tier-1 hard-invariant kernel and typed Phase B rules."""

from __future__ import annotations

import ast
import copy
from importlib import import_module
from importlib.resources import files
import json
import os
from pathlib import Path
import subprocess
import sysconfig

import numpy as np
from pydantic import ValidationError
import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.human_activity import IrrigationParams
from supy.data_model.core.model import Model, ModelPhysics, WaterUseMethod
from supy.data_model.core.profile import WeeklyProfile
from supy.data_model.core.site import Site, SiteProperties, StebbsProperties
from supy.data_model.core.type import RefValue
from supy.data_model.validation.pipeline.phase_b_rules import (
    RulesRegistry,
    ValidationContext,
    ValidationResult,
    validate_irrigation_doy,
)
from supy.data_model.validation.pipeline.phase_b_rules.physics_rules import (
    validate_spartacus_dependencies,
    validate_storageheatmethod_dependencies,
)
from supy.data_model.validation.rules.advisory import (
    check_irrigation_season_advisory,
    check_spartacus_lowest_layer_advisories,
)
from supy.data_model.validation.rules.hard_invariants import (
    check_dls_doy_pair,
    check_irrigation_doy_pair,
    check_same_surface_property,
    check_spartacus_storage_heat,
    check_stebbs_internal_shading,
    is_modelled_irrigation_active,
)
from supy.data_model.validation.rules.typed_view import (
    PhysicsView,
    TypedConfigView,
    as_float,
    method_code,
)

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-SCHEMA", "C-PARITY"),
]

_REPO_ROOT = Path(__file__).resolve().parents[2]
_RULES_DIR = _REPO_ROOT / "src" / "supy" / "data_model" / "validation" / "rules"
_PHASE_B_RULES_DIR = (
    _REPO_ROOT / "src" / "supy" / "data_model" / "validation" / "pipeline" / "phase_b_rules"
)


def _find_raw_value_accesses(directory: Path) -> list[str]:
    """Return ``file:line`` strings for any raw ``["value"]`` or ``.get("value")``."""
    offenders: list[str] = []
    for py_file in sorted(directory.glob("*.py")):
        tree = ast.parse(py_file.read_text(encoding="utf-8"), filename=str(py_file))
        for node in ast.walk(tree):
            if (
                isinstance(node, ast.Subscript)
                and isinstance(node.slice, ast.Constant)
                and node.slice.value == "value"
            ):
                offenders.append(f"{py_file.name}:{node.lineno} (['value'])")
            elif (
                isinstance(node, ast.Call)
                and isinstance(node.func, ast.Attribute)
                and node.func.attr == "get"
                and node.args
                and isinstance(node.args[0], ast.Constant)
                and node.args[0].value == "value"
            ):
                offenders.append(f"{py_file.name}:{node.lineno} (.get('value'))")
    return offenders


def test_rules_and_phase_b_rules_have_no_raw_value_key_accesses() -> None:
    """Typed rules must unwrap via ``document.py`` / ``TypedConfigView``, never raw ``"value"``."""
    offenders = _find_raw_value_accesses(_RULES_DIR) + _find_raw_value_accesses(
        _PHASE_B_RULES_DIR
    )
    assert offenders == [], f"Raw 'value' key accesses remain: {offenders}"


def test_hard_invariants_module_has_no_top_level_core_or_pipeline_imports() -> None:
    """``hard_invariants.py`` must stay free of top-level ``core``/``pipeline`` imports."""
    path = _RULES_DIR / "hard_invariants.py"
    tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    forbidden: list[str] = []
    for node in tree.body:
        if isinstance(node, ast.ImportFrom) and node.module:
            if "core" in node.module.split(".") or "pipeline" in node.module.split("."):
                forbidden.append(f"line {node.lineno}: from {node.module}")
        elif isinstance(node, ast.Import):
            for alias in node.names:
                if "core" in alias.name or "pipeline" in alias.name:
                    forbidden.append(f"line {node.lineno}: import {alias.name}")
    assert forbidden == [], f"Circular-import hazard in hard_invariants.py: {forbidden}"


def test_typed_config_view_normalises_bare_and_refvalue_equivalently() -> None:
    """``TypedConfigView`` reads bare scalars and RefValue mappings identically."""
    bare = {
        "model": {
            "control": {"start_time": "2024-06-01"},
            "physics": {
                "net_radiation": 1003,
                "storage_heat": 5,
                "stebbs": {"enabled": True, "parameters": "provided", "same_albedo_wall": 0},
            },
        },
        "sites": [
            {
                "name": "BareSite",
                "gridiv": 42,
                "properties": {
                    "z": 30.0,
                    "land_cover": {"bldgs": {"sfr": 0.4, "bldgh": 10.0}},
                    "vertical_layers": {
                        "height": [0.0, 10.0, 20.0],
                        "building_frac": [0.4, 0.2],
                        "walls": [{"alb": 0.25}, {"alb": 0.30}],
                    },
                    "building_archetype": {"reflectivity_wall_external": 0.25},
                },
            }
        ],
    }
    wrapped = {
        "model": {
            "control": {"start_time": {"value": "2024-06-01"}},
            "physics": {
                "net_radiation": {"value": 1003},
                "storage_heat": {"value": 5},
                "stebbs": {
                    "enabled": {"value": True},
                    "parameters": {"value": "provided"},
                    "same_albedo_wall": {"value": 0},
                },
            },
        },
        "sites": [
            {
                "name": "BareSite",
                "gridiv": {"value": 42},
                "properties": {
                    "z": {"value": 30.0},
                    "land_cover": {"bldgs": {"sfr": {"value": 0.4}, "bldgh": {"value": 10.0}}},
                    "vertical_layers": {
                        "height": {"value": [0.0, 10.0, 20.0]},
                        "building_frac": {"value": [0.4, 0.2]},
                        "walls": [{"alb": {"value": 0.25}}, {"alb": {"value": 0.30}}],
                    },
                    "building_archetype": {"reflectivity_wall_external": {"value": 0.25}},
                },
            }
        ],
    }

    v_bare = TypedConfigView.from_mapping(bare)
    v_wrap = TypedConfigView.from_mapping(wrapped)

    assert v_bare.physics.is_spartacus_enabled is True
    assert v_wrap.physics.is_spartacus_enabled is True
    assert v_bare.physics.code("storage_heat") == v_wrap.physics.code("storage_heat") == 5
    assert v_bare.physics.stebbs_method == v_wrap.physics.stebbs_method == 2
    assert v_bare.any_buildings_present() is True
    assert v_wrap.any_buildings_present() is True
    assert v_bare.sites[0].gridid == v_wrap.sites[0].gridid == 42
    assert v_bare.sites[0].last_nonzero_height() == v_wrap.sites[0].last_nonzero_height() == 20.0
    assert v_bare.sites[0].first_vertical_value("building_frac") == 0.4
    assert v_wrap.sites[0].first_vertical_value("building_frac") == 0.4
    assert v_bare.sites[0].facet_layer_values("walls", "alb") == [0.25, 0.30]
    assert v_wrap.sites[0].facet_layer_values("walls", "alb") == [0.25, 0.30]


@pytest.mark.parametrize(
    ("ie_start", "ie_end"),
    [
        (120, None),
        (None, 240),
        ({"value": 120}, {"value": None}),
        ({"ref": {"desc": "disabled"}}, 240),
    ],
)
def test_irrigation_window_pairing_enforced_when_active_and_skipped_when_inactive(
    ie_start, ie_end
) -> None:
    """Half-configured irrigation windows fail when modelled irrigation is active and pass when inactive."""
    # Standalone IrrigationParams sub-model does not know model.physics.water_use,
    # so pairing is enforced at SUEWSConfig and Phase B boundaries instead.
    irr_only = IrrigationParams(ie_start=ie_start, ie_end=ie_end)
    assert irr_only is not None

    active_daywat = WeeklyProfile(monday=1.0)
    with pytest.raises(
        ValidationError, match="Both ie_start and ie_end must be specified together"
    ):
        SUEWSConfig(
            model=Model(physics=ModelPhysics(water_use=WaterUseMethod.MODELLED)),
            sites=[
                Site(
                    properties=SiteProperties(
                        irrigation=IrrigationParams(
                            ie_start=ie_start, ie_end=ie_end, daywat=active_daywat
                        )
                    )
                )
            ],
        )

    # Inactive via water_use=OBSERVED (even with daywat enabled) succeeds cleanly.
    obs_cfg = SUEWSConfig(
        model=Model(physics=ModelPhysics(water_use=WaterUseMethod.OBSERVED)),
        sites=[
            Site(
                properties=SiteProperties(
                    irrigation=IrrigationParams(
                        ie_start=ie_start, ie_end=ie_end, daywat=active_daywat
                    )
                )
            )
        ],
    )
    obs_cfg.validate_doy_parameters()

    # Inactive via all daywat == 0.0 succeeds cleanly.
    zero_daywat_cfg = SUEWSConfig(
        model=Model(physics=ModelPhysics(water_use=WaterUseMethod.MODELLED)),
        sites=[
            Site(
                properties=SiteProperties(
                    irrigation=IrrigationParams(ie_start=ie_start, ie_end=ie_end)
                )
            )
        ],
    )
    zero_daywat_cfg.validate_doy_parameters()

    # Direct kernel/Phase B rule enforces pairing when active and skips when inactive.
    start_scalar = (
        ie_start["value"] if isinstance(ie_start, dict) and "value" in ie_start else ie_start
    )
    end_scalar = (
        ie_end["value"] if isinstance(ie_end, dict) and "value" in ie_end else ie_end
    )
    phase_b_results = validate_irrigation_doy(start_scalar, end_scalar, 51.5, 2024, "KCL")
    assert any(
        r.status == "ERROR" and "Both ie_start and ie_end must be specified together" in r.message
        for r in phase_b_results
    )

    active_ctx = ValidationContext(
        yaml_data={
            "model": {"physics": {"water_use": 0}},
            "sites": [
                {
                    "name": "KCL",
                    "properties": {
                        "lat": 51.5,
                        "irrigation": {
                            "daywat": {"monday": 1.0},
                            "ie_start": ie_start,
                            "ie_end": ie_end,
                        },
                    },
                }
            ],
        },
        model_year=2024,
    )
    assert any(
        r.status == "ERROR" and "Both ie_start and ie_end must be specified together" in r.message
        for r in RulesRegistry()["irrigation"](active_ctx)
    )

    for inactive_water_use, inactive_daywat in ((1, {"monday": 1.0}), (0, {"monday": 0.0})):
        inactive_ctx = ValidationContext(
            yaml_data={
                "model": {"physics": {"water_use": inactive_water_use}},
                "sites": [
                    {
                        "name": "KCL",
                        "properties": {
                            "lat": 51.5,
                            "irrigation": {
                                "daywat": inactive_daywat,
                                "ie_start": ie_start,
                                "ie_end": ie_end,
                            },
                        },
                    }
                ],
            },
            model_year=2024,
        )
        assert RulesRegistry()["irrigation"](inactive_ctx) == []


def test_validate_doy_parameters_catches_post_load_unpaired_irrigation() -> None:
    """Post-construction mutation leaving only one irrigation DOY fails iff modelled irrigation is active."""
    cfg_active = SUEWSConfig(sites=[Site()])
    cfg_active.sites[0].properties.irrigation.daywat.monday = 1.0
    cfg_active.sites[0].properties.irrigation.ie_start = 120
    with pytest.raises(
        ValueError, match="Both ie_start and ie_end must be specified together"
    ):
        cfg_active.validate_doy_parameters()

    cfg_inactive = SUEWSConfig(sites=[Site()])
    cfg_inactive.sites[0].properties.irrigation.ie_start = 120
    cfg_inactive.validate_doy_parameters()


def test_scheme_gated_irrigation_predicate_and_cli_engine_boundaries(
    tmp_path: Path,
) -> None:
    """Scheme-gated irrigation predicate, invalid water_use isolation, CLI, and engine handoff."""
    assert is_modelled_irrigation_active(0, {"daywat": {"monday": 1.0}}) is True
    assert is_modelled_irrigation_active("modelled", {"daywat": {"sunday": 1.0}}) is True
    assert is_modelled_irrigation_active(1, {"daywat": {"monday": 1.0}}) is False
    assert is_modelled_irrigation_active("observed", {"daywat": {"monday": 1.0}}) is False
    assert is_modelled_irrigation_active(0, {"daywat": {"monday": 0.0}}) is False
    assert is_modelled_irrigation_active("not_a_method", {"daywat": {"monday": 1.0}}) is False

    sample_dir = files("supy").joinpath("sample_data")
    for item in sample_dir.iterdir():
        if item.is_file():
            (tmp_path / item.name).write_bytes(item.read_bytes())
    sample_payload = yaml.safe_load(
        (tmp_path / "sample_config.yml").read_text(encoding="utf-8")
    )

    # Invalid water_use selector must still fail on its own (never silently coerced).
    bad_wu = copy.deepcopy(sample_payload)
    bad_wu["model"]["physics"]["water_use"] = "bogus_method"
    with pytest.raises(ValidationError):
        SUEWSConfig(**bad_wu)

    # Inactive observed irrigation accepts unpaired and out-of-range/fractional ie_start/ie_end
    # through SUEWSConfig, installed CLI, and engine handoff.
    inactive_obs = copy.deepcopy(sample_payload)
    inactive_obs["model"]["physics"]["water_use"] = {"value": 1}
    inactive_obs["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = 1.0
    inactive_obs["sites"][0]["properties"]["irrigation"]["ie_start"] = {"value": 20.5}
    inactive_obs["sites"][0]["properties"]["irrigation"]["ie_end"] = None
    inactive_path = tmp_path / "inactive_obs.yml"
    inactive_path.write_text(
        yaml.safe_dump(inactive_obs, sort_keys=False), encoding="utf-8"
    )

    loaded_inactive = SUEWSConfig.from_yaml(str(inactive_path))
    loaded_inactive.validate_for_engine()

    # Active modelled irrigation rejects unpaired ie_start/ie_end at SUEWSConfig and installed CLI.
    active_unpaired = copy.deepcopy(sample_payload)
    active_unpaired["model"]["physics"]["water_use"] = {"value": 0}
    active_unpaired["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = 1.0
    active_unpaired["sites"][0]["properties"]["irrigation"]["ie_start"] = {"value": 120}
    active_unpaired["sites"][0]["properties"]["irrigation"]["ie_end"] = None
    active_path = tmp_path / "active_unpaired.yml"
    active_path.write_text(
        yaml.safe_dump(active_unpaired, sort_keys=False), encoding="utf-8"
    )

    with pytest.raises(
        ValueError, match="Both ie_start and ie_end must be specified together"
    ):
        SUEWSConfig.from_yaml(str(active_path))

    suews_exe = Path(sysconfig.get_path("scripts")) / (
        "suews" + (".exe" if os.name == "nt" else "")
    )
    assert suews_exe.is_file(), f"Missing installed CLI: {suews_exe}"

    ok_cli = subprocess.run(
        [
            str(suews_exe),
            "validate",
            "-p",
            "BC",
            "--forcing",
            "off",
            "--format",
            "json",
            str(inactive_path),
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=180,
    )
    assert ok_cli.returncode == 0, ok_cli.stdout + ok_cli.stderr
    ok_env = json.loads(ok_cli.stdout)
    assert ok_env["errors"] == []

    fail_cli = subprocess.run(
        [
            str(suews_exe),
            "validate",
            "-p",
            "BC",
            "--forcing",
            "off",
            "--format",
            "json",
            str(active_path),
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=180,
    )
    assert fail_cli.returncode == 1, fail_cli.stdout + fail_cli.stderr
    fail_env = json.loads(fail_cli.stdout)
    assert fail_env["status"] == "error"
    assert "Both ie_start and ie_end must be specified together" in fail_cli.stdout


def test_shared_spartacus_and_shading_invariants_match_phase_b_and_models() -> None:
    """Shared kernel functions drive both Pydantic models and Phase B rules."""
    # SPARTACUS observed storage heat rejected in kernel and Phase B
    kernel_v = check_spartacus_storage_heat(1003, 0, bldgs_present=True)
    phase_b_v = validate_spartacus_dependencies(True, 1003, 0)
    assert len(kernel_v) == 1
    assert len(phase_b_v) == 1
    assert phase_b_v[0].status == "ERROR"
    assert phase_b_v[0].message == kernel_v[0].message

    # Internal shading mode=1 missing reduction_factor_shading rejected in both
    with pytest.raises(ValidationError, match="reduction_factor_shading must be provided"):
        StebbsProperties(internal_shading=1)

    ctx = ValidationContext(
        yaml_data={
            "model": {"physics": {"stebbs": {"enabled": True, "parameters": "default"}}},
            "sites": [
                {
                    "gridiv": 1,
                    "properties": {"stebbs": {"internal_shading": 1}},
                }
            ],
        }
    )
    shading_results = RulesRegistry()["internal_shading"](ctx)
    assert any(
        r.status == "ERROR" and "reduction_factor_shading must be provided" in r.message
        for r in shading_results
    )

    # Same-surface uniformity check catches layer/archetype mismatch directly
    surface_v = check_same_surface_property(
        [0.2, 0.3],
        0.2,
        site_name="TestSite",
        layers_attr="walls",
        layer_field="alb",
        archetype_attr="reflectivity_wall_external",
        param_name="same_albedo_wall",
        property_name="albedo",
    )
    assert any("must be identical" in v.message for v in surface_v)

    # DLS pair kernel catches leap-year overflow and flags half-configured pair
    dls_v, half, _, _ = check_dls_doy_pair(366, None, model_year=2023, site_name="KCL")
    assert half is True
    assert any("365" in v.message for v in dls_v)

    # Irrigation pair kernel rejects explicit 0 and accepts both-None
    irr_v, _, _ = check_irrigation_doy_pair(0, 200, model_year=2024, site_name="KCL")
    assert any("between 1 and 366" in v.message for v in irr_v)
    ok_v, s_int, e_int = check_irrigation_doy_pair(None, None, model_year=2024)
    assert ok_v == [] and s_int is None and e_int is None


def test_rules_kernel_hardening_nan_gridid_shading_and_typed_view() -> None:
    """Kernel rejects NaN/inf, explicit-None shading, unhashable modes, and frozen RefValue gridiv."""
    # NaN / inf must be rejected by check_same_surface_property; original indices preserved.
    nan_v = check_same_surface_property(
        [float("nan"), 0.2, 0.8],
        0.5,
        site_name="S1",
        layers_attr="walls",
        layer_field="alb",
        archetype_attr="reflectivity_wall_external",
        param_name="same_albedo_wall",
        property_name="albedo",
    )
    assert any("not a finite number" in v.message for v in nan_v)
    assert any("walls[1]=0.2, walls[2]=0.8" in v.message for v in nan_v)

    # Explicit None with require_explicit_presence=True is rejected; unhashable mode does not crash.
    null_sh = check_stebbs_internal_shading(
        1,
        {"reduction_factor_shading": None},
        present_fields={"reduction_factor_shading"},
        require_explicit_presence=True,
    )
    assert any(v.code == "STEBBS.REDUCTION_FACTOR_SHADING.MISSING" for v in null_sh)
    bad_mode = check_stebbs_internal_shading([], {})
    assert any(v.code == "STEBBS.INTERNAL_SHADING.INVALID_MODE" for v in bad_mode)
    huge_sh = check_stebbs_internal_shading(1, {"reduction_factor_shading": 10**400})
    assert any(v.code == "STEBBS.REDUCTION_FACTOR_SHADING.OUT_OF_RANGE" for v in huge_sh)

    # TypedView handles RefValue gridiv without MappingProxyType pickle errors, exact SPARTACUS, overflow.
    assert as_float(10**400) is None
    assert method_code([]) is None
    assert method_code(1.5) is None
    assert method_code("bogus") is None
    assert PhysicsView({"net_radiation": "1001"}).is_spartacus_enabled is True
    assert PhysicsView({"net_radiation": "not_spartacus"}).is_spartacus_enabled is False
    assert PhysicsView({"net_radiation": 1001.5}).is_spartacus_enabled is False

    ctx = ValidationContext(
        yaml_data={
            "model": {"physics": {"net_radiation": 1001, "stebbs": {"same_albedo_wall": 1}}},
            "sites": [
                {
                    "name": "Urban",
                    "gridiv": {"value": 42, "ref": {"desc": "id"}},
                    "properties": {
                        "land_cover": {
                            "bldgs": {"value": {"sfr": 0.6}, "ref": {"desc": "wrapped"}},
                            "paved": {"sfr": 0.4},
                        },
                        "vertical_layers": {
                            "building_frac": [0.2],
                            "walls": [{"alb": 0.2}, {"alb": 0.4}],
                        },
                        "building_archetype": {"reflectivity_wall_external": 0.2},
                    },
                }
            ],
        }
    )
    view = TypedConfigView.from_mapping(ctx.yaml_data)
    assert dict(view.sites[0].surface_fractions()) == {"bldgs": 0.6, "paved": 0.4}
    adv = check_spartacus_lowest_layer_advisories(view.sites[0], sfr_tol=1e-4)
    assert adv and adv[0].site_gridid == 42
    # Must serialise cleanly via ValidationResult.to_dict() (no MappingProxyType pickle error).
    assert ValidationResult.from_advisory(adv[0]).to_dict()["site_gridid"] == 42

    # Same-surface advisory preserves real site_index and scalar site_gridid.
    ctx_adv = ValidationContext(
        yaml_data={
            "model": {"physics": {"stebbs": {"same_albedo_wall": 0}}},
            "sites": [
                {
                    "name": "Urban",
                    "gridiv": {"value": 42, "ref": {"desc": "id"}},
                    "properties": {
                        "vertical_layers": {"walls": [{"alb": 0.2}, {"alb": 0.4}]},
                        "building_archetype": {"reflectivity_wall_external": 0.2},
                    },
                }
            ],
        }
    )
    same_alb = RulesRegistry()["same_albedo"](ctx_adv)
    assert len(same_alb) == 1
    assert same_alb[0].status == "WARNING"
    assert same_alb[0].site_index == 0
    assert same_alb[0].site_gridid == 42
    assert same_alb[0].to_dict()["site_gridid"] == 42

    # ValidationResult.from_violation preserves the kernel violation code.
    from_v = ValidationResult.from_violation(nan_v[0])
    assert from_v.code == nan_v[0].code

    # Leap-year irrigation advisory counts 366-day wrap accurately.
    leap_adv = check_irrigation_season_advisory(366, 1, 51.5, "London", model_year=2024)
    assert any("spans 1 days" in f.message for f in leap_adv)

    # ValidationResult preserves explicit severity and rejects invalid mutations.
    res = ValidationResult(status="PASS", severity="ERROR", category="TEST", parameter="x")
    assert res.severity == "ERROR"
    with pytest.raises(ValueError, match="Invalid validation severity"):
        res.severity = "NOT_A_SEVERITY"


@pytest.mark.parametrize(
    ("label", "water_use", "daywat_monday"),
    [
        ("observed_water_use_with_daywat_on", 1, 1.0),
        ("modelled_water_use_with_all_daywat_zero", 0, 0.0),
    ],
)
def test_inactive_irrigation_survives_df_state_roundtrip(
    label: str, water_use: int, daywat_monday: float
) -> None:
    """Inactive irrigation configs with unused invalid DOYs survive to_df_state -> from_df_state."""
    from supy.data_model.core.config import SUEWSConfig

    sample_path = (
        Path(__file__).resolve().parents[2]
        / "src"
        / "supy"
        / "sample_data"
        / "sample_config.yml"
    )
    import yaml

    raw = yaml.safe_load(sample_path.read_text(encoding="utf-8"))
    raw["model"]["physics"]["water_use"] = {"value": water_use}
    irr = raw["sites"][0]["properties"]["irrigation"]
    for day in (
        "monday",
        "tuesday",
        "wednesday",
        "thursday",
        "friday",
        "saturday",
        "sunday",
    ):
        irr["daywat"][day] = 0.0
    irr["daywat"]["monday"] = daywat_monday
    irr["ie_start"] = {"value": 20.5}
    irr["ie_end"] = None

    cfg = SUEWSConfig.model_validate(raw)
    df_state = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(df_state)
    rebuilt_irr = rebuilt.sites[0].properties.irrigation
    rebuilt_start = (
        rebuilt_irr.ie_start.value
        if hasattr(rebuilt_irr.ie_start, "value")
        else rebuilt_irr.ie_start
    )
    assert rebuilt_start == pytest.approx(20.5), label
    assert rebuilt_irr.ie_end is None, label

    # Also verify legacy df_state with stale out-of-range inactive ie_start loads cleanly.
    df_stale = df_state.copy()
    df_stale.loc[df_stale.index[0], ("ie_start", "0")] = 400
    df_stale.loc[df_stale.index[0], ("ie_end", "0")] = 0
    rebuilt_stale = SUEWSConfig.from_df_state(df_stale)
    stale_irr = rebuilt_stale.sites[0].properties.irrigation
    stale_start = (
        stale_irr.ie_start.value
        if hasattr(stale_irr.ie_start, "value")
        else stale_irr.ie_start
    )
    assert stale_start == 400, label
    assert stale_irr.ie_end is None, label


    # Also verify container/string inactive placeholders do not break df_state or SUEWSSimulation.
    for opaque_start, opaque_end in (
        ([20, 30], None),
        ([20], [30]),
        ({"unexpected": 1}, "abc"),
        (float("nan"), None),
    ):
        raw_opaque = copy.deepcopy(raw)
        raw_opaque["sites"][0]["properties"]["irrigation"]["ie_start"] = opaque_start
        raw_opaque["sites"][0]["properties"]["irrigation"]["ie_end"] = opaque_end
        cfg_opaque = SUEWSConfig.model_validate(raw_opaque)
        df_opaque = cfg_opaque.to_df_state()
        assert df_opaque.loc[df_opaque.index[0], ("ie_start", "0")] == 0, label
        assert df_opaque.loc[df_opaque.index[0], ("ie_end", "0")] == 0, label
        rebuilt_opaque = SUEWSConfig.from_df_state(df_opaque)
        assert rebuilt_opaque.sites[0].properties.irrigation.ie_start is None, label
        assert rebuilt_opaque.sites[0].properties.irrigation.ie_end is None, label
        sim = SUEWSSimulation()
        sim.update_config(cfg_opaque)
        assert sim.state_init is not None, label


def test_daywat_string_flag_parity_between_raw_mapping_and_model() -> None:
    """Raw mappings and validated models agree on string-numeric and boolean daywat flags."""
    import yaml
    from pydantic import ValidationError
    from supy.data_model.core.config import SUEWSConfig
    from supy.data_model.validation.pipeline.phase_b_rules.other_rules import (
        validate_irrigation_parameters,
    )
    from supy.data_model.validation.rules.hard_invariants import (
        is_modelled_irrigation_active,
    )

    sample_path = (
        Path(__file__).resolve().parents[2]
        / "src"
        / "supy"
        / "sample_data"
        / "sample_config.yml"
    )
    raw = yaml.safe_load(sample_path.read_text(encoding="utf-8"))
    raw["model"]["physics"]["water_use"] = {"value": 0}
    irr = raw["sites"][0]["properties"]["irrigation"]
    for day in (
        "monday",
        "tuesday",
        "wednesday",
        "thursday",
        "friday",
        "saturday",
        "sunday",
    ):
        irr["daywat"][day] = "0.0"
    irr["ie_start"] = {"value": 20.5}
    irr["ie_end"] = None

    for active_flag in ("1.0", True):
        irr["daywat"]["monday"] = active_flag
        assert is_modelled_irrigation_active(0, irr) is True
        phase_b_results = validate_irrigation_parameters(
            ValidationContext(yaml_data=raw, model_year=2024)
        )
        assert any(r.status == "ERROR" for r in phase_b_results)
        with pytest.raises(ValidationError):
            SUEWSConfig.model_validate(raw)

    # Controls: string zero and boolean False remain inactive in both raw mapping and model.
    for inactive_flag in ("0.0", False):
        irr["daywat"]["monday"] = inactive_flag
        assert is_modelled_irrigation_active(0, irr) is False
        phase_b_inactive = validate_irrigation_parameters(
            ValidationContext(yaml_data=raw, model_year=2024)
        )
        assert not any(r.status == "ERROR" for r in phase_b_inactive)
        cfg = SUEWSConfig.model_validate(raw)
        assert cfg is not None


def test_df_state_nan_doy_rejected_when_active_accepted_when_inactive_and_water_use_method_alias() -> None:
    """from_df_state rejects NaN DOYs when active, keeps 0/0 disabled, accepts inactive NaN, and honours water_use_method."""
    sample_path = (
        Path(__file__).resolve().parents[2]
        / "src"
        / "supy"
        / "sample_data"
        / "sample_config.yml"
    )
    raw = yaml.safe_load(sample_path.read_text(encoding="utf-8"))
    raw["model"]["physics"]["water_use"] = {"value": 0}
    irr = raw["sites"][0]["properties"]["irrigation"]
    irr["daywat"]["monday"] = 1.0
    irr["ie_start"] = {"value": 120}
    irr["ie_end"] = {"value": 240}

    active_cfg = SUEWSConfig.model_validate(raw)
    df_active_nan = active_cfg.to_df_state()
    df_active_nan.loc[df_active_nan.index[0], ("ie_start", "0")] = float("nan")
    df_active_nan.loc[df_active_nan.index[0], ("ie_end", "0")] = float("nan")
    with pytest.raises(ValidationError):
        SUEWSConfig.from_df_state(df_active_nan)

    # Explicit 0/0 sentinel in active df_state decodes to disabled None/None cleanly.
    df_active_zero = active_cfg.to_df_state()
    df_active_zero.loc[df_active_zero.index[0], ("ie_start", "0")] = 0
    df_active_zero.loc[df_active_zero.index[0], ("ie_end", "0")] = 0
    rebuilt_zero = SUEWSConfig.from_df_state(df_active_zero)
    assert rebuilt_zero.sites[0].properties.irrigation.ie_start is None
    assert rebuilt_zero.sites[0].properties.irrigation.ie_end is None

    # Inactive df_state with NaN bounds loads cleanly and updates SUEWSSimulation.
    df_inactive_nan = df_active_nan.copy()
    df_inactive_nan.loc[df_inactive_nan.index[0], ("waterusemethod", "0")] = 1
    rebuilt_inactive = SUEWSConfig.from_df_state(df_inactive_nan)
    sim = SUEWSSimulation()
    sim.update_config(rebuilt_inactive)
    assert sim.state_init is not None

    # Legacy alias water_use_method=1 also relaxes child DOY validation on direct construction.
    raw_alias = copy.deepcopy(raw)
    raw_alias["model"]["physics"].pop("water_use", None)
    raw_alias["model"]["physics"]["water_use_method"] = {"value": 1}
    raw_alias["sites"][0]["properties"]["irrigation"]["ie_start"] = {"value": 20.5}
    raw_alias["sites"][0]["properties"]["irrigation"]["ie_end"] = None
    cfg_alias_direct = SUEWSConfig(**raw_alias)
    assert cfg_alias_direct.sites[0].properties.irrigation.ie_end is None
    cfg_alias_dict = SUEWSConfig.from_dict(raw_alias)
    assert cfg_alias_dict.sites[0].properties.irrigation.ie_end is None


def test_inactive_to_active_irrigation_live_mutation_enforces_strict_doy() -> None:
    """Activating an inactive config keeps strict integer-DOY bounds on live mutations."""
    config_to_engine_yaml = import_module("supy._run_rust").config_to_engine_yaml
    raw_sample = yaml.safe_load(
        files("supy")
        .joinpath("sample_data", "sample_config.yml")
        .read_text(encoding="utf-8")
    )

    # Inactive sample (water_use=0, all daywat=0.0) activated via daywat.monday=1.0
    # must reject booleans, fractional floats, and out-of-range DOYs immediately on
    # both .value = ... and direct attribute assignment (without coercing True -> 1).
    cfg = SUEWSConfig.model_validate(copy.deepcopy(raw_sample))
    irr = cfg.sites[0].properties.irrigation
    assert irr.ie_start.value == 1
    assert irr.ie_end.value == 366

    irr.daywat.monday = 1.0

    for bad_bool in (True, False, np.bool_(True)):
        with pytest.raises(ValueError, match="boolean"):
            irr.ie_start.value = bad_bool
        assert irr.ie_start.value == 1
        assert type(irr.ie_start.value) is int

    with pytest.raises(ValueError, match="between 1 and 366"):
        irr.ie_start.value = 0
    with pytest.raises(ValueError, match="between 1 and 366"):
        irr.ie_end.value = 367
    with pytest.raises(ValueError, match="integer day of year"):
        irr.ie_start.value = 20.5

    # Direct field replacement after daywat.monday=1.0 also enforces strict validation.
    with pytest.raises(ValueError, match="boolean"):
        irr.ie_start = True  # type: ignore[assignment]
    with pytest.raises(ValueError, match="boolean"):
        irr.ie_start = {"value": True}  # type: ignore[assignment]
    with pytest.raises(ValueError, match="between 1 and 366"):
        irr.ie_start = 0
    with pytest.raises(ValueError, match="between 1 and 366"):
        irr.ie_end = 367

    # Deep copy of an inactive config keeps strict wrapper validators after activation.
    cfg_inactive_copy = copy.deepcopy(SUEWSConfig.model_validate(copy.deepcopy(raw_sample)))
    cfg_inactive_copy.sites[0].properties.irrigation.daywat.monday = 1.0
    with pytest.raises(ValueError, match="boolean"):
        cfg_inactive_copy.sites[0].properties.irrigation.ie_start.value = True

    # Valid whole-number float/int mutations still succeed and export cleanly.
    irr.ie_start.value = 100.0
    irr.ie_end.value = 250
    assert irr.ie_start.value == 100
    assert type(irr.ie_start.value) is int
    assert config_to_engine_yaml(cfg)


def test_inactive_to_active_irrigation_rejects_sentinel_and_bool_at_engine_handoff() -> None:
    """Raw/sentinel/bool inactive DOYs are preserved while inactive and rejected once active."""
    config_to_engine_yaml = import_module("supy._run_rust").config_to_engine_yaml
    raw_sample = yaml.safe_load(
        files("supy")
        .joinpath("sample_data", "sample_config.yml")
        .read_text(encoding="utf-8")
    )

    for bad_val, expected_pattern in (
        (0, r"between 1 and 366"),
        (367, r"between 1 and 366"),
        (True, r"boolean"),
        (False, r"boolean"),
        (20.5, r"integer day of year"),
    ):
        raw_bad = copy.deepcopy(raw_sample)
        raw_bad["sites"][0]["properties"]["irrigation"]["ie_start"] = {"value": bad_val}
        raw_bad["sites"][0]["properties"]["irrigation"]["ie_end"] = {"value": 250}

        cfg_bad = SUEWSConfig.model_validate(raw_bad)
        irr_bad = cfg_bad.sites[0].properties.irrigation
        assert irr_bad.ie_start.value == bad_val
        assert type(irr_bad.ie_start.value) is type(bad_val)
        assert config_to_engine_yaml(cfg_bad)

        irr_bad.daywat.monday = 1.0
        with pytest.raises(ValueError, match=expected_pattern):
            config_to_engine_yaml(cfg_bad)
        with pytest.raises(ValidationError):
            SUEWSConfig.model_validate(cfg_bad.model_dump())

    # SUEWSSimulation.update_config() + run() reject inactive -> active invalid DOYs.
    sim = SUEWSSimulation.from_sample_data()
    raw_sim_bad = copy.deepcopy(raw_sample)
    raw_sim_bad["sites"][0]["properties"]["irrigation"]["ie_start"] = {"value": True}
    raw_sim_bad["sites"][0]["properties"]["irrigation"]["ie_end"] = {"value": 250}
    cfg_sim_bad = SUEWSConfig.model_validate(raw_sim_bad)
    forcing = sim.forcing.df.iloc[:12].copy()
    sim.update_forcing(forcing)
    sim.update_config(cfg_sim_bad)
    sim.config.sites[0].properties.irrigation.daywat.monday = 1.0
    with pytest.raises(ValueError, match="boolean"):
        sim.run(start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1)

    # water_use=OBSERVED -> MODELLED transition syncs wrappers and restores strict guards.
    raw_obs = copy.deepcopy(raw_sample)
    raw_obs["model"]["physics"]["water_use"] = {"value": 1}
    raw_obs["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = 1.0
    cfg_obs = SUEWSConfig.model_validate(raw_obs)
    irr_obs = cfg_obs.sites[0].properties.irrigation
    irr_obs.ie_start = RefValue[int](value=100)
    cfg_obs.model.physics.water_use = WaterUseMethod.MODELLED
    # Even before engine export, the RefValue wrapper rejects bool/OOB .value mutations.
    with pytest.raises(ValueError, match="boolean"):
        irr_obs.ie_start.value = True
    assert config_to_engine_yaml(cfg_obs)
    with pytest.raises(ValueError, match="between 1 and 366"):
        irr_obs.ie_start.value = 0
    with pytest.raises(ValueError, match="between 1 and 366"):
        irr_obs.ie_end.value = 367
    with pytest.raises(ValueError, match="boolean"):
        irr_obs.ie_start = True  # type: ignore[assignment]

    # Observed config with preserved boolean DOY fails on engine export after switching to MODELLED.
    raw_obs_bool = copy.deepcopy(raw_obs)
    raw_obs_bool["sites"][0]["properties"]["irrigation"]["ie_start"] = {"value": True}
    cfg_obs_bool = SUEWSConfig.model_validate(raw_obs_bool)
    assert config_to_engine_yaml(cfg_obs_bool)
    cfg_obs_bool.model.physics.water_use = WaterUseMethod.MODELLED
    with pytest.raises(ValueError, match="boolean"):
        config_to_engine_yaml(cfg_obs_bool)


def test_boolean_physics_selectors_preserve_phase_b_compatibility_semantics() -> None:
    """Boolean physics selectors normalise to 0/1 in method_code and Phase B rules."""
    assert method_code(False) == 0
    assert method_code(True) == 1
    assert method_code({"value": False}) == 0
    assert method_code({"value": True}) == 1

    for ohm_false in (False, {"value": False}):
        res = validate_storageheatmethod_dependencies(1, ohm_false)
        assert res[0].status == "PASS"

    for ohm_true in (True, {"value": True}):
        res = validate_storageheatmethod_dependencies(5, ohm_true)
        assert res[0].status == "ERROR"

    for storage_false in (False, {"value": False}):
        res = validate_spartacus_dependencies(True, 1003, storage_false)
        assert len(res) == 1
        assert res[0].status == "ERROR"


@pytest.mark.parametrize(
    "bad_z",
    [
        float("inf"),
        float("-inf"),
        float("nan"),
        {"value": float("inf")},
        {"value": float("-inf")},
        {"value": float("nan")},
    ],
    ids=["inf", "-inf", "nan", "wrapped-inf", "wrapped--inf", "wrapped-nan"],
)
def test_forcing_height_rule_reports_non_finite_z_instead_of_skipping(bad_z) -> None:
    """Non-finite properties.z emits a FORCING error rather than being treated as absent."""
    rule = RulesRegistry()["forcing_height"]
    for bldgs_sfr in (0.4, 0.0):
        ctx = ValidationContext(
            yaml_data={
                "model": {"physics": {}},
                "sites": [
                    {
                        "name": "TestSite",
                        "gridiv": 1,
                        "properties": {
                            "z": bad_z,
                            "land_cover": {
                                "bldgs": {"sfr": bldgs_sfr, "bldgh": 10.0}
                            },
                        },
                    }
                ],
            }
        )
        res = rule(ctx)
        assert any(
            r.status == "ERROR"
            and r.category == "FORCING"
            and r.parameter == "properties.z"
            for r in res
        ), res

    # Control: genuinely absent / null z still skips cleanly.
    for absent_z in (None, {"value": None}):
        ctx_absent = ValidationContext(
            yaml_data={
                "model": {"physics": {}},
                "sites": [
                    {
                        "name": "TestSite",
                        "gridiv": 1,
                        "properties": {
                            "z": absent_z,
                            "land_cover": {"bldgs": {"sfr": 0.4, "bldgh": 10.0}},
                        },
                    }
                ],
            }
        )
        assert rule(ctx_absent) == []


def test_oversized_inactive_irrigation_doy_exports_safely_and_fails_when_active() -> None:
    """Oversized integer placeholders (10**400) project to 0 in df_state when inactive and fail when active."""
    raw_sample = yaml.safe_load(
        files("supy")
        .joinpath("sample_data", "sample_config.yml")
        .read_text(encoding="utf-8")
    )
    huge = 10**400

    for entry in (huge, {"value": huge}):
        for water_use, daywat_mon in ((1, 1.0), (0, 0.0)):
            raw_inactive = copy.deepcopy(raw_sample)
            raw_inactive["model"]["physics"]["water_use"] = {"value": water_use}
            irr = raw_inactive["sites"][0]["properties"]["irrigation"]
            irr["daywat"]["monday"] = daywat_mon
            irr["ie_start"] = copy.deepcopy(entry)
            irr["ie_end"] = None

            cfg = SUEWSConfig.model_validate(raw_inactive)
            df_state = cfg.to_df_state()
            assert df_state.loc[df_state.index[0], ("ie_start", "0")] == 0
            assert df_state.loc[df_state.index[0], ("ie_end", "0")] == 0
            sim = SUEWSSimulation()
            sim.update_config(cfg)
            assert sim.state_init is not None

        raw_active = copy.deepcopy(raw_sample)
        raw_active["model"]["physics"]["water_use"] = {"value": 0}
        raw_active["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = 1.0
        raw_active["sites"][0]["properties"]["irrigation"]["ie_start"] = copy.deepcopy(entry)
        raw_active["sites"][0]["properties"]["irrigation"]["ie_end"] = {"value": 250}
        with pytest.raises(ValidationError):
            SUEWSConfig.model_validate(raw_active)


def test_numpy_daywat_scalars_enforce_active_irrigation_before_df_state_export() -> None:
    """NumPy scalar weekday flags match built-in scalars on live mutation and to_df_state()."""
    config_to_engine_yaml = import_module("supy._run_rust").config_to_engine_yaml
    raw_sample = yaml.safe_load(
        files("supy")
        .joinpath("sample_data", "sample_config.yml")
        .read_text(encoding="utf-8")
    )

    assert is_modelled_irrigation_active(np.int64(0), {"daywat": {"monday": np.int64(1)}}) is True
    assert is_modelled_irrigation_active(0, {"daywat": {"monday": np.float64(1.0)}}) is True
    assert is_modelled_irrigation_active(0, {"daywat": {"monday": np.bool_(True)}}) is True
    assert is_modelled_irrigation_active(0, {"daywat": {"monday": np.int64(0)}}) is False
    assert is_modelled_irrigation_active(0, {"daywat": {"monday": np.float64(np.nan)}}) is False

    for flag_val in (1, np.int64(1), np.float64(1.0)):
        # Live direct assignment must reject invalid DOY once a NumPy weekday flag is enabled.
        cfg_live = SUEWSConfig.model_validate(copy.deepcopy(raw_sample))
        irr_live = cfg_live.sites[0].properties.irrigation
        irr_live.daywat.monday = flag_val
        with pytest.raises(ValueError, match="boolean"):
            irr_live.ie_start = True  # type: ignore[assignment]

        # Pre-existing inactive invalid bounds must raise on to_df_state() before lossy 0/0 projection.
        raw_bad = copy.deepcopy(raw_sample)
        raw_bad["sites"][0]["properties"]["irrigation"]["ie_start"] = {"value": True}
        raw_bad["sites"][0]["properties"]["irrigation"]["ie_end"] = None
        cfg_export = SUEWSConfig.model_validate(raw_bad)
        cfg_export.sites[0].properties.irrigation.daywat.monday = flag_val
        with pytest.raises(ValueError, match="boolean"):
            cfg_export.to_df_state()
        with pytest.raises(ValueError, match="boolean"):
            config_to_engine_yaml(cfg_export)


def test_water_use_scheme_transitions_update_direct_doy_assignment_immediately() -> None:
    """Switching model.physics.water_use updates direct DOY assignment before any export."""
    raw_sample = yaml.safe_load(
        files("supy")
        .joinpath("sample_data", "sample_config.yml")
        .read_text(encoding="utf-8")
    )

    # OBSERVED -> MODELLED (no intervening export/validation): direct field replacement rejects immediately.
    raw_obs = copy.deepcopy(raw_sample)
    raw_obs["model"]["physics"]["water_use"] = {"value": 1}
    raw_obs["sites"][0]["properties"]["irrigation"]["daywat"]["monday"] = 1.0
    cfg = SUEWSConfig.model_validate(raw_obs)
    irr = cfg.sites[0].properties.irrigation
    cfg.model.physics.water_use = WaterUseMethod.MODELLED
    with pytest.raises(ValueError, match="boolean"):
        irr.ie_start = True  # type: ignore[assignment]
    with pytest.raises(ValueError, match="between 1 and 366"):
        irr.ie_start = 0

    # MODELLED -> OBSERVED (no intervening export/validation): inactive placeholder assignment is permitted.
    cfg.model.physics.water_use = WaterUseMethod.OBSERVED
    irr.ie_start = 20.5  # type: ignore[assignment]
    assert irr.ie_start == pytest.approx(20.5)

    # Both copy.deepcopy() and model_copy(deep=True) rebind live parent links in both directions.
    for clone in (copy.deepcopy(cfg), cfg.model_copy(deep=True)):
        clone.model.physics.water_use = WaterUseMethod.MODELLED
        with pytest.raises(ValueError, match="boolean"):
            clone.sites[0].properties.irrigation.ie_start = True  # type: ignore[assignment]
        clone.model.physics.water_use = WaterUseMethod.OBSERVED
        clone.sites[0].properties.irrigation.ie_start = 42.5  # type: ignore[assignment]
        assert clone.sites[0].properties.irrigation.ie_start == pytest.approx(42.5)

    # from_df_state() also wires live parent link before any export.
    rebuilt = SUEWSConfig.from_df_state(cfg.to_df_state())
    rebuilt.model.physics.water_use = WaterUseMethod.MODELLED
    with pytest.raises(ValueError, match="boolean"):
        rebuilt.sites[0].properties.irrigation.ie_start = True  # type: ignore[assignment]


def test_phase_b_cli_reports_malformed_sites_instead_of_silently_skipping(
    tmp_path: Path,
) -> None:
    """Standalone Phase B reports non-mapping site entries rather than dropping them."""
    sample_dir = files("supy").joinpath("sample_data")
    sample_payload = yaml.safe_load(
        sample_dir.joinpath("sample_config.yml").read_text(encoding="utf-8")
    )
    suews_exe = Path(sysconfig.get_path("scripts")) / (
        "suews" + (".exe" if os.name == "nt" else "")
    )
    assert suews_exe.is_file(), f"Missing installed CLI: {suews_exe}"

    cases = (
        ("all_null", [None], "sites[0]"),
        ("mixed_valid_and_null", [copy.deepcopy(sample_payload["sites"][0]), None], "sites[1]"),
        ("null_container", None, "sites"),
    )
    for label, bad_sites, expected_param in cases:
        payload = copy.deepcopy(sample_payload)
        payload["sites"] = bad_sites
        cfg_path = tmp_path / f"bad_sites_{label}.yml"
        cfg_path.write_text(yaml.safe_dump(payload, sort_keys=False), encoding="utf-8")

        proc = subprocess.run(
            [
                str(suews_exe),
                "validate",
                "-p",
                "B",
                "--science-fixes",
                "off",
                "--forcing",
                "off",
                "--format",
                "json",
                str(cfg_path),
            ],
            cwd=tmp_path,
            capture_output=True,
            text=True,
            timeout=180,
            check=False,
        )
        assert proc.returncode != 0, f"{label}: {proc.stdout}\n{proc.stderr}"
        envelope = json.loads(proc.stdout)
        assert envelope["status"] == "error", f"{label}: {envelope}"
        assert expected_param in proc.stdout, f"{label}: {proc.stdout}"
