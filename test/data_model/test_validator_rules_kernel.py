"""Unit and contract tests for Wave 3 shared Tier-1 hard-invariant kernel and typed Phase B rules."""

from __future__ import annotations

import ast
from pathlib import Path

from pydantic import ValidationError
import pytest

from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.human_activity import IrrigationParams
from supy.data_model.core.site import Site, StebbsProperties
from supy.data_model.validation.pipeline.phase_b_rules import (
    RulesRegistry,
    ValidationContext,
    validate_irrigation_doy,
)
from supy.data_model.validation.pipeline.phase_b_rules.physics_rules import (
    validate_spartacus_dependencies,
)
from supy.data_model.validation.rules.hard_invariants import (
    check_dls_doy_pair,
    check_irrigation_doy_pair,
    check_same_surface_property,
    check_spartacus_storage_heat,
)
from supy.data_model.validation.rules.typed_view import TypedConfigView

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
def test_irrigation_window_pairing_enforced_in_model_and_phase_b(ie_start, ie_end) -> None:
    """Half-configured irrigation windows fail identically in Pydantic and Phase B."""
    with pytest.raises(ValidationError, match="Both ie_start and ie_end must be specified together"):
        IrrigationParams(ie_start=ie_start, ie_end=ie_end)

    start_scalar = ie_start["value"] if isinstance(ie_start, dict) and "value" in ie_start else ie_start
    end_scalar = ie_end["value"] if isinstance(ie_end, dict) and "value" in ie_end else ie_end
    phase_b_results = validate_irrigation_doy(start_scalar, end_scalar, 51.5, 2024, "KCL")
    assert any(
        r.status == "ERROR" and "Both ie_start and ie_end must be specified together" in r.message
        for r in phase_b_results
    )


def test_validate_doy_parameters_catches_post_load_unpaired_irrigation() -> None:
    """Post-construction mutation leaving only one irrigation DOY fails at engine handoff."""
    cfg = SUEWSConfig(sites=[Site()])
    cfg.sites[0].properties.irrigation.ie_start = 120
    with pytest.raises(ValueError, match="Both ie_start and ie_end must be specified together"):
        cfg.validate_doy_parameters()


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
