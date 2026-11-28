"""Scheme-gated validation tests for STEBBS and building archetype (#2363, #2348).

Enforces the standing validation principle: STEBBS subordinate physics switches,
``StebbsProperties``, and ``ArchetypeProperties`` constraints are not enforced
while STEBBS is inactive (``stebbs.enabled == False`` / ``stebbsmethod == 0``),
while remaining strictly validated when STEBBS is active (``stebbsmethod != 0``),
on standalone model construction, and after live post-construction activation.
"""

from __future__ import annotations

import copy
from importlib.resources import files
import json
from pathlib import Path
import warnings

from pydantic import ValidationError
import pytest
import yaml

from supy._run_rust import config_to_engine_yaml
from supy.data_model.configuration.publisher import generate_json_schema
from supy.data_model.core import SUEWSConfig
from supy.data_model.core.model import (
    REQUIRED_STEBBS_SWITCHES,
    ModelPhysics,
    RCMethod,
    SameAlbedoWall,
    SetpointMethod,
    StebbsPhysics,
)
from supy.data_model.core.site import ArchetypeProperties, StebbsProperties
from supy.data_model.core.type import RefValue

pytestmark = [
    pytest.mark.unit,
    pytest.mark.cfg,
    pytest.mark.core,
    pytest.mark.contract("C-VALID", "C-EMIT", "C-ROUNDTRIP"),
]

PROJECT_ROOT = Path(__file__).resolve().parents[2]


@pytest.fixture(scope="module")
def sample_dict() -> dict:
    """Load packaged sample configuration (``stebbs.enabled: false``)."""
    text = (
        files("supy")
        .joinpath("sample_data", "sample_config.yml")
        .read_text(encoding="utf-8")
    )
    return yaml.safe_load(text)


def _assert_clean_json_dump(cfg: SUEWSConfig) -> dict:
    """Dump *cfg* in JSON mode and verify no Pydantic serialisation warnings occur."""
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        dumped = cfg.model_dump(exclude_none=True, mode="json")
    ser_warnings = [
        w for w in caught if "PydanticSerializationUnexpectedValue" in str(w.message)
    ]
    assert not ser_warnings, f"Unexpected serialisation warnings: {ser_warnings}"
    return dumped


def _enable_stebbs(data: dict, *, parameters: int = 1) -> None:
    """Enable STEBBS cleanly on *data* with default or provided parameters."""
    physics = data["model"]["physics"]
    physics.pop("stebbsmethod", None)
    physics.pop("stebbs_method", None)
    physics["stebbs"] = {
        "enabled": True,
        "parameters": parameters,
        "capacitance": 0,
        "setpoint": 0,
        "same_albedo_wall": 0,
        "same_albedo_roof": 0,
        "same_emissivity_wall": 0,
        "same_emissivity_roof": 0,
    }


def _disable_stebbs(data: dict) -> None:
    physics = data["model"]["physics"]
    physics.pop("stebbsmethod", None)
    physics.pop("stebbs_method", None)
    physics["stebbs"] = {
        "enabled": False,
        "parameters": 1,
        "capacitance": 0,
        "setpoint": 0,
        "same_albedo_wall": 0,
        "same_albedo_roof": 0,
        "same_emissivity_wall": 0,
        "same_emissivity_roof": 0,
    }


# =============================================================================
# 1. Inactive STEBBS subordinate physics switches
# =============================================================================


@pytest.mark.parametrize("switch_name", REQUIRED_STEBBS_SWITCHES)
@pytest.mark.parametrize(
    "relaxed_value",
    [
        None,
        {"value": None},
        "TBD",
        {"value": "TBD"},
        "",
        -999,
        {"value": 99},
    ],
)
def test_inactive_stebbs_relaxes_subordinate_switches(
    sample_dict: dict, tmp_path: Path, switch_name: str, relaxed_value: object
) -> None:
    """When ``stebbs.enabled`` is False, subordinate switches accept null/placeholder/relaxed values."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["model"]["physics"]["stebbs"][switch_name] = relaxed_value

    cfg = SUEWSConfig.from_dict(data)
    cfg.validate_for_engine()
    _assert_clean_json_dump(cfg)
    yaml_text = config_to_engine_yaml(cfg, for_file_output=False)
    assert isinstance(yaml_text, str) and yaml_text

    # YAML round-trip preserves explicit nulls and placeholders without error.
    out_yaml = tmp_path / f"{switch_name}.yml"
    cfg.to_yaml(str(out_yaml))
    reloaded = SUEWSConfig.from_yaml(str(out_yaml))
    reloaded.validate_for_engine()
    if relaxed_value is None:
        assert getattr(reloaded.model.physics.stebbs, switch_name) is None
    elif isinstance(relaxed_value, dict) and relaxed_value.get("value") is None:
        raw_val = getattr(reloaded.model.physics.stebbs, switch_name)
        assert isinstance(raw_val, RefValue) and raw_val.value is None

    # DataFrame state round-trip succeeds cleanly.
    df_state = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(df_state)
    rebuilt.validate_for_engine()
    assert [s.gridiv for s in rebuilt.sites] == [s.gridiv for s in cfg.sites]


def test_inactive_stebbs_resolves_valid_readable_names_and_skips_same_surface_checks(
    sample_dict: dict,
) -> None:
    """Inactive STEBBS keeps valid readable switch names resolved and skips same_albedo/emissivity checks."""
    data = copy.deepcopy(sample_dict)
    data["model"]["physics"]["stebbs"] = {
        "enabled": False,
        "capacitance": "parameterise",
        "setpoint": {"value": "scheduled"},
        "same_albedo_wall": "enabled",
        "same_albedo_roof": 1,
        "same_emissivity_wall": "enabled",
        "same_emissivity_roof": 1,
    }
    # Deliberately introduce mismatched wall/roof albedo & emissivity across layers
    # and against building_archetype -- must NOT fail while STEBBS is disabled.
    vl = data["sites"][0]["properties"]["vertical_layers"]
    vl["walls"][0]["alb"] = {"value": 0.1}
    vl["walls"][1]["alb"] = {"value": 0.9}
    vl["walls"][0]["emis"] = {"value": 0.7}
    vl["walls"][1]["emis"] = {"value": 0.95}
    vl["roofs"][0]["alb"] = {"value": 0.2}
    vl["roofs"][1]["alb"] = {"value": 0.8}
    vl["roofs"][0]["emis"] = {"value": 0.6}
    vl["roofs"][1]["emis"] = {"value": 0.99}

    cfg = SUEWSConfig.from_dict(data)
    cfg.validate_for_engine()
    stebbs_phys = cfg.model.physics.stebbs
    assert stebbs_phys.capacitance.value == RCMethod.PARAMETERISE
    assert stebbs_phys.setpoint.value == SetpointMethod.SCHEDULED
    assert stebbs_phys.same_albedo_wall.value == SameAlbedoWall.ENABLED


@pytest.mark.parametrize(
    "physics_patch",
    [
        {"stebbsmethod": 0, "rcmethod": "TBD", "setpointmethod": None},
        {"stebbs": "none", "rcmethod": {"value": "TBD"}, "same_albedo_wall": 1},
        {"stebbs": {"value": 0}, "outer_cap_fraction": -999},
        {"stebbs": {"capacitance": "TBD", "setpoint": None}},
    ],
)
def test_inactive_stebbs_legacy_and_omitted_enabled_forms(
    sample_dict: dict, physics_patch: dict
) -> None:
    """Flat ``stebbsmethod: 0``, ``stebbs: none``, and omitted ``stebbs.enabled`` relax subordinate switches."""
    data = copy.deepcopy(sample_dict)
    physics = data["model"]["physics"]
    physics.pop("stebbs", None)
    physics.update(physics_patch)
    # Also mismatch wall albedo so same_albedo_wall=1 proves same-surface is gated.
    data["sites"][0]["properties"]["vertical_layers"]["walls"][0]["alb"] = {
        "value": 0.1
    }
    data["sites"][0]["properties"]["vertical_layers"]["walls"][1]["alb"] = {
        "value": 0.8
    }

    with warnings.catch_warnings():
        warnings.simplefilter("ignore", DeprecationWarning)
        cfg = SUEWSConfig.from_dict(data)
    cfg.validate_for_engine()
    _assert_clean_json_dump(cfg)
    assert cfg.model.physics.stebbs.enabled == False  # noqa: E712


# =============================================================================
# 2. Inactive STEBBS properties & building_archetype
# =============================================================================


@pytest.mark.parametrize(
    ("mutator", "description"),
    [
        (
            lambda d: d["sites"][0]["properties"].__setitem__("stebbs", None),
            "null properties.stebbs block",
        ),
        (
            lambda d: d["sites"][0]["properties"].pop("stebbs", None),
            "omitted properties.stebbs block",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__(
                "building_archetype", None
            ),
            "null properties.building_archetype block",
        ),
        (
            lambda d: d["sites"][0]["properties"].pop("building_archetype", None),
            "omitted properties.building_archetype block",
        ),
        (
            lambda d: d["sites"][0]["properties"]["stebbs"].update(
                convection_coefficient_wall_internal=-999.0,
                depth_ground=0.0,
                efficiency_heating_system_air=1.5,
                fraction_convective_heating=-0.2,
                setpoint_temperature_heating_water=150.0,
                internal_shading=3,
                destination_waste_heat=-1,
                volume_hot_water={"value": "TBD"},
                temperature_air_annual_mean="TBD",
                profile_appliance=None,
                profile_flow_hot_water={"value": None},
            ),
            "relaxed/null/placeholder StebbsProperties fields",
        ),
        (
            lambda d: (
                d["sites"][0]["properties"]["stebbs"].__setitem__(
                    "internal_shading", {"value": 1}
                ),
                d["sites"][0]["properties"]["stebbs"].pop(
                    "reduction_factor_shading", None
                ),
            ),
            "internal_shading=1 without reduction_factor_shading when STEBBS off",
        ),
        (
            lambda d: (
                d["sites"][0]["properties"]["stebbs"].__setitem__(
                    "internal_shading", {"value": 2}
                ),
                d["sites"][0]["properties"]["stebbs"].pop(
                    "temperature_threshold_shading", None
                ),
                d["sites"][0]["properties"]["stebbs"].pop(
                    "radiation_threshold_shading", None
                ),
            ),
            "internal_shading=2 without thresholds when STEBBS off",
        ),
        (
            lambda d: d["sites"][0]["properties"]["building_archetype"].update(
                archetype_height=-10.0,
                thickness_wall=0.0,
                ratio_window_to_wall=1.8,
                occupants=-5,
                setpoint_temperature_heating_air=45.0,
                setpoint_temperature_cooling_air=5.0,
                area_footprint={"value": "TBD"},
                hhs0={"value": None},
                age_0_4=-0.5,
                age_65plus={"value": "TBD"},
                profile_metabolism=None,
                profile_setpoint_temperature_heating_air={"value": None},
            ),
            "relaxed/null/placeholder ArchetypeProperties fields",
        ),
        (
            lambda d: (
                d["sites"][0]["properties"]["stebbs"].pop("volume_hot_water", None),
                d["sites"][0]["properties"]["stebbs"].__setitem__(
                    "DHWWaterVolume", -999.0
                ),
            ),
            "legacy renamed StebbsProperties field with sentinel",
        ),
    ],
)
def test_inactive_stebbs_relaxes_properties_and_archetype(
    sample_dict: dict, tmp_path: Path, mutator, description: str
) -> None:
    """When STEBBS is inactive, relaxed/null/placeholder STEBBS and archetype blocks load and export."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    mutator(data)

    with warnings.catch_warnings():
        warnings.simplefilter("ignore", DeprecationWarning)
        cfg = SUEWSConfig.from_dict(data)
    cfg.validate_for_engine()
    _assert_clean_json_dump(cfg)
    yaml_text = config_to_engine_yaml(cfg, for_file_output=False)
    assert isinstance(yaml_text, str) and yaml_text, description

    # YAML round-trip
    out_yaml = tmp_path / "inactive_stebbs.yml"
    cfg.to_yaml(str(out_yaml))
    reloaded = SUEWSConfig.from_yaml(str(out_yaml))
    reloaded.validate_for_engine()

    # df_state round-trip
    df_state = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(df_state)
    rebuilt.validate_for_engine()
    assert [s.gridiv for s in rebuilt.sites] == [s.gridiv for s in cfg.sites], (
        description
    )


def test_inactive_stebbs_preserves_explicit_null_blocks_across_yaml_roundtrip(
    sample_dict: dict, tmp_path: Path
) -> None:
    """Explicit ``properties.stebbs: null`` and ``building_archetype: null`` survive ``to_yaml``."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["sites"][0]["properties"]["stebbs"] = None
    data["sites"][0]["properties"]["building_archetype"] = None

    cfg = SUEWSConfig.from_dict(data)
    assert cfg.sites[0].properties.stebbs is None
    assert cfg.sites[0].properties.building_archetype is None

    out_yaml = tmp_path / "null_blocks.yml"
    cfg.to_yaml(str(out_yaml))
    reloaded = SUEWSConfig.from_yaml(str(out_yaml))
    assert reloaded.sites[0].properties.stebbs is None
    assert reloaded.sites[0].properties.building_archetype is None


# =============================================================================
# 3. Structural / non-finite / master-switch invariants stay strictly enforced
# =============================================================================


@pytest.mark.parametrize(
    ("mutator", "pattern"),
    [
        (
            lambda d: d["model"]["physics"]["stebbs"].__setitem__("enabled", None),
            r"enabled",
        ),
        (
            lambda d: d["model"]["physics"]["stebbs"].__setitem__(
                "enabled", {"value": None}
            ),
            r"enabled",
        ),
        (
            lambda d: d["model"]["physics"]["stebbs"].__setitem__("parameters", None),
            r"parameters",
        ),
        (
            lambda d: d["model"]["physics"]["stebbs"].__setitem__(
                "capacitance", float("nan")
            ),
            r"capacitance",
        ),
        (
            lambda d: d["model"]["physics"]["stebbs"].__setitem__(
                "setpoint", {"value": float("inf")}
            ),
            r"setpoint",
        ),
        (
            lambda d: d["model"]["physics"]["stebbs"].__setitem__(
                "capacitance", [1, 2]
            ),
            r"capacitance",
        ),
        (
            lambda d: d["sites"][0]["properties"]["stebbs"].__setitem__(
                "depth_ground", float("nan")
            ),
            r"depth_ground",
        ),
        (
            lambda d: d["sites"][0]["properties"]["stebbs"].__setitem__(
                "fraction_convective_heating", {"value": float("inf")}
            ),
            r"fraction_convective_heating",
        ),
        (
            lambda d: d["sites"][0]["properties"]["building_archetype"].__setitem__(
                "thickness_wall", {"value": float("nan")}
            ),
            r"thickness_wall",
        ),
        (
            lambda d: d["sites"][0]["properties"]["stebbs"].__setitem__(
                "deep_soil_temperature", {"value": 99.0}
            ),
            r"deep_soil_temperature is not an input",
        ),
    ],
)
def test_inactive_stebbs_still_rejects_master_nulls_nonfinite_and_structural_errors(
    sample_dict: dict, mutator, pattern: str
) -> None:
    """Master-switch nulls, NaN/Inf, container shapes, and deep_soil_temperature mismatches always fail."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    mutator(data)

    with pytest.raises(ValueError, match=pattern):
        SUEWSConfig.from_dict(data)


# =============================================================================
# 4. Active STEBBS (stebbsmethod 1 and 2) remains strictly validated
# =============================================================================


@pytest.mark.parametrize("parameters", [1, 2])
@pytest.mark.parametrize(
    ("mutator", "pattern"),
    [
        (
            lambda d: d["model"]["physics"]["stebbs"].__setitem__("capacitance", None),
            r"capacitance",
        ),
        (
            lambda d: d["model"]["physics"]["stebbs"].__setitem__(
                "capacitance", {"value": None}
            ),
            r"capacitance",
        ),
        (
            lambda d: d["model"]["physics"]["stebbs"].__setitem__("capacitance", "TBD"),
            r"capacitance",
        ),
        (
            lambda d: d["model"]["physics"]["stebbs"].__setitem__("setpoint", -999),
            r"setpoint",
        ),
        (
            lambda d: d["model"]["physics"]["stebbs"].__setitem__(
                "same_albedo_wall", "TBD"
            ),
            r"same_albedo_wall",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__("stebbs", None),
            r"stebbs",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__(
                "building_archetype", None
            ),
            r"building_archetype|archetype_name",
        ),
        (
            lambda d: d["sites"][0]["properties"]["stebbs"].__setitem__(
                "depth_ground", -1.0
            ),
            r"depth_ground",
        ),
        (
            lambda d: d["sites"][0]["properties"]["stebbs"].__setitem__(
                "depth_ground", "TBD"
            ),
            r"depth_ground",
        ),
        (
            lambda d: d["sites"][0]["properties"]["stebbs"].__setitem__(
                "internal_shading", 3
            ),
            r"internal_shading",
        ),
        (
            lambda d: (
                d["sites"][0]["properties"]["stebbs"].__setitem__(
                    "internal_shading", 1
                ),
                d["sites"][0]["properties"]["stebbs"].pop(
                    "reduction_factor_shading", None
                ),
            ),
            r"reduction_factor_shading",
        ),
        (
            lambda d: d["sites"][0]["properties"]["stebbs"].__setitem__(
                "destination_waste_heat", 2
            ),
            r"destination_waste_heat",
        ),
        (
            lambda d: d["sites"][0]["properties"]["building_archetype"].__setitem__(
                "thickness_wall", -0.5
            ),
            r"thickness_wall",
        ),
        (
            lambda d: d["sites"][0]["properties"]["building_archetype"].__setitem__(
                "ratio_window_to_wall", 1.5
            ),
            r"ratio_window_to_wall",
        ),
        (
            lambda d: d["sites"][0]["properties"]["building_archetype"].__setitem__(
                "hhs0", {"value": None}
            ),
            r"hhs0",
        ),
    ],
)
def test_active_stebbs_enforces_strict_validation(
    sample_dict: dict, parameters: int, mutator, pattern: str
) -> None:
    """When STEBBS is active (parameters=1 or 2), all switch/property/archetype rules remain strict."""
    data = copy.deepcopy(sample_dict)
    _enable_stebbs(data, parameters=parameters)
    mutator(data)

    with pytest.raises(ValueError, match=pattern):
        SUEWSConfig.from_dict(data)


def test_active_stebbs_same_albedo_and_emissivity_checks_remain_strict(
    sample_dict: dict,
) -> None:
    """When STEBBS is active, ``same_albedo_*`` / ``same_emissivity_*`` mismatches are rejected."""
    data = copy.deepcopy(sample_dict)
    _enable_stebbs(data, parameters=1)
    data["model"]["physics"]["stebbs"]["same_albedo_wall"] = 1
    data["sites"][0]["properties"]["vertical_layers"]["walls"][0]["alb"] = {
        "value": 0.1
    }
    data["sites"][0]["properties"]["vertical_layers"]["walls"][1]["alb"] = {
        "value": 0.8
    }

    with pytest.raises(ValueError, match="all walls albedo values must be identical"):
        SUEWSConfig.from_dict(data)


# =============================================================================
# 5. Standalone construction & explicit StebbsPhysics(enabled=False)
# =============================================================================


def test_standalone_models_remain_strict() -> None:
    """Standalone construction without inactive STEBBS context stays strictly validated."""
    with pytest.raises(ValidationError):
        StebbsPhysics(capacitance="TBD")
    with pytest.raises(ValidationError):
        StebbsPhysics(enabled=True, capacitance="TBD")
    with pytest.raises(ValidationError):
        StebbsProperties(depth_ground=-1.0)
    with pytest.raises(ValidationError):
        StebbsProperties(internal_shading=1)
    with pytest.raises(ValidationError):
        ArchetypeProperties(thickness_wall=-1.0)
    with pytest.raises(ValidationError):
        ArchetypeProperties(hhs0={"value": None})

    # Explicit enabled=False on StebbsPhysics / ModelPhysics relaxes subordinate switches.
    sp_off = StebbsPhysics(enabled=False, capacitance="TBD", setpoint=None)
    assert sp_off.enabled is False
    assert sp_off.capacitance == "TBD"
    assert sp_off.setpoint is None

    mp_off = ModelPhysics(stebbs={"enabled": False, "capacitance": "TBD"})
    assert mp_off.stebbs.capacitance == "TBD"


# =============================================================================
# 6. Live post-construction transitions, RefValue guards, and deep-copy isolation
# =============================================================================


@pytest.mark.parametrize("parameters", [1, 2])
@pytest.mark.parametrize(
    ("setup_inactive", "pattern"),
    [
        (
            lambda d: d["model"]["physics"]["stebbs"].__setitem__("capacitance", "TBD"),
            r"capacitance",
        ),
        (
            lambda d: d["model"]["physics"]["stebbs"].__setitem__(
                "setpoint", {"value": None}
            ),
            r"setpoint",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__("stebbs", None),
            r"stebbs",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__(
                "building_archetype", None
            ),
            r"building_archetype|archetype_name",
        ),
        (
            lambda d: d["sites"][0]["properties"]["stebbs"].__setitem__(
                "depth_ground", -5.0
            ),
            r"depth_ground",
        ),
        (
            lambda d: (
                d["sites"][0]["properties"]["stebbs"].__setitem__(
                    "internal_shading", 1
                ),
                d["sites"][0]["properties"]["stebbs"].pop(
                    "reduction_factor_shading", None
                ),
            ),
            r"reduction_factor_shading",
        ),
        (
            lambda d: d["sites"][0]["properties"]["building_archetype"].__setitem__(
                "thickness_wall", -2.0
            ),
            r"thickness_wall",
        ),
        (
            lambda d: d["sites"][0]["properties"]["building_archetype"].__setitem__(
                "hhs0", {"value": None}
            ),
            r"hhs0",
        ),
    ],
)
def test_live_inactive_to_active_transition_rejects_relaxed_values(
    sample_dict: dict, parameters: int, setup_inactive, pattern: str
) -> None:
    """Flipping ``stebbs.enabled`` False -> True blocks engine handoff on previously relaxed values."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["model"]["physics"]["stebbs"]["parameters"] = parameters
    setup_inactive(data)

    cfg = SUEWSConfig.from_dict(data)
    cfg.validate_for_engine()

    # Activate STEBBS in-place and verify validate_for_engine / to_df_state / engine YAML reject.
    cfg.model.physics.stebbs.enabled = True
    with pytest.raises(ValueError, match=pattern):
        cfg.validate_for_engine()
    with pytest.raises(ValueError, match=pattern):
        cfg.to_df_state()
    with pytest.raises(ValueError, match=pattern):
        config_to_engine_yaml(cfg, for_file_output=False)


def test_live_refvalue_guard_and_deepcopy_isolation(sample_dict: dict) -> None:
    """Live ``RefValue.value = None`` respects parent ``stebbs.enabled`` and isolates deep copies."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["model"]["physics"]["stebbs"]["capacitance"] = {"value": 0}
    data["sites"][0]["properties"]["building_archetype"]["hhs0"] = {"value": 0.0}

    cfg = SUEWSConfig.from_dict(data)
    cloned = copy.deepcopy(cfg)

    # While inactive on original, in-place RefValue.value = None succeeds.
    cfg.model.physics.stebbs.capacitance.value = None
    cfg.sites[0].properties.building_archetype.hhs0.value = None
    cfg.validate_for_engine()

    # Activating STEBBS on the deep copy does NOT affect the original, and blocks null on the copy.
    cloned.model.physics.stebbs.enabled = True
    with pytest.raises(ValueError, match="capacitance"):
        cloned.model.physics.stebbs.capacitance.value = None
    with pytest.raises(ValueError, match="hhs0"):
        cloned.sites[0].properties.building_archetype.hhs0.value = None

    # Original remains inactive and valid.
    cfg.validate_for_engine()

    # Reset relaxed capacitance/hhs0, then replace site.properties.stebbs via dict
    # to confirm dict assignment inherits parent STEBBS state and fails on activation.
    cfg.model.physics.stebbs.capacitance = 0
    cfg.sites[0].properties.building_archetype.hhs0 = 0.0
    cfg.sites[0].properties.stebbs = {"depth_ground": -5.0}
    assert cfg.sites[0].properties.stebbs.depth_ground == -5.0
    cfg.model.physics.stebbs.enabled = True
    with pytest.raises(ValueError, match="depth_ground"):
        cfg.validate_for_engine()


def test_inactive_numeric_strings_preserve_numeric_values_before_and_after_activation(
    sample_dict: dict,
) -> None:
    """Quoted numeric strings remain numeric when inactive and in engine YAML after activation."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["sites"][0]["properties"]["stebbs"]["temperature_air_annual_mean"] = "12.5"
    data["sites"][0]["properties"]["stebbs"]["depth_ground"] = "4.5"
    data["sites"][0]["properties"]["stebbs"]["internal_shading"] = "1"

    cfg = SUEWSConfig.from_dict(data)
    stebbs = cfg.sites[0].properties.stebbs
    assert stebbs.temperature_air_annual_mean == 12.5
    assert isinstance(stebbs.temperature_air_annual_mean, float)
    assert stebbs.deep_soil_temperature == 12.5
    assert stebbs.depth_ground == 4.5
    assert isinstance(stebbs.depth_ground, float)
    assert stebbs.internal_shading == 1
    assert isinstance(stebbs.internal_shading, int)

    engine_yaml_inactive = yaml.safe_load(
        config_to_engine_yaml(cfg, for_file_output=False)
    )
    assert (
        engine_yaml_inactive["sites"][0]["properties"]["stebbs"][
            "temperature_air_annual_mean"
        ]
        == 12.5
    )

    # Even if a numeric string is bypassed onto an inactive RefValue before live activation,
    # active revalidation syncs the coerced numeric value back onto the live config and YAML.
    object.__setattr__(stebbs, "depth_ground", RefValue(value="4.5"))
    stebbs.model_fields_set.add("depth_ground")
    cfg.model.physics.stebbs.enabled = True
    cfg.validate_for_engine()
    assert stebbs.depth_ground.value == 4.5
    assert isinstance(stebbs.depth_ground.value, float)
    engine_yaml_active = yaml.safe_load(
        config_to_engine_yaml(cfg, for_file_output=False)
    )
    emitted_depth = engine_yaml_active["sites"][0]["properties"]["stebbs"][
        "depth_ground"
    ]
    emitted_val = (
        emitted_depth["value"] if isinstance(emitted_depth, dict) else emitted_depth
    )
    assert emitted_val == 4.5
    assert isinstance(emitted_val, float)


@pytest.mark.parametrize("parameters", [1, 2])
@pytest.mark.parametrize(
    ("section", "field_name"),
    [
        ("stebbs", "depth_ground"),
        ("stebbs", "convection_coefficient_roof_internal"),
        ("stebbs", "convection_coefficient_roof_external"),
        ("building_archetype", "thickness_roof"),
    ],
)
def test_active_stebbs_rejects_consumed_nulls_for_both_parameter_sources(
    sample_dict: dict, parameters: int, section: str, field_name: str
) -> None:
    """Consumed STEBBS/roof null parameters are rejected for both parameters=1 and 2."""
    # Direct active construction
    data = copy.deepcopy(sample_dict)
    _enable_stebbs(data, parameters=parameters)
    data["sites"][0]["properties"][section][field_name] = None
    with pytest.raises((ValidationError, ValueError), match=field_name):
        SUEWSConfig.from_dict(data)

    # Inactive load -> live False -> True activation
    inactive = copy.deepcopy(sample_dict)
    _disable_stebbs(inactive)
    inactive["model"]["physics"]["stebbs"]["parameters"] = parameters
    inactive["sites"][0]["properties"][section][field_name] = None
    cfg = SUEWSConfig.from_dict(inactive)
    cfg.validate_for_engine()
    cfg.model.physics.stebbs.enabled = True
    with pytest.raises(ValueError, match=field_name):
        cfg.validate_for_engine()


def test_active_stebbs_capacitance_dependent_nulls_rejected(sample_dict: dict) -> None:
    """Active capacitance=1/2 rejects null roof-fraction / outer-layer parameters."""
    data_rc1 = copy.deepcopy(sample_dict)
    _enable_stebbs(data_rc1, parameters=1)
    data_rc1["model"]["physics"]["stebbs"]["capacitance"] = 1
    data_rc1["sites"][0]["properties"]["building_archetype"][
        "fraction_heat_capacity_roof_external"
    ] = None
    with pytest.raises(
        (ValidationError, ValueError), match="fraction_heat_capacity_roof_external"
    ):
        SUEWSConfig.from_dict(data_rc1)

    data_rc2 = copy.deepcopy(sample_dict)
    _enable_stebbs(data_rc2, parameters=2)
    data_rc2["model"]["physics"]["stebbs"]["capacitance"] = 2
    data_rc2["sites"][0]["properties"]["building_archetype"]["thickness_roof_outer"] = (
        None
    )
    with pytest.raises((ValidationError, ValueError), match="thickness_roof_outer"):
        SUEWSConfig.from_dict(data_rc2)


@pytest.mark.parametrize(
    ("section", "profile_field"),
    [
        ("stebbs", "profile_flow_hot_water"),
        ("building_archetype", "profile_metabolism"),
    ],
)
def test_inactive_ten_minute_profile_slot_placeholders_roundtrip_and_reject_on_activation(
    sample_dict: dict, tmp_path: Path, section: str, profile_field: str
) -> None:
    """Inactive 10-minute profiles accept placeholder slot strings, keep key checks, and fail on activation."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    profile = {
        "working_day": {str(i): (0.0 if i != 1 else "TBD") for i in range(1, 145)},
        "holiday": {str(i): (0.0 if i != 2 else "0.25") for i in range(1, 145)},
    }
    data["sites"][0]["properties"][section][profile_field] = profile

    cfg = SUEWSConfig.from_dict(data)
    cfg.validate_for_engine()
    prof_obj = getattr(cfg.sites[0].properties, section)
    stored_prof = getattr(prof_obj, profile_field)
    assert stored_prof.working_day["1"] == "TBD"
    assert stored_prof.holiday["2"] == 0.25

    yaml_path = tmp_path / f"inactive_{profile_field}.yml"
    cfg.to_yaml(yaml_path)
    reloaded = SUEWSConfig.from_yaml(yaml_path)
    reloaded_prof = getattr(
        getattr(reloaded.sites[0].properties, section), profile_field
    )
    assert reloaded_prof.working_day["1"] == "TBD"
    assert reloaded_prof.holiday["2"] == 0.25

    df_state = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(df_state)
    rebuilt_prof = getattr(getattr(rebuilt.sites[0].properties, section), profile_field)
    assert rebuilt_prof.working_day["1"] == "TBD"

    # Flipping enabled -> True rejects the placeholder slot inside the 10-minute profile.
    cfg.model.physics.stebbs.enabled = True
    with pytest.raises(ValueError, match=profile_field):
        cfg.validate_for_engine()

    # Non-finite values and invalid keys inside inactive profiles remain rejected.
    bad_nan = copy.deepcopy(data)
    bad_nan["sites"][0]["properties"][section][profile_field]["working_day"]["1"] = (
        "nan"
    )
    with pytest.raises((ValidationError, ValueError)):
        SUEWSConfig.from_dict(bad_nan)

    bad_key = copy.deepcopy(data)
    bad_key["sites"][0]["properties"][section][profile_field]["working_day"]["145"] = (
        "TBD"
    )
    with pytest.raises(
        (ValidationError, ValueError), match="Invalid 10-minute profile"
    ):
        SUEWSConfig.from_dict(bad_key)


def test_standalone_stebbs_physics_refvalue_guard_tracks_enabled_mutations() -> None:
    """Standalone StebbsPhysics updates subordinate RefValue guards on enabled toggles."""
    phys = StebbsPhysics(enabled=False, capacitance={"value": 0})
    phys.enabled = True
    with pytest.raises(ValueError, match="capacitance"):
        phys.capacitance.value = None

    phys.enabled = False
    phys.capacitance.value = None
    assert phys.capacitance.value is None


def test_to_yaml_preserves_ref_metadata_on_inactive_null_refvalues(
    sample_dict: dict, tmp_path: Path
) -> None:
    """YAML null restoration preserves RefValue ``ref`` metadata across switches, stebbs, and archetype."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["model"]["physics"]["stebbs"]["capacitance"] = {
        "value": None,
        "ref": {"desc": "switch estimate"},
    }
    data["sites"][0]["properties"]["stebbs"]["volume_hot_water"] = {
        "value": None,
        "ref": {"desc": "site estimate"},
    }
    data["sites"][0]["properties"]["building_archetype"]["hhs0"] = {
        "value": None,
        "ref": {"desc": "archetype estimate"},
    }

    cfg = SUEWSConfig.from_dict(data)
    yaml_path = tmp_path / "null_ref_metadata.yml"
    cfg.to_yaml(yaml_path)
    dumped = yaml.safe_load(yaml_path.read_text(encoding="utf-8"))
    assert dumped["model"]["physics"]["stebbs"]["capacitance"] == {
        "value": None,
        "ref": {"desc": "switch estimate"},
    }
    assert dumped["sites"][0]["properties"]["stebbs"]["volume_hot_water"] == {
        "value": None,
        "ref": {"desc": "site estimate"},
    }
    assert dumped["sites"][0]["properties"]["building_archetype"]["hhs0"] == {
        "value": None,
        "ref": {"desc": "archetype estimate"},
    }

    reloaded = SUEWSConfig.from_yaml(yaml_path)
    assert reloaded.model.physics.stebbs.capacitance.value is None
    assert reloaded.model.physics.stebbs.capacitance.ref.desc == "switch estimate"
    assert reloaded.sites[0].properties.stebbs.volume_hot_water.value is None
    assert (
        reloaded.sites[0].properties.stebbs.volume_hot_water.ref.desc == "site estimate"
    )
    assert reloaded.sites[0].properties.building_archetype.hhs0.value is None
    assert (
        reloaded.sites[0].properties.building_archetype.hhs0.ref.desc
        == "archetype estimate"
    )


@pytest.mark.parametrize(
    "field_name",
    [
        "temperature_air_annual_mean",
        "temperature_air_month_mean_diffmax",
        "temperature_air_indoor_initial",
    ],
)
@pytest.mark.parametrize("bad_value", ["TBD", None])
def test_ehc_implicit_and_dyohm_protect_shared_stebbs_temperatures_when_stebbs_off(
    sample_dict: dict, field_name: str, bad_value
) -> None:
    """EHC implicit (5, coupling=1) and DyOHM (6) keep shared STEBBS temperatures strict."""
    # EHC implicit consumes all three boundary temperatures even with STEBBS off.
    data_ehc = copy.deepcopy(sample_dict)
    _disable_stebbs(data_ehc)
    data_ehc["model"]["physics"]["storage_heat"] = 5
    data_ehc["model"]["physics"]["ehc_surface_coupling"] = 1
    data_ehc["sites"][0]["properties"]["stebbs"][field_name] = bad_value
    with pytest.raises((ValidationError, ValueError), match=field_name):
        SUEWSConfig.from_dict(data_ehc)

    # Pure-STEBBS fields (depth_ground, building_archetype) remain relaxed under EHC implicit.
    data_ehc_pure = copy.deepcopy(sample_dict)
    _disable_stebbs(data_ehc_pure)
    data_ehc_pure["model"]["physics"]["storage_heat"] = 5
    data_ehc_pure["model"]["physics"]["ehc_surface_coupling"] = 1
    data_ehc_pure["sites"][0]["properties"]["stebbs"]["depth_ground"] = "TBD"
    data_ehc_pure["sites"][0]["properties"]["building_archetype"] = None
    cfg_ehc_pure = SUEWSConfig.from_dict(data_ehc_pure)
    assert cfg_ehc_pure.sites[0].properties.stebbs.depth_ground == "TBD"
    assert cfg_ehc_pure.sites[0].properties.building_archetype is None

    # EHC explicit (coupling=0) relaxes all three temperatures when STEBBS is off.
    data_ehc_exp = copy.deepcopy(sample_dict)
    _disable_stebbs(data_ehc_exp)
    data_ehc_exp["model"]["physics"]["storage_heat"] = 5
    data_ehc_exp["model"]["physics"]["ehc_surface_coupling"] = 0
    data_ehc_exp["sites"][0]["properties"]["stebbs"][field_name] = bad_value
    cfg_exp = SUEWSConfig.from_dict(data_ehc_exp)
    assert getattr(cfg_exp.sites[0].properties.stebbs, field_name) == bad_value

    # DyOHM (6) consumes temperature_air_annual_mean even with STEBBS off.
    if field_name == "temperature_air_annual_mean":
        data_dyohm = copy.deepcopy(sample_dict)
        _disable_stebbs(data_dyohm)
        data_dyohm["model"]["physics"]["storage_heat"] = 6
        data_dyohm["sites"][0]["properties"]["stebbs"][field_name] = bad_value
        with pytest.raises((ValidationError, ValueError), match=field_name):
            SUEWSConfig.from_dict(data_dyohm)


def test_live_master_switch_assignments_normalised_and_validated(
    sample_dict: dict, tmp_path: Path
) -> None:
    """Live assignments to ``stebbs.enabled`` and ``stebbs.parameters`` validate consistently."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["model"]["physics"]["stebbs"]["capacitance"] = "TBD"

    cfg = SUEWSConfig.from_dict(data)
    cfg.model.physics.stebbs.enabled = "false"
    assert cfg.model.physics.stebbs.enabled is False
    df_state = cfg.to_df_state()
    assert int(df_state.loc[1, ("stebbsmethod", "0")]) == 0

    with pytest.raises((ValidationError, ValueError)):
        cfg.model.physics.stebbs.parameters = 99
    with pytest.raises((ValidationError, ValueError)):
        cfg.model.physics.stebbs.enabled = "bogus"

    # Even if bypassed with object.__setattr__, engine/YAML/DataFrame export rejects parameters=99.
    object.__setattr__(cfg.model.physics.stebbs, "parameters", 99)
    with pytest.raises(ValueError, match="stebbs"):
        cfg.validate_for_engine()
    with pytest.raises(ValueError, match="stebbs"):
        cfg.to_yaml(tmp_path / "bad_master.yml")
    with pytest.raises(ValueError, match="STEBBS"):
        cfg.to_df_state()


@pytest.mark.parametrize("parameters", [1, 2])
@pytest.mark.parametrize(
    ("section", "profile_field"),
    [
        ("stebbs", "profile_flow_hot_water"),
        ("building_archetype", "profile_metabolism"),
    ],
)
@pytest.mark.parametrize("bad_slot", [None, float("nan"), float("inf")])
def test_active_stebbs_rejects_null_and_non_finite_ten_minute_profile_slots(
    sample_dict: dict, parameters: int, section: str, profile_field: str, bad_slot
) -> None:
    """Active STEBBS rejects null and non-finite 10-minute profile slots for parameters=1 and 2."""
    profile = {
        "working_day": {str(i): (0.0 if i != 1 else bad_slot) for i in range(1, 145)},
        "holiday": {str(i): 0.0 for i in range(1, 145)},
    }
    data = copy.deepcopy(sample_dict)
    _enable_stebbs(data, parameters=parameters)
    data["sites"][0]["properties"][section][profile_field] = copy.deepcopy(profile)
    with pytest.raises((ValidationError, ValueError), match=profile_field):
        SUEWSConfig.from_dict(data)

    if bad_slot is None:
        # Inactive load -> live activation must also reject null profile slots.
        inactive = copy.deepcopy(sample_dict)
        _disable_stebbs(inactive)
        inactive["model"]["physics"]["stebbs"]["parameters"] = parameters
        inactive["sites"][0]["properties"][section][profile_field] = copy.deepcopy(
            profile
        )
        cfg = SUEWSConfig.from_dict(inactive)
        cfg.validate_for_engine()
        cfg.model.physics.stebbs.enabled = True
        with pytest.raises(ValueError, match=profile_field):
            cfg.validate_for_engine()


def test_inactive_multi_site_null_ten_minute_profile_slots_roundtrip_df_state(
    sample_dict: dict,
) -> None:
    """Multi-site df_state round-trip preserves inactive None profile slots despite pandas NaN."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    site1 = copy.deepcopy(data["sites"][0])
    site2 = copy.deepcopy(data["sites"][0])
    site1["name"] = "Site1"
    site1["gridiv"] = 1
    site2["name"] = "Site2"
    site2["gridiv"] = 2
    site1["properties"]["stebbs"]["profile_flow_hot_water"] = {
        "working_day": {str(i): (None if i == 1 else 0.0) for i in range(1, 145)},
        "holiday": {str(i): 0.0 for i in range(1, 145)},
    }
    site2["properties"]["stebbs"]["profile_flow_hot_water"] = {
        "working_day": {str(i): 0.0 for i in range(1, 145)},
        "holiday": {str(i): 0.0 for i in range(1, 145)},
    }
    data["sites"] = [site1, site2]

    cfg = SUEWSConfig.from_dict(data)
    df_state = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(df_state)
    assert (
        rebuilt.sites[0].properties.stebbs.profile_flow_hot_water.working_day["1"]
        is None
    )
    assert (
        rebuilt.sites[1].properties.stebbs.profile_flow_hot_water.working_day["1"]
        == 0.0
    )


def test_to_yaml_rejects_relaxed_values_after_live_stebbs_activation(
    sample_dict: dict, tmp_path: Path
) -> None:
    """Activating STEBBS in-place prevents ``to_yaml()`` from exporting relaxed/null values."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["model"]["physics"]["stebbs"]["capacitance"] = None

    cfg = SUEWSConfig.from_dict(data)
    cfg.to_yaml(tmp_path / "inactive_ok.yml")

    cfg.model.physics.stebbs.enabled = True
    with pytest.raises(ValueError, match="capacitance"):
        cfg.to_yaml(tmp_path / "active_bad.yml")


def test_standalone_stebbs_physics_guard_tracks_wrapped_enabled_value_mutations() -> (
    None
):
    """Standalone StebbsPhysics guards follow in-place ``enabled.value`` mutations."""
    phys = StebbsPhysics(enabled={"value": False}, capacitance={"value": 0})
    phys.enabled.value = True
    with pytest.raises(ValueError, match="capacitance"):
        phys.capacitance.value = None

    phys.enabled.value = False
    phys.capacitance.value = None
    assert phys.capacitance.value is None


def test_inactive_stebbs_reference_rejects_extra_keys(sample_dict: dict) -> None:
    """Inactive RefValue metadata still enforces ``extra='forbid'`` under ``from_dict``."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["sites"][0]["properties"]["stebbs"]["depth_ground"] = {
        "value": "TBD",
        "ref": {"typo": 123},
    }
    with pytest.raises((ValidationError, ValueError), match="typo"):
        SUEWSConfig.from_dict(data)


@pytest.mark.parametrize("parameters", [1, 2])
def test_active_capacitance_2_requires_wall_outer_even_when_wwr_is_one(
    sample_dict: dict, parameters: int
) -> None:
    """``capacitance=2`` rejects null wall-outer params even when ``ratio_window_to_wall == 1.0``."""
    data = copy.deepcopy(sample_dict)
    _enable_stebbs(data, parameters=parameters)
    data["model"]["physics"]["stebbs"]["capacitance"] = 2
    data["sites"][0]["properties"]["building_archetype"]["ratio_window_to_wall"] = 1.0
    data["sites"][0]["properties"]["building_archetype"]["thickness_wall_outer"] = None
    with pytest.raises((ValidationError, ValueError), match="thickness_wall_outer"):
        SUEWSConfig.from_dict(data)


@pytest.mark.parametrize(
    ("section", "profile_field"),
    [
        ("stebbs", "profile_flow_hot_water"),
        ("building_archetype", "profile_metabolism"),
    ],
)
def test_inactive_null_profile_survives_df_state_roundtrip_and_rejects_on_activation(
    sample_dict: dict, section: str, profile_field: str
) -> None:
    """Whole-profile null (or wrapped null) stays None through df_state and fails on activation."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["sites"][0]["properties"][section][profile_field] = {"value": None}

    cfg = SUEWSConfig.from_dict(data)
    assert getattr(getattr(cfg.sites[0].properties, section), profile_field) is None

    df_state = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(df_state)
    assert getattr(getattr(rebuilt.sites[0].properties, section), profile_field) is None

    rebuilt.model.physics.stebbs.enabled = True
    with pytest.raises(ValueError, match=profile_field):
        rebuilt.validate_for_engine()


def test_boolean_explicit_ehc_coupling_consistent_across_constructors(
    sample_dict: dict,
) -> None:
    """``ehc_surface_coupling=False`` is classified as explicit (0) by both ``SUEWSConfig`` and ``from_dict``."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["model"]["physics"]["storage_heat"] = 5
    data["model"]["physics"]["ehc_surface_coupling"] = False
    data["sites"][0]["properties"]["stebbs"]["temperature_air_indoor_initial"] = "TBD"

    cfg_direct = SUEWSConfig(**copy.deepcopy(data))
    cfg_from_dict = SUEWSConfig.from_dict(copy.deepcopy(data))
    assert cfg_direct.sites[0].properties.stebbs.temperature_air_indoor_initial == "TBD"
    assert (
        cfg_from_dict.sites[0].properties.stebbs.temperature_air_indoor_initial == "TBD"
    )


def test_wrapped_master_switches_retain_non_null_guards_after_config_load(
    sample_dict: dict,
) -> None:
    """Wrapped ``stebbs.enabled`` and ``stebbs.parameters`` reject ``.value = None`` immediately."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["model"]["physics"]["stebbs"]["enabled"] = {"value": False}
    data["model"]["physics"]["stebbs"]["parameters"] = {"value": 1}

    cfg = SUEWSConfig.from_dict(data)
    with pytest.raises(ValueError, match="enabled"):
        cfg.model.physics.stebbs.enabled.value = None
    with pytest.raises(ValueError, match="parameters"):
        cfg.model.physics.stebbs.parameters.value = None


def test_standalone_stebbs_physics_deepcopy_preserves_live_guard_owner() -> None:
    """Deep-copied standalone ``StebbsPhysics`` keeps subordinate guards bound to the copy."""
    phys = StebbsPhysics(enabled={"value": False}, capacitance={"value": 0})
    cloned = copy.deepcopy(phys)

    cloned.enabled.value = True
    with pytest.raises(ValueError, match="capacitance"):
        cloned.capacitance.value = None

    # Original remains unaffected and disabled.
    phys.capacitance.value = None
    assert phys.capacitance.value is None


def test_live_ehc_implicit_activation_enforces_shared_temperature_bounds(
    sample_dict: dict, tmp_path: Path
) -> None:
    """Switching ``storage_heat`` 1 -> 5 (implicit EHC) revalidates ``gt=0`` on diffmax."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["model"]["physics"]["storage_heat"] = 1
    data["sites"][0]["properties"]["stebbs"][
        "temperature_air_month_mean_diffmax"
    ] = -999.0

    cfg = SUEWSConfig.from_dict(data)
    cfg.validate_for_engine()

    cfg.model.physics.storage_heat = 5
    cfg.model.physics.ehc_surface_coupling = 1
    with pytest.raises(ValueError, match="temperature_air_month_mean_diffmax"):
        cfg.validate_for_engine()
    with pytest.raises(ValueError, match="temperature_air_month_mean_diffmax"):
        config_to_engine_yaml(cfg, for_file_output=False)
    with pytest.raises(ValueError, match="temperature_air_month_mean_diffmax"):
        cfg.to_yaml(tmp_path / "bad_ehc_diffmax.yml")


def test_dyohm_building_method_8_allows_inactive_null_stebbs_and_annual_mean(
    sample_dict: dict,
) -> None:
    """Method 8 (``dyohm_building``) does not consume ``deep_soil_temperature``."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["model"]["physics"]["storage_heat"] = 8
    data["sites"][0]["properties"]["stebbs"]["temperature_air_annual_mean"] = None
    cfg = SUEWSConfig.from_dict(data)
    assert cfg.sites[0].properties.stebbs.temperature_air_annual_mean is None
    cfg.validate_for_engine()

    data_null_block = copy.deepcopy(sample_dict)
    _disable_stebbs(data_null_block)
    data_null_block["model"]["physics"]["storage_heat"] = 8
    data_null_block["sites"][0]["properties"]["stebbs"] = None
    cfg_null = SUEWSConfig.from_dict(data_null_block)
    assert cfg_null.sites[0].properties.stebbs is None
    cfg_null.validate_for_engine()


@pytest.mark.parametrize(
    ("section", "profile_field", "bad_ref"),
    [
        ("stebbs", "profile_appliance", {"unexpected": 1}),
        ("stebbs", "profile_flow_hot_water", 123),
        ("building_archetype", "profile_metabolism", {"unexpected": 1}),
        ("building_archetype", "profile_metabolism", 123),
    ],
)
def test_inactive_wrapped_null_profile_validates_reference_strictly(
    sample_dict: dict, section: str, profile_field: str, bad_ref
) -> None:
    """Wrapped-null inactive profiles still validate ``ref`` metadata with ``extra='forbid'``."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["sites"][0]["properties"][section][profile_field] = {
        "value": None,
        "ref": bad_ref,
    }
    with pytest.raises((ValidationError, ValueError)):
        SUEWSConfig.from_dict(data)


@pytest.mark.parametrize(
    ("section", "profile_field"),
    [
        ("stebbs", "profile_appliance"),
        ("building_archetype", "profile_metabolism"),
    ],
)
def test_sparse_inactive_ten_minute_profile_preserves_placeholders_across_df_state(
    sample_dict: dict, section: str, profile_field: str
) -> None:
    """Sparse inactive 10-minute profiles preserve placeholders through df_state and fail on activation."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["sites"][0]["properties"][section][profile_field] = {
        "working_day": {"1": "TBD"},
        "holiday": {"1": 0.0},
    }

    cfg = SUEWSConfig.from_dict(data)
    df_state = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(df_state)
    rebuilt_prof = getattr(getattr(rebuilt.sites[0].properties, section), profile_field)
    assert rebuilt_prof.working_day == {"1": "TBD"}
    assert rebuilt_prof.holiday == {"1": 0.0}

    rebuilt.model.physics.stebbs.enabled = True
    with pytest.raises(ValueError, match=profile_field):
        rebuilt.validate_for_engine()


def test_inactive_null_day_ten_minute_profile_roundtrips_df_state_and_yaml(
    sample_dict: dict, tmp_path: Path
) -> None:
    """Inactive 10-minute profiles with ``working_day=None`` round-trip via df_state and YAML."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["sites"][0]["properties"]["stebbs"]["profile_appliance"] = {
        "working_day": None,
        "holiday": {"1": "TBD"},
    }

    cfg = SUEWSConfig.from_dict(data)
    cfg.validate_for_engine()

    df_state = cfg.to_df_state()
    rebuilt_df = SUEWSConfig.from_df_state(df_state)
    prof_df = rebuilt_df.sites[0].properties.stebbs.profile_appliance
    assert prof_df.working_day is None
    assert prof_df.holiday == {"1": "TBD"}

    yaml_path = tmp_path / "null_day_profile.yml"
    cfg.to_yaml(yaml_path)
    rebuilt_yaml = SUEWSConfig.from_yaml(yaml_path)
    prof_yaml = rebuilt_yaml.sites[0].properties.stebbs.profile_appliance
    assert prof_yaml.working_day is None
    assert prof_yaml.holiday == {"1": "TBD"}

    rebuilt_yaml.model.physics.stebbs.enabled = True
    with pytest.raises(ValueError, match="profile_appliance"):
        rebuilt_yaml.validate_for_engine()


@pytest.mark.parametrize(
    ("storage_heat", "ehc_coupling"),
    [(5, 1), (6, 1)],
)
def test_live_numeric_string_mutation_coerced_on_ehc_and_dyohm_activation(
    sample_dict: dict, tmp_path: Path, storage_heat: int, ehc_coupling: int
) -> None:
    """Mutating wrapped ``temperature_air_annual_mean.value = '12.5'`` coerces on EHC/DyOHM activation."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["model"]["physics"]["storage_heat"] = 1
    data["sites"][0]["properties"]["stebbs"]["temperature_air_annual_mean"] = {
        "value": "TBD"
    }

    cfg = SUEWSConfig.from_dict(data)
    cfg.sites[0].properties.stebbs.temperature_air_annual_mean.value = "12.5"
    cfg.model.physics.storage_heat = storage_heat
    cfg.model.physics.ehc_surface_coupling = ehc_coupling

    cfg.validate_for_engine()
    assert cfg.sites[0].properties.stebbs.temperature_air_annual_mean.value == 12.5
    assert isinstance(
        cfg.sites[0].properties.stebbs.temperature_air_annual_mean.value, float
    )
    engine_doc = yaml.safe_load(config_to_engine_yaml(cfg, for_file_output=False))
    emitted = engine_doc["sites"][0]["properties"]["stebbs"][
        "temperature_air_annual_mean"
    ]
    emitted_val = emitted["value"] if isinstance(emitted, dict) else emitted
    assert emitted_val == 12.5
    assert isinstance(emitted_val, float)
    cfg.to_yaml(tmp_path / f"ehc_dyohm_sh{storage_heat}.yml")


def test_inactive_null_and_empty_day_profile_does_not_fabricate_defaults_on_df_state(
    sample_dict: dict,
) -> None:
    """``{'working_day': None, 'holiday': {}}`` stays unusable across df_state and fails on activation."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["sites"][0]["properties"]["stebbs"]["profile_appliance"] = {
        "working_day": None,
        "holiday": {},
    }

    cfg = SUEWSConfig.from_dict(data)
    df_state = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(df_state)
    assert rebuilt.sites[0].properties.stebbs.profile_appliance is None

    rebuilt.model.physics.stebbs.enabled = True
    with pytest.raises(ValueError, match="profile_appliance"):
        rebuilt.validate_for_engine()


def test_live_wwr_numeric_string_normalised_before_conditional_window_checks(
    sample_dict: dict,
) -> None:
    """Mutating ``ratio_window_to_wall.value = '0'`` before activation excludes window params cleanly."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["sites"][0]["properties"]["building_archetype"]["ratio_window_to_wall"] = {
        "value": "TBD"
    }
    data["sites"][0]["properties"]["building_archetype"]["thickness_window"] = None

    cfg = SUEWSConfig.from_dict(data)
    cfg.sites[0].properties.building_archetype.ratio_window_to_wall.value = "0"
    cfg.model.physics.stebbs.enabled = True

    cfg.validate_for_engine()
    assert cfg.sites[0].properties.building_archetype.ratio_window_to_wall.value == 0.0
    config_to_engine_yaml(cfg, for_file_output=False)


def test_live_non_finite_string_mutations_rejected_when_stebbs_inactive(
    sample_dict: dict, tmp_path: Path
) -> None:
    """In-place ``'nan'``/``'inf'`` string mutations on inactive profiles or RefValues fail export."""
    data = copy.deepcopy(sample_dict)
    _disable_stebbs(data)
    data["sites"][0]["properties"]["stebbs"]["depth_ground"] = {"value": "TBD"}

    cfg = SUEWSConfig.from_dict(data)
    cfg.sites[0].properties.stebbs.profile_appliance.working_day["1"] = "nan"
    with pytest.raises(ValueError, match="finite"):
        cfg.validate_for_engine()
    with pytest.raises(ValueError, match="finite"):
        config_to_engine_yaml(cfg, for_file_output=False)
    with pytest.raises(ValueError, match="finite"):
        cfg.to_yaml(tmp_path / "bad_nan_profile.yml")

    cfg.sites[0].properties.stebbs.profile_appliance.working_day["1"] = 0.0
    cfg.sites[0].properties.stebbs.depth_ground.value = "inf"
    with pytest.raises(ValueError, match="finite"):
        cfg.validate_for_engine()
    with pytest.raises(ValueError, match="finite"):
        cfg.to_yaml(tmp_path / "bad_inf_scalar.yml")


@pytest.mark.parametrize("parameters", [1, 2])
def test_active_constant_setpoint_with_null_setpoint_profiles_roundtrips_df_state(
    sample_dict: dict, parameters: int
) -> None:
    """Active STEBBS with ``setpoint=0`` and null setpoint profiles round-trips cleanly via df_state."""
    data = copy.deepcopy(sample_dict)
    _enable_stebbs(data, parameters=parameters)
    data["model"]["physics"]["stebbs"]["setpoint"] = 0
    data["sites"][0]["properties"]["building_archetype"][
        "profile_setpoint_temperature_heating_air"
    ] = None
    data["sites"][0]["properties"]["building_archetype"][
        "profile_setpoint_temperature_cooling_air"
    ] = None

    cfg = SUEWSConfig.from_dict(data)
    cfg.validate_for_engine()
    df_state = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(df_state)
    assert (
        rebuilt.sites[
            0
        ].properties.building_archetype.profile_setpoint_temperature_heating_air
        is None
    )
    assert (
        rebuilt.sites[
            0
        ].properties.building_archetype.profile_setpoint_temperature_cooling_air
        is None
    )
    rebuilt.validate_for_engine()


@pytest.mark.parametrize("parameters", [1, 2])
def test_active_stebbs_requires_complete_144_slot_ten_minute_profiles(
    sample_dict: dict, parameters: int
) -> None:
    """Active STEBBS rejects incomplete 10-minute profiles on load, live activation, and df_state."""
    sparse_profile = {"working_day": {"1": 0.0}, "holiday": {"1": 0.0}}

    # Direct active construction
    active_data = copy.deepcopy(sample_dict)
    _enable_stebbs(active_data, parameters=parameters)
    active_data["sites"][0]["properties"]["stebbs"]["profile_flow_hot_water"] = (
        copy.deepcopy(sparse_profile)
    )
    with pytest.raises((ValidationError, ValueError), match="profile_flow_hot_water"):
        SUEWSConfig.from_dict(active_data)

    # Inactive load -> live activation
    inactive_data = copy.deepcopy(sample_dict)
    _disable_stebbs(inactive_data)
    inactive_data["model"]["physics"]["stebbs"]["parameters"] = parameters
    inactive_data["sites"][0]["properties"]["stebbs"]["profile_flow_hot_water"] = (
        copy.deepcopy(sparse_profile)
    )
    cfg = SUEWSConfig.from_dict(inactive_data)
    df_state = cfg.to_df_state()
    df_state.loc[1, ("stebbsmethod", "0")] = parameters
    rebuilt = SUEWSConfig.from_df_state(df_state)
    assert rebuilt.sites[0].properties.stebbs.profile_flow_hot_water.working_day == {
        "1": 0.0
    }
    with pytest.raises(ValueError, match="profile_flow_hot_water"):
        rebuilt.validate_for_engine()

    cfg.model.physics.stebbs.enabled = True
    with pytest.raises(ValueError, match="profile_flow_hot_water"):
        cfg.validate_for_engine()


# =============================================================================
# 7. JSON Schema invariance (zero schema drift)
# =============================================================================


def test_json_schema_unchanged() -> None:
    """Scheme-gated STEBBS validation leaves committed ``schema.json`` byte-for-byte invariant."""
    committed_path = (
        PROJECT_ROOT / "docs" / "source" / "inputs" / "tables" / "schema.json"
    )
    committed = json.loads(committed_path.read_text(encoding="utf-8"))
    generated = generate_json_schema()
    assert generated == committed
