"""Scheme-gated validation tests for snow and observed soil moisture (#2363, #2348).

Enforces the standing validation principle: optional physical-scheme parameters
are not constrained while their owning scheme is inactive (``snow_use == 0`` or
``soil_moisture != 1``), while remaining strictly validated when the scheme is
active, on standalone construction, or after live post-construction activation.
"""

from __future__ import annotations

import copy
from importlib.resources import files
import json
from pathlib import Path
import warnings

import pandas as pd
from pydantic import ValidationError
import pytest
import yaml

from supy import SUEWSSimulation
from supy._run_rust import config_to_engine_yaml
from supy.data_model.configuration.publisher import generate_json_schema
from supy.data_model.core import SUEWSConfig
from supy.data_model.core.model import SMDMethod, SnowUse
from supy.data_model.core.site import SnowParams, SoilObservationConfig
from supy.data_model.core.state import (
    InitialStatePaved,
    InitialStates,
    SurfaceInitialState,
)
from supy.data_model.core.surface import PavedProperties
from supy.data_model.core.type import RefValue

pytestmark = [
    pytest.mark.native,
    pytest.mark.cfg,
    pytest.mark.core,
    pytest.mark.contract("C-VALID", "C-EMIT", "C-ROUNDTRIP", "C-RESPONSE"),
]

PROJECT_ROOT = Path(__file__).resolve().parents[2]
NON_WATER_SURFACES = ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil")


@pytest.fixture(scope="module")
def sample_dict() -> dict:
    """Load packaged sample configuration (``snow: disabled``, ``soil_moisture: modelled``)."""
    text = (
        files("supy")
        .joinpath("sample_data", "sample_config.yml")
        .read_text(encoding="utf-8")
    )
    return yaml.safe_load(text)


def _set_snow_use(data: dict, value: object) -> None:
    physics = data["model"]["physics"]
    physics.pop("snow", None)
    physics.pop("snowuse", None)
    physics["snow_use"] = value


def _make_active_observed_soil_dict(base: dict) -> dict:
    data = copy.deepcopy(base)
    data["model"]["physics"]["soil_moisture"] = "observed"
    for site in data["sites"]:
        props = site["properties"]
        props["soil_observation"] = {"layer_depth": 0.2, "layer_top": 0.0}
        for surf in NON_WATER_SURFACES:
            props["land_cover"][surf].update(
                soil_depth=200.0,
                soil_store_capacity=80.0,
                soil_field_capacity=0.6,
            )
    return data


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


# =============================================================================
# 1. Snow scheme-gated validation (snow_use == 0 vs snow_use != 0)
# =============================================================================


@pytest.mark.parametrize(
    ("mutator", "description"),
    [
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                water_holding_capacity_min=0.9, water_holding_capacity_max=0.1
            ),
            "inverted water_holding_capacity min/max",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                water_holding_capacity_min=-999.0, water_holding_capacity_max=1.5
            ),
            "out-of-range water_holding_capacity",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                snow_albedo_min=0.95, snow_albedo_max=0.2
            ),
            "inverted snow_albedo min/max",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                snow_albedo_min=-999.0, snow_albedo_max=1.8
            ),
            "out-of-range snow_albedo",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                narp_emissivity_snow=-999.0
            ),
            "negative sentinel narp_emissivity_snow",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                precipitation_threshold_albedo_reset=-1.0
            ),
            "negative precipitation_threshold_albedo_reset",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                water_holding_capacity_max=None,
                snow_albedo_min={"value": None},
                narp_emissivity_snow={"value": None},
            ),
            "null and wrapped-null SnowParams scalars",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].__setitem__(
                "snow_profile_24hr", None
            ),
            "null properties.snow.snow_profile_24hr",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].__setitem__(
                "snow_profile_24hr", {"value": None}
            ),
            "wrapped-null properties.snow.snow_profile_24hr",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__("snow", None),
            "null properties.snow block",
        ),
        (
            lambda d: d["sites"][0]["properties"].pop("snow", None),
            "omitted properties.snow block",
        ),
        (
            lambda d: d["sites"][0]["initial_states"].__setitem__("snowalb", -999.0),
            "out-of-range finite initial_states.snowalb",
        ),
        (
            lambda d: d["sites"][0]["initial_states"]["paved"].update(
                snowfrac=-999.0,
                snowpack=-5.0,
                icefrac=2.0,
                snowwater=-1.0,
                snowdens=-50.0,
            ),
            "out-of-range finite surface snow initial states",
        ),
        (
            lambda d: d["sites"][0]["initial_states"]["paved"].update(
                snowfrac=None,
                snowpack={"value": None},
                icefrac=None,
                snowwater=None,
                snowdens=None,
            ),
            "null surface snow initial states",
        ),
    ],
)
def test_snow_disabled_accepts_relaxed_snow_params_and_states(
    sample_dict: dict, mutator, description: str
) -> None:
    """When ``snow_use == 0``, inactive snow parameters and states never block load or export."""
    data = copy.deepcopy(sample_dict)
    _set_snow_use(data, 0)
    mutator(data)

    cfg = SUEWSConfig.from_dict(data)
    cfg.validate_for_engine()
    _assert_clean_json_dump(cfg)
    yaml_text = config_to_engine_yaml(cfg, for_file_output=False)
    assert isinstance(yaml_text, str) and yaml_text

    # Round-trip via df_state must also succeed cleanly and preserve grid IDs.
    df_state = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(df_state)
    assert [s.gridiv for s in rebuilt.sites] == [s.gridiv for s in cfg.sites], (
        description
    )
    assert ("snowfrac", "(0,)") in df_state.columns
    assert ("snowpack", "(0,)") in df_state.columns


def test_snow_disabled_accepts_legacy_snow_aliases_and_omitted_switch(
    sample_dict: dict,
) -> None:
    """Legacy snow keys and omitted ``snow_use`` (defaults to 0) accept relaxed values."""
    data = copy.deepcopy(sample_dict)
    data["model"]["physics"].pop("snow", None)
    data["model"]["physics"].pop("snow_use", None)
    snow = data["sites"][0]["properties"]["snow"]
    snow.pop("water_holding_capacity_min", None)
    snow.pop("narp_emissivity_snow", None)
    snow["crwmin"] = 0.95
    snow["narp_emis_snow"] = -999.0

    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        cfg = SUEWSConfig.from_dict(data)
    cfg.validate_for_engine()


@pytest.mark.parametrize(
    ("mutator", "pattern"),
    [
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                water_holding_capacity_min=0.9, water_holding_capacity_max=0.1
            ),
            r"crwmin.*must be less than crwmax",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                water_holding_capacity_min=-0.1
            ),
            r"crwmin.*must be in range \[0, 1\]",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                snow_albedo_min=0.9, snow_albedo_max=0.2
            ),
            r"snowalbmin.*must be less than snowalbmax",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(snow_albedo_max=1.5),
            r"snowalbmax.*must be in range \[0, 1\]",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                narp_emissivity_snow=1.5
            ),
            r"narp_emissivity_snow",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                precipitation_threshold_albedo_reset=-0.5
            ),
            r"precipitation_threshold_albedo_reset",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                water_holding_capacity_max={"value": None}
            ),
            r"water_holding_capacity_max",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].__setitem__(
                "snow_profile_24hr", None
            ),
            r"snow_profile_24hr",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__("snow", None),
            r"snow",
        ),
        (
            lambda d: d["sites"][0]["initial_states"].__setitem__("snowalb", 1.5),
            r"snowalb",
        ),
        (
            lambda d: d["sites"][0]["initial_states"]["paved"].__setitem__(
                "snowfrac", -0.2
            ),
            r"snowfrac",
        ),
        (
            lambda d: d["sites"][0]["initial_states"]["paved"].__setitem__(
                "snowpack", -5.0
            ),
            r"snowpack",
        ),
        (
            lambda d: d["sites"][0]["initial_states"]["paved"].__setitem__(
                "snowfrac", None
            ),
            r"initial_states\.paved\.snowfrac",
        ),
        (
            lambda d: d["sites"][0]["initial_states"]["paved"].__setitem__(
                "snowpack", {"value": None}
            ),
            r"initial_states\.paved\.snowpack",
        ),
    ],
)
def test_snow_enabled_strictly_rejects_invalid_snow_params_and_states(
    sample_dict: dict, mutator, pattern: str
) -> None:
    """When ``snow_use == 1``, invalid snow parameters and states are strictly rejected."""
    data = copy.deepcopy(sample_dict)
    _set_snow_use(data, 1)
    mutator(data)

    with pytest.raises((ValueError, ValidationError), match=pattern):
        SUEWSConfig.from_dict(data)


@pytest.mark.parametrize(
    ("mutator", "pattern"),
    [
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                snow_albedo_min=0.9, snow_albedo_max=0.2
            ),
            r"snowalbmin.*must be less than snowalbmax",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                narp_emissivity_snow=-999.0
            ),
            r"narp_emissivity_snow",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].update(
                water_holding_capacity_max={"value": None}
            ),
            r"water_holding_capacity_max",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].__setitem__(
                "snow_profile_24hr", None
            ),
            r"snow_profile_24hr",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__("snow", None),
            r"snow",
        ),
        (
            lambda d: d["sites"][0]["initial_states"].__setitem__("snowalb", -999.0),
            r"snowalb",
        ),
        (
            lambda d: d["sites"][0]["initial_states"]["paved"].__setitem__(
                "snowfrac", -999.0
            ),
            r"snowfrac",
        ),
        (
            lambda d: d["sites"][0]["initial_states"]["paved"].__setitem__(
                "snowpack", {"value": None}
            ),
            r"initial_states\.paved\.snowpack",
        ),
    ],
)
def test_snow_activation_after_load_and_deepcopy_enforces_strict_checks(
    sample_dict: dict, mutator, pattern: str
) -> None:
    """Enabling ``snow_use`` post-construction (including on a deep copy) catches relaxed values."""
    data = copy.deepcopy(sample_dict)
    _set_snow_use(data, 0)
    mutator(data)

    original = SUEWSConfig.from_dict(data)
    original.validate_for_engine()

    copied = copy.deepcopy(original)
    copied.model.physics.snow_use = SnowUse.ENABLED
    with pytest.raises(ValueError, match=pattern):
        copied.validate_for_engine()

    # Original config stays snow-disabled and unaffected by the copy's mutation.
    original.validate_for_engine()

    model_copied = original.model_copy(deep=True)
    model_copied.model.physics.snow_use = SnowUse.ENABLED
    with pytest.raises(ValueError, match=pattern):
        model_copied.validate_for_engine()


def test_snow_disabled_live_mutation_allows_relaxation_and_rejects_when_active(
    sample_dict: dict,
) -> None:
    """Live attribute edits follow the owning config's live ``snow_use`` switch."""
    data = copy.deepcopy(sample_dict)
    _set_snow_use(data, 0)
    cfg = SUEWSConfig.from_dict(data)

    cfg.sites[0].properties.snow.water_holding_capacity_max = None
    cfg.sites[0].properties.snow.narp_emissivity_snow = -999.0
    cfg.validate_for_engine()

    cfg.sites[0].properties.snow = None
    cfg.validate_for_engine()

    # Standalone models without an owning config keep strict validation.
    with pytest.raises(ValidationError):
        SnowParams(narp_emissivity_snow=-999.0)
    with pytest.raises(ValidationError):
        SnowParams(water_holding_capacity_max=None)
    with pytest.raises(ValidationError):
        SurfaceInitialState(snowfrac=-0.5)
    with pytest.raises(ValidationError):
        InitialStates(snowalb=1.5)


def test_active_snow_rejects_null_snow_states_before_df_state_export(
    sample_dict: dict,
) -> None:
    """Active snow rejects null ground-surface snow states before df_state or engine export."""
    data = copy.deepcopy(sample_dict)
    _set_snow_use(data, 1)
    cfg = SUEWSConfig.from_dict(data)

    # Mutate snowpack to None/wrapped-None after load -> must fail at handoff AND to_df_state.
    for bad_val in (None, RefValue(value=None)):
        mutated = copy.deepcopy(cfg)
        mutated.sites[0].initial_states.paved.snowpack = bad_val
        with pytest.raises(ValueError, match=r"initial_states\.paved\.snowpack"):
            mutated.validate_for_engine()
        with pytest.raises(ValueError, match=r"initial_states\.paved\.snowpack"):
            mutated.to_df_state()

    # Standalone ground surface state must never silently invent 0.0 for None.
    standalone = InitialStatePaved(snowfrac=None, snowpack=None)
    standalone_cols = standalone._to_df_state_cols(1)
    assert ("snowfrac", "(0,)") not in standalone_cols
    assert ("snowpack", "(0,)") not in standalone_cols


def test_inplace_refvalue_edits_follow_live_scheme_transitions(
    sample_dict: dict,
) -> None:
    """In-place ``RefValue.value = None`` consults the owning config's live scheme state."""
    # Snow: load active -> disable -> in-place .value = None succeeds; deep copy reactivated fails.
    snow_data = copy.deepcopy(sample_dict)
    _set_snow_use(snow_data, 1)
    snow_cfg = SUEWSConfig.from_dict(snow_data)
    with pytest.raises(ValueError, match="water_holding_capacity_max"):
        snow_cfg.sites[0].properties.snow.water_holding_capacity_max.value = None

    snow_cfg.model.physics.snow_use = SnowUse.DISABLED
    snow_cfg.sites[0].properties.snow.water_holding_capacity_max.value = None
    snow_cfg.validate_for_engine()

    copied_snow = copy.deepcopy(snow_cfg)
    copied_snow.model.physics.snow_use = SnowUse.ENABLED
    with pytest.raises(ValueError, match="water_holding_capacity_max"):
        copied_snow.sites[0].properties.snow.water_holding_capacity_max.value = None
    with pytest.raises(ValueError, match="water_holding_capacity_max"):
        copied_snow.validate_for_engine()

    # Original remains snow-disabled and valid.
    snow_cfg.validate_for_engine()

    # Observed soil: load active -> switch to modelled -> in-place .value = None succeeds.
    soil_data = _make_active_observed_soil_dict(sample_dict)
    soil_data["sites"][0]["properties"]["soil_observation"]["layer_depth"] = {
        "value": 0.2
    }
    soil_cfg = SUEWSConfig.from_dict(soil_data)
    with pytest.raises(ValueError, match="layer_depth"):
        soil_cfg.sites[0].properties.soil_observation.layer_depth.value = None

    soil_cfg.model.physics.soil_moisture = SMDMethod.MODELLED
    soil_cfg.sites[0].properties.soil_observation.layer_depth.value = None
    soil_cfg.validate_for_engine()

    copied_soil = copy.deepcopy(soil_cfg)
    copied_soil.model.physics.soil_moisture = SMDMethod.OBSERVED
    with pytest.raises(ValueError, match="layer_depth"):
        copied_soil.sites[0].properties.soil_observation.layer_depth.value = None
    with pytest.raises(ValueError, match="layer_depth"):
        copied_soil.validate_for_engine()


def test_replacing_child_snow_and_soil_models_preserves_scheme_ownership(
    sample_dict: dict,
) -> None:
    """Replacing ``properties.snow`` or ``properties.soil_observation`` binds to parent config."""
    data = copy.deepcopy(sample_dict)
    _set_snow_use(data, 0)
    data["model"]["physics"]["soil_moisture"] = "modelled"
    cfg = SUEWSConfig.from_dict(data)

    # Fresh replacement SnowParams inherits inactive snow_use == 0 ownership.
    cfg.sites[0].properties.snow = SnowParams()
    cfg.sites[0].properties.snow.water_holding_capacity_max = None
    cfg.sites[0].properties.snow.snow_profile_24hr = None
    cfg.validate_for_engine()

    # model_copy(update=...) replacement also passes handoff validation when inactive.
    replaced = SnowParams().model_copy(update={"water_holding_capacity_max": None})
    cfg.sites[0].properties.snow = replaced
    cfg.validate_for_engine()

    # Fresh replacement SoilObservationConfig inherits inactive soil_moisture == 0 ownership.
    cfg.sites[0].properties.soil_observation = SoilObservationConfig(layer_depth=0.2)
    cfg.sites[0].properties.soil_observation.layer_depth = None
    cfg.validate_for_engine()

    # Dictionary replacements inherit the parent's inactive scheme context.
    cfg.sites[0].properties.soil_observation = {}
    assert cfg.sites[0].properties.soil_observation.layer_depth is None
    cfg.sites[0].properties.snow = {"narp_emissivity_snow": -999.0}
    assert cfg.sites[0].properties.snow.narp_emissivity_snow == -999.0
    cfg.sites[0].properties.snow.water_holding_capacity_max = None
    cfg.validate_for_engine()

    # Dictionary replacements are strictly rejected when the owning scheme is active.
    active_snow_data = copy.deepcopy(sample_dict)
    _set_snow_use(active_snow_data, 1)
    active_snow_cfg = SUEWSConfig.from_dict(active_snow_data)
    with pytest.raises(ValidationError):
        active_snow_cfg.sites[0].properties.snow = {"narp_emissivity_snow": -999.0}

    active_soil_cfg = SUEWSConfig.from_dict(
        _make_active_observed_soil_dict(sample_dict)
    )
    with pytest.raises(ValidationError):
        active_soil_cfg.sites[0].properties.soil_observation = {}

    # Activating either scheme catches the relaxed/null values on the replaced child.
    cfg_snow_on = copy.deepcopy(cfg)
    cfg_snow_on.model.physics.snow_use = SnowUse.ENABLED
    with pytest.raises(ValueError, match="water_holding_capacity_max"):
        cfg_snow_on.validate_for_engine()

    cfg_soil_on = copy.deepcopy(cfg)
    cfg_soil_on.model.physics.soil_moisture = SMDMethod.OBSERVED
    with pytest.raises(ValueError, match="layer_depth"):
        cfg_soil_on.validate_for_engine()


def test_df_state_roundtrip_preserves_snow_absence_and_rejects_on_activation(
    sample_dict: dict,
) -> None:
    """DataFrame export never fabricates snow numbers for explicit nulls."""
    # Active snow with wrapped null snow_density_min preserves None (never 100.0).
    active_data = copy.deepcopy(sample_dict)
    _set_snow_use(active_data, 1)
    active_data["sites"][0]["properties"]["snow"]["snow_density_min"] = {"value": None}
    active_cfg = SUEWSConfig.from_dict(active_data)
    active_df = active_cfg.to_df_state()
    grid_id = active_cfg.sites[0].gridiv
    assert pd.isna(active_df.loc[grid_id, ("snowdensmin", "0")])
    rebuilt_active = SUEWSConfig.from_df_state(active_df)
    assert isinstance(
        rebuilt_active.sites[0].properties.snow.snow_density_min, RefValue
    )
    assert rebuilt_active.sites[0].properties.snow.snow_density_min.value is None

    # Inactive snow with null scalar, null profile, and null surface state preserves
    # absence across df_state round-trip and fails if snow is later enabled.
    inactive_data = copy.deepcopy(sample_dict)
    _set_snow_use(inactive_data, 0)
    inactive_data["sites"][0]["properties"]["snow"]["tau_cold_snow"] = None
    inactive_data["sites"][0]["properties"]["snow"]["water_holding_capacity_max"] = {
        "value": None
    }
    inactive_data["sites"][0]["properties"]["snow"]["snow_profile_24hr"] = None
    inactive_data["sites"][0]["initial_states"]["paved"]["snowfrac"] = None
    inactive_cfg = SUEWSConfig.from_dict(inactive_data)
    inactive_df = inactive_cfg.to_df_state()
    assert pd.isna(inactive_df.loc[grid_id, ("tau_a", "0")])
    assert pd.isna(inactive_df.loc[grid_id, ("crwmax", "0")])
    assert pd.isna(inactive_df.loc[grid_id, ("snowprof_24hr", "(0, 0)")])
    assert pd.isna(inactive_df.loc[grid_id, ("snowfrac", "(0,)")])

    rebuilt_inactive = SUEWSConfig.from_df_state(inactive_df)
    assert rebuilt_inactive.sites[0].properties.snow.tau_cold_snow is None
    assert rebuilt_inactive.sites[0].properties.snow.water_holding_capacity_max is None
    assert rebuilt_inactive.sites[0].properties.snow.snow_profile_24hr is None
    assert rebuilt_inactive.sites[0].initial_states.paved.snowfrac is None
    rebuilt_inactive.validate_for_engine()

    rebuilt_inactive.model.physics.snow_use = SnowUse.ENABLED
    with pytest.raises(ValueError, match="water_holding_capacity_max"):
        rebuilt_inactive.validate_for_engine()
    rebuilt_inactive.sites[0].properties.snow.water_holding_capacity_max = 0.1
    with pytest.raises(ValueError, match="tau_cold_snow"):
        rebuilt_inactive.validate_for_engine()
    rebuilt_inactive.sites[0].properties.snow.tau_cold_snow = 0.018
    with pytest.raises(ValueError, match="snow_profile_24hr"):
        rebuilt_inactive.validate_for_engine()

    # Inactive snow with properties.snow = None also avoids fabricating usable defaults.
    null_block_data = copy.deepcopy(sample_dict)
    _set_snow_use(null_block_data, 0)
    null_block_data["sites"][0]["properties"]["snow"] = None
    null_block_cfg = SUEWSConfig.from_dict(null_block_data)
    null_block_df = null_block_cfg.to_df_state()
    assert pd.isna(null_block_df.loc[grid_id, ("tau_a", "0")])
    rebuilt_null_block = SUEWSConfig.from_df_state(null_block_df)
    rebuilt_null_block.validate_for_engine()
    rebuilt_null_block.model.physics.snow_use = SnowUse.ENABLED
    with pytest.raises(ValueError, match="water_holding_capacity_max"):
        rebuilt_null_block.validate_for_engine()


def test_snowalb_relaxed_construction_retains_unconditional_non_null_guard(
    sample_dict: dict,
) -> None:
    """Wrapped ``snowalb`` still rejects ``.value = None`` after snow-disabled construction."""
    for raw_snowalb in ({"value": 0.5}, {"value": -999.0}):
        data = copy.deepcopy(sample_dict)
        _set_snow_use(data, 0)
        data["sites"][0]["initial_states"]["snowalb"] = raw_snowalb
        cfg = SUEWSConfig.from_dict(data)
        with pytest.raises(ValueError, match="snowalb"):
            cfg.sites[0].initial_states.snowalb.value = None
        cfg.model.physics.snow_use = SnowUse.ENABLED
        with pytest.raises(ValueError, match="snowalb"):
            cfg.sites[0].initial_states.snowalb.value = None


def test_yaml_roundtrip_preserves_inactive_snow_and_soil_nulls(
    sample_dict: dict, tmp_path: Path
) -> None:
    """``to_yaml()`` -> ``from_yaml()`` preserves explicit inactive nulls instead of defaults."""
    data = copy.deepcopy(sample_dict)
    _set_snow_use(data, 0)
    data["model"]["physics"]["soil_moisture"] = "modelled"
    data["sites"][0]["properties"]["snow"]["tau_cold_snow"] = None
    data["sites"][0]["properties"]["snow"]["water_holding_capacity_max"] = {
        "value": None
    }
    data["sites"][0]["properties"]["snow"]["snow_profile_24hr"] = None
    data["sites"][0]["initial_states"]["paved"]["snowfrac"] = None
    data["sites"][0]["properties"]["soil_observation"] = {
        "layer_depth": 0.2,
        "layer_top": None,
        "soil_not_rocks": {"value": None},
    }
    cfg = SUEWSConfig.from_dict(data)
    yaml_path = tmp_path / "roundtrip_nulls.yml"
    cfg.to_yaml(str(yaml_path))

    reloaded = SUEWSConfig.from_yaml(str(yaml_path))
    assert reloaded.sites[0].properties.snow.tau_cold_snow is None
    assert isinstance(
        reloaded.sites[0].properties.snow.water_holding_capacity_max, RefValue
    )
    assert reloaded.sites[0].properties.snow.water_holding_capacity_max.value is None
    assert reloaded.sites[0].properties.snow.snow_profile_24hr is None
    assert reloaded.sites[0].initial_states.paved.snowfrac is None
    assert reloaded.sites[0].properties.soil_observation.layer_top is None
    assert reloaded.sites[0].properties.soil_observation.soil_not_rocks.value is None
    reloaded.validate_for_engine()

    # Enabling either scheme rejects the reloaded configuration just like the original.
    reloaded_snow = copy.deepcopy(reloaded)
    reloaded_snow.model.physics.snow_use = SnowUse.ENABLED
    with pytest.raises(ValueError, match="water_holding_capacity_max"):
        reloaded_snow.validate_for_engine()

    reloaded_soil = copy.deepcopy(reloaded)
    reloaded_soil.model.physics.soil_moisture = SMDMethod.OBSERVED
    with pytest.raises(ValueError, match="layer_top"):
        reloaded_soil.validate_for_engine()

    # properties.snow: null also survives YAML round-trip without inventing SnowParams().
    null_snow_data = copy.deepcopy(sample_dict)
    _set_snow_use(null_snow_data, 0)
    null_snow_data["sites"][0]["properties"]["snow"] = None
    null_snow_cfg = SUEWSConfig.from_dict(null_snow_data)
    null_snow_path = tmp_path / "roundtrip_null_snow_block.yml"
    null_snow_cfg.to_yaml(str(null_snow_path))
    reloaded_null_snow = SUEWSConfig.from_yaml(str(null_snow_path))
    assert reloaded_null_snow.sites[0].properties.snow is None


def test_soil_df_state_roundtrip_preserves_inactive_null_and_nan(
    sample_dict: dict,
) -> None:
    """Soil DataFrame round-trip preserves ``layer_top=None`` and ``soil_not_rocks=NaN``."""
    data = _make_active_observed_soil_dict(sample_dict)
    data["model"]["physics"]["soil_moisture"] = "modelled"
    data["sites"][0]["properties"]["soil_observation"] = {
        "layer_depth": 0.2,
        "layer_top": None,
        "soil_not_rocks": float("nan"),
    }
    cfg = SUEWSConfig.from_dict(data)
    df_state = cfg.to_df_state()
    grid_id = cfg.sites[0].gridiv
    assert pd.isna(df_state.loc[grid_id, ("obs_sm_layer_top", "0")])
    assert pd.isna(df_state.loc[grid_id, ("obs_sm_soil_not_rocks", "0")])

    rebuilt = SUEWSConfig.from_df_state(df_state)
    assert rebuilt.sites[0].properties.soil_observation is not None
    assert rebuilt.sites[0].properties.soil_observation.layer_top is None
    assert pd.isna(rebuilt.sites[0].properties.soil_observation.soil_not_rocks)
    rebuilt.validate_for_engine()

    # Activating observed soil rejects the rebuilt configuration rather than defaulting.
    rebuilt.model.physics.soil_moisture = SMDMethod.OBSERVED
    with pytest.raises(ValueError, match="layer_top"):
        rebuilt.validate_for_engine()


@pytest.mark.parametrize(
    ("mutator", "description"),
    [
        (
            lambda d: d["sites"][0]["properties"]["snow"].__setitem__(
                "water_holding_capacity_max", {"value": float("nan")}
            ),
            "wrapped NaN snow scalar",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].__setitem__(
                "tau_cold_snow", float("inf")
            ),
            "bare +Inf snow scalar",
        ),
        (
            lambda d: d["sites"][0]["properties"]["snow"].__setitem__(
                "snow_density_min", {"value": float("-inf")}
            ),
            "wrapped -Inf optional snow scalar",
        ),
        (
            lambda d: d["sites"][0]["initial_states"]["paved"].__setitem__(
                "snowpack", float("inf")
            ),
            "bare +Inf surface snowpack state",
        ),
    ],
)
def test_non_finite_inactive_snow_values_rejected_in_lockstep_with_native(
    sample_dict: dict, mutator, description: str
) -> None:
    """NaN/Inf snow scalars and states fail in Python even when ``snow_use == 0``."""
    data = copy.deepcopy(sample_dict)
    _set_snow_use(data, 0)
    mutator(data)
    with pytest.raises((ValidationError, ValueError)):
        SUEWSConfig.from_dict(data)

    # Also verify live mutation and handoff guard reject non-finite values.
    clean = copy.deepcopy(sample_dict)
    _set_snow_use(clean, 0)
    cfg = SUEWSConfig.from_dict(clean)
    with pytest.raises((ValidationError, ValueError)):
        cfg.sites[0].properties.snow.water_holding_capacity_max = {
            "value": float("nan")
        }
    cfg.sites[0].properties.snow.water_holding_capacity_max.value = float("nan")
    with pytest.raises(ValueError, match="water_holding_capacity_max"):
        cfg.validate_for_engine()


# =============================================================================
# 2. Observed soil scheme-gated validation (soil_moisture != 1 vs == 1)
# =============================================================================


@pytest.mark.parametrize(
    ("mutator", "description"),
    [
        (
            lambda d: d["sites"][0]["properties"].pop("soil_observation", None),
            "omitted soil_observation",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__("soil_observation", {}),
            "empty soil_observation (omitted layer_depth)",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__(
                "soil_observation",
                {
                    "layer_depth": -999.0,
                    "layer_top": -0.5,
                    "bulk_density": 0.0,
                    "soil_not_rocks": 0.0,
                },
            ),
            "out-of-range soil_observation fields",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__(
                "soil_observation",
                {
                    "layer_depth": None,
                    "layer_top": {"value": None},
                    "soil_not_rocks": float("nan"),
                    "bulk_density": float("inf"),
                },
            ),
            "null and non-finite soil_observation fields",
        ),
        (
            lambda d: d["sites"][0]["properties"]["land_cover"]["paved"].__setitem__(
                "soil_field_capacity", -999.0
            ),
            "negative sentinel soil_field_capacity",
        ),
        (
            lambda d: d["sites"][0]["properties"]["land_cover"]["grass"].__setitem__(
                "soil_field_capacity", 0.0
            ),
            "zero soil_field_capacity",
        ),
        (
            lambda d: d["sites"][0]["properties"]["land_cover"]["bldgs"].__setitem__(
                "soil_field_capacity", 1.5
            ),
            "above-unity soil_field_capacity",
        ),
        (
            lambda d: d["sites"][0]["properties"]["land_cover"]["paved"].__setitem__(
                "soil_field_capacity", float("nan")
            ),
            "NaN soil_field_capacity",
        ),
        (
            lambda d: d["sites"][0]["properties"]["land_cover"]["water"].__setitem__(
                "soil_field_capacity", -999.0
            ),
            "sentinel soil_field_capacity on water surface",
        ),
    ],
)
def test_modelled_soil_accepts_relaxed_soil_observation_and_field_capacity(
    sample_dict: dict, mutator, description: str
) -> None:
    """When ``soil_moisture != 1``, inactive observed-soil fields never block load or export."""
    data = copy.deepcopy(sample_dict)
    data["model"]["physics"]["soil_moisture"] = "modelled"
    mutator(data)

    cfg = SUEWSConfig.from_dict(data)
    cfg.validate_for_engine()
    _assert_clean_json_dump(cfg)
    yaml_text = config_to_engine_yaml(cfg, for_file_output=False)
    assert isinstance(yaml_text, str) and yaml_text

    # Round-trip via df_state must also succeed cleanly and preserve grid IDs.
    df_state = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(df_state)
    assert [s.gridiv for s in rebuilt.sites] == [s.gridiv for s in cfg.sites], (
        description
    )
    assert ("snowfrac", "(0,)") in df_state.columns
    assert ("snowpack", "(0,)") in df_state.columns


@pytest.mark.parametrize(
    ("mutator", "pattern"),
    [
        (
            lambda d: d["sites"][0]["properties"].pop("soil_observation", None),
            r"soil_observation",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__("soil_observation", {}),
            r"layer_depth",
        ),
        (
            lambda d: d["sites"][0]["properties"]["soil_observation"].__setitem__(
                "layer_depth", -0.2
            ),
            r"layer_depth",
        ),
        (
            lambda d: d["sites"][0]["properties"]["soil_observation"].__setitem__(
                "layer_top", 0.05
            ),
            r"layer_top",
        ),
        (
            lambda d: d["sites"][0]["properties"]["soil_observation"].__setitem__(
                "soil_not_rocks", 0.0
            ),
            r"soil_not_rocks",
        ),
        (
            lambda d: d["sites"][0]["properties"]["soil_observation"].__setitem__(
                "bulk_density", 0.0
            ),
            r"bulk_density",
        ),
        (
            lambda d: d["sites"][0]["properties"]["land_cover"]["paved"].__setitem__(
                "soil_field_capacity", None
            ),
            r"soil_field_capacity",
        ),
        (
            lambda d: d["sites"][0]["properties"]["land_cover"]["paved"].__setitem__(
                "soil_field_capacity", -999.0
            ),
            r"soil_field_capacity",
        ),
        (
            lambda d: d["sites"][0]["properties"]["land_cover"]["paved"].__setitem__(
                "soil_field_capacity", 0.3
            ),
            r"soil_field_capacity",
        ),
    ],
)
def test_observed_soil_strictly_rejects_invalid_observation_and_field_capacity(
    sample_dict: dict, mutator, pattern: str
) -> None:
    """When ``soil_moisture == 1``, invalid/missing observed-soil parameters fail strictly."""
    data = _make_active_observed_soil_dict(sample_dict)
    mutator(data)

    with pytest.raises((ValueError, ValidationError), match=pattern):
        SUEWSConfig.from_dict(data)


@pytest.mark.parametrize(
    ("mutator", "pattern"),
    [
        (
            lambda d: d["sites"][0]["properties"].__setitem__("soil_observation", {}),
            r"layer_depth",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__(
                "soil_observation", {"layer_depth": -1.0}
            ),
            r"layer_depth",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__(
                "soil_observation", {"layer_depth": 0.2, "layer_top": -0.1}
            ),
            r"layer_top",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__(
                "soil_observation", {"layer_depth": 0.2, "soil_not_rocks": 0.0}
            ),
            r"soil_not_rocks",
        ),
        (
            lambda d: d["sites"][0]["properties"].__setitem__(
                "soil_observation", {"layer_depth": 0.2, "bulk_density": -1.0}
            ),
            r"bulk_density",
        ),
        (
            lambda d: d["sites"][0]["properties"]["land_cover"]["paved"].__setitem__(
                "soil_field_capacity", -999.0
            ),
            r"soil_field_capacity",
        ),
    ],
)
def test_observed_soil_activation_after_load_and_deepcopy_enforces_strict_checks(
    sample_dict: dict, mutator, pattern: str
) -> None:
    """Switching ``soil_moisture`` from 0 to 1 post-construction catches relaxed values."""
    data = _make_active_observed_soil_dict(sample_dict)
    data["model"]["physics"]["soil_moisture"] = "modelled"
    mutator(data)

    original = SUEWSConfig.from_dict(data)
    original.validate_for_engine()

    copied = copy.deepcopy(original)
    copied.model.physics.soil_moisture = SMDMethod.OBSERVED
    with pytest.raises(ValueError, match=pattern):
        copied.validate_for_engine()

    # Original config stays modelled and valid.
    original.validate_for_engine()

    model_copied = original.model_copy(deep=True)
    model_copied.model.physics.soil_moisture = SMDMethod.OBSERVED
    with pytest.raises(ValueError, match=pattern):
        model_copied.validate_for_engine()


def test_standalone_soil_observation_and_surface_keep_strict_validation() -> None:
    """Standalone models constructed outside SUEWSConfig enforce strict bounds."""
    with pytest.raises(ValidationError):
        SoilObservationConfig()
    with pytest.raises(ValidationError):
        SoilObservationConfig(layer_depth=-0.1)
    with pytest.raises(ValidationError):
        PavedProperties(soil_field_capacity=0.0)
    with pytest.raises(ValidationError):
        PavedProperties(soil_field_capacity=1.5)


# =============================================================================
# 3. End-to-end Rust engine execution invariance & zero JSON schema diff
# =============================================================================


def test_inactive_snow_and_observed_soil_run_bit_identical_in_engine(
    short_sample_sim,
) -> None:
    """Relaxed inactive snow and observed-soil configs run bit-identically to control."""
    control_sim = short_sample_sim()
    df_forcing = control_sim.forcing.df.iloc[:12].copy()
    control_sim.update_forcing(df_forcing)
    df_control = control_sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    ).df

    raw_relaxed = control_sim.config.model_dump(mode="json")
    _set_snow_use(raw_relaxed, 0)
    raw_relaxed["model"]["physics"]["soil_moisture"] = "modelled"
    site0 = raw_relaxed["sites"][0]
    site0["properties"]["snow"].update(
        water_holding_capacity_min=0.95,
        water_holding_capacity_max=0.05,
        snow_albedo_min=0.95,
        snow_albedo_max=0.1,
        narp_emissivity_snow=-999.0,
        precipitation_threshold_albedo_reset=-5.0,
        tau_cold_snow={"value": None},
    )
    site0["properties"]["soil_observation"] = {
        "layer_depth": -999.0,
        "layer_top": -1.0,
        "bulk_density": 0.0,
        "soil_not_rocks": 0.0,
    }
    for surf in NON_WATER_SURFACES:
        site0["properties"]["land_cover"][surf]["soil_field_capacity"] = -999.0
    site0["initial_states"]["snowalb"] = -999.0
    site0["initial_states"]["paved"].update(
        snowfrac=-999.0,
        snowpack=-10.0,
        icefrac=2.0,
        snowwater=-1.0,
        snowdens=-50.0,
    )

    relaxed_cfg = SUEWSConfig.from_dict(raw_relaxed)
    relaxed_sim = SUEWSSimulation()
    relaxed_sim.update_config(relaxed_cfg, auto_load_forcing=False)
    relaxed_sim.update_forcing(df_forcing)
    df_relaxed = relaxed_sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    ).df

    pd.testing.assert_frame_equal(df_relaxed, df_control, check_exact=True)


def test_inactive_null_snow_block_and_profile_run_bit_identical_in_engine(
    short_sample_sim,
) -> None:
    """``properties.snow: null`` and ``snow_profile_24hr: null`` match control bit-identically."""
    control_sim = short_sample_sim()
    df_forcing = control_sim.forcing.df.iloc[:12].copy()
    control_sim.update_forcing(df_forcing)
    df_control = control_sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    ).df

    for mutator in (
        lambda d: d["sites"][0]["properties"].__setitem__("snow", None),
        lambda d: d["sites"][0]["properties"]["snow"].__setitem__(
            "snow_profile_24hr", None
        ),
    ):
        raw_variant = control_sim.config.model_dump(mode="json")
        _set_snow_use(raw_variant, 0)
        mutator(raw_variant)

        variant_cfg = SUEWSConfig.from_dict(raw_variant)
        variant_sim = SUEWSSimulation()
        variant_sim.update_config(variant_cfg, auto_load_forcing=False)
        variant_sim.update_forcing(df_forcing)
        df_variant = variant_sim.run(
            start_date=df_forcing.index[0],
            end_date=df_forcing.index[-1],
            n_jobs=1,
        ).df

        pd.testing.assert_frame_equal(df_variant, df_control, check_exact=True)


def test_json_schema_unchanged_by_scheme_gating() -> None:
    """Scheme-gated validation introduces zero diff to snow/soil schema definitions."""
    schema_path = PROJECT_ROOT / "docs" / "source" / "inputs" / "tables" / "schema.json"
    committed = json.loads(schema_path.read_text(encoding="utf-8"))
    live_defs = generate_json_schema()["$defs"]
    committed_defs = committed["$defs"]

    for def_name in (
        "SnowParams",
        "SoilObservationConfig",
        "SiteProperties",
        "SurfaceInitialState",
        "InitialStates",
    ):
        assert live_defs[def_name] == committed_defs[def_name], (
            f"Unexpected schema diff in $defs.{def_name}"
        )

    assert (
        live_defs["PavedProperties"]["properties"]["soil_field_capacity"]
        == committed_defs["PavedProperties"]["properties"]["soil_field_capacity"]
    )
