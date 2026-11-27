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
from supy.data_model.core.state import InitialStates, SurfaceInitialState
from supy.data_model.core.surface import PavedProperties

pytestmark = [
    pytest.mark.api,
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

    # Round-trip via df_state must also succeed cleanly.
    df_state = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(df_state)
    assert len(rebuilt.sites) == len(cfg.sites), description


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

    # Round-trip via df_state must also succeed cleanly.
    df_state = cfg.to_df_state()
    rebuilt = SUEWSConfig.from_df_state(df_state)
    assert len(rebuilt.sites) == len(cfg.sites), description


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


def test_inactive_null_snow_block_runs_bit_identical_in_engine(
    short_sample_sim,
) -> None:
    """``properties.snow: null`` under ``snow_use == 0`` executes bit-identically in engine."""
    control_sim = short_sample_sim()
    df_forcing = control_sim.forcing.df.iloc[:12].copy()
    control_sim.update_forcing(df_forcing)
    df_control = control_sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    ).df

    raw_null_snow = control_sim.config.model_dump(mode="json")
    _set_snow_use(raw_null_snow, 0)
    raw_null_snow["sites"][0]["properties"]["snow"] = None

    null_snow_cfg = SUEWSConfig.from_dict(raw_null_snow)
    null_snow_sim = SUEWSSimulation()
    null_snow_sim.update_config(null_snow_cfg, auto_load_forcing=False)
    null_snow_sim.update_forcing(df_forcing)
    df_null_snow = null_snow_sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    ).df

    pd.testing.assert_frame_equal(df_null_snow, df_control, check_exact=True)


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
