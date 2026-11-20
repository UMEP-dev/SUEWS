"""Guards for the conditionally-required field registry.

The registry is consumed by two unrelated places: the configuration validator,
which emits its message strings, and the documentation generator, which
describes its conditions to readers. Neither notices if the other drifts, so
the coupling is pinned here.
"""

from __future__ import annotations

from importlib.resources import files
from pathlib import Path

import pytest
import yaml

from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.human_activity import (
    AnthropogenicEmissions,
    CO2Params,
    IrrigationParams,
)
from supy.data_model.core.hydro import WaterDistribution
from supy.data_model.core.site import (
    Conductance,
    DectrProperties,
    EvetrProperties,
    GrassProperties,
    LAIParams,
    StebbsProperties,
)
from supy.data_model.core.surface import (
    BldgsProperties,
    BsoilProperties,
    PavedProperties,
    SurfaceProperties,
    ThermalLayers,
)
from supy.data_model.core.type import SurfaceType
from supy.data_model.validation.required_fields import (
    BUILDING_REQUIRED,
    BUILDING_REQUIRED_PROVIDED_FAI,
    CO2_POINT_SOURCE_REQUIRED,
    CONDUCTANCE_REQUIRED,
    DECIDUOUS_REQUIRED,
    DECIDUOUS_REQUIRED_PROVIDED_FAI,
    DOC_REQUIRED_WHEN,
    DYOHM_BUILDING_LAYERS_REQUIRED,
    EVERGREEN_REQUIRED,
    EVERGREEN_REQUIRED_PROVIDED_FAI,
    LAI_CALCULATED_ONLY_REQUIRED,
    LAI_REQUIRED,
    RSL_VARIABLE_BLDGS_REQUIRED,
    SOIL_OBSERVED_REQUIRED,
    SOIL_REQUIRED,
    STEBBS_SHADING_REQUIRED_MODES,
    WATER_DISTRIBUTION_REQUIRED,
    required_when,
)

pytestmark = pytest.mark.api

MODELS = {
    "Conductance": Conductance,
    "LAIParams": LAIParams,
    "SurfaceProperties": SurfaceProperties,
    "PavedProperties": PavedProperties,
    "BldgsProperties": BldgsProperties,
    "EvetrProperties": EvetrProperties,
    "DectrProperties": DectrProperties,
    "GrassProperties": GrassProperties,
    "BsoilProperties": BsoilProperties,
    "AnthropogenicEmissions": AnthropogenicEmissions,
    "CO2Params": CO2Params,
    "ThermalLayers": ThermalLayers,
    "StebbsProperties": StebbsProperties,
    "IrrigationParams": IrrigationParams,
    "WaterDistribution": WaterDistribution,
}

# Field lists the validators consume, keyed by the model that declares them.
# A field added to any of these must also be described in DOC_REQUIRED_WHEN.
RULE_FIELDS_BY_MODEL = {
    "SurfaceProperties": SOIL_OBSERVED_REQUIRED,
    "PavedProperties": SOIL_OBSERVED_REQUIRED,
    "BldgsProperties": RSL_VARIABLE_BLDGS_REQUIRED + SOIL_OBSERVED_REQUIRED,
    "EvetrProperties": SOIL_OBSERVED_REQUIRED,
    "DectrProperties": SOIL_OBSERVED_REQUIRED,
    "GrassProperties": SOIL_OBSERVED_REQUIRED,
    "BsoilProperties": SOIL_OBSERVED_REQUIRED,
    "ThermalLayers": DYOHM_BUILDING_LAYERS_REQUIRED,
    "StebbsProperties": tuple(STEBBS_SHADING_REQUIRED_MODES),
    "WaterDistribution": tuple(
        {field for fields in WATER_DISTRIBUTION_REQUIRED.values() for field in fields}
    ),
}

ALL_TABLES = (
    LAI_REQUIRED,
    LAI_CALCULATED_ONLY_REQUIRED,
    CONDUCTANCE_REQUIRED,
    BUILDING_REQUIRED,
    BUILDING_REQUIRED_PROVIDED_FAI,
    EVERGREEN_REQUIRED,
    EVERGREEN_REQUIRED_PROVIDED_FAI,
    DECIDUOUS_REQUIRED,
    DECIDUOUS_REQUIRED_PROVIDED_FAI,
    CO2_POINT_SOURCE_REQUIRED,
    SOIL_REQUIRED,
)

EXPECTED_TABLES = {
    "LAI_REQUIRED": {
        "lai_max": (
            "Maximum LAI is required for active vegetation",
            "Add maximum leaf area index for full leaf-on conditions",
        ),
    },
    "LAI_CALCULATED_ONLY_REQUIRED": {
        "base_temperature": (
            "Base temperature is required for active vegetation",
            "Add the base temperature for growing degree day accumulation",
        ),
        "base_temperature_senescence": (
            "Senescence base temperature is required for active vegetation",
            "Add the base temperature for senescence degree day accumulation",
        ),
        "gdd_full": (
            "Growing degree days for full LAI are required for active vegetation",
            "Add the growing degree day threshold for full leaf-on conditions",
        ),
        "sdd_full": (
            "Senescence degree days are required for active vegetation",
            "Add the senescence degree day threshold for leaf-off conditions",
        ),
    },
    "CONDUCTANCE_REQUIRED": {
        "g_max": (
            "Surface conductance scaling coefficient is required for active vegetation",
            "Add g_max for evapotranspiration calculations",
        ),
        "g_k": (
            "Solar radiation response parameter is required for active vegetation",
            "Add g_k for evapotranspiration calculations",
        ),
        "g_q_base": (
            "Vapour pressure deficit base parameter is required for active vegetation",
            "Add g_q_base for evapotranspiration calculations",
        ),
        "g_q_shape": (
            "Vapour pressure deficit shape parameter is required for active vegetation",
            "Add g_q_shape for evapotranspiration calculations",
        ),
        "g_t": (
            "Temperature response parameter is required for active vegetation",
            "Add g_t for evapotranspiration calculations",
        ),
        "g_sm": (
            "Soil moisture response parameter is required for active vegetation",
            "Add g_sm for evapotranspiration calculations",
        ),
        "kmax": (
            "Maximum shortwave radiation parameter is required for active vegetation",
            "Add kmax for evapotranspiration calculations",
        ),
        "s1": (
            "Lower soil moisture threshold is required for active vegetation",
            "Add s1 for evapotranspiration calculations",
        ),
        "s2": (
            "Soil moisture dependence parameter is required for active vegetation",
            "Add s2 for evapotranspiration calculations",
        ),
        "tl": (
            "Lower temperature threshold is required for active vegetation",
            "Add tl for evapotranspiration calculations",
        ),
        "th": (
            "Upper temperature threshold is required for active vegetation",
            "Add th for evapotranspiration calculations",
        ),
    },
    "BUILDING_REQUIRED": {
        "bldgh": (
            "Building height is required when buildings are active",
            "Add building height in meters",
        ),
    },
    "BUILDING_REQUIRED_PROVIDED_FAI": {
        "faibldg": (
            "Building frontal area index is required when buildings are active",
            "Add frontal area index for wind and roughness calculations",
        ),
    },
    "EVERGREEN_REQUIRED": {
        "height_evergreen_tree": (
            "Evergreen tree height is required when evergreen vegetation is active",
            "Add evergreen tree height in meters",
        ),
    },
    "EVERGREEN_REQUIRED_PROVIDED_FAI": {
        "fai_evergreen_tree": (
            "Evergreen tree frontal area index is required when evergreen vegetation is active",
            "Add evergreen tree frontal area index",
        ),
    },
    "DECIDUOUS_REQUIRED": {
        "height_deciduous_tree": (
            "Deciduous tree height is required when deciduous vegetation is active",
            "Add deciduous tree height in meters",
        ),
    },
    "DECIDUOUS_REQUIRED_PROVIDED_FAI": {
        "fai_deciduous_tree": (
            "Deciduous tree frontal area index is required when deciduous vegetation is active",
            "Add deciduous tree frontal area index",
        ),
    },
    "CO2_POINT_SOURCE_REQUIRED": {
        "emission_co2_point_source": (
            "CO2 point source is required when the emissions method uses it",
            "Add the whole-grid CO2 point source in kg C per day (0 if there is none)",
        ),
    },
    "SOIL_REQUIRED": {
        "soil_depth": (
            "Soil depth is required for active non-water surfaces",
            "Add soil depth in millimetres beneath this surface",
        ),
        "soil_store_capacity": (
            "Soil store capacity is required for active non-water surfaces",
            "Add maximum water storage capacity of the soil layer in kg m-2",
        ),
        "saturated_hydraulic_conductivity": (
            "Saturated hydraulic conductivity is required for active non-water surfaces",
            "Add saturated hydraulic conductivity of the soil layer in mm s-1",
        ),
    },
}


def test_documented_fields_exist_on_their_models() -> None:
    """Every documented condition names a field that really exists."""
    # ARRANGE / ACT
    missing = [
        f"{model_name}.{field}"
        for model_name, fields in DOC_REQUIRED_WHEN.items()
        for field in fields
        if field not in MODELS[model_name].model_fields
    ]

    # ASSERT
    assert not missing


def test_every_required_field_has_a_documented_condition() -> None:
    """A field the validator can demand must be explainable to the reader."""
    # ARRANGE
    documented = {field for fields in DOC_REQUIRED_WHEN.values() for field in fields}

    # ACT
    undocumented = {field for table in ALL_TABLES for field in table} - documented

    # ASSERT
    assert not undocumented


def test_every_physics_rule_field_has_a_documented_condition() -> None:
    """Fields the physics-dependent validators demand are described on their model."""
    # ARRANGE / ACT
    undocumented = [
        f"{model_name}.{field}"
        for model_name, fields in RULE_FIELDS_BY_MODEL.items()
        for field in fields
        if field not in DOC_REQUIRED_WHEN.get(model_name, {})
    ]

    # ASSERT
    assert not undocumented


@pytest.mark.parametrize(
    ("model_name", "field", "option"),
    [
        ("LAIParams", "gdd_full", "``model.physics.laimethod``"),
        ("BldgsProperties", "faibldg", "``model.physics.frontal_area_index``"),
        ("BldgsProperties", "faibldg", "``model.physics.roughness_sublayer``"),
        ("ThermalLayers", "dz", "``model.physics.storage_heat``"),
        ("StebbsProperties", "reduction_factor_shading", "``internal_shading``"),
        ("CO2Params", "emission_co2_point_source", "``model.physics.emissions``"),
        ("SurfaceProperties", "soil_field_capacity", "``model.physics.soil_moisture``"),
    ],
)
def test_physics_conditions_name_their_option(
    model_name: str, field: str, option: str
) -> None:
    """A physics-dependent condition names the option that triggers it."""
    # ARRANGE / ACT / ASSERT
    assert option in required_when(model_name, field)


def test_water_distribution_conditions_name_the_right_surfaces() -> None:
    """Each destination's condition lists exactly the surfaces that need it."""
    # ARRANGE / ACT / ASSERT
    for surface, fields in WATER_DISTRIBUTION_REQUIRED.items():
        for field in WaterDistribution.model_fields:
            if not field.startswith("to_"):
                continue
            named = f"``{surface}``" in required_when("WaterDistribution", field)
            assert named == (field in fields), (surface, field)


def _load_sample_raw() -> dict:
    return yaml.safe_load(
        (files("supy") / "sample_data" / "sample_config.yml").read_text(
            encoding="utf-8"
        )
    )


def test_water_distribution_missing_field_rejected_by_from_yaml(
    tmp_path: Path,
) -> None:
    """Omitting a required waterdist destination fails SUEWSConfig.from_yaml()."""
    # ARRANGE
    raw = _load_sample_raw()
    del raw["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"]["to_runoff"]
    cfg_path = tmp_path / "missing_to_runoff.yml"
    cfg_path.write_text(yaml.safe_dump(raw), encoding="utf-8")

    # ACT / ASSERT
    with pytest.raises(ValueError, match="Missing required field to_runoff for paved"):
        SUEWSConfig.from_yaml(str(cfg_path))


def test_water_distribution_null_refvalue_rejected_by_from_yaml(
    tmp_path: Path,
) -> None:
    """Passing {value: null} on a waterdist destination fails SUEWSConfig.from_yaml()."""
    # ARRANGE
    raw = _load_sample_raw()
    raw["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"]["to_runoff"] = {
        "value": None
    }
    cfg_path = tmp_path / "null_to_runoff.yml"
    cfg_path.write_text(yaml.safe_dump(raw), encoding="utf-8")

    # ACT / ASSERT
    with pytest.raises(ValueError, match="Missing required field to_runoff for paved"):
        SUEWSConfig.from_yaml(str(cfg_path))


def test_water_distribution_non_unit_sum_rejected_by_from_yaml(
    tmp_path: Path,
) -> None:
    """A waterdist block whose fractions do not sum to 1.0 fails SUEWSConfig.from_yaml()."""
    # ARRANGE
    raw = _load_sample_raw()
    raw["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"]["to_runoff"] = {
        "value": 0.5
    }
    cfg_path = tmp_path / "bad_sum.yml"
    cfg_path.write_text(yaml.safe_dump(raw), encoding="utf-8")

    # ACT / ASSERT
    with pytest.raises(ValueError, match=r"Water distribution sum must be 1\.0"):
        SUEWSConfig.from_yaml(str(cfg_path))


@pytest.mark.parametrize(
    ("case", "payload"),
    [
        ("omitted", None),
        ("null", None),
        ("wrapped-null", {"value": None}),
    ],
)
def test_water_distribution_null_or_omitted_disallowed_terminal_accepted(
    case: str, payload: object, tmp_path: Path
) -> None:
    """Omitted, null, and wrapped-null inactive terminal aliases load and keep to_runoff."""
    # ARRANGE
    raw = _load_sample_raw()
    dist = raw["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"]
    if case != "omitted":
        dist["to_soilstore"] = payload
    if case == "wrapped-null":
        dist["to_paved"] = {"value": None}
    cfg_path = tmp_path / f"paved_soilstore_{case}.yml"
    cfg_path.write_text(yaml.safe_dump(raw), encoding="utf-8")

    # ACT
    cfg_yaml = SUEWSConfig.from_yaml(str(cfg_path))
    cfg_dict = SUEWSConfig.from_dict(raw)

    # ASSERT - to_df_state must preserve to_runoff=0.98 and self-slot=0.0 (no NaN)
    for cfg in (cfg_yaml, cfg_dict):
        df_state = cfg.to_df_state()
        assert df_state.loc[1, ("waterdist", "(0, 0)")] == pytest.approx(0.0)
        assert df_state.loc[1, ("waterdist", "(7, 0)")] == pytest.approx(0.98)


@pytest.mark.parametrize(
    ("case", "payload"),
    [
        ("zero", {"value": 0.0}),
        ("bare-zero", 0.0),
        ("half", {"value": 0.5}),
        ("bare-half", 0.5),
    ],
)
def test_water_distribution_paved_soilstore_beside_runoff_rejected(
    case: str, payload: object, tmp_path: Path
) -> None:
    """Any numeric paved.waterdist.to_soilstore (including 0.0) is rejected."""
    # ARRANGE
    raw = _load_sample_raw()
    raw["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"][
        "to_soilstore"
    ] = payload
    cfg_path = tmp_path / f"paved_soilstore_{case}.yml"
    cfg_path.write_text(yaml.safe_dump(raw), encoding="utf-8")

    # ACT / ASSERT
    pattern = r"Disallowed water distribution field to_soilstore for paved"
    with pytest.raises(ValueError, match=pattern):
        SUEWSConfig.from_yaml(str(cfg_path))
    with pytest.raises(ValueError, match=pattern):
        SUEWSConfig.from_dict(raw)


@pytest.mark.parametrize(
    ("surface", "disallowed_field"),
    [
        ("paved", "to_soilstore"),
        ("paved", "to_paved"),
        ("bldgs", "to_soilstore"),
        ("bldgs", "to_bldgs"),
        ("dectr", "to_runoff"),
        ("dectr", "to_dectr"),
        ("evetr", "to_runoff"),
        ("evetr", "to_evetr"),
        ("grass", "to_runoff"),
        ("grass", "to_grass"),
        ("bsoil", "to_runoff"),
        ("bsoil", "to_bsoil"),
    ],
)
@pytest.mark.parametrize("raw_value", [0.0, 0.5])
def test_water_distribution_disallowed_destinations_rejected_all_surfaces(
    surface: str, disallowed_field: str, raw_value: float, tmp_path: Path
) -> None:
    """Every non-water surface rejects self-routing and the opposite terminal field."""
    # ARRANGE
    raw = _load_sample_raw()
    raw["sites"][0]["properties"]["land_cover"][surface]["waterdist"][
        disallowed_field
    ] = {"value": raw_value}
    cfg_path = tmp_path / f"{surface}_{disallowed_field}_{raw_value}.yml"
    cfg_path.write_text(yaml.safe_dump(raw), encoding="utf-8")

    # ACT / ASSERT
    pattern = rf"Disallowed water distribution field {disallowed_field} for {surface}"
    with pytest.raises(ValueError, match=pattern):
        SUEWSConfig.from_yaml(str(cfg_path))


def test_water_distribution_disallowed_rejected_on_direct_init() -> None:
    """Direct WaterDistribution(surface_type, ...) also rejects disallowed fields."""
    # ACT / ASSERT
    with pytest.raises(
        ValueError, match="Disallowed water distribution field to_soilstore for paved"
    ):
        WaterDistribution(SurfaceType.PAVED, to_soilstore=0.0)
    with pytest.raises(
        ValueError, match="Disallowed water distribution field to_paved for paved"
    ):
        WaterDistribution(SurfaceType.PAVED, to_paved=0.5)
    with pytest.raises(
        ValueError, match="Disallowed water distribution field to_runoff for grass"
    ):
        WaterDistribution(SurfaceType.GRASS, to_runoff=0.0)


def test_water_distribution_water_surface_null_allowed_non_null_rejected(
    tmp_path: Path,
) -> None:
    """water.waterdist: null loads cleanly while any mapping is rejected."""
    # ARRANGE
    raw_null = _load_sample_raw()
    raw_null["sites"][0]["properties"]["land_cover"]["water"]["waterdist"] = None
    path_null = tmp_path / "water_null.yml"
    path_null.write_text(yaml.safe_dump(raw_null), encoding="utf-8")

    raw_bad = _load_sample_raw()
    raw_bad["sites"][0]["properties"]["land_cover"]["water"]["waterdist"] = {
        "to_paved": {"value": 0.5}
    }
    path_bad = tmp_path / "water_bad.yml"
    path_bad.write_text(yaml.safe_dump(raw_bad), encoding="utf-8")

    # ACT / ASSERT
    cfg = SUEWSConfig.from_yaml(str(path_null))
    assert cfg.sites[0].properties.land_cover.water.waterdist is None
    with pytest.raises(
        ValueError, match="Water surface should not have water distribution"
    ):
        SUEWSConfig.from_yaml(str(path_bad))


@pytest.mark.parametrize("bad_value", [-0.1, -999.0, float("nan")])
def test_water_distribution_invalid_numeric_values_rejected(
    bad_value: float, tmp_path: Path
) -> None:
    """Negative, sentinel (-999), and NaN waterdist fractions are rejected."""
    # ARRANGE
    raw = _load_sample_raw()
    raw["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"]["to_runoff"] = {
        "value": bad_value
    }
    cfg_path = tmp_path / "invalid_numeric.yml"
    cfg_path.write_text(yaml.safe_dump(raw), encoding="utf-8")

    # ACT / ASSERT
    with pytest.raises(ValueError):
        SUEWSConfig.from_yaml(str(cfg_path))
    with pytest.raises(ValueError):
        SUEWSConfig.from_dict(raw)


def test_shading_conditions_follow_the_shared_modes() -> None:
    """Documented shading modes are the ones the validators enforce."""
    # ARRANGE / ACT / ASSERT
    for field, modes in STEBBS_SHADING_REQUIRED_MODES.items():
        condition = required_when("StebbsProperties", field)
        assert condition.startswith(
            "``internal_shading`` is " + " or ".join(f"``{m}``" for m in modes) + ";"
        )


def test_message_strings_are_frozen() -> None:
    """Pin every user-facing annotated-YAML message and fix string."""
    # ARRANGE
    actual_tables = {
        "LAI_REQUIRED": LAI_REQUIRED,
        "LAI_CALCULATED_ONLY_REQUIRED": LAI_CALCULATED_ONLY_REQUIRED,
        "CONDUCTANCE_REQUIRED": CONDUCTANCE_REQUIRED,
        "BUILDING_REQUIRED": BUILDING_REQUIRED,
        "BUILDING_REQUIRED_PROVIDED_FAI": BUILDING_REQUIRED_PROVIDED_FAI,
        "EVERGREEN_REQUIRED": EVERGREEN_REQUIRED,
        "EVERGREEN_REQUIRED_PROVIDED_FAI": EVERGREEN_REQUIRED_PROVIDED_FAI,
        "DECIDUOUS_REQUIRED": DECIDUOUS_REQUIRED,
        "DECIDUOUS_REQUIRED_PROVIDED_FAI": DECIDUOUS_REQUIRED_PROVIDED_FAI,
        "CO2_POINT_SOURCE_REQUIRED": CO2_POINT_SOURCE_REQUIRED,
        "SOIL_REQUIRED": SOIL_REQUIRED,
    }

    # ACT / ASSERT
    assert actual_tables == EXPECTED_TABLES


def test_required_when_returns_condition_or_empty() -> None:
    """The generator lookup answers for known fields and declines otherwise."""
    # ARRANGE / ACT / ASSERT
    assert "``model.physics.laimethod`` is ``1`` (MODELLED)" in required_when(
        "LAIParams", "base_temperature_senescence"
    )
    assert required_when("BldgsProperties", "bldgh").startswith(
        "buildings are present"
    )
    # store_cap is in no validator, so nothing may be claimed about it.
    assert not required_when("StorageDrainParams", "store_cap")
    assert not required_when("LAIParams", "not_a_field")


def _load_config(path_yaml: Path, clock: str, **dls: float) -> SUEWSConfig:
    """Load a configuration through the public YAML path."""
    config = {
        "sites": [{"properties": {"anthropogenic_emissions": dls}}],
        "model": {"control": {"output": {"timestamp_reference": clock}}},
    }
    path_yaml.write_text(yaml.safe_dump(config), encoding="utf-8")
    return SUEWSConfig.from_yaml(str(path_yaml))


@pytest.mark.parametrize(
    ("missing", "given"), [("startdls", "enddls"), ("enddls", "startdls")]
)
def test_documented_dls_condition_matches_validator(
    missing: str, given: str, tmp_path: Path
) -> None:
    """The documented DLS conditions are the ones the validator enforces."""
    # ARRANGE
    path_yaml = tmp_path / "config.yml"
    window = {"startdls": 86.0, "enddls": 303.0}
    condition = required_when("AnthropogenicEmissions", missing)

    # ACT / ASSERT - the daylight output clock needs the whole window ...
    assert "timestamp_reference: daylight" in condition
    with pytest.raises(ValueError, match="timestamp_reference='daylight' requires"):
        _load_config(path_yaml, "daylight", **{given: window[given]})
    _load_config(path_yaml, "daylight", **window)

    # ... and so does giving the other end, whatever the clock ...
    assert f"``{given}`` is given" in condition
    with pytest.raises(ValueError, match="must both be set or both be None"):
        _load_config(path_yaml, "follow", **{given: window[given]})

    # ... but otherwise the window may be omitted.
    _load_config(path_yaml, "follow")
    _load_config(path_yaml, "utc")
