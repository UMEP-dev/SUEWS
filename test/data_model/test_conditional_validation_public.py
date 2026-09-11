"""Public-path contracts for conditional configuration validation."""

from importlib.resources import files

import pytest
import yaml

from supy.data_model.core import SUEWSConfig
from supy.data_model.core.model import NetRadiationMethod, StorageHeatMethod

pytestmark = pytest.mark.api


def _sample_config():
    """Load a fresh mutable copy of the packaged sample configuration."""
    return yaml.safe_load(
        files("supy")
        .joinpath("sample_data/sample_config.yml")
        .read_text(encoding="utf-8")
    )


def _site_properties(data):
    return data["sites"][0]["properties"]


def _enable_spartacus(data):
    """Enable SPARTACUS while working around the sample-data issue in #1699."""
    data["model"]["physics"]["net_radiation"] = {
        "value": NetRadiationMethod.LDOWN_SS_OBSERVED.value
    }
    vertical_layers = _site_properties(data)["vertical_layers"]
    for key in ("veg_frac", "veg_scale"):
        values = vertical_layers[key]["value"]
        values[2] = 0.0


def _assert_public_rejection(data, *expected_fragments):
    with pytest.raises(ValueError) as excinfo:
        SUEWSConfig.from_dict(data)

    message = str(excinfo.value)
    assert "Critical validation failed" in message
    for fragment in expected_fragments:
        assert fragment in message


def test_rsl_requires_building_frontal_area_index():
    data = _sample_config()
    data["model"]["physics"]["roughness_sublayer"] = {"value": 2}
    _site_properties(data)["land_cover"]["bldgs"]["faibldg"] = {"value": None}

    _assert_public_rejection(data, "bldgs.faibldg must be set")


def test_stebbs_storage_heat_requires_spartacus_radiation():
    data = _sample_config()
    data["model"]["physics"]["storage_heat"] = {"value": StorageHeatMethod.STEBBS.value}

    _assert_public_rejection(
        data,
        "requires model.physics.net_radiation to be a SPARTACUS-Surface method",
    )


def test_stebbs_storage_heat_loads_with_spartacus_radiation():
    data = _sample_config()
    data["model"]["physics"]["storage_heat"] = {"value": StorageHeatMethod.STEBBS.value}
    _enable_spartacus(data)

    SUEWSConfig.from_dict(data)


@pytest.mark.parametrize(
    "method", [StorageHeatMethod.DyOHM.value, StorageHeatMethod.DyOHM_BUILDING.value]
)
def test_dyohm_variants_load_without_spartacus(method):
    data = _sample_config()
    data["model"]["physics"]["storage_heat"] = {"value": method}

    SUEWSConfig.from_dict(data)


def test_stebbs_missing_required_parameter_is_rejected():
    data = _sample_config()
    data["model"]["physics"]["stebbs"] = {
        "enabled": True,
        "parameter_source": "default",
    }
    _site_properties(data)["stebbs"]["convection_coefficient_wall_internal"] = {
        "value": None
    }

    _assert_public_rejection(data, "Missing required STEBBS parameters")


def test_missing_internal_shading_defaults_to_disabled():
    """Existing YAML without curtain settings keeps shading disabled."""
    data = _sample_config()
    stebbs = _site_properties(data)["stebbs"]
    for field_name in (
        "internal_shading",
        "reduction_factor_shading",
        "temperature_threshold_shading",
        "radiation_threshold_shading",
    ):
        stebbs.pop(field_name)

    config = SUEWSConfig.from_dict(data)

    assert config.sites[0].properties.stebbs.internal_shading == 0


def test_internal_shading_rejects_invalid_mode():
    data = _sample_config()
    _site_properties(data)["stebbs"]["internal_shading"] = {"value": 3}

    with pytest.raises(ValueError) as excinfo:
        SUEWSConfig.from_dict(data)

    message = str(excinfo.value)
    assert "internal_shading" in message
    assert "Input should be 0, 1 or 2" in message


def test_constant_internal_shading_requires_reduction_factor():
    data = _sample_config()
    stebbs = _site_properties(data)["stebbs"]
    stebbs["internal_shading"] = {"value": 1}
    stebbs.pop("reduction_factor_shading")

    with pytest.raises(ValueError) as excinfo:
        SUEWSConfig.from_dict(data)

    assert (
        "reduction_factor_shading must be provided when internal_shading is 1 or 2"
        in str(excinfo.value)
    )


@pytest.mark.parametrize(
    "missing_field",
    ["temperature_threshold_shading", "radiation_threshold_shading"],
)
def test_controlled_internal_shading_requires_both_thresholds(missing_field):
    data = _sample_config()
    stebbs = _site_properties(data)["stebbs"]
    stebbs.update({
        "internal_shading": {"value": 2},
        "reduction_factor_shading": {"value": 0.4},
        "temperature_threshold_shading": {"value": 24.0},
        "radiation_threshold_shading": {"value": 200.0},
    })
    del stebbs[missing_field]

    with pytest.raises(ValueError) as excinfo:
        SUEWSConfig.from_dict(data)

    assert f"{missing_field} must be provided when internal_shading is 2" in str(
        excinfo.value
    )


@pytest.mark.parametrize("shading_mode", [1, 2])
def test_valid_internal_shading_configuration_loads(shading_mode):
    data = _sample_config()
    stebbs = _site_properties(data)["stebbs"]
    stebbs.update({
        "internal_shading": {"value": shading_mode},
        "reduction_factor_shading": {"value": 0.4},
    })
    if shading_mode == 2:
        stebbs.update({
            "temperature_threshold_shading": {"value": 24.0},
            "radiation_threshold_shading": {"value": 200.0},
        })

    config = SUEWSConfig.from_dict(data)

    assert config.sites[0].properties.stebbs.internal_shading == shading_mode


@pytest.mark.parametrize(
    "field,value",
    [
        ("reduction_factor_shading", -0.1),
        ("reduction_factor_shading", 1.1),
        ("radiation_threshold_shading", -1.0),
        ("temperature_threshold_shading", -273.15),
    ],
)
def test_internal_shading_rejects_out_of_range_parameters(field, value):
    data = _sample_config()
    stebbs = _site_properties(data)["stebbs"]
    stebbs.update({
        "internal_shading": {"value": 2},
        "reduction_factor_shading": {"value": 0.4},
        "temperature_threshold_shading": {"value": 24.0},
        "radiation_threshold_shading": {"value": 200.0},
    })
    stebbs[field] = {"value": value}

    with pytest.raises(ValueError) as excinfo:
        SUEWSConfig.from_dict(data)

    assert field in str(excinfo.value)


def test_internal_shading_roundtrips_through_legacy_df_state():
    data = _sample_config()
    _site_properties(data)["stebbs"].update({
        "internal_shading": {"value": 2},
        "reduction_factor_shading": {"value": 0.4},
        "temperature_threshold_shading": {"value": 24.0},
        "radiation_threshold_shading": {"value": 200.0},
    })
    config = SUEWSConfig.from_dict(data)

    df_state = config.to_df_state()
    reconstructed = SUEWSConfig.from_df_state(df_state)
    stebbs = reconstructed.sites[0].properties.stebbs

    assert stebbs.internal_shading == 2
    assert stebbs.reduction_factor_shading == pytest.approx(0.4)
    assert stebbs.temperature_threshold_shading == pytest.approx(24.0)
    assert stebbs.radiation_threshold_shading == pytest.approx(200.0)


def test_legacy_df_state_without_shading_columns_defaults_to_disabled():
    config = SUEWSConfig.from_dict(_sample_config())
    shading_columns = {
        "internalshading",
        "reductionfactorshading",
        "temperaturethresholdshading",
        "radiationthresholdshading",
    }
    full_state = config.to_df_state()
    columns_to_drop = [col for col in full_state.columns if col[0] in shading_columns]
    df_state = full_state.drop(columns=columns_to_drop)

    reconstructed = SUEWSConfig.from_df_state(df_state)

    assert reconstructed.sites[0].properties.stebbs.internal_shading == 0


def test_missing_destination_waste_heat_defaults_to_indoor():
    """Existing YAML keeps space-heating waste heat indoors by default."""
    data = _sample_config()
    _site_properties(data)["stebbs"].pop("destination_waste_heat", None)

    config = SUEWSConfig.from_dict(data)

    assert config.sites[0].properties.stebbs.destination_waste_heat == 0


@pytest.mark.parametrize("destination", [-1, 2])
def test_destination_waste_heat_rejects_invalid_value(destination):
    data = _sample_config()
    _site_properties(data)["stebbs"]["destination_waste_heat"] = {"value": destination}

    with pytest.raises(ValueError) as excinfo:
        SUEWSConfig.from_dict(data)

    message = str(excinfo.value)
    assert "destination_waste_heat" in message
    assert "Input should be 0 or 1" in message


def test_destination_waste_heat_roundtrips_through_legacy_df_state():
    data = _sample_config()
    _site_properties(data)["stebbs"]["destination_waste_heat"] = {"value": 1}
    config = SUEWSConfig.from_dict(data)

    df_state = config.to_df_state()
    reconstructed = SUEWSConfig.from_df_state(df_state)

    assert reconstructed.sites[0].properties.stebbs.destination_waste_heat == 1


def test_legacy_df_state_without_destination_waste_heat_defaults_to_indoor():
    config = SUEWSConfig.from_dict(_sample_config())
    full_state = config.to_df_state()
    columns_to_drop = [
        col for col in full_state.columns if col[0] == "destinationwasteheat"
    ]
    df_state = full_state.drop(columns=columns_to_drop)

    reconstructed = SUEWSConfig.from_df_state(df_state)

    assert reconstructed.sites[0].properties.stebbs.destination_waste_heat == 0


SAME_SURFACE_CASES = [
    pytest.param(
        "same_albedo_wall",
        "walls",
        "alb",
        "reflectivity_wall_external",
        "albedo",
        id="albedo-wall",
    ),
    pytest.param(
        "same_albedo_roof",
        "roofs",
        "alb",
        "reflectivity_roof_external",
        "albedo",
        id="albedo-roof",
    ),
    pytest.param(
        "same_emissivity_wall",
        "walls",
        "emis",
        "emissivity_wall_external",
        "emissivity",
        id="emissivity-wall",
    ),
    pytest.param(
        "same_emissivity_roof",
        "roofs",
        "emis",
        "emissivity_roof_external",
        "emissivity",
        id="emissivity-roof",
    ),
]


def _enable_same_surface(data, flag):
    data["model"]["physics"]["stebbs"] = {
        "enabled": True,
        "parameter_source": "default",
        flag: "enabled",
    }


@pytest.mark.parametrize(
    "flag,surfaces_field,property_field,_archetype_field,property_name",
    SAME_SURFACE_CASES,
)
def test_same_surface_requires_identical_values(
    flag,
    surfaces_field,
    property_field,
    _archetype_field,
    property_name,
):
    data = _sample_config()
    _enable_same_surface(data, flag)
    surfaces = _site_properties(data)["vertical_layers"][surfaces_field]
    surfaces[0][property_field] = {"value": 0.5}
    surfaces[1][property_field] = {"value": 0.6}

    _assert_public_rejection(
        data,
        f"so all {surfaces_field} {property_name} values must be identical",
    )


@pytest.mark.parametrize(
    "flag,surfaces_field,property_field,archetype_field,_property_name",
    SAME_SURFACE_CASES,
)
def test_same_surface_requires_archetype_match(
    flag,
    surfaces_field,
    property_field,
    archetype_field,
    _property_name,
):
    data = _sample_config()
    _enable_same_surface(data, flag)
    for surface in _site_properties(data)["vertical_layers"][surfaces_field]:
        surface[property_field] = {"value": 0.5}
    _site_properties(data)["building_archetype"][archetype_field] = {"value": 0.222}

    _assert_public_rejection(
        data,
        f"must equal properties.building_archetype.{archetype_field}",
    )


def test_spartacus_rejects_archetype_above_domain():
    data = _sample_config()
    _enable_spartacus(data)
    data["model"]["physics"]["stebbs"] = {
        "enabled": True,
        "parameter_source": "default",
    }
    _site_properties(data)["building_archetype"]["archetype_height"] = {"value": 999.0}

    _assert_public_rejection(data, "exceeding SPARTACUS domain top")


def test_spartacus_rejects_building_above_domain():
    data = _sample_config()
    _enable_spartacus(data)
    _site_properties(data)["land_cover"]["bldgs"]["bldgh"] = {"value": 999.0}

    _assert_public_rejection(data, "bldgh=999.0", "exceeding SPARTACUS domain top")


def test_spartacus_allows_archetype_above_domain_when_stebbs_is_disabled():
    data = _sample_config()
    _enable_spartacus(data)
    _site_properties(data)["building_archetype"]["archetype_height"] = {
        "value": 999.0
    }

    SUEWSConfig.from_dict(data)


def test_spartacus_rejects_invalid_layer_geometry():
    data = _sample_config()
    _enable_spartacus(data)
    vertical_layers = _site_properties(data)["vertical_layers"]
    vertical_layers["building_frac"] = {"value": [0.43, 0.9, 0.9]}
    vertical_layers["veg_frac"] = {"value": [0.3, 0.3, 0.0]}

    _assert_public_rejection(
        data,
        "vertical_layers.building_frac[1] + vertical_layers.veg_frac[1]",
    )


def test_spartacus_rejects_tree_above_all_layers():
    data = _sample_config()
    _enable_spartacus(data)
    _site_properties(data)["land_cover"]["dectr"]["height_deciduous_tree"] = {
        "value": 500.0
    }

    _assert_public_rejection(data, "exceeds all vertical_layers heights")


def test_consistent_spartacus_configuration_loads():
    data = _sample_config()
    _enable_spartacus(data)

    SUEWSConfig.from_dict(data)


@pytest.mark.parametrize("field", ["veg_frac", "veg_scale"])
def test_spartacus_rejects_nonzero_vegetation_above_tree_canopy(field):
    """Each top-layer field is independently required to be zero (gh#1699)."""
    data = _sample_config()
    original = _site_properties(data)["vertical_layers"][field]["value"][2]
    assert original > 0
    _enable_spartacus(data)
    _site_properties(data)["vertical_layers"][field]["value"][2] = original

    _assert_public_rejection(data, f"{field}[2] should be zero")
