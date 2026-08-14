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
