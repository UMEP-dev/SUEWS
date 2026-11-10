"""The data model emits the STEBBS values the Rust bridge used to fill in.

gh#2001 (part of gh#1991):

- ``stebbs.deep_soil_temperature`` is computed, not an input: it is the annual
  mean air temperature (gh#1240), the value the bridge fell back to.
- ``building_archetype.hhs0`` and the five ``age_*`` occupant fractions sit in
  the kernel's archetype type but no physics reads them; they are emitted with
  zero defaults, the value the bridge's struct started from.

All are internal: a clean export does not show them to users.
"""

import copy

import pytest
import yaml

from supy._env import trv_supy_module
from supy.data_model.core import SUEWSConfig

pytestmark = [pytest.mark.api, pytest.mark.cfg]

OCCUPANCY_FIELDS = (
    "hhs0",
    "age_0_4",
    "age_5_11",
    "age_12_18",
    "age_19_64",
    "age_65plus",
)


@pytest.fixture(scope="module")
def sample_dict():
    text = (trv_supy_module / "sample_data" / "sample_config.yml").read_text(
        encoding="utf-8"
    )
    return yaml.safe_load(text)


def _unwrap(value):
    return value["value"] if isinstance(value, dict) else value


def _site_dump(data):
    """Dump the first site exactly as supy hands it to the bridge."""
    config = SUEWSConfig.from_dict(data)
    return config.model_dump(exclude_none=True, mode="json")["sites"][0]


@pytest.mark.parametrize("annual_mean", [10.0, 12.5])
def test_deep_soil_temperature_is_the_annual_mean(sample_dict, annual_mean):
    data = copy.deepcopy(sample_dict)
    stebbs = data["sites"][0]["properties"]["stebbs"]
    stebbs["temperature_air_annual_mean"] = {"value": annual_mean}

    dumped = _site_dump(data)["properties"]["stebbs"]

    assert _unwrap(dumped["deep_soil_temperature"]) == annual_mean
    assert _unwrap(dumped["temperature_air_annual_mean"]) == annual_mean


def test_deep_soil_temperature_is_not_an_input(sample_dict):
    data = copy.deepcopy(sample_dict)
    stebbs = data["sites"][0]["properties"]["stebbs"]
    stebbs["temperature_air_annual_mean"] = {"value": 11.0}
    stebbs["deep_soil_temperature"] = {"value": 5.0}

    with pytest.raises(ValueError, match="deep_soil_temperature is not an input"):
        SUEWSConfig.from_dict(data)


@pytest.mark.parametrize("field", OCCUPANCY_FIELDS)
def test_occupancy_descriptors_are_emitted_as_zero(sample_dict, field):
    dumped = _site_dump(copy.deepcopy(sample_dict))["properties"]["building_archetype"]
    assert _unwrap(dumped[field]) == 0.0


def test_new_keys_stay_out_of_user_export(sample_dict, tmp_path):
    path_out = tmp_path / "clean.yml"
    SUEWSConfig(**copy.deepcopy(sample_dict)).to_yaml(
        str(path_out), include_internal=False
    )
    props = yaml.safe_load(path_out.read_text(encoding="utf-8"))["sites"][0][
        "properties"
    ]
    assert "deep_soil_temperature" not in props["stebbs"]
    for field in OCCUPANCY_FIELDS:
        assert field not in props["building_archetype"]


def test_df_state_round_trip_keeps_deep_soil_temperature(sample_dict):
    """The computed field has no DataFrame column and is recomputed on reload."""
    data = copy.deepcopy(sample_dict)
    data["sites"][0]["properties"]["stebbs"]["temperature_air_annual_mean"] = {
        "value": 12.5
    }
    config = SUEWSConfig.from_dict(data)

    df_state = config.to_df_state()
    reloaded = SUEWSConfig.from_df_state(df_state)

    assert "deep_soil_temperature" not in df_state.columns.get_level_values(0)
    stebbs = reloaded.sites[0].properties.stebbs
    assert _unwrap(stebbs.deep_soil_temperature) == 12.5


@pytest.mark.parametrize("reload_yaml", [False, True])
def test_update_annual_mean_recomputes_deep_soil(sample_dict, tmp_path, reload_yaml):
    """Public patches do not treat a previously emitted temperature as input."""
    from supy import SUEWSSimulation

    sim = SUEWSSimulation()
    sim.update_config(copy.deepcopy(sample_dict), auto_load_forcing=False)
    if reload_yaml:
        path = tmp_path / "export.yml"
        sim.config.to_yaml(str(path))
        sim = SUEWSSimulation()
        sim.update_config(SUEWSConfig.from_yaml(str(path)), auto_load_forcing=False)

    sim.update_config(
        {"sites": {0: {"properties": {"stebbs": {
            "temperature_air_annual_mean": 15.0,
        }}}}},
        auto_load_forcing=False,
    )

    stebbs = sim.config.sites[0].properties.stebbs
    assert stebbs.deep_soil_temperature == 15.0
    assert stebbs.model_dump(mode="json")["deep_soil_temperature"] == 15.0
    with pytest.raises(ValueError, match="deep_soil_temperature is not an input"):
        sim.update_config(
            {"sites": {0: {"properties": {"stebbs": {
                "deep_soil_temperature": 5.0,
            }}}}},
            auto_load_forcing=False,
        )
