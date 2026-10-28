"""Whole-store observation configuration, independent of forcing conversion."""
from copy import deepcopy
from pathlib import Path

import pytest
import yaml

from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.model import ModelPhysics
from supy.data_model.core.site import SoilObservationConfig
from supy.units import parse_unit

pytestmark = [pytest.mark.api, pytest.mark.cfg]
NON_WATER = ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil")
SAMPLE = Path(__file__).parents[2] / "src/supy/sample_data/sample_config.yml"


@pytest.fixture
def observed_config():
    cfg = yaml.safe_load(SAMPLE.read_text())
    cfg["model"]["physics"]["soil_moisture"] = "observed"
    for site in cfg["sites"]:
        props = site["properties"]
        props["soil_observation"] = {"layer_depth": 0.2}
        for name in NON_WATER:
            surface = props["land_cover"][name]
            surface.update(soil_depth=200.0, soil_store_capacity=80.0, soil_field_capacity=0.6)
    return cfg


def test_whole_store_and_state_roundtrip(observed_config):
    # 1000 * 0.6 * 0.2 = 120 kg m-2 at FC; AWC 80 implies theta_wp=0.2.
    cfg = SUEWSConfig.model_validate(observed_config)
    state = cfg.to_df_state(use_conditional_validation=False)
    obs = cfg.sites[0].properties.soil_observation
    assert obs.layer_top == 0
    assert obs.soil_not_rocks == 1
    assert obs.bulk_density is None
    restored = SoilObservationConfig.from_df_state(state, state.index[0])
    assert restored.layer_depth == obs.layer_depth
    assert restored.layer_top == obs.layer_top
    assert restored.bulk_density is None
    assert ("obs_sm_depth", "0") not in state.columns
    assert ("obs_sm_smcap", "0") not in state.columns
    assert state.loc[state.index[0], ("soil_field_capacity", "(0,)")] == 0.6
    SUEWSConfig.model_validate(cfg.model_dump())


@pytest.mark.parametrize("field,value", [
    ("layer_depth", 0), ("layer_depth", -1), ("layer_depth", float("nan")),
    ("layer_depth", float("inf")), ("layer_top", -1), ("layer_top", float("nan")),
    ("bulk_density", 0), ("bulk_density", float("inf")),
    ("soil_not_rocks", 0), ("soil_not_rocks", 1.1), ("soil_not_rocks", float("nan")),
])
def test_invalid_metadata(field, value):
    with pytest.raises(ValueError):
        SoilObservationConfig(**{"layer_depth": 0.2, field: value})


@pytest.mark.parametrize("key", ["depth", "smcap", "obs_sm_depth", "obs_sm_smcap", "unit"])
def test_retired_or_unit_override_rejected(key):
    with pytest.raises(ValueError):
        SoilObservationConfig(layer_depth=0.2, **{key: 1})


@pytest.mark.parametrize("key", ["soil_moisture", "soil_moisture_deficit", "smdmethod", "smd_method"])
def test_selector_aliases(key):
    assert ModelPhysics(**{key: "observed"}).soil_moisture.value.value == 1
    with pytest.raises(ValueError):
        ModelPhysics(**{key: 2})
    if key != "soil_moisture":
        with pytest.raises(ValueError):
            ModelPhysics(**{key: 0, "soil_moisture": 0})


@pytest.mark.parametrize("value", [None, 0, -999, float("nan"), float("inf"), 1.1, 0.39])
def test_field_capacity_guard(observed_config, value):
    # AWC 80 / depth 200 mm requires theta_fc >= 0.4.
    observed_config["sites"][0]["properties"]["land_cover"]["paved"]["soil_field_capacity"] = value
    with pytest.raises(ValueError, match="soil_field_capacity"):
        SUEWSConfig.model_validate(observed_config)


@pytest.mark.parametrize("top,depth,valid", [
    (0, 0.2, True), (0, 0.2000009, True), (0, 0.2000011, False),
    (0, 0.1, False), (0, 0.3, False), (0.01, 0.2, False),
])
def test_layer_geometry(observed_config, top, depth, valid):
    observed_config["sites"][0]["properties"]["soil_observation"] = {"layer_top": top, "layer_depth": depth}
    if valid:
        SUEWSConfig.model_validate(observed_config)
    else:
        with pytest.raises(ValueError, match="layer"):
            SUEWSConfig.model_validate(observed_config)


def test_missing_and_heterogeneous_metadata(observed_config):
    cfg = deepcopy(observed_config)
    del cfg["sites"][0]["properties"]["soil_observation"]
    with pytest.raises(ValueError, match="soil_observation"):
        SUEWSConfig.model_validate(cfg)
    observed_config["sites"][0]["properties"]["land_cover"]["paved"]["soil_depth"] = 300
    with pytest.raises(ValueError, match="layer_depth"):
        SUEWSConfig.model_validate(observed_config)


def test_zero_area_ignored_and_all_water_rejected(observed_config):
    cover = observed_config["sites"][0]["properties"]["land_cover"]
    for name in NON_WATER:
        cover[name]["sfr"] = 0
        cover[name]["soil_field_capacity"] = None
    cover["grass"].update(sfr=0.5, soil_field_capacity=0.6)
    cover["water"]["sfr"] = 0.5
    SUEWSConfig.model_validate(observed_config)
    cover["grass"]["sfr"] = 0
    cover["water"]["sfr"] = 1
    with pytest.raises(ValueError, match="non-water area"):
        SUEWSConfig.model_validate(observed_config)


def test_observation_units_parse():
    for field in SoilObservationConfig.model_fields.values():
        if field.json_schema_extra and field.json_schema_extra.get("unit"):
            parse_unit(field.json_schema_extra["unit"])


@pytest.mark.parametrize("method", [1, 2])
def test_confirmed_migration_preserves_units_references_and_method(observed_config, method, capsys):
    from supy.data_model.configuration.migration import SchemaMigrator
    from supy.util.converter.yaml_upgrade import SoilMigrationContext
    cfg = deepcopy(observed_config)
    cfg["schema_version"] = "2026.6.dev5"
    physics = cfg["model"]["physics"]
    physics.pop("soil_moisture")
    physics["soil_moisture_deficit"] = {"value": method}
    ref = {"desc": "Synthetic measured layer"}
    for site in cfg["sites"]:
        site["properties"]["soil_observation"] = {
            "depth": {"value": 200, "ref": ref}, "smcap": 0.9,
            "bulk_density": {"value": 1.25, "ref": ref}, "soil_not_rocks": 0.8}
    original = deepcopy(cfg)
    context = SoilMigrationContext(
        layer_bounds={i: (0, 0.2) for i in range(len(cfg["sites"]))},
        original_method=method,
        forcing_manifest={"completed": True, "original_method": method,
                          "source_unit": "m3 m-3" if method == 1 else "kg kg-1",
                          "input_sha256": "a" * 64, "output_sha256": "b" * 64})
    migrated = SchemaMigrator().migrate(cfg, migration_context=context)
    assert cfg == original
    obs = migrated["sites"][0]["properties"]["soil_observation"]
    assert obs["layer_depth"] == {"value": 0.2, "ref": ref}
    assert obs["bulk_density"] == {"value": 1250.0, "ref": ref}
    assert obs["soil_not_rocks"] == 0.8
    assert "smcap" not in obs
    assert migrated["model"]["physics"]["soil_moisture"]["value"] == "observed"
    assert SchemaMigrator().migrate(migrated) == migrated
    captured = capsys.readouterr()
    assert f"original method {method}" in captured.out + captured.err


def test_shared_forcing_requires_matching_bulk_field_capacity(observed_config):
    other = deepcopy(observed_config["sites"][0])
    other["gridiv"] = 99
    for name in NON_WATER:
        other["properties"]["land_cover"][name]["soil_field_capacity"] = 0.7
    observed_config["sites"].append(other)
    with pytest.raises(ValueError, match="Shared forcing"):
        SUEWSConfig.model_validate(observed_config)


def test_shared_bulk_store_uses_non_water_area_weighting(observed_config):
    first = observed_config["sites"][0]
    cover = first["properties"]["land_cover"]
    for name in NON_WATER:
        cover[name]["sfr"] = 0
    cover["paved"].update(sfr=0.3, soil_store_capacity=100)
    cover["grass"].update(sfr=0.2, soil_store_capacity=50)
    cover["water"]["sfr"] = 0.5
    second = deepcopy(first)
    second["gridiv"] = 99
    cover2 = second["properties"]["land_cover"]
    cover2["paved"]["sfr"] = 0
    cover2["grass"].update(sfr=0.5, soil_store_capacity=80)
    # C=(0.3*100+0.2*50)/0.5=80, FC=1000*0.6*0.2=120 for both.
    observed_config["sites"].append(second)
    SUEWSConfig.model_validate(observed_config)
    cover2["grass"]["soil_store_capacity"] = 81
    with pytest.raises(ValueError, match="Shared forcing"):
        SUEWSConfig.model_validate(observed_config)


def test_invalid_observation_rejected_with_optional_checks_disabled(observed_config):
    observed_config["sites"][0]["properties"]["soil_observation"]["layer_depth"] = 0.1
    with pytest.raises(ValueError, match="layer_depth"):
        SUEWSConfig.from_dict(observed_config, use_conditional_validation=False)
