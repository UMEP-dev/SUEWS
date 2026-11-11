"""Invalid SPARTACUS inputs fail at configuration and native bridge boundaries."""

import copy
from importlib import import_module
import json

import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model.core import SUEWSConfig

pytestmark = [pytest.mark.api, pytest.mark.core]


@pytest.fixture(scope="module")
def radiation_inputs():
    sim = SUEWSSimulation.from_sample_data()
    config = sim.config.model_dump(exclude_none=True, mode="json")
    config["model"]["physics"]["net_radiation"] = {"value": 1003}
    # Use the supported facet-resolved configuration at the schema boundary.
    config["model"]["physics"]["storage_heat"] = {"value": 5}
    layers = config["sites"][0]["properties"]["vertical_layers"]
    for field in ("veg_frac", "veg_scale"):
        layers[field]["value"][-1] = 0.0
    run_rust = import_module("supy._run_rust")
    forcing = run_rust._prepare_forcing_block(sim.forcing.df.iloc[:2])
    return config, forcing.ravel(order="C").tolist(), run_rust._check_rust_available()


def malformed(config, field, value):
    result = copy.deepcopy(config)
    props = result["sites"][0]["properties"]
    owner = props["vertical_layers"] if field == "building_frac" else props["spartacus"]
    owner[field] = {"value": value}
    return result


INVALID = [
    ("building_frac", [0.2, 0.4, 0.1]),
    ("building_frac", [-999.0, 0.2, 0.1]),
    ("building_frac", [float("nan"), 0.2, 0.1]),
    ("n_stream_sw_urban", 0),
    ("n_stream_lw_forest", -999),
    ("n_vegetation_region_urban", 3),
    ("n_stream_sw_urban", float("nan")),
    ("n_vegetation_region_urban", 1.5),
]


@pytest.mark.parametrize("field,value", INVALID)
@pytest.mark.parametrize("surface", ["mapping", "yaml"])
def test_config_rejects_invalid_radiation(radiation_inputs, tmp_path, field, value, surface):
    config, _, _ = radiation_inputs
    data = malformed(config, field, value)
    with pytest.raises(ValueError, match=field):
        if surface == "mapping":
            SUEWSConfig.from_dict(data)
        else:
            path = tmp_path / "invalid.yml"
            path.write_text(yaml.safe_dump(data), encoding="utf-8")
            SUEWSConfig.from_yaml(path)


@pytest.mark.parametrize("field,value", [INVALID[0], *INVALID[3:]])
@pytest.mark.parametrize("parallel", [False, True])
def test_bridge_rejects_invalid_radiation(radiation_inputs, field, value, parallel):
    config, forcing, bridge = radiation_inputs
    invalid = yaml.safe_dump(malformed(config, field, value))
    with pytest.raises((RuntimeError, ValueError), match=field):
        if parallel:
            bridge.run_suews_multi([json.dumps(config), invalid], forcing, 2, 2)
        else:
            bridge.run_suews(invalid, forcing, 2)


@pytest.mark.parametrize("field", [
    "n_stream_sw_forest", "n_stream_lw_forest", "n_vegetation_region_forest",
])
@pytest.mark.parametrize("value", [0.5, None, "invalid", float("nan"), float("inf")])
@pytest.mark.parametrize("parallel", [False, True])
def test_bridge_rejects_malformed_forest_counts(radiation_inputs, field, value, parallel):
    # Bypass Pydantic: the Rust YAML reader must reject supplied malformed
    # counts instead of retaining a default or copying the urban stream count.
    config, forcing, bridge = radiation_inputs
    invalid = yaml.safe_dump(malformed(config, field, value))
    with pytest.raises((RuntimeError, ValueError), match=field):
        if parallel:
            bridge.run_suews_multi([json.dumps(config), invalid], forcing, 2, 2)
        else:
            bridge.run_suews(invalid, forcing, 2)
