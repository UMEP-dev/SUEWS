"""Unused layer emissivity stays absent without changing non-SPARTACUS output."""

from copy import deepcopy
from importlib import import_module

import pandas as pd
import pytest
import yaml

from supy import SUEWSSimulation

_run_rust = import_module("supy._run_rust")
pytestmark = [pytest.mark.physics, pytest.mark.core, pytest.mark.rust]


def test_missing_layer_emissivity_preserves_non_spartacus_outputs():
    sim = SUEWSSimulation.from_sample_data()
    dict_config = sim.config.model_dump(exclude_none=True, mode="json")
    dict_config["model"]["physics"]["net_radiation"] = {"value": 3}
    dict_config["model"]["physics"]["storage_heat"] = {"value": 1}
    forcing = _run_rust._prepare_forcing_block(sim.forcing.df.iloc[:12])
    dict_absent = deepcopy(dict_config)
    layers = dict_absent["sites"][0]["properties"]["vertical_layers"]
    for facet in ["roofs", "walls"]:
        for layer in layers[facet]:
            layer.pop("emis", None)
    outputs = []
    for config in [dict_config, dict_absent]:
        result = _run_rust._check_rust_available().run_suews(
            yaml.safe_dump(config), forcing.ravel(order="C").tolist(), 12
        )
        assert result[2] == 12
        outputs.append(_run_rust._parse_output_block(result[0], 12, 1))
    pd.testing.assert_frame_equal(outputs[0], outputs[1], check_exact=True)
