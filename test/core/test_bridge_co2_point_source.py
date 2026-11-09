"""Witness point-source absence and mass-to-flux conversion at the native boundary."""

from copy import deepcopy
from importlib import import_module

import numpy as np
import pandas as pd
import pytest
import yaml

from supy import SUEWSSimulation

_run_rust = import_module("supy._run_rust")
pytestmark = [pytest.mark.physics, pytest.mark.core, pytest.mark.rust]


@pytest.fixture(scope="module")
def bridge_inputs():
    sim = SUEWSSimulation.from_sample_data()
    df_forcing = sim.forcing.df.iloc[:12]
    return (
        sim.config.model_dump(exclude_none=True, mode="json"),
        _run_rust._prepare_forcing_block(df_forcing).ravel(order="C").tolist(),
        len(df_forcing),
    )


def run_point_source(inputs, method, point_source):
    dict_config, forcing, count = inputs
    dict_config = deepcopy(dict_config)
    dict_config["model"]["physics"]["emissions"] = {"value": method}
    dict_co2 = dict_config["sites"][0]["properties"]["anthropogenic_emissions"]["co2"]
    if point_source is None:
        dict_co2.pop("emission_co2_point_source", None)
    else:
        dict_co2["emission_co2_point_source"] = point_source
    result = _run_rust._check_rust_available().run_suews(
        yaml.safe_dump(dict_config), forcing, count
    )
    assert result[2] == count
    return _run_rust._parse_output_block(result[0], count, 1)


@pytest.mark.parametrize("method", range(7))
def test_inactive_absence_preserves_all_outputs(bridge_inputs, method):
    """NaN absence takes the kernel's no-point-source branch for methods 0-6."""
    df_zero = run_point_source(bridge_inputs, method, 0.0)
    for absent in [None, float("nan")]:
        df_absent = run_point_source(bridge_inputs, method, absent)
        pd.testing.assert_frame_equal(df_absent, df_zero, check_exact=True)
    np.testing.assert_array_equal(df_zero["SUEWS"]["FcPoint"], 0.0)


def test_active_point_source_matches_mass_to_flux_conversion(bridge_inputs):
    df_zero = run_point_source(bridge_inputs, 12, 0.0)
    df_source = run_point_source(bridge_inputs, 12, 4.0)
    area = bridge_inputs[0]["sites"][0]["properties"]["surfacearea"]["value"]
    expected = 4.0 * 1e9 / (12 * 86400 * area)
    np.testing.assert_allclose(df_source["SUEWS"]["FcPoint"], expected, rtol=1e-12)
    np.testing.assert_allclose(
        df_source["SUEWS"]["Fc"] - df_zero["SUEWS"]["Fc"],
        expected,
        rtol=1e-12,
    )
