from importlib.resources import files

import pytest
import yaml

from supy.data_model.core import SUEWSConfig
from supy.data_model.core.site import SPARTACUSParams

pytestmark = pytest.mark.api


def _inner_value(value):
    while hasattr(value, "value"):
        value = value.value
    return value


def test_spartacus_params_legacy_df_state_uses_defaults_for_forest_columns():
    state = SPARTACUSParams(
        air_ext_lw=0.25,
        n_stream_lw_urban=4,
        n_stream_sw_urban=4,
        n_vegetation_region_urban=2,
    ).to_df_state(grid_id=1)
    legacy_state = state.drop(
        columns=[
            ("n_stream_lw_forest", "0"),
            ("n_stream_sw_forest", "0"),
            ("n_vegetation_region_forest", "0"),
        ]
    )

    restored = SPARTACUSParams.from_df_state(legacy_state, grid_id=1)

    assert _inner_value(restored.air_ext_lw) == 0.25
    assert _inner_value(restored.n_stream_lw_urban) == 4
    assert _inner_value(restored.n_stream_sw_urban) == 4
    assert _inner_value(restored.n_vegetation_region_urban) == 2
    assert _inner_value(restored.n_stream_lw_forest) == 2
    assert _inner_value(restored.n_stream_sw_forest) == 1
    assert _inner_value(restored.n_vegetation_region_forest) == 1


def test_use_sw_direct_albedo_defaults_off_when_omitted():
    """A YAML without the switch must leave direct albedo off (GH#1858).

    The kernel's own default is off, so the Python default must agree:
    the dumped config is what the Rust bridge reads for every Python run.
    """
    data = yaml.safe_load(
        files("supy")
        .joinpath("sample_data/sample_config.yml")
        .read_text(encoding="utf-8")
    )
    del data["sites"][0]["properties"]["spartacus"]["use_sw_direct_albedo"]

    config = SUEWSConfig.from_dict(data)
    dumped = config.model_dump(exclude_none=True, mode="json")
    flag = dumped["sites"][0]["properties"]["spartacus"]["use_sw_direct_albedo"]

    assert _inner_value(config.sites[0].properties.spartacus.use_sw_direct_albedo) == 0
    assert (flag["value"] if isinstance(flag, dict) else flag) == 0
