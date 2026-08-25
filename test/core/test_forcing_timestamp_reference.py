"""Behavioural coverage for forcing timestamp-reference control."""

from copy import deepcopy
from importlib.resources import files
import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

# The test intentionally exercises the private native boundary.
# ruff: disable[import-private-name]
from supy import SUEWSSimulation
from supy._run_rust import (
    _load_rust_module,
    _parse_output_block,
    _prepare_forcing_block,
)

# ruff: enable[import-private-name]

pytestmark = pytest.mark.physics


def _config_dict(simulation):
    return simulation.config.model_dump(exclude_none=True, mode="json")


def _set_timestamp_reference(config, reference):
    updated = deepcopy(config)
    updated["model"]["control"]["forcing"]["timestamp_reference"] = reference
    return updated


def _run(rust_module, config, forcing, state_json=None):
    args = (
        json.dumps(config),
        _prepare_forcing_block(forcing).ravel(order="C").tolist(),
        len(forcing),
    )
    if state_json is None:
        return rust_module.run_suews(*args)
    return rust_module.run_suews_with_state(*args, state_json)


def _output_datetimes(output, length):
    # The bridge returns the block as native-endian bytes (GH-1718); older
    # builds returned a list of floats. Accept both.
    if isinstance(output, (bytes, bytearray, memoryview)):
        block = np.frombuffer(output, dtype=np.float64).reshape(length, -1)
    else:
        block = np.asarray(output).reshape(length, -1)
    return (
        pd.to_datetime({
            "year": block[:, 0].astype(int),
            "month": 1,
            "day": 1,
        })
        + pd.to_timedelta(block[:, 1] - 1, unit="D")
        + pd.to_timedelta(block[:, 2], unit="h")
        + pd.to_timedelta(block[:, 3], unit="min")
    )


def test_omitted_timestamp_reference_is_bit_identical_to_explicit_local_standard_time():
    """The new control must not change an existing configuration or run."""
    simulation = SUEWSSimulation(
        str(files("supy").joinpath("sample_data/sample_config.yml"))
    )
    config = _config_dict(simulation)
    config["model"]["control"]["forcing"].pop("timestamp_reference", None)
    explicit = _set_timestamp_reference(config, "local_standard_time")
    forcing = simulation.forcing.df.iloc[:12]
    rust_module = _load_rust_module()

    implicit_output, implicit_state, implicit_len = _run(rust_module, config, forcing)
    explicit_output, explicit_state, explicit_len = _run(rust_module, explicit, forcing)

    assert implicit_len == explicit_len == len(forcing)
    np.testing.assert_array_equal(explicit_output, implicit_output)
    assert json.loads(explicit_state) == json.loads(implicit_state)


@pytest.mark.parametrize("offset_hours", [5.75, -3.5])
def test_utc_forcing_retains_utc_output_clock_through_rollover_and_restart(
    offset_hours,
):
    """UTC is the main/output/day clock, including fractional-zone rollover."""
    simulation = SUEWSSimulation(
        str(files("supy").joinpath("sample_data/sample_config.yml"))
    )
    config = _config_dict(simulation)
    config["sites"][0]["properties"]["timezone"]["value"] = offset_hours
    config = _set_timestamp_reference(config, "utc")

    forcing = simulation.forcing.df.iloc[:12].copy()
    forcing.index = pd.date_range("2011-12-31 23:55", periods=12, freq="5min")
    rust_module = _load_rust_module()

    output, _, length = _run(rust_module, config, forcing)
    pd.testing.assert_index_equal(
        pd.DatetimeIndex(_output_datetimes(output, length)),
        forcing.index,
        exact=True,
    )

    first = forcing.iloc[:6]
    second = forcing.iloc[6:]
    first_output, first_state, first_len = _run(rust_module, config, first)
    second_output, _, second_len = _run(
        rust_module, config, second, state_json=first_state
    )
    chunked_datetimes = pd.DatetimeIndex(
        _output_datetimes(first_output, first_len).tolist()
        + _output_datetimes(second_output, second_len).tolist()
    )
    pd.testing.assert_index_equal(chunked_datetimes, forcing.index, exact=True)


@pytest.mark.parametrize(
    ("offset_hours", "local_day_end_utc"),
    [
        (5.75, "2011-12-31 18:10"),
        (-3.5, "2012-01-01 03:25"),
    ],
)
def test_utc_daily_state_boundary_remains_at_utc_midnight(
    offset_hours,
    local_day_end_utc,
):
    """DailyState closes the UTC day, not the derived local-standard day."""
    simulation = SUEWSSimulation(
        str(files("supy").joinpath("sample_data/sample_config.yml"))
    )
    config = _config_dict(simulation)
    config["sites"][0]["properties"]["timezone"]["value"] = offset_hours
    config = _set_timestamp_reference(config, "utc")
    rust_module = _load_rust_module()

    def daily_state_at(timestamp):
        forcing = simulation.forcing.df.iloc[:1].copy()
        forcing.index = pd.DatetimeIndex([timestamp])
        output, _, length = _run(rust_module, config, forcing)
        frame = _parse_output_block(output, length, grid_id=1)
        return frame.loc[:, "DailyState"].to_numpy()

    assert np.all(np.isnan(daily_state_at(local_day_end_utc)))
    assert np.any(np.isfinite(daily_state_at("2011-12-31 23:55")))


def test_utc_multi_site_outputs_share_the_forcing_clock():
    """Site offsets must not split a shared UTC forcing/output index."""
    simulation = SUEWSSimulation(
        str(files("supy").joinpath("sample_data/sample_config.yml"))
    )
    config = _set_timestamp_reference(_config_dict(simulation), "utc")
    configs = []
    for offset_hours in (5.75, -3.5):
        site_config = deepcopy(config)
        site_config["sites"][0]["properties"]["timezone"]["value"] = offset_hours
        configs.append(json.dumps(site_config))

    forcing = simulation.forcing.df.iloc[:12].copy()
    forcing.index = pd.date_range("2011-12-31 23:55", periods=12, freq="5min")
    rust_module = _load_rust_module()
    results = rust_module.run_suews_multi(
        configs,
        _prepare_forcing_block(forcing).ravel(order="C").tolist(),
        len(forcing),
        2,
    )

    assert [result[0] for result in results] == [0, 1]
    for _, output, _, length in results:
        pd.testing.assert_index_equal(
            pd.DatetimeIndex(_output_datetimes(output, length)),
            forcing.index,
            exact=True,
        )


@pytest.mark.parametrize("offset_hours", [5.75, -3.5])
def test_utc_aligns_solar_and_local_profiles_with_equivalent_standard_time(
    offset_hours,
):
    """Solar, anthropogenic and STEBBS profiles use the derived site clock."""
    fixture = (
        Path(__file__).resolve().parents[1]
        / "fixtures"
        / "data_test"
        / "stebbs_test"
        / "sample_config.yml"
    )
    simulation = SUEWSSimulation(str(fixture))
    config = _config_dict(simulation)
    config["sites"][0]["properties"]["timezone"]["value"] = offset_hours
    local_forcing = simulation.forcing.df.iloc[:1].copy()
    utc_forcing = local_forcing.copy()
    utc_forcing.index -= pd.Timedelta(hours=offset_hours)
    rust_module = _load_rust_module()

    local_result = _run(
        rust_module,
        _set_timestamp_reference(config, "local_standard_time"),
        local_forcing,
    )
    utc_result = _run(
        rust_module,
        _set_timestamp_reference(config, "utc"),
        utc_forcing,
    )
    assert local_result[2] == utc_result[2] == 1

    local_members = json.loads(local_result[1])["state"]["members"]
    utc_members = json.loads(utc_result[1])["state"]["members"]
    # The legacy solar converter truncates floating-point seconds. Equivalent
    # UTC and LST paths can therefore differ by at most one second of solar
    # motion even though they represent the same instant.
    np.testing.assert_allclose(
        utc_members["solar_state"]["values"],
        local_members["solar_state"]["values"],
        rtol=0.0,
        atol=0.005,
    )
    np.testing.assert_allclose(
        utc_members["stebbs_state"]["values"],
        local_members["stebbs_state"]["values"],
        rtol=0.0,
        atol=0.0005,
    )

    local_frame = _parse_output_block(local_result[0], local_result[2], grid_id=1)
    utc_frame = _parse_output_block(utc_result[0], utc_result[2], grid_id=1)
    np.testing.assert_array_equal(
        utc_frame.loc[:, ("SUEWS", "QF")].to_numpy(),
        local_frame.loc[:, ("SUEWS", "QF")].to_numpy(),
    )
    np.testing.assert_allclose(
        utc_frame.loc[:, "SUEWS"].to_numpy(),
        local_frame.loc[:, "SUEWS"].to_numpy(),
        rtol=0.0,
        atol=0.05,
        equal_nan=True,
    )
    np.testing.assert_allclose(
        utc_frame.loc[:, "BEERS"].to_numpy(),
        local_frame.loc[:, "BEERS"].to_numpy(),
        rtol=0.0,
        atol=0.05,
        equal_nan=True,
    )
    np.testing.assert_allclose(
        utc_frame.loc[:, "STEBBS"].to_numpy(),
        local_frame.loc[:, "STEBBS"].to_numpy(),
        rtol=0.0,
        atol=0.0005,
    )
