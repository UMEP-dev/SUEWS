"""Regression tests for the revived AnOHM storage-heat scheme.

AnOHM (Sun et al., 2017) was disabled for plumbing reasons: a future-data
forcing dependency and later removal of its minpack solver. It is revived with
a calendar-day forcing double buffer and minpack-free solvers. These tests
guard the engaged physics path and persistence across chunks and restarts.

AnOHM remains an internal / not-recommended option (StorageHeatMethod._internal).
"""

from importlib import import_module
import json

import numpy as np
import pandas as pd
import pytest

import supy as sp
from supy.data_model.core.model import StorageHeatMethod

pytestmark = pytest.mark.physics

ANOHM_METHOD = 3
OHM_METHOD = 1
PARITY_RTOL = 1.0e-12
PARITY_ATOL = 1.0e-12
UNINTERRUPTED_CHUNK_DAYS = 3660


def _make_simulation(method: int = ANOHM_METHOD) -> sp.SUEWSSimulation:
    """Create a sample simulation with the requested storage-heat method."""
    sim = sp.SUEWSSimulation.from_sample_data()
    sim.update_config({
        "model": {
            "physics": {
                "net_radiation": {"value": 3},
                "storage_heat": {"value": method},
            }
        }
    })
    return sim


def _forcing_slice(
    sim: sp.SUEWSSimulation, days: int, start_hour: int = 0
) -> pd.DataFrame:
    """Select a cadence-independent forcing interval from the sample year."""
    forcing = sim.forcing
    assert forcing is not None
    index = forcing.df.index
    steps_per_day = int(pd.Timedelta(days=1) / (index[1] - index[0]))
    start_pos = start_hour * steps_per_day // 24
    end_pos = start_pos + days * steps_per_day
    return forcing.df.iloc[start_pos:end_pos]


def _run_storage_method(
    method: int,
    days: int = 4,
    start_hour: int = 0,
    chunk_day: int = UNINTERRUPTED_CHUNK_DAYS,
):
    """Run the sample site and return both simulation and typed output."""
    sim = _make_simulation(method)
    sim.update_forcing(_forcing_slice(sim, days, start_hour))
    output = sim.run(chunk_day=chunk_day, n_jobs=1)
    return sim, output


def _ohm_checkpoint_state(sim: sp.SUEWSSimulation):
    """Decode the actual Rust checkpoint OHM_STATE through its public codec."""
    checkpoint = sim.checkpoint
    assert checkpoint is not None
    state_json = next(iter(checkpoint.grid_states.values()))
    checkpoint_payload = json.loads(state_json)
    ohm_payload = checkpoint_payload["state"]["members"]["ohm_state"]

    bridge = import_module("supy.suews_bridge")
    state = bridge.OhmState.from_values_payload(
        ohm_payload["schema_version"], ohm_payload["values"]
    )
    return ohm_payload, state.to_dict()


def _assert_ohm_state_close(actual: dict, expected: dict) -> None:
    """Assert codec-visible OHM state equivalence at restart tolerance."""
    assert actual.keys() == expected.keys()
    np.testing.assert_allclose(
        list(actual.values()),
        list(expected.values()),
        rtol=PARITY_RTOL,
        atol=PARITY_ATOL,
        equal_nan=True,
    )


@pytest.mark.smoke
@pytest.mark.core
def test_anohm_runs_and_is_finite():
    """Method 3 runs end-to-end with a fully finite storage-heat flux."""
    _, output = _run_storage_method(ANOHM_METHOD)
    qs = output.df["SUEWS", "QS"].to_numpy()
    assert qs.size > 0
    assert np.isfinite(qs).all(), "AnOHM produced non-finite QS"
    assert not np.any(qs <= -900.0), "AnOHM produced -999 sentinel QS"
    assert np.nanmax(np.abs(qs)) < 1000.0


@pytest.mark.core
def test_anohm_path_is_engaged():
    """A settled AnOHM day diverges from the OHM spin-up fallback."""
    _, output_anohm = _run_storage_method(ANOHM_METHOD)
    _, output_ohm = _run_storage_method(OHM_METHOD)
    qs_anohm = output_anohm.df["SUEWS", "QS"].to_numpy()
    qs_ohm = output_ohm.df["SUEWS", "QS"].to_numpy()

    steps_per_day = qs_anohm.size // 4
    day3 = slice(steps_per_day * 2, steps_per_day * 3)
    assert np.nanmax(np.abs(qs_anohm[day3] - qs_ohm[day3])) > 1.0, (
        "AnOHM did not diverge from OHM -- the AnOHM path may not be engaged"
    )


@pytest.mark.core
def test_anohm_marked_internal():
    """AnOHM stays an internal / not-recommended option."""
    assert StorageHeatMethod.ANOHM._internal is True


@pytest.mark.core
def test_anohm_midday_start_preserves_partial_state_and_engages():
    """A midday start records real samples and later engages AnOHM."""
    sim_anohm, output_anohm = _run_storage_method(ANOHM_METHOD, days=2, start_hour=12)
    _, output_ohm = _run_storage_method(OHM_METHOD, days=2, start_hour=12)
    qs_anohm = output_anohm.df["SUEWS", "QS"].to_numpy()
    qs_ohm = output_ohm.df["SUEWS", "QS"].to_numpy()

    assert np.isfinite(qs_anohm).all()
    final_half_day = slice(-(qs_anohm.size // 4), None)
    assert np.nanmax(np.abs(qs_anohm[final_half_day] - qs_ohm[final_half_day])) > 1.0

    _, state = _ohm_checkpoint_state(sim_anohm)
    working_count = int(state["anohm_working_count"])
    coeff_count = int(state["anohm_coeff_count"])
    working_sd = [
        value for name, value in state.items() if name.startswith("anohm_working_sd.")
    ]
    coeff_sd = [
        value for name, value in state.items() if name.startswith("anohm_coeff_sd.")
    ]

    assert 0 < working_count < 24
    assert sum(value > -900.0 for value in working_sd) == working_count
    assert state["anohm_coeff_ready"] >= 0.5
    assert coeff_count >= 6
    assert sum(value > 5.0 for value in coeff_sd) >= 6
    assert state["anohm_working_day"] > state["anohm_coeff_day"]


@pytest.mark.core
def test_anohm_external_restart_matches_uninterrupted_state_path_and_output():
    """A restart inside buffer fill preserves later AnOHM engagement exactly."""
    sim_seed = _make_simulation()
    forcing = _forcing_slice(sim_seed, days=4)
    steps_per_day = int(pd.Timedelta(days=1) / (forcing.index[1] - forcing.index[0]))
    split_pos = steps_per_day * 3 // 4

    sim_full = _make_simulation()
    sim_full.update_forcing(forcing)
    output_full = sim_full.run(chunk_day=UNINTERRUPTED_CHUNK_DAYS, n_jobs=1)

    sim_first = _make_simulation()
    sim_first.update_forcing(forcing.iloc[:split_pos])
    sim_first.run(chunk_day=UNINTERRUPTED_CHUNK_DAYS, n_jobs=1)
    partial_payload, partial_state = _ohm_checkpoint_state(sim_first)

    assert (
        partial_payload["schema_version"]
        == import_module("supy.suews_bridge").ohm_state_schema_version()
    )
    assert len(partial_payload["values"]) == len(
        import_module("supy.suews_bridge").OhmState.field_names()
    )
    assert 0 < partial_state["anohm_working_count"] < 24
    assert partial_state["anohm_coeff_ready"] < 0.5

    sim_second = sp.SUEWSSimulation.from_checkpoint(
        sim_first.config, sim_first.checkpoint
    )
    sim_second.update_forcing(forcing.iloc[split_pos:])
    output_second = sim_second.run(chunk_day=UNINTERRUPTED_CHUNK_DAYS, n_jobs=1)

    expected_second = output_full.df.loc[output_second.df.index]
    pd.testing.assert_frame_equal(
        output_second.df,
        expected_second,
        check_exact=False,
        rtol=PARITY_RTOL,
        atol=PARITY_ATOL,
    )

    _, state_full = _ohm_checkpoint_state(sim_full)
    _, state_second = _ohm_checkpoint_state(sim_second)
    assert state_second["anohm_coeff_ready"] >= 0.5
    _assert_ohm_state_close(state_second, state_full)


@pytest.mark.core
def test_anohm_chunked_run_matches_uninterrupted_state_path_and_output():
    """One-day chunks preserve AnOHM state, path and output exactly."""
    sim_full, output_full = _run_storage_method(ANOHM_METHOD)
    sim_chunked, output_chunked = _run_storage_method(ANOHM_METHOD, chunk_day=1)

    pd.testing.assert_frame_equal(
        output_chunked.df,
        output_full.df,
        check_exact=False,
        rtol=PARITY_RTOL,
        atol=PARITY_ATOL,
    )

    _, state_full = _ohm_checkpoint_state(sim_full)
    _, state_chunked = _ohm_checkpoint_state(sim_chunked)
    assert state_chunked["anohm_coeff_ready"] >= 0.5
    _assert_ohm_state_close(state_chunked, state_full)
