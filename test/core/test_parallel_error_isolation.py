"""Fatal error isolation between parallel grid workers (gh#1736).

The Rust bridge runs one grid per Rayon worker thread. The Fortran fatal
error store used to be a single process-global ``SAVE`` block, so a fatal
error raised by one grid was visible to, and could be reset by, every other
grid running at the same time. These tests pin the per-thread behaviour:
the failing grid is named deterministically, and a valid batch run after a
failed one completes with the expected grid ids and lengths.
"""

from __future__ import annotations

import copy
from importlib import import_module
import json

import pytest

import supy as sp

_run_rust = import_module("supy._run_rust")

pytestmark = [pytest.mark.api, pytest.mark.rust, pytest.mark.core]

# ErrorHint code raised by the stability scheme when the measurement height
# sits below the displacement height (z < zd): fatal, raised on every timestep.
FATAL_CODE = 32
N_STEPS = 288 * 2  # two days at the sample 5-minute timestep


@pytest.fixture(scope="module")
def bridge_inputs():
    sim = sp.SUEWSSimulation.from_sample_data()
    df_forcing = sim._df_forcing.iloc[:N_STEPS]
    config_dict = sim.config.model_dump(exclude_none=True, mode="json")
    forcing_block = _run_rust._prepare_forcing_block(df_forcing)
    return {
        "rust": _run_rust._check_rust_available(),
        "config_dict": config_dict,
        "forcing_flat": forcing_block.ravel(order="C").tolist(),
        "len_sim": len(df_forcing),
    }


def _grid_json(config_dict: dict, gridiv: int, failing: bool) -> str:
    site = copy.deepcopy(config_dict["sites"][0])
    site["gridiv"] = gridiv
    if failing:
        # Measurement height below the displacement height of the sample
        # site: the stability scheme raises fatal ErrorHint 32 immediately.
        site["properties"]["z"] = {"value": 0.5}
    grid_dict = dict(config_dict)
    grid_dict["sites"] = [site]
    return json.dumps(grid_dict)


def _run_multi(inputs: dict, failing_flags: list[bool], max_workers: int):
    configs = [
        _grid_json(inputs["config_dict"], idx + 1, failing)
        for idx, failing in enumerate(failing_flags)
    ]
    return inputs["rust"].run_suews_multi(
        configs, inputs["forcing_flat"], inputs["len_sim"], max_workers
    )


def _assert_names_grid(exc_info, grid_index: int) -> None:
    message = str(exc_info.value)
    assert message.startswith(f"grid {grid_index}: "), message
    assert f"(code {FATAL_CODE})" in message, message


@pytest.mark.parametrize("trial", range(5))
def test_parallel_failure_names_the_failing_grid(bridge_inputs, trial):
    """A valid grid must never be reported as the failing one.

    Before gh#1736 the process-global flag set by grid 1 was copied into
    grid 0's state at the end of grid 0's next timestep, so the batch error
    was attributed to grid 0 in a large fraction of runs.
    """
    with pytest.raises(RuntimeError) as exc_info:
        _run_multi(bridge_inputs, [False, True, False, False], max_workers=4)
    _assert_names_grid(exc_info, 1)


def test_serial_and_parallel_name_the_same_grid(bridge_inputs):
    with pytest.raises(RuntimeError) as serial_exc:
        _run_multi(bridge_inputs, [False, True, False, False], max_workers=1)
    with pytest.raises(RuntimeError) as parallel_exc:
        _run_multi(bridge_inputs, [False, True, False, False], max_workers=4)
    assert str(serial_exc.value) == str(parallel_exc.value)


def test_failing_grid_survives_later_grid_resets(bridge_inputs):
    """More grids than workers: later grids start (and reset their own
    error store) while the failing grid is still running. The failure must
    still be reported, and against the right grid."""
    with pytest.raises(RuntimeError) as exc_info:
        _run_multi(bridge_inputs, [True] + [False] * 7, max_workers=2)
    _assert_names_grid(exc_info, 0)


def test_valid_batch_after_failure_runs_clean(bridge_inputs):
    """A failed parallel batch leaves no residue: the next valid batch runs to
    completion for every grid with the right grid ids and lengths."""
    with pytest.raises(RuntimeError):
        _run_multi(bridge_inputs, [False, True, False, False], max_workers=4)

    results = sorted(_run_multi(bridge_inputs, [False] * 4, max_workers=4))
    assert [r[0] for r in results] == [0, 1, 2, 3]
    assert all(r[3] == bridge_inputs["len_sim"] for r in results)
    assert all(len(r[1]) == len(results[0][1]) for r in results)


@pytest.mark.xfail(
    reason="gh#1741: implicitly saved Fortran locals make parallel output "
    "differ from serial; promote to a regular test once fixed",
    raises=AssertionError,
    strict=False,
)
def test_parallel_output_matches_serial(bridge_inputs):
    """Identical valid grids must give bit-identical output in serial and
    parallel execution."""
    parallel = sorted(_run_multi(bridge_inputs, [False] * 4, max_workers=4))
    serial = sorted(_run_multi(bridge_inputs, [False] * 4, max_workers=1))
    for parallel_result, serial_result in zip(parallel, serial, strict=True):
        idx_p, out_p, state_p, len_p, _warnings_p = parallel_result
        idx_s, out_s, state_s, len_s, _warnings_s = serial_result
        assert idx_p == idx_s
        assert len_p == len_s
        assert bytes(out_p) == bytes(out_s)
        assert state_p == state_s


def test_public_multi_grid_run_names_the_failing_grid():
    """Same contract through the public config path used by SUEWSSimulation."""
    sim = sp.SUEWSSimulation.from_sample_data()
    config = sim.config
    base = config.sites[0]
    sites = []
    for idx in range(4):
        site = base.model_copy(deep=True)
        site.gridiv = idx + 1
        if idx == 1:
            site.properties.z.value = 0.5
        sites.append(site)
    config.sites = sites
    df_forcing = sim._df_forcing.iloc[:N_STEPS]

    with pytest.raises(RuntimeError) as exc_info:
        _run_rust.run_suews_rust_multi(config, df_forcing, max_workers=4)
    _assert_names_grid(exc_info, 1)
