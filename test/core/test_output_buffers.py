"""The kernel writes run output straight into caller-owned arrays (gh#1849).

``run_suews_into`` and ``run_suews_multi_into`` hand the Fortran kernel the
memory of two NumPy arrays, which then become the output frame without a
copy. These tests pin the contract: the frame built that way is exactly the
frame parsed from the legacy flat block, it shares the kernel's buffer, and
unsuitable buffers are refused before the kernel can write into them.
"""

from importlib.resources import as_file
import json

from conftest import SHORT_RUN_STEPS
import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation

# The test intentionally exercises the private native boundary.
# ruff: disable[import-private-name]
from supy._env import trv_supy_module
from supy._run_rust import (
    _alloc_output_buffers,
    _frame_from_output_buffers,
    _load_rust_module,
    _parse_output_block,
    _prepare_forcing_block,
)

# ruff: enable[import-private-name]

pytestmark = pytest.mark.physics

GRID_ID = 1


@pytest.fixture(scope="module")
def native_inputs():
    """Return the bridge module, a config JSON and a short forcing block."""
    with as_file(trv_supy_module / "sample_data") as path_sample_dir:
        simulation = SUEWSSimulation(str(path_sample_dir / "sample_config.yml"))
    config_json = json.dumps(
        simulation.config.model_dump(exclude_none=True, mode="json")
    )
    df_forcing = simulation.forcing.df.iloc[:SHORT_RUN_STEPS]
    return {
        "rust": _load_rust_module(),
        "config_json": config_json,
        "forcing_flat": _prepare_forcing_block(df_forcing).ravel(order="C").tolist(),
        "len_sim": len(df_forcing),
    }


def _run_legacy(inputs, state_json=None):
    args = (inputs["config_json"], inputs["forcing_flat"], inputs["len_sim"])
    if state_json is None:
        output_flat, state_out, len_sim = inputs["rust"].run_suews(*args)[:3]
    else:
        output_flat, state_out, len_sim = inputs["rust"].run_suews_with_state(
            *args, state_json
        )[:3]
    return _parse_output_block(output_flat, len_sim, GRID_ID), state_out


def _run_into(inputs, state_json=None):
    ar_data, ar_datetime = _alloc_output_buffers(inputs["len_sim"])
    state_out, len_sim, _ = inputs["rust"].run_suews_into(
        inputs["config_json"],
        inputs["forcing_flat"],
        inputs["len_sim"],
        ar_data,
        ar_datetime,
        state_json,
    )
    assert len_sim == inputs["len_sim"]
    return _frame_from_output_buffers(ar_data, ar_datetime, GRID_ID), state_out, ar_data


def test_buffer_run_matches_legacy_flat_block(native_inputs):
    """A fresh run and a checkpoint continuation match the legacy path exactly."""
    df_legacy, state_legacy = _run_legacy(native_inputs)
    df_into, state_into, _ = _run_into(native_inputs)

    pd.testing.assert_frame_equal(df_into, df_legacy, check_exact=True)
    assert state_into == state_legacy

    df_legacy_next, _ = _run_legacy(native_inputs, state_legacy)
    df_into_next, _, _ = _run_into(native_inputs, state_into)
    pd.testing.assert_frame_equal(df_into_next, df_legacy_next, check_exact=True)


def test_output_frame_is_the_kernel_buffer(native_inputs):
    """The frame wraps the array the kernel wrote into rather than a copy."""
    df_into, _, ar_data = _run_into(native_inputs)

    assert np.shares_memory(df_into.to_numpy(), ar_data)


def test_parallel_grids_match_serial_runs(native_inputs):
    """Each grid of a Rayon run fills its own buffers as a serial run would."""
    df_serial, state_serial, _ = _run_into(native_inputs)
    list_buffers = [_alloc_output_buffers(native_inputs["len_sim"]) for _ in range(2)]

    results = native_inputs["rust"].run_suews_multi_into(
        [native_inputs["config_json"]] * 2,
        native_inputs["forcing_flat"],
        native_inputs["len_sim"],
        [ar_data for ar_data, _ in list_buffers],
        [ar_datetime for _, ar_datetime in list_buffers],
    )

    assert sorted(idx for idx, *_ in results) == [0, 1]
    for idx, state_json, len_sim, _ in results:
        assert len_sim == native_inputs["len_sim"]
        assert state_json == state_serial
        df_grid = _frame_from_output_buffers(*list_buffers[idx], GRID_ID)
        pd.testing.assert_frame_equal(df_grid, df_serial, check_exact=True)


@pytest.mark.parametrize(
    ("make_buffers", "message"),
    [
        (
            lambda d, t: (np.ascontiguousarray(d), t),
            "Fortran-contiguous",
        ),
        (
            lambda d, t: (np.zeros((d.shape[0] - 1, d.shape[1]), order="F"), t),
            "elements",
        ),
        (
            lambda d, t: (d, np.zeros((t.shape[0], t.shape[1] + 1), order="F")),
            "elements",
        ),
        (lambda d, t: (_read_only(d), t), "writable"),
    ],
    ids=["c-order", "short-data", "wide-datetime", "read-only"],
)
def test_unsuitable_buffers_are_refused(native_inputs, make_buffers, message):
    """The bridge refuses buffers the kernel cannot safely write into."""
    ar_data, ar_datetime = make_buffers(
        *_alloc_output_buffers(native_inputs["len_sim"])
    )

    with pytest.raises(ValueError, match=message):
        native_inputs["rust"].run_suews_into(
            native_inputs["config_json"],
            native_inputs["forcing_flat"],
            native_inputs["len_sim"],
            ar_data,
            ar_datetime,
        )


def test_grids_sharing_a_buffer_are_refused(native_inputs):
    """Two grids may not be handed the same output memory."""
    ar_data, ar_datetime = _alloc_output_buffers(native_inputs["len_sim"])

    with pytest.raises(ValueError, match="share memory"):
        native_inputs["rust"].run_suews_multi_into(
            [native_inputs["config_json"]] * 2,
            native_inputs["forcing_flat"],
            native_inputs["len_sim"],
            [ar_data, ar_data],
            [ar_datetime, ar_datetime.copy(order="F")],
        )


def _read_only(ar: np.ndarray) -> np.ndarray:
    ar.flags.writeable = False
    return ar
