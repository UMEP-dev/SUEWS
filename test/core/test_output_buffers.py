"""The kernel exchanges forcing and output with SuPy as arrays (gh#1849).

``run_suews_into`` and ``run_suews_multi_into`` take the forcing as a NumPy
array rather than a list of Python floats, and hand the Fortran kernel the
memory of two output arrays, which then become the output frame without a
copy; the grids of a multi-grid run write into row blocks of one shared
array. These tests pin the contract: the frame built that way is exactly the
frame parsed from the legacy list-in, flat-block-out path or joined from
per-grid frames, it shares the kernel's buffer, and unsuitable forcing or
output arrays are refused before the kernel runs.
"""

from importlib import import_module
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
    run_suews_rust,
    run_suews_rust_multi,
    run_suews_rust_multi_with_state,
    run_suews_rust_with_state,
)

# ruff: enable[import-private-name]

pytestmark = pytest.mark.physics

_run_rust = import_module("supy._run_rust")

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
    forcing_block = _prepare_forcing_block(df_forcing)
    return {
        "config": simulation.config,
        "df_forcing": df_forcing,
        "rust": _load_rust_module(),
        "config_json": config_json,
        "forcing_block": forcing_block,
        "forcing_flat": forcing_block.ravel(order="C").tolist(),
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
        inputs["forcing_block"],
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
    """Each grid of a Rayon run fills its own buffers as a serial run would.

    The two grids differ in latitude, so their outputs differ and a grid
    written into the other grid's buffers would not go unnoticed.
    """
    dict_config = json.loads(native_inputs["config_json"])
    dict_config["sites"][0]["properties"]["lat"]["value"] += 10.0
    list_config_jsons = [native_inputs["config_json"], json.dumps(dict_config)]
    list_serial = [
        _run_into({**native_inputs, "config_json": config_json})
        for config_json in list_config_jsons
    ]
    assert not list_serial[0][0].equals(list_serial[1][0])
    list_buffers = [_alloc_output_buffers(native_inputs["len_sim"]) for _ in range(2)]

    results = native_inputs["rust"].run_suews_multi_into(
        list_config_jsons,
        native_inputs["forcing_block"],
        native_inputs["len_sim"],
        [ar_data for ar_data, _ in list_buffers],
        [ar_datetime for _, ar_datetime in list_buffers],
    )

    assert sorted(idx for idx, *_ in results) == [0, 1]
    for idx, state_json, len_sim, _ in results:
        df_serial, state_serial, _ = list_serial[idx]
        assert len_sim == native_inputs["len_sim"]
        assert state_json == state_serial
        df_grid = _frame_from_output_buffers(*list_buffers[idx], GRID_ID)
        pd.testing.assert_frame_equal(df_grid, df_serial, check_exact=True)


@pytest.mark.parametrize(
    ("make_buffers", "message"),
    [
        (
            lambda d, t: (np.ascontiguousarray(d), t),
            "column-major",
        ),
        (
            # every other row of a taller array: rows are not adjacent
            lambda d, t: (np.zeros((2 * d.shape[0], d.shape[1]), order="F")[::2], t),
            "column-major",
        ),
        (
            # columns closer together than a column is long: they overlap
            lambda d, t: (
                np.lib.stride_tricks.as_strided(
                    d, shape=d.shape, strides=(d.itemsize, d.itemsize * (len(d) - 1))
                ),
                t,
            ),
            "column-major",
        ),
        (
            lambda d, t: (np.zeros((d.shape[0] - 1, d.shape[1]), order="F"), t),
            "shape",
        ),
        (
            lambda d, t: (d, np.zeros((t.shape[0], t.shape[1] + 1), order="F")),
            "shape",
        ),
        (lambda d, t: (np.zeros(d.shape[::-1], order="F"), t), "shape"),
        (lambda d, t: (_read_only(d), t), "writable"),
    ],
    ids=[
        "c-order",
        "strided-rows",
        "overlapping-columns",
        "short-data",
        "wide-datetime",
        "transposed-data",
        "read-only",
    ],
)
def test_unsuitable_buffers_are_refused(native_inputs, make_buffers, message):
    """The bridge refuses buffers the kernel cannot safely write into."""
    ar_data, ar_datetime = make_buffers(
        *_alloc_output_buffers(native_inputs["len_sim"])
    )

    with pytest.raises(ValueError, match=message):
        native_inputs["rust"].run_suews_into(
            native_inputs["config_json"],
            native_inputs["forcing_block"],
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
            native_inputs["forcing_block"],
            native_inputs["len_sim"],
            [ar_data, ar_data],
            [ar_datetime, ar_datetime.copy(order="F")],
        )


def test_read_only_forcing_is_accepted(native_inputs):
    """The bridge copies the forcing for the kernel, so it may be read-only."""
    forcing_block = native_inputs["forcing_block"].copy()
    _read_only(forcing_block)
    df_into, _, _ = _run_into(native_inputs)

    df_read_only, _, _ = _run_into({**native_inputs, "forcing_block": forcing_block})

    pd.testing.assert_frame_equal(df_read_only, df_into, check_exact=True)
    np.testing.assert_array_equal(forcing_block, native_inputs["forcing_block"])


@pytest.mark.parametrize(
    ("make_forcing", "error", "message"),
    [
        (lambda f: f.astype(np.float32), TypeError, "float64"),
        (lambda f: f.astype(np.int64), TypeError, "float64"),
        (lambda f: f.ravel().tolist(), TypeError, "float64"),
        (np.asfortranarray, ValueError, "C-contiguous"),
        (
            lambda f: np.zeros((f.shape[0], 2 * f.shape[1]))[:, ::2],
            ValueError,
            "C-contiguous",
        ),
        (lambda f: f[:-1], ValueError, "shape"),
        (lambda f: np.ascontiguousarray(f.T), ValueError, "shape"),
        (lambda f: f.ravel(), ValueError, "shape"),
    ],
    ids=[
        "float32",
        "int64",
        "list",
        "f-order",
        "strided",
        "short",
        "transposed",
        "flat",
    ],
)
def test_unsuitable_forcing_is_refused(native_inputs, make_forcing, error, message):
    """Both bridge entry points refuse forcing the kernel cannot read as is."""
    forcing_block = make_forcing(native_inputs["forcing_block"])
    ar_data, ar_datetime = _alloc_output_buffers(native_inputs["len_sim"])

    with pytest.raises(error, match=message):
        native_inputs["rust"].run_suews_into(
            native_inputs["config_json"],
            forcing_block,
            native_inputs["len_sim"],
            ar_data,
            ar_datetime,
        )
    with pytest.raises(error, match=message):
        native_inputs["rust"].run_suews_multi_into(
            [native_inputs["config_json"]],
            forcing_block,
            native_inputs["len_sim"],
            [ar_data],
            [ar_datetime],
        )


def test_grids_overlapping_in_a_shared_array_are_refused(native_inputs):
    """Row blocks of one array may serve several grids only if they are disjoint."""
    len_sim = native_inputs["len_sim"]
    ar_data, ar_datetime = _alloc_output_buffers(2 * len_sim)
    rows_b = slice(len_sim // 2, len_sim // 2 + len_sim)

    with pytest.raises(ValueError, match="share memory"):
        native_inputs["rust"].run_suews_multi_into(
            [native_inputs["config_json"]] * 2,
            native_inputs["forcing_block"],
            len_sim,
            [ar_data[:len_sim], ar_data[rows_b]],
            [ar_datetime[:len_sim], ar_datetime[len_sim:]],
        )


def _two_lat_config_jsons(inputs):
    """Two grid configs differing in latitude, so their outputs differ."""
    dict_config = json.loads(inputs["config_json"])
    dict_config["sites"][0]["properties"]["lat"]["value"] += 10.0
    return [inputs["config_json"], json.dumps(dict_config)]


def test_grids_write_into_row_blocks_of_one_array(native_inputs):
    """Grids sharing one output array each fill exactly their own rows.

    Each block matches a run of that grid into an array of its own, whether
    the grids run in parallel or one after another.
    """
    len_sim = native_inputs["len_sim"]
    list_config_jsons = _two_lat_config_jsons(native_inputs)
    list_own = [
        _run_into({**native_inputs, "config_json": config_json})
        for config_json in list_config_jsons
    ]
    list_rows = [slice(0, len_sim), slice(len_sim, 2 * len_sim)]

    ar_data, ar_datetime = _alloc_output_buffers(2 * len_sim)
    results = native_inputs["rust"].run_suews_multi_into(
        list_config_jsons,
        native_inputs["forcing_block"],
        len_sim,
        [ar_data[rows] for rows in list_rows],
        [ar_datetime[rows] for rows in list_rows],
    )
    ar_data_serial, ar_datetime_serial = _alloc_output_buffers(2 * len_sim)
    for config_json, rows in zip(list_config_jsons, list_rows):
        native_inputs["rust"].run_suews_into(
            config_json,
            native_inputs["forcing_block"],
            len_sim,
            ar_data_serial[rows],
            ar_datetime_serial[rows],
        )

    assert sorted(idx for idx, *_ in results) == [0, 1]
    for idx, state_json, _, _ in results:
        df_own, state_own, _ = list_own[idx]
        assert state_json == state_own
        for ar_d, ar_t in [
            (ar_data, ar_datetime),
            (ar_data_serial, ar_datetime_serial),
        ]:
            rows = list_rows[idx]
            df_block = _frame_from_output_buffers(ar_d[rows], ar_t[rows], GRID_ID)
            pd.testing.assert_frame_equal(df_block, df_own, check_exact=True)


def _two_grid_config(config):
    """Two grids at different latitudes, the one listed first with the larger ID.

    Their outputs differ, so a grid's rows labelled with the other grid's ID
    would not match the per-grid runs.
    """
    config_two = config.model_copy(deep=True)
    site_other = config_two.sites[0].model_copy(deep=True)
    config_two.sites[0].gridiv = 2
    site_other.gridiv = 1
    site_other.properties.lat.value += 10.0
    config_two.sites.append(site_other)
    return config_two


@pytest.mark.parametrize("serial_mode", [False, True], ids=["rayon", "serial"])
def test_multi_grid_frame_matches_joined_grid_frames(
    native_inputs, monkeypatch, serial_mode
):
    """A multi-grid run gives the frame the per-grid frames joined to.

    That frame is the one array every grid wrote into, and a continuation
    from the run's checkpoint states matches the per-grid continuations too.
    """
    config = _two_grid_config(native_inputs["config"])
    df_forcing = native_inputs["df_forcing"]
    n_first = len(df_forcing) // 2
    list_store = []
    alloc = _run_rust._alloc_output_buffers

    def capture_alloc(n_rows):
        list_store.append(alloc(n_rows))
        return list_store[-1]

    monkeypatch.setattr(_run_rust, "_alloc_output_buffers", capture_alloc)

    df_multi, dict_state, _ = run_suews_rust_multi(
        config, df_forcing.iloc[:n_first], serial_mode=serial_mode
    )
    df_multi_next, _, _ = run_suews_rust_multi_with_state(
        config, df_forcing.iloc[n_first:], dict_state, serial_mode=serial_mode
    )

    assert len(list_store) == 2
    ar_store = list_store[0][0]
    assert ar_store.shape[0] in {2 * n_first, 2 * n_first + 1}
    assert np.shares_memory(df_multi.to_numpy(), ar_store)

    list_first, list_next = [], []
    for site in config.sites:
        config_site = config.model_copy(update={"sites": [site]})
        df_site, _, _ = run_suews_rust(
            config_site, df_forcing.iloc[:n_first], grid_id=site.gridiv
        )
        list_first.append(df_site)
        df_site_next, _, _ = run_suews_rust_with_state(
            config_site,
            df_forcing.iloc[n_first:],
            grid_id=site.gridiv,
            state_json=dict_state[site.gridiv],
        )
        list_next.append(df_site_next)

    assert not np.array_equal(
        list_first[0].to_numpy(), list_first[1].to_numpy(), equal_nan=True
    )
    pd.testing.assert_frame_equal(
        df_multi, pd.concat(list_first).sort_index(), check_exact=True
    )
    pd.testing.assert_frame_equal(
        df_multi_next, pd.concat(list_next).sort_index(), check_exact=True
    )


def _read_only(ar: np.ndarray) -> np.ndarray:
    ar.flags.writeable = False
    return ar
