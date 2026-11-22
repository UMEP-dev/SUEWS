"""Pair intact output with failures that identify each deliberate mutation."""

from functools import partial
import re
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest
import test_core_physics as core
import test_dyohm_building as dyohm
import test_option_sweep as sweep
import test_rslprof as wind

pytestmark = [pytest.mark.api, pytest.mark.contract("C-BALANCE", "C-RESPONSE")]


@pytest.fixture
def frames():
    times = pd.date_range("2012-06-01 00:05", periods=4, freq="5min")
    index = pd.MultiIndex.from_product([[1], times], names=["grid", "datetime"])
    columns = [
        ("SUEWS", name) for name in ("State", "Rain", "Irr", "Evap", "RO", "U10")
    ]
    columns += [
        ("debug", f"ss_{surface}_next")
        for surface in ("Paved", "Bldgs", "EveTr", "DecTr", "Grass", "BSoil", "Water")
    ]
    columns += [("RSL", f"{field}_{i}") for field in ("z", "U") for i in range(1, 31)]
    output = pd.DataFrame(0.0, index=index, columns=pd.MultiIndex.from_tuples(columns))
    for i in range(1, 31):
        output["RSL", f"z_{i}"] = float(i)
    state = pd.DataFrame(
        [[1 / 7] * 7],
        index=[1],
        columns=pd.MultiIndex.from_product([
            ["sfr_surf"],
            [f"({i},)" for i in range(7)],
        ]),
    )
    output["SUEWS", "Fcld"] = np.nan  # Documented unavailable sample forcing.
    output["RSL", "elm"] = np.nan  # Not applicable under MOST.
    return output, state, pd.DataFrame(index=times)


def _damage(output, column, defect):
    result = output.copy()
    if defect is None:
        return result
    if defect == "missing":
        return result.drop(columns=[column])
    if defect == "row":
        return result.iloc[:-1]
    if defect == "all_nan":
        result[column] = np.nan
    else:
        result.loc[result.index[1], column] = defect
    return result


def _check_outcome(run, expected):
    if expected is None:
        run()
    else:
        with pytest.raises(AssertionError, match=re.escape(expected)):
            run()


def _invalid_cell(index, defect):
    if defect == "all_nan":
        return index[0], "nan"
    return index[1], str(defect)


@pytest.mark.core
@pytest.mark.parametrize(
    "entry,column",
    [
        ("test_wind_speeds_positive", ("SUEWS", "U10")),
        ("test_most_height_array_monotonic", ("RSL", "z_30")),
        ("test_tall_building_no_negative_winds", ("RSL", "U_30")),
        ("test_full_building_auto_rsl_grid_runs_without_nan", ("SUEWS", "U10")),
    ],
)
@pytest.mark.parametrize(
    "defect", [None, np.nan, np.inf, -999.0, "missing", "all_nan", "row"]
)
def test_wind_entry_rejects_damage(monkeypatch, frames, entry, column, defect):
    output, state, forcing = frames
    # Three profiles simulate only the first two forcing steps.
    output, forcing = output.iloc[:2], forcing.iloc[:2]
    damaged = _damage(output, column, defect)
    monkeypatch.setattr(wind, "run_simulation", lambda *a, **kw: (damaged, None))
    args = (10.0, 1.0, 7.0) if entry == "test_wind_speeds_positive" else ()
    expected = None
    if defect == "missing":
        expected = f"Missing wind output columns for grids=[1]: {[column]}"
    elif defect == "row":
        expected = (
            "Incomplete wind output rows (grid, datetime): "
            f"missing={output.index[-1:].tolist()}, unexpected=[], duplicates=[]"
        )
    elif defect is not None:
        row, value = _invalid_cell(output.index, defect)
        expected = f"Invalid wind output: {column} at grid/time={row}: {value}"
    run = partial(getattr(wind.TestWindProfiles(), entry), (state, forcing), *args)
    _check_outcome(run, expected)


@pytest.mark.core
@pytest.mark.parametrize(
    "column", [("SUEWS", "State"), ("debug", "ss_Paved_next"), ("SUEWS", "Rain")]
)
@pytest.mark.parametrize("defect", [None, np.nan, np.inf, -999.0, "missing"])
@pytest.mark.parametrize("entry", ["sweep", "core"])
def test_water_entry_rejects_damage(monkeypatch, frames, column, defect, entry):
    output, state, forcing = frames
    damaged = _damage(output, column, defect)
    if entry == "sweep":
        monkeypatch.setattr(sweep, "patch_physics", lambda *a: state)
        monkeypatch.setattr(sweep, "run_simulation", lambda *a, **kw: (damaged, None))
        run = partial(
            sweep.test_option_value_runs_finite_and_conserves_water,
            "control",
            SimpleNamespace(name="control"),
            None,
            (state, forcing),
        )
    else:
        test = core.TestPhysicalValidation()
        test.df_output, test.df_state_init = damaged, state
        test.df_forcing_week = forcing
        run = test.test_water_balance_consistency
    expected = None
    if defect == "missing":
        expected = f"Missing water balance columns for grid=1: {[column]}"
    elif defect is not None:
        expected = f"Invalid water balance input: {column} at grid=1, time={forcing.index[1]}: {defect}"
    _check_outcome(run, expected)


@pytest.mark.core
def test_water_first_interval_has_no_predecessor(frames):
    output, state, forcing = frames
    output.loc[output.index[0], ("SUEWS", "Rain")] = 1.0
    np.testing.assert_equal(sweep.water_balance_residual_mm(output, state), 0.0)
    test = core.TestPhysicalValidation()
    test.df_output, test.df_state_init = output, state
    test.df_forcing_week = forcing
    test.test_water_balance_consistency()


def _qs_diagnostic(index, defect):
    if defect is None:
        return None
    if defect == "missing":
        return "Missing DyOHM QS column for grid=1"
    if defect == "row":
        return (
            "Incomplete or misordered DyOHM QS rows (grid, datetime): "
            f"missing={index[-1:].tolist()}, unexpected=[], duplicates=[]"
        )
    row, value = _invalid_cell(index, defect)
    return f"Invalid DyOHM QS: grid/time={row}: {value}"


@pytest.mark.core
@pytest.mark.parametrize(
    "defect", [None, np.nan, np.inf, -999.0, "missing", "all_nan", "row"]
)
def test_dyohm_sensitivity_rejects_damage(monkeypatch, frames, defect):
    output, state, forcing = frames
    baseline = pd.DataFrame({"QS": [1.0] * len(forcing)}, index=output.index)
    changed = baseline.copy()
    changed.iloc[2, 0] = 2.0  # A genuine response must not hide another missing cell.
    changed = _damage(changed, "QS", defect)
    results = iter([baseline, changed, baseline.copy()])
    monkeypatch.setattr(dyohm, "load_sample_frames", lambda: (state, forcing))
    monkeypatch.setattr(dyohm, "_run_storage_case", lambda *a, **kw: next(results))
    _check_outcome(
        partial(dyohm.test_dyohm_uses_building_material_not_wall, 8),
        _qs_diagnostic(baseline.index, defect),
    )


@pytest.mark.core
@pytest.mark.parametrize(
    "defect", [None, np.nan, np.inf, -999.0, "missing", "all_nan", "row"]
)
def test_dyohm_relative_sensitivity_rejects_damage(monkeypatch, frames, defect):
    _, state, _ = frames
    times = pd.date_range("2012-06-01 00:05", periods=4, freq="12h")
    forcing = pd.DataFrame(index=times)
    index = pd.MultiIndex.from_product([[1], times], names=["grid", "datetime"])
    baseline = pd.DataFrame(1.0, index=index, columns=["QS", *dyohm.QS_SURF_COLUMNS])
    changed = baseline.copy()
    changed.loc[:, :] = 2.0
    changed = _damage(changed, "QS", defect)
    results = iter([baseline, baseline.copy(), baseline.copy(), changed])
    monkeypatch.setattr(dyohm, "load_sample_frames", lambda: (state, forcing))
    monkeypatch.setattr(dyohm, "_run_storage_case", lambda *a, **kw: next(results))
    _check_outcome(
        dyohm.test_dyohm_building_qs_behavior_relative_to_ohm,
        _qs_diagnostic(baseline.index, defect),
    )


@pytest.fixture
def two_grids(frames):
    output, state, forcing = frames
    output = pd.concat([output.loc[1], output.loc[1]], keys=[7, 1], names=["grid"])
    state = pd.concat([state, state.rename(index={1: 7})])
    state.loc[:, :] = 0.0
    state.loc[1, ("sfr_surf", "(0,)")] = 1.0
    state.loc[7, ("sfr_surf", "(1,)")] = 1.0
    for grid, surface, rain in ((1, "Paved", 1.0), (7, "Bldgs", 2.0)):
        output.loc[grid, ("debug", f"ss_{surface}_next")] = (
            np.arange(len(forcing)) * rain
        )
        output.loc[grid, ("SUEWS", "Rain")] = rain
    # Output grid order and both column orders deliberately differ from state order.
    return output.iloc[:, ::-1], state.iloc[:, ::-1], forcing


@pytest.mark.core
def test_water_balance_aligns_grid_surface_and_time(two_grids):
    output, state, _ = two_grids
    for grid in (1, 7):
        np.testing.assert_equal(
            sweep.water_balance_residual_mm(output, state, grid=grid), 0.0
        )
    damaged = output.copy()
    damaged.loc[(7, damaged.loc[7].index[1]), ("SUEWS", "Rain")] += 0.5
    np.testing.assert_equal(
        sweep.water_balance_residual_mm(damaged, state, grid=7), 0.5
    )
    np.testing.assert_equal(
        sweep.water_balance_residual_mm(damaged, state, grid=1), 0.0
    )


@pytest.mark.core
@pytest.mark.parametrize("defect", [np.nan, "missing", "row"])
def test_wind_reports_field_and_grid_time(monkeypatch, two_grids, defect):
    output, state, forcing = two_grids
    # Missing row and invalid cell belong to grid 7, despite the initial state order.
    output = output.sort_index()
    column = ("SUEWS", "U10")
    if defect == "missing":
        damaged = output.drop(columns=[column])
        expected = "U10"
    elif defect == "row":
        damaged = output.drop(index=(7, forcing.index[1]))
        expected = "2012-06-01 00:10:00"
    else:
        damaged = output.copy()
        damaged.loc[(7, forcing.index[1]), column] = defect
        expected = "U10.*7.*2012-06-01 00:10:00"
    monkeypatch.setattr(wind, "run_simulation", lambda *a, **kw: (damaged, None))
    with pytest.raises(AssertionError, match=expected):
        wind.TestWindProfiles().test_wind_speeds_positive(
            (state, forcing), 10.0, 1.0, 7.0
        )


@pytest.mark.core
def test_wind_accepts_complete_multiple_grids(monkeypatch, two_grids):
    output, state, forcing = two_grids
    monkeypatch.setattr(wind, "run_simulation", lambda *a, **kw: (output, None))
    wind.TestWindProfiles().test_wind_speeds_positive((state, forcing), 10.0, 1.0, 7.0)


@pytest.mark.core
def test_water_reports_field_and_grid_time(two_grids):
    output, state, forcing = two_grids
    output.loc[(7, forcing.index[1]), ("debug", "ss_Bldgs_next")] = np.nan
    with pytest.raises(
        AssertionError, match=r"ss_Bldgs_next.*grid=7.*2012-06-01 00:10:00"
    ):
        sweep.water_balance_residual_mm(output, state, grid=7)
