"""Check that each expected carbon flux is present at every native timestep."""

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSOutput
from supy.data_model.output.suews_vars import SUEWS_VARIABLES

pytestmark = pytest.mark.api

# The owning registry supplies identities; the driver supplies activation:
# suews_ctrl_driver.f95:SUEWS_cal_BiogenCO2 calculates these for emissions >= 11
# and explicitly zeros the biogenic components otherwise. No spin-up exemption.
CARBON_NAMES = ("Fc", "FcPhoto", "FcRespi")
CARBON_COLUMNS = tuple(
    (variable.group, variable.name)
    for variable in SUEWS_VARIABLES
    if variable.name in CARBON_NAMES
)


def assert_complete_carbon(frame, expected_index):
    """Require every selected carbon value at independently expected native rows."""
    assert {name for _, name in CARBON_COLUMNS} == set(CARBON_NAMES)
    assert len(expected_index) > 0, "Expected carbon window is empty"
    assert expected_index.is_unique and frame.index.is_unique
    assert frame.columns.is_unique, "Duplicate output columns"
    missing = [column for column in CARBON_COLUMNS if column not in frame.columns]
    assert not missing, f"Missing carbon columns: {missing}"
    unexpected = frame.index.difference(expected_index)
    assert unexpected.empty, f"Unexpected output rows: {unexpected[:3].tolist()}"
    # Reindex an independently generated window to expose a missing model step.
    selected = frame.reindex(expected_index).loc[:, list(CARBON_COLUMNS)]
    values = selected.to_numpy(dtype=float)
    bad = ~np.isfinite(values) | np.equal(values, -999.0)
    if bad.any():
        row, column = np.argwhere(bad)[0]
        grid, stamp = selected.index[row]
        name = selected.columns[column][1]
        raise AssertionError(
            f"{name} at grid {grid}, {stamp}: {values[row, column]} "
            f"({bad.sum()} invalid carbon values)"
        )


def _complete_frame():
    index = pd.MultiIndex.from_product(
        [[1], pd.date_range("2012-01-01", periods=3, freq="5min")],
        names=["grid", "datetime"],
    )
    return pd.DataFrame(
        np.tile([1.0, -2.0, 3.0], (len(index), 1)),
        index=index,
        columns=pd.MultiIndex.from_tuples(CARBON_COLUMNS, names=["group", "var"]),
    )


@pytest.mark.parametrize("bad", [np.nan, np.inf, -np.inf, -999.0])
@pytest.mark.parametrize("name", CARBON_NAMES)
def test_completeness_rejects_one_seeded_cell(name, bad):
    frame = _complete_frame()
    expected_index = frame.index.copy()
    # Independent seed: only one cell changes, with valid neighbours retained.
    frame.loc[expected_index[1], ("SUEWS", name)] = bad
    with pytest.raises(AssertionError, match=rf"{name} at grid 1, 2012-01-01 00:05:00"):
        assert_complete_carbon(frame, expected_index)


@pytest.mark.parametrize("defect", ["column", "all_nan", "nan_row", "row", "empty"])
def test_completeness_rejects_missing_extent(defect):
    frame = _complete_frame()
    expected_index = frame.index.copy()
    if defect == "column":
        frame = frame.drop(columns=[("SUEWS", "FcPhoto")])
    elif defect == "all_nan":
        frame["SUEWS", "FcPhoto"] = np.nan
    elif defect == "nan_row":
        frame.loc[expected_index[1], :] = np.nan
    elif defect == "row":
        frame = frame.drop(index=expected_index[1])
    else:
        frame = frame.iloc[:0]
    with pytest.raises(AssertionError):
        assert_complete_carbon(frame, expected_index)


def test_completeness_accepts_zero_and_ignores_unselected_missing_fields():
    frame = _complete_frame() * 0.0
    frame["SUEWS", "Fcld"] = np.nan
    assert_complete_carbon(frame, frame.index)


def test_parquet_null_does_not_hide_missing_carbon(tmp_path):
    pq = pytest.importorskip(
        "pyarrow.parquet", reason="Parquet output requires pyarrow"
    )

    frame = _complete_frame()
    expected_index = frame.index.copy()
    frame.loc[expected_index[1], ("SUEWS", "Fc")] = np.nan
    state = pd.DataFrame({("snowuse", "0"): [0], ("tstep", "0"): [300]}, index=[1])
    files = SUEWSOutput(frame, state).save(tmp_path, format="parquet", freq_s=300)
    (path,) = [file for file in files if file.name.endswith("_output.parquet")]
    table = pq.read_table(path)
    assert table.column(str(("SUEWS", "Fc"))).null_count == 1
    with pytest.raises(AssertionError, match="Fc at grid 1"):
        assert_complete_carbon(pd.read_parquet(path), expected_index)


@pytest.mark.physics
@pytest.mark.parametrize("emissions", [2, 12, 22, 32, 42])
def test_native_carbon_complete_from_first_to_last_step(short_sample_sim, emissions):
    sim = short_sample_sim()
    sim.update_config({"model": {"physics": {"emissions": {"value": emissions}}}})
    forcing = sim.forcing.df.iloc[:576].copy()
    sim.update_forcing(forcing)
    output = sim.run(start_date=forcing.index[0], end_date=forcing.index[-1], n_jobs=1)
    # Native output is expected for each input step, including the first step.
    grids = [site.gridiv for site in sim.config.sites]
    expected_index = pd.MultiIndex.from_product(
        [grids, forcing.index], names=["grid", "datetime"]
    )
    assert_complete_carbon(output.df, expected_index)
    if emissions == 2:
        biogenic = output.df.loc[:, [("SUEWS", "FcPhoto"), ("SUEWS", "FcRespi")]]
        assert (biogenic == 0).all().all()
