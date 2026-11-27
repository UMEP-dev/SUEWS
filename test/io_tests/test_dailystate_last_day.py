"""DailyState publishes every reached day-end, separately for each grid.

Under SUEWS's ``closed="right", label="right"`` convention, day ``D`` spans
``(D 00:00, D+1 00:00]`` and ``DailyState`` publishes at closing midnight
``D+1 00:00``. See ``suews_phys_dailystate.f95:update_DailyStateLine`` and
``docs/source/inputs/timestamp-convention.rst``.
"""

import numpy as np
import pandas as pd
import pytest

pytestmark = [pytest.mark.api, pytest.mark.contract("C-CLOCK", "C-OUTPUT")]

START = "2012-01-01"
END = "2012-03-02 01:15"
MIDDLE = pd.Timestamp("2012-01-12 00:00")


def _publication_calendar(forcing, step, grids):
    """Derive closing-midnight day-ends from independent, regular forcing."""
    assert len(forcing) > 0
    assert step >= pd.Timedelta(minutes=1)
    assert step % pd.Timedelta(minutes=1) == pd.Timedelta(0)
    assert pd.Timedelta(hours=1) % step == pd.Timedelta(0)
    assert forcing.equals(pd.date_range(forcing[0], forcing[-1], freq=step))
    closing = pd.date_range(
        forcing[0].normalize(),
        forcing[-1].normalize() + pd.Timedelta(days=1),
        freq="D",
    )
    # Keep every reached closing midnight D+1 00:00 (including a 1-step partial
    # first day if forcing starts at 00:00); exclude an unfinished last day.
    closing = closing[(closing >= forcing[0]) & (closing <= forcing[-1])]
    assert closing.isin(forcing).all()
    return pd.MultiIndex.from_product(
        [grids, closing], names=["grid", "datetime"]
    ).sort_values()


def _assert_daily_coverage(daily, expected):
    """Check publication identity and validity without rejecting sparse slots."""
    assert daily.index.is_unique, "duplicate DailyState rows"
    assert daily.columns.is_unique, "duplicate DailyState columns"
    # Any populated field counts as a publication, including a stray row
    # whose P_day is NaN. Empty subdaily rows are intentionally permitted.
    actual = daily.index[daily.notna().any(axis=1)].sort_values()
    pd.testing.assert_index_equal(actual, expected)
    precipitation = daily.loc[expected, "P_day"].to_numpy()
    assert np.isfinite(precipitation).all(), "missing/nonfinite daily P_day"
    # Other DailyState slots may legitimately be unpopulated. All values
    # that ARE published must be finite and must not leak the kernel sentinel.
    values = daily.loc[expected].to_numpy()
    populated = values[~np.isnan(values)]
    assert np.isfinite(populated).all(), "nonfinite DailyState value"
    assert not np.equal(populated, -999.0).any(), "DailyState missing-data sentinel"


@pytest.fixture(scope="module")
def run_input(sample_config_loaded, sample_data_loaded):
    _, df_forcing = sample_data_loaded
    step = pd.Timedelta(seconds=sample_config_loaded.model.control.tstep)
    forcing = df_forcing.index.copy()
    # Independently express the documented date-only/exact-time bounds;
    # do not call the production period slicer to construct the oracle.
    forcing = forcing[(forcing > pd.Timestamp(START)) & (forcing <= pd.Timestamp(END))]
    assert step == pd.Timedelta(minutes=5)
    assert forcing[0] == pd.Timestamp(START) + step
    assert forcing[-1] == pd.Timestamp(END)
    grids = [site.gridiv for site in sample_config_loaded.sites]
    return None, forcing, step, grids


@pytest.fixture(scope="module")
def expected_publications(run_input):
    _, forcing, step, grids = run_input
    return _publication_calendar(forcing, step, grids)


@pytest.fixture(scope="module")
def output(sample_config_run_cached):
    """The original public-API sample run, including leap day and partial end."""
    return sample_config_run_cached(START, END)


def test_last_partial_day_has_no_dailystate_row(output, expected_publications):
    _assert_daily_coverage(output.df["DailyState"], expected_publications)


def test_sample_calendar_covers_first_middle_leap_and_last_days(
    run_input, expected_publications
):
    _, _, _, grids = run_input
    for grid in grids:
        stamps = expected_publications[
            expected_publications.get_level_values("grid") == grid
        ]
        assert len(stamps) == 61  # 31 January + 29 February + 1 March
        for stamp in (
            "2012-01-02 00:00",
            MIDDLE,
            "2012-03-01 00:00",
            "2012-03-02 00:00",
        ):
            assert (grid, pd.Timestamp(stamp)) in stamps
        assert (grid, pd.Timestamp("2012-03-03 00:00")) not in stamps


@pytest.mark.parametrize(
    "start,end,step,publications",
    [
        (
            "2012-02-28 00:00",
            "2012-03-01 01:15",
            "5min",
            ["2012-02-28 00:00", "2012-02-29 00:00", "2012-03-01 00:00"],
        ),
        (
            "2012-02-28 00:05",
            "2012-03-01 01:15",
            "5min",
            ["2012-02-29 00:00", "2012-03-01 00:00"],
        ),
        ("2012-02-29 12:00", "2012-02-29 23:55", "5min", []),
        ("2012-02-29 12:00", "2012-03-01 00:00", "5min", ["2012-03-01 00:00"]),
        ("2012-02-29 12:00", "2012-03-01 01:00", "30min", ["2012-03-01 00:00"]),
    ],
)
def test_calendar_boundary_semantics(start, end, step, publications):
    forcing = pd.date_range(start, end, freq=step)
    actual = _publication_calendar(forcing, pd.Timedelta(step), [1, 2])
    # Compare timestamps, not the resolution inferred for an empty index.
    assert actual.tolist() == [
        (grid, pd.Timestamp(stamp)) for grid in (1, 2) for stamp in publications
    ]


@pytest.mark.parametrize(
    "damage",
    [
        "missing-first",
        "missing-middle",
        "missing-leap",
        "missing-last",
        "delete-middle",
        "truncate-first",
        "truncate-last",
        "extra",
        "wrong-time",
        "partial-final",
        "extra-before",
        "extra-after",
        "extra-without-p-day",
        "duplicate",
    ],
)
def test_coverage_rejects_damaged_publications(output, expected_publications, damage):
    daily = output.df["DailyState"].copy()
    grid = expected_publications[0][0]
    middle = (grid, MIDDLE)
    if damage.startswith("missing-"):
        stamp = {
            "missing-first": expected_publications[0][1],
            "missing-middle": MIDDLE,
            "missing-leap": pd.Timestamp("2012-03-01 00:00"),
            "missing-last": expected_publications[-1][1],
        }[damage]
        daily.loc[(grid, stamp), :] = np.nan
    elif damage == "delete-middle":
        daily = daily.drop(index=middle)
    elif damage.startswith("truncate-"):
        times = daily.index.get_level_values("datetime")
        if damage == "truncate-first":
            keep = times > pd.Timestamp("2012-01-02 00:00")
        else:
            keep = times < pd.Timestamp("2012-03-02 00:00")
        daily = daily.loc[keep]
    elif damage == "duplicate":
        daily = pd.concat([daily, daily.loc[[middle]]])
    else:
        stamp = {
            "partial-final": pd.Timestamp(END),
            "extra-before": pd.Timestamp("2012-01-01 00:00"),
            "extra-after": pd.Timestamp("2012-03-03 00:00"),
        }.get(damage, MIDDLE - pd.Timedelta(minutes=5))
        daily.loc[(grid, stamp), :] = daily.loc[middle].to_numpy()
        if damage == "wrong-time":
            daily.loc[middle, :] = np.nan
        elif damage == "extra-without-p-day":
            daily.loc[(grid, stamp), "P_day"] = np.nan
            daily.loc[(grid, stamp), "Tmin"] = 0.0
    with pytest.raises(AssertionError):
        _assert_daily_coverage(daily, expected_publications)


@pytest.mark.parametrize("value", [np.nan, np.inf, -np.inf, -999.0])
def test_coverage_rejects_invalid_p_day(output, expected_publications, value):
    daily = output.df["DailyState"].copy()
    daily.loc[(expected_publications[0][0], MIDDLE), "P_day"] = value
    with pytest.raises(AssertionError):
        _assert_daily_coverage(daily, expected_publications)


@pytest.mark.parametrize("value", [np.inf, -np.inf, -999.0])
def test_coverage_rejects_invalid_other_field(output, expected_publications, value):
    daily = output.df["DailyState"].copy()
    daily.loc[(expected_publications[0][0], MIDDLE), "Tmin"] = value
    with pytest.raises(AssertionError):
        _assert_daily_coverage(daily, expected_publications)


def test_coverage_accepts_zero_and_sparse_slots(output, expected_publications):
    daily = output.df["DailyState"].copy()
    # Retain all native time slots but deliberately empty optional fields.
    # A zero P_day must still count as a publication, not as missing data.
    daily.loc[:, :] = np.nan
    daily.loc[expected_publications, "P_day"] = 0.0
    _assert_daily_coverage(daily, expected_publications)


@pytest.mark.parametrize(
    "damage", [None, "missing-middle", "missing-grid", "extra-grid"]
)
def test_separate_grid_coverage(output, run_input, damage):
    _, forcing, step, grids = run_input
    # Replication tests the checker, not independent two-grid model physics.
    one_grid = output.df["DailyState"].xs(grids[0], level="grid")
    daily = pd.concat([one_grid, one_grid], keys=[1, 2], names=["grid"])
    expected = _publication_calendar(forcing, step, [1, 2])
    _assert_daily_coverage(daily, expected)
    if damage is None:
        return
    daily = daily.copy()
    if damage == "missing-middle":
        daily.loc[(2, MIDDLE), :] = np.nan
    elif damage == "missing-grid":
        daily = daily.drop(index=2, level="grid")
    else:
        daily = pd.concat([daily, pd.concat([one_grid], keys=[3], names=["grid"])])
    with pytest.raises(AssertionError):
        _assert_daily_coverage(daily, expected)
