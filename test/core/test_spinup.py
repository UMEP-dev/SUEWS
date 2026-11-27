"""End-to-end and contract tests for ``supy.util.get_spinup_state`` (gh#2091)."""

from __future__ import annotations

from importlib.resources import as_file, files

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pandas as pd
import pytest

from supy.data_model import SUEWSConfig
from supy.suews_forcing import SUEWSForcing
from supy.suews_sim import SUEWSSimulation
from supy.util import get_spinup_state, read_forcing

pytestmark = [pytest.mark.api, pytest.mark.contract("C-SPINUP")]


def _extend_forcing(frame: pd.DataFrame, *, days: int, start: str) -> pd.DataFrame:
    """Repeat real sample forcing rows to span ``days`` at 5-minute resolution."""
    idx = pd.date_range(start, periods=days * TIMESTEPS_PER_DAY + 1, freq="5min")
    extended = frame.iloc[np.arange(len(idx)) % len(frame)].copy()
    extended.index = idx
    return extended


def test_get_spinup_state_730d_real_simulation_evolves_states(
    sample_data_loaded: tuple[pd.DataFrame, pd.DataFrame],
) -> None:
    """Real >= 730-day spin-up completes without error and writes evolved states."""
    # ARRANGE: 730-day forcing ending in spring so LAI, GDD, porosity, soilstore evolve.
    # Start 2018-01-01 so spring 2019 aligns with spring forcing from the sample year.
    # A 30-day window ending in late May accumulates enough spring GDD for LAI to move
    # even when gdd_base is positive.
    df_state_raw, df_forcing_raw = sample_data_loaded
    df_forcing = _extend_forcing(df_forcing_raw, days=730, start="2018-01-01")
    df_state_init = df_state_raw.copy()

    # ACT
    df_state_spinup, df_output_lc, df_state_lc = get_spinup_state(
        df_state_init,
        df_forcing,
        start_analysis="2019-05-20",
        spinup_days=30,
        save_spinup=True,
    )

    # ASSERT: shape, index, and output preserved
    assert df_state_spinup.shape == df_state_init.shape
    assert df_state_spinup.index.equals(df_state_init.index)
    assert len(df_output_lc) > 0
    assert list(df_state_lc.index) == list(range(7))

    # Genuinely evolved states (not a stale copy of df_state_init)
    grid_id = df_state_spinup.index[0]
    assert np.isfinite(df_state_spinup.loc[grid_id, ("soilstore_surf", "(2,)")]).all()
    assert df_state_spinup.loc[grid_id, ("soilstore_surf", "(2,)")] != pytest.approx(
        df_state_init.loc[grid_id, ("soilstore_surf", "(2,)")]
    )
    assert df_state_spinup.loc[grid_id, ("lai_id", "(1,)")] != pytest.approx(
        df_state_init.loc[grid_id, ("lai_id", "(1,)")]
    )
    assert df_state_spinup.loc[grid_id, ("porosity_id", "0")] != pytest.approx(0.5)
    assert df_state_spinup.loc[grid_id, ("decidcap_id", "0")] != pytest.approx(
        df_state_init.loc[grid_id, ("decidcap_id", "0")]
    )

    # Each surface came from its own 100%-cover spin-up grid (not misaligned diagonals)
    for surf_idx in range(7):
        col = f"({surf_idx},)"
        assert df_state_spinup.loc[grid_id, ("soilstore_surf", col)] == pytest.approx(
            df_state_lc.loc[surf_idx, ("soilstore_surf", col)]
        )
        assert df_state_spinup.loc[grid_id, ("state_surf", col)] == pytest.approx(
            df_state_lc.loc[surf_idx, ("state_surf", col)]
        )
        assert df_state_spinup.loc[grid_id, ("alb", col)] == pytest.approx(
            df_state_lc.loc[surf_idx, ("alb", col)]
        )
    for grid_idx, veg_idx in ((2, 0), (3, 1), (4, 2)):
        veg_col = f"({veg_idx},)"
        for pheno_var in ("lai_id", "gdd_id", "sdd_id"):
            assert df_state_spinup.loc[grid_id, (pheno_var, veg_col)] == pytest.approx(
                df_state_lc.loc[grid_idx, (pheno_var, veg_col)]
            )
    assert df_state_spinup.loc[grid_id, ("gdd_id", "(1,)")] > 0.0
    assert df_state_spinup.loc[grid_id, ("albdectr_id", "0")] == pytest.approx(
        df_state_lc.loc[3, ("albdectr_id", "0")]
    )
    assert df_state_spinup.loc[grid_id, ("porosity_id", "0")] == pytest.approx(
        df_state_lc.loc[3, ("porosity_id", "0")]
    )
    assert df_state_spinup.loc[grid_id, ("decidcap_id", "0")] == pytest.approx(
        df_state_lc.loc[3, ("decidcap_id", "0")]
    )

    # Returned state reloads cleanly into SUEWSSimulation via SUEWSConfig
    sim_next = SUEWSSimulation(SUEWSConfig.from_df_state(df_state_spinup))
    assert sim_next.state_init.loc[grid_id, ("porosity_id", "0")] == pytest.approx(
        df_state_spinup.loc[grid_id, ("porosity_id", "0")]
    )
    assert sim_next.state_init.loc[grid_id, ("gdd_id", "(1,)")] == pytest.approx(
        df_state_spinup.loc[grid_id, ("gdd_id", "(1,)")]
    )
    assert sim_next.state_init.loc[grid_id, ("sdd_id", "(1,)")] == pytest.approx(
        df_state_spinup.loc[grid_id, ("sdd_id", "(1,)")]
    )


def test_get_spinup_state_transfers_evolved_gdd_and_sdd(
    sample_data_loaded: tuple[pd.DataFrame, pd.DataFrame],
) -> None:
    """Spin-up hands off evolved gdd_id (growth) and sdd_id (senescence) alongside lai_id."""
    df_state_raw, df_forcing_raw = sample_data_loaded
    df_forcing = _extend_forcing(df_forcing_raw, days=730, start="2018-01-01")
    grid_id = df_state_raw.index[0]

    # Growth season: start from zero GDD/SDD and verify positive GDD evolves and hands off.
    df_init_spring = df_state_raw.copy()
    for veg_idx in range(3):
        df_init_spring.loc[:, ("gdd_id", f"({veg_idx},)")] = 0.0
        df_init_spring.loc[:, ("sdd_id", f"({veg_idx},)")] = 0.0

    df_spring, _, df_spring_lc = get_spinup_state(
        df_init_spring,
        df_forcing,
        start_analysis="2019-05-20",
        spinup_days=15,
        save_spinup=True,
    )
    for grid_idx, veg_idx in ((2, 0), (3, 1), (4, 2)):
        col = f"({veg_idx},)"
        assert df_spring.loc[grid_id, ("gdd_id", col)] > 0.0
        assert df_spring.loc[grid_id, ("gdd_id", col)] == pytest.approx(
            df_spring_lc.loc[grid_idx, ("gdd_id", col)]
        )
        assert df_spring.loc[grid_id, ("sdd_id", col)] == pytest.approx(
            df_spring_lc.loc[grid_idx, ("sdd_id", col)]
        )

    # Senescence season: start from zero GDD/SDD in October and verify negative SDD hands off.
    df_init_autumn = df_state_raw.copy()
    for veg_idx in range(3):
        df_init_autumn.loc[:, ("gdd_id", f"({veg_idx},)")] = 0.0
        df_init_autumn.loc[:, ("sdd_id", f"({veg_idx},)")] = 0.0

    df_autumn, _, df_autumn_lc = get_spinup_state(
        df_init_autumn,
        df_forcing,
        start_analysis="2019-11-15",
        spinup_days=15,
        save_spinup=True,
    )
    for grid_idx, veg_idx in ((2, 0), (3, 1), (4, 2)):
        col = f"({veg_idx},)"
        assert df_autumn.loc[grid_id, ("sdd_id", col)] < 0.0
        assert df_autumn.loc[grid_id, ("sdd_id", col)] == pytest.approx(
            df_autumn_lc.loc[grid_idx, ("sdd_id", col)]
        )
        assert df_autumn.loc[grid_id, ("gdd_id", col)] == pytest.approx(
            df_autumn_lc.loc[grid_idx, ("gdd_id", col)]
        )


def test_get_spinup_state_1_to_2_year_forcing_duplicates_and_runs(
    sample_forcing_parsed,
) -> None:
    """Forcing between 364 and 730 days still duplicates and returns evolved state."""
    # ARRANGE: real 1-year sample forcing (366 days in 2012)
    df_state_init, df_forcing = sample_forcing_parsed()

    # ACT
    df_state_spinup = get_spinup_state(
        df_state_init,
        df_forcing,
        start_analysis="2012-04-10",
        spinup_days=3,
    )

    # ASSERT
    assert df_state_spinup.shape == df_state_init.shape
    grid_id = df_state_spinup.index[0]
    assert df_state_spinup.loc[grid_id, ("soilstore_surf", "(2,)")] != pytest.approx(
        df_state_init.loc[grid_id, ("soilstore_surf", "(2,)")]
    )


def test_get_spinup_state_multi_grid_preserves_index_and_broadcasts(
    sample_data_loaded: tuple[pd.DataFrame, pd.DataFrame],
) -> None:
    """Multi-grid input keeps its grid index and receives spun-up states on every row."""
    # ARRANGE
    df_state_raw, df_forcing_raw = sample_data_loaded
    df_multi = pd.concat([df_state_raw, df_state_raw], ignore_index=True)
    df_multi.index = pd.Index([10, 20], name="grid")
    df_forcing = _extend_forcing(df_forcing_raw, days=730, start="2018-01-01")

    # ACT
    df_spun = get_spinup_state(
        df_multi,
        df_forcing,
        start_analysis="2019-12-31",
        spinup_days=1,
    )

    # ASSERT
    assert list(df_spun.index) == [10, 20]
    assert df_spun.loc[10, ("soilstore_surf", "(2,)")] == pytest.approx(
        df_spun.loc[20, ("soilstore_surf", "(2,)")]
    )
    assert df_spun.loc[10, ("porosity_id", "0")] == pytest.approx(
        df_spun.loc[20, ("porosity_id", "0")]
    )


def test_get_spinup_state_rejects_forcing_shorter_than_one_year(
    sample_forcing_parsed,
) -> None:
    """Forcing shorter than 364 days raises ValueError."""
    # ARRANGE
    df_state_init, short_forcing = sample_forcing_parsed(TIMESTEPS_PER_DAY * 10)

    # ACT / ASSERT
    with pytest.raises(ValueError, match="shorter than one year"):
        get_spinup_state(
            df_state_init,
            short_forcing,
            start_analysis="2012-01-10",
            spinup_days=5,
        )


@pytest.mark.parametrize(
    "source", ["simulation", "hourly_file", "multiple_files", "two_years"]
)
def test_get_spinup_state_without_freq_matches_regular_input(
    sample_forcing_parsed, tmp_path, source
) -> None:
    """Losing frequency metadata changes neither output nor evolved state."""
    # ARRANGE: exercise real loaders, including two distinct source files.
    df_state, df_forcing = sample_forcing_parsed()
    start_analysis = "2012-04-10"
    if source in {"hourly_file", "multiple_files"}:
        with as_file(files("supy") / "sample_data") as path_sample:
            path_forcing = path_sample / "Kc_2012_data_60.txt"
            if source == "hourly_file":
                df_forcing = read_forcing(str(path_forcing), tstep_mod=None)
            else:
                # Keep file units intact; the public loader converts them.
                df_file = pd.read_csv(path_forcing, sep=r"\s+")
                midpoint = len(df_file) // 2
                paths = [tmp_path / "first.txt", tmp_path / "second.txt"]
                for path, part in zip(
                    paths,
                    [df_file.iloc[:midpoint], df_file.iloc[midpoint:]],
                    strict=True,
                ):
                    part.to_csv(path, sep=" ", index=False, encoding="utf-8")
                df_forcing = SUEWSForcing.from_file(paths).df
    elif source == "two_years":
        df_forcing = _extend_forcing(df_forcing, days=730, start="2018-01-01")
        start_analysis = "2019-04-10"

    assert df_forcing.index.freq is not None
    forcing_before = df_forcing.copy(deep=True)
    df_without_freq = df_forcing.copy()
    df_without_freq.index = pd.DatetimeIndex(
        df_forcing.index.to_numpy(), name=df_forcing.index.name
    )
    assert df_without_freq.index.freq is None
    before = df_without_freq.copy(deep=True)
    state_before = df_state.copy(deep=True)

    # ACT: no simulation or checkpoint mocks; both calls use the public API.
    expected = get_spinup_state(
        df_state, df_forcing, start_analysis, spinup_days=1, save_spinup=True
    )
    actual = get_spinup_state(
        df_state, df_without_freq, start_analysis, spinup_days=1, save_spinup=True
    )

    # ASSERT: identical inputs apart from metadata must yield identical values.
    for result, reference in zip(actual, expected, strict=True):
        pd.testing.assert_frame_equal(result, reference, check_exact=True)
    assert not actual[1].empty
    grid_id = df_state.index[0]
    assert actual[0].loc[grid_id, ("soilstore_surf", "(2,)")] != pytest.approx(
        df_state.loc[grid_id, ("soilstore_surf", "(2,)")]
    )
    pd.testing.assert_frame_equal(df_without_freq, before, check_exact=True)
    assert df_without_freq.index.freq is None
    pd.testing.assert_frame_equal(df_state, state_before, check_exact=True)
    pd.testing.assert_frame_equal(df_forcing, forcing_before, check_exact=True)


@pytest.mark.parametrize("rows", [1, 2])
@pytest.mark.parametrize("strip_freq", [False, True])
def test_get_spinup_state_short_input_keeps_duration_error(
    sample_forcing_parsed, rows, strip_freq
) -> None:
    """One or two ordinary rows fail the duration guard, not pandas inference."""
    df_state, df_forcing = sample_forcing_parsed(rows)
    if strip_freq:
        df_forcing.index = pd.DatetimeIndex(df_forcing.index.to_numpy())

    with pytest.raises(ValueError, match="shorter than one year"):
        get_spinup_state(df_state, df_forcing, "2012-01-02", spinup_days=1)


@pytest.mark.parametrize(
    "invalid",
    ["empty", "gap", "duplicate", "reversed", "nat", "all_nat", "non_datetime"],
)
def test_get_spinup_state_rejects_invalid_duplication_index(
    sample_forcing_parsed, invalid
) -> None:
    """Duplication must not invent a cadence or relabel irregular observations."""
    df_state, df_forcing = sample_forcing_parsed()
    message = "regular, increasing DatetimeIndex"
    if invalid == "empty":
        df_forcing = df_forcing.iloc[:0]
        message = "empty"
    elif invalid == "gap":
        df_forcing = df_forcing.drop(df_forcing.index[10])
    elif invalid == "duplicate":
        df_forcing = pd.concat([df_forcing.iloc[:11], df_forcing.iloc[10:]])
    elif invalid == "reversed":
        df_forcing = df_forcing.iloc[::-1]
    elif invalid == "non_datetime":
        df_forcing = df_forcing.reset_index(drop=True)
        message = "pandas.DatetimeIndex"
    else:
        timestamps = df_forcing.index.to_numpy(copy=True)
        message = "must not contain NaT"
        if invalid == "all_nat":
            timestamps[:] = np.datetime64("NaT")
        else:
            timestamps[10] = np.datetime64("NaT")
        df_forcing.index = pd.DatetimeIndex(timestamps)

    with pytest.raises(ValueError, match=message):
        get_spinup_state(df_state, df_forcing, "2012-04-10", spinup_days=1)


@pytest.mark.parametrize(
    "forcing_mode", ["two_year_5min", "one_year_5min", "one_year_hourly"]
)
def test_get_spinup_state_exact_pre_analysis_boundary_succeeds(
    sample_forcing_parsed,
    forcing_mode: str,
) -> None:
    """Complete interval-end forcing matching ``(start_analysis - spinup_days, start_analysis]`` runs."""
    # ARRANGE: interval-end forcing whose first interval starts at 00:00:00.
    df_state_init, df_forcing_1yr = sample_forcing_parsed()
    if forcing_mode == "two_year_5min":
        # Drop the leading 00:00 row so intervals start at 2018-01-01 00:00:00 (row 00:05).
        df_forcing = _extend_forcing(df_forcing_1yr, days=730, start="2018-01-01").iloc[
            1:
        ]
        start_analysis = "2018-01-02"
        expected_first_output = pd.Timestamp("2018-01-01 00:05:00")
        expected_last_output = pd.Timestamp("2018-01-02 00:00:00")
    elif forcing_mode == "one_year_5min":
        # Real 2012 sample forcing starts at 2012-01-01 00:05:00 (interval start 00:00:00).
        df_forcing = df_forcing_1yr
        start_analysis = "2012-01-02"
        expected_first_output = pd.Timestamp("2012-01-01 00:05:00")
        expected_last_output = pd.Timestamp("2012-01-02 00:00:00")
    else:
        with as_file(files("supy") / "sample_data") as path_sample:
            df_forcing = read_forcing(
                str(path_sample / "Kc_2012_data_60.txt"), tstep_mod=None
            )
        start_analysis = "2012-01-02"
        expected_first_output = pd.Timestamp("2012-01-01 00:05:00")
        expected_last_output = pd.Timestamp("2012-01-02 00:00:00")

    # ACT
    df_state_spinup, df_output_lc, _ = get_spinup_state(
        df_state_init,
        df_forcing,
        start_analysis=start_analysis,
        spinup_days=1,
        save_spinup=True,
    )

    # ASSERT: full 288-step (1-day) spin-up window ran with evolved state.
    assert df_state_spinup.shape == df_state_init.shape
    assert df_state_spinup.index.equals(df_state_init.index)
    assert not df_output_lc.empty
    timestamps = df_output_lc.index.get_level_values("datetime")
    assert timestamps.min() == expected_first_output
    assert timestamps.max() == expected_last_output
    assert len(df_output_lc.xs(0, level="grid")) == TIMESTEPS_PER_DAY
    grid_id = df_state_spinup.index[0]
    assert np.isfinite(df_state_spinup.loc[grid_id, ("soilstore_surf", "(2,)")]).all()
    assert df_state_spinup.loc[grid_id, ("soilstore_surf", "(2,)")] != pytest.approx(
        df_state_init.loc[grid_id, ("soilstore_surf", "(2,)")]
    )


@pytest.mark.parametrize(
    (
        "case",
        "start_analysis",
        "spinup_days",
        "extra_leading_drop",
        "expected_start",
        "expected_earliest",
        "expected_available",
    ),
    [
        (
            "1_step_short_via_start_analysis",
            "2018-01-10 23:55:00",
            10,
            0,
            "2017-12-31 23:55:00",
            "2018-01-01 00:05:00",
            "9 days 23:55:00",
        ),
        (
            "1_step_short_via_earliest_row",
            "2018-01-11",
            10,
            1,
            "2018-01-01 00:00:00",
            "2018-01-01 00:10:00",
            "9 days 23:55:00",
        ),
        (
            "multi_day_short_gh2129",
            "2018-01-03",
            10,
            0,
            "2017-12-24 00:00:00",
            "2018-01-01 00:05:00",
            "2 days 00:00:00",
        ),
        (
            "zero_pre_analysis_at_start",
            "2018-01-01",
            10,
            0,
            "2017-12-22 00:00:00",
            "2018-01-01 00:05:00",
            "0 days 00:00:00",
        ),
        (
            "start_analysis_precedes_forcing",
            "2017-12-15",
            10,
            0,
            "2017-12-05 00:00:00",
            "2018-01-01 00:05:00",
            "0 days 00:00:00",
        ),
    ],
)
def test_get_spinup_state_rejects_insufficient_pre_analysis_forcing(
    sample_data_loaded: tuple[pd.DataFrame, pd.DataFrame],
    case: str,
    start_analysis: str,
    spinup_days: int,
    extra_leading_drop: int,
    expected_start: str,
    expected_earliest: str,
    expected_available: str,
) -> None:
    """Insufficient pre-analysis forcing fails loudly instead of silently truncating."""
    # ARRANGE: interval-end >=2-year forcing starting at 2018-01-01 00:05 (interval 00:00).
    df_state_raw, df_forcing_raw = sample_data_loaded
    df_state = df_state_raw.copy()
    df_forcing = _extend_forcing(df_forcing_raw, days=731, start="2018-01-01").iloc[
        1 + extra_leading_drop :
    ]
    state_before = df_state.copy(deep=True)
    forcing_before = df_forcing.copy(deep=True)

    # ACT / ASSERT
    with pytest.raises(ValueError, match="Insufficient pre-analysis forcing") as exc:
        get_spinup_state(
            df_state,
            df_forcing,
            start_analysis=start_analysis,
            spinup_days=spinup_days,
        )
    message = str(exc.value)
    assert f"spinup_days={spinup_days}" in message, case
    assert expected_start in message, case
    assert expected_earliest in message, case
    assert expected_available in message, case
    pd.testing.assert_frame_equal(df_state, state_before, check_exact=True)
    pd.testing.assert_frame_equal(df_forcing, forcing_before, check_exact=True)


def test_get_spinup_state_rejects_insufficient_pre_analysis_on_1_year_forcing(
    sample_forcing_parsed,
) -> None:
    """1-to-2-year forcing also fails loudly when pre-analysis data is too short."""
    # ARRANGE: real 2012 sample forcing covers (2012-01-01 00:00, 2013-01-01 00:00].
    df_state, df_forcing = sample_forcing_parsed()

    # ACT / ASSERT: 1-step short (5 minutes) and default 365-day spin-up both fail loudly.
    with pytest.raises(
        ValueError, match="Insufficient pre-analysis forcing"
    ) as exc_step:
        get_spinup_state(
            df_state,
            df_forcing,
            start_analysis="2012-01-10 23:55:00",
            spinup_days=10,
        )
    assert "9 days 23:55:00" in str(exc_step.value)

    with pytest.raises(
        ValueError, match="Insufficient pre-analysis forcing"
    ) as exc_default:
        get_spinup_state(
            df_state,
            df_forcing,
            start_analysis="2012-04-10",
            spinup_days=365,
        )
    assert "spinup_days=365" in str(exc_default.value)
    assert "2011-04-11 00:00:00" in str(exc_default.value)
    assert "100 days 00:00:00" in str(exc_default.value)


@pytest.mark.parametrize(
    ("start_analysis", "expected_available"),
    [
        ("2020-01-05", "6 days 00:00:00"),
        ("2020-02-01", "0 days 00:00:00"),
    ],
)
def test_get_spinup_state_rejects_start_analysis_after_forcing_end(
    sample_data_loaded: tuple[pd.DataFrame, pd.DataFrame],
    start_analysis: str,
    expected_available: str,
) -> None:
    """Forcing ending before ``start_analysis`` fails loudly instead of truncating."""
    # ARRANGE
    df_state_raw, df_forcing_raw = sample_data_loaded
    df_forcing = _extend_forcing(df_forcing_raw, days=730, start="2018-01-01")

    # ACT / ASSERT
    with pytest.raises(ValueError, match="Insufficient forcing coverage") as exc:
        get_spinup_state(
            df_state_raw.copy(),
            df_forcing,
            start_analysis=start_analysis,
            spinup_days=10,
        )
    message = str(exc.value)
    assert "spinup_days=10" in message
    assert "2020-01-01 00:00:00" in message
    assert expected_available in message


def test_get_spinup_state_rejects_off_cadence_start_analysis(
    sample_data_loaded: tuple[pd.DataFrame, pd.DataFrame],
) -> None:
    """Off-cadence ``start_analysis`` raises ValueError instead of truncating the end."""
    df_state_raw, df_forcing_raw = sample_data_loaded
    df_forcing_5m = _extend_forcing(df_forcing_raw, days=730, start="2018-01-01")

    with pytest.raises(ValueError, match="not aligned with the forcing cadence"):
        get_spinup_state(
            df_state_raw.copy(),
            df_forcing_5m,
            start_analysis="2019-04-15 00:03:00",
            spinup_days=10,
        )

    with as_file(files("supy") / "sample_data") as path_sample:
        df_forcing_hourly = read_forcing(
            str(path_sample / "Kc_2012_data_60.txt"), tstep_mod=None
        )
    with pytest.raises(ValueError, match="not aligned with the forcing cadence"):
        get_spinup_state(
            df_state_raw.copy(),
            df_forcing_hourly,
            start_analysis="2012-04-15 00:30:00",
            spinup_days=10,
        )


@pytest.mark.parametrize(
    "invalid",
    ["empty", "gap", "duplicate", "reversed", "nat", "all_nat", "non_datetime"],
)
def test_get_spinup_state_rejects_invalid_2_year_forcing_index(
    sample_data_loaded: tuple[pd.DataFrame, pd.DataFrame],
    invalid: str,
) -> None:
    """Two-year forcing also enforces a valid, regular, increasing DatetimeIndex."""
    # ARRANGE
    df_state_raw, df_forcing_raw = sample_data_loaded
    df_forcing = _extend_forcing(df_forcing_raw, days=730, start="2018-01-01")
    message = "regular, increasing DatetimeIndex"
    if invalid == "empty":
        df_forcing = df_forcing.iloc[:0]
        message = "empty"
    elif invalid == "gap":
        df_forcing = df_forcing.drop(df_forcing.index[100])
    elif invalid == "duplicate":
        df_forcing = pd.concat([df_forcing.iloc[:101], df_forcing.iloc[100:]])
    elif invalid == "reversed":
        df_forcing = df_forcing.iloc[::-1]
    elif invalid == "non_datetime":
        df_forcing = df_forcing.reset_index(drop=True)
        message = "pandas.DatetimeIndex"
    else:
        timestamps = df_forcing.index.to_numpy(copy=True)
        message = "must not contain NaT"
        if invalid == "all_nat":
            timestamps[:] = np.datetime64("NaT")
        else:
            timestamps[100] = np.datetime64("NaT")
        df_forcing.index = pd.DatetimeIndex(timestamps)

    # ACT / ASSERT
    with pytest.raises(ValueError, match=message):
        get_spinup_state(
            df_state_raw.copy(),
            df_forcing,
            start_analysis="2019-04-15",
            spinup_days=10,
        )


@pytest.mark.parametrize("bad_days", [0, -1, -999, False, True, 1.5, float("nan")])
def test_get_spinup_state_rejects_invalid_spinup_days(
    sample_data_loaded: tuple[pd.DataFrame, pd.DataFrame],
    bad_days,
) -> None:
    """Non-positive, boolean, or non-integer ``spinup_days`` raises ValueError."""
    df_state_raw, df_forcing_raw = sample_data_loaded
    df_forcing = _extend_forcing(df_forcing_raw, days=730, start="2018-01-01")

    with pytest.raises(ValueError, match="spinup_days must be a positive integer"):
        get_spinup_state(
            df_state_raw.copy(),
            df_forcing,
            start_analysis="2019-04-15",
            spinup_days=bad_days,
        )


@pytest.mark.parametrize(
    "bad_start", ["not-a-date", "", None, pd.NaT, 20190415, float("nan")]
)
def test_get_spinup_state_rejects_invalid_start_analysis(
    sample_data_loaded: tuple[pd.DataFrame, pd.DataFrame],
    bad_start,
) -> None:
    """Unparseable, missing, or numeric ``start_analysis`` raises ValueError."""
    df_state_raw, df_forcing_raw = sample_data_loaded
    df_forcing = _extend_forcing(df_forcing_raw, days=730, start="2018-01-01")

    with pytest.raises(ValueError, match="Invalid start_analysis timestamp"):
        get_spinup_state(
            df_state_raw.copy(),
            df_forcing,
            start_analysis=bad_start,
            spinup_days=10,
        )


def test_get_spinup_state_rejects_timezone_mismatch_and_sparse_window(
    sample_data_loaded: tuple[pd.DataFrame, pd.DataFrame],
) -> None:
    """Timezone mismatch and 1-row daily spin-up slices fail loudly."""
    df_state_raw, df_forcing_raw = sample_data_loaded
    df_forcing = _extend_forcing(df_forcing_raw, days=730, start="2018-01-01")

    with pytest.raises(ValueError, match="timezone-naive"):
        get_spinup_state(
            df_state_raw.copy(),
            df_forcing,
            start_analysis="2019-04-15T00:00:00Z",
            spinup_days=10,
        )

    # Daily forcing spanning 730 days aligns on midnight but yields only 1 row for 1 day.
    idx_daily = pd.date_range("2018-01-01", periods=731, freq="1D")
    df_daily = df_forcing_raw.iloc[
        np.arange(len(idx_daily)) % len(df_forcing_raw)
    ].copy()
    df_daily.index = idx_daily
    with pytest.raises(ValueError, match="Insufficient forcing records"):
        get_spinup_state(
            df_state_raw.copy(),
            df_daily,
            start_analysis="2019-04-15",
            spinup_days=1,
        )
