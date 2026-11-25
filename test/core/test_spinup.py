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
    # ARRANGE: 730-day forcing ending in spring so LAI, porosity, soilstore evolve.
    # Start 2018-01-01 so April 2019 aligns with spring forcing from the sample year.
    df_state_raw, df_forcing_raw = sample_data_loaded
    df_forcing = _extend_forcing(df_forcing_raw, days=730, start="2018-01-01")
    df_state_init = df_state_raw.copy()

    # ACT
    df_state_spinup, df_output_lc, df_state_lc = get_spinup_state(
        df_state_init,
        df_forcing,
        start_analysis="2019-04-15",
        spinup_days=10,
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
    assert df_state_spinup.loc[grid_id, ("lai_id", "(0,)")] == pytest.approx(
        df_state_lc.loc[2, ("lai_id", "(0,)")]
    )
    assert df_state_spinup.loc[grid_id, ("lai_id", "(1,)")] == pytest.approx(
        df_state_lc.loc[3, ("lai_id", "(1,)")]
    )
    assert df_state_spinup.loc[grid_id, ("lai_id", "(2,)")] == pytest.approx(
        df_state_lc.loc[4, ("lai_id", "(2,)")]
    )
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
