"""End-to-end and contract tests for ``supy.util.get_spinup_state`` (gh#2091)."""

from __future__ import annotations

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pandas as pd
import pytest

from supy.data_model import SUEWSConfig
from supy.suews_sim import SUEWSSimulation
from supy.util import get_spinup_state

pytestmark = pytest.mark.api


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
