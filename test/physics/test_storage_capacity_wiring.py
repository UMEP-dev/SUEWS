"""Regression tests for the storage-capacity rows reaching the kernel (gh#2018).

The kernel takes a surface's current storage capacity from row 6 of
``StoreDrainPrm(6, nsurf)``; row 5 holds ``store_max``. supy and the legacy
tables write ``store_max`` to row 5 and ``store_cap`` to row 6, but the Rust
YAML loader wrote them the other way round, so a run took its drainage
threshold from ``store_max`` and ignored ``store_cap``.

No shipped configuration shows this: every sample surface either has
``store_max == store_cap`` or uses a drainage equation that does not read the
capacity. These tests switch the paved surface to the threshold drainage
equation (``drain_eq`` 1, Falk and Niemczynowicz 1978), under which no water
drains until the surface state exceeds the current storage capacity, and check
that the capacity the kernel uses is ``store_cap``:

  * raising ``store_cap`` alone changes the paved surface state; and
  * raising ``store_max`` alone leaves every output unchanged, because no
    live kernel code reads row 5.

Both assertions fail when the two rows are swapped.
"""

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation

from conftest import TIMESTEPS_PER_DAY

pytestmark = pytest.mark.physics

# Sample-config paved capacity [mm]; the threshold the base run drains above.
STORE_LOW_MM = 0.48
# A capacity the paved state reaches less often, so drainage starts later.
STORE_HIGH_MM = 2.0
# Smallest paved-state difference [mm] counted as a real response to store_cap.
MIN_STATE_RESPONSE_MM = 0.05

N_DAYS = 10


def _run_paved_threshold_drainage(store_max: float, store_cap: float):
    """Run ``N_DAYS`` of the sample case with threshold drainage on paved.

    Returns the SUEWS output group for grid 1 and the forcing used.
    """
    sim = SUEWSSimulation.from_sample_data()
    cfg = sim._config
    sdp = cfg.sites[0].properties.land_cover.paved.storage_drain_params
    sdp.drain_eq.value = 1
    sdp.drain_coef_1.value = 10.0
    sdp.drain_coef_2.value = 3.0
    sdp.store_min.value = STORE_LOW_MM
    sdp.store_max.value = store_max
    sdp.store_cap.value = store_cap
    sim._df_state_init = cfg.to_df_state()

    df_forcing = sim.forcing.df.copy().iloc[: TIMESTEPS_PER_DAY * N_DAYS]
    sim.update_forcing(df_forcing)
    results = sim.run(end_date=df_forcing.index[-1])
    df_suews = results.xs("SUEWS", level="group", axis=1).xs(1, level="grid")
    return df_suews, df_forcing


@pytest.fixture(scope="module")
def runs() -> dict[str, pd.DataFrame]:
    """Base run plus one run per changed capacity field, shared by the tests."""
    df_base, df_forcing = _run_paved_threshold_drainage(
        store_max=STORE_LOW_MM, store_cap=STORE_LOW_MM
    )
    df_cap, _ = _run_paved_threshold_drainage(
        store_max=STORE_LOW_MM, store_cap=STORE_HIGH_MM
    )
    df_max, _ = _run_paved_threshold_drainage(
        store_max=STORE_HIGH_MM, store_cap=STORE_LOW_MM
    )
    return {
        "base": df_base,
        "cap": df_cap,
        "max": df_max,
        "forcing": df_forcing,
    }


@pytest.mark.core
def test_window_reaches_the_threshold(runs):
    """Guard: the window has rain and the base run drains above capacity.

    Without this the store_cap response below could pass or fail for a
    dry window rather than for the wiring.
    """
    assert runs["forcing"]["rain"].sum() > 0.0, "forcing window has no rain"
    assert runs["base"]["StPaved"].max() > STORE_LOW_MM, (
        "paved state never exceeds the base capacity, so the threshold "
        "drainage equation is never exercised in this window"
    )
    assert runs["base"]["Drainage"].sum() > 0.0


@pytest.mark.core
def test_store_cap_sets_the_drainage_threshold(runs):
    """Raising ``store_cap`` alone must change the paved surface state."""
    diff = (runs["cap"]["StPaved"] - runs["base"]["StPaved"]).abs().max()
    assert diff > MIN_STATE_RESPONSE_MM, (
        "changing store_cap from "
        f"{STORE_LOW_MM} to {STORE_HIGH_MM} mm moved StPaved by at most "
        f"{diff} mm: the kernel is not reading store_cap as the current "
        "storage capacity (regression of gh#2018)"
    )


@pytest.mark.core
def test_store_max_alone_changes_nothing(runs):
    """Raising ``store_max`` alone must leave the outputs unchanged."""
    for col in ("StPaved", "Drainage", "State", "RO", "QE"):
        np.testing.assert_allclose(
            runs["max"][col].to_numpy(),
            runs["base"][col].to_numpy(),
            rtol=0.0,
            atol=1e-12,
            err_msg=(
                f"changing store_max alone moved {col}: the kernel is "
                "reading store_max as the current storage capacity "
                "(regression of gh#2018)"
            ),
        )
