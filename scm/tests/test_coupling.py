"""Smoke test of the two-way SUEWS-column coupling (short but real run)."""

import numpy as np
import pytest

supy = pytest.importorskip("supy")

from suews_scm.coupling import CoupledSCM  # noqa: E402


@pytest.fixture(scope="module")
def sample():
    df_state, df_forcing = supy.load_sample_data()
    return df_state, df_forcing


def test_coupled_run_two_hours_stays_physical(sample):
    df_state, df_forcing = sample
    scm = CoupledSCM(df_state, df_forcing)
    df, snaps = scm.run("2012-07-01 00:05", "2012-07-01 02:00")

    assert len(df) == 24  # 5-min steps over ~2 h
    for colname in ("tair_mod", "rh_mod", "u_mod", "qh", "qe", "h_bl"):
        assert np.all(np.isfinite(df[colname]))
    # July night in London: modelled air temperature within sane bounds
    assert df["tair_mod"].between(0.0, 40.0).all()
    assert df["rh_mod"].between(2.0, 100.0).all()
    # the column was initialised from the first observed record, so the
    # first coupled step must start very close to the observation
    assert abs(df["tair_mod"].iloc[0] - df["tair_obs"].iloc[0]) < 0.5
    # snapshots collected
    assert len(snaps["times"]) >= 2
    assert np.all(np.isfinite(snaps["theta"][-1]))
