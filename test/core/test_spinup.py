"""Unit tests for ``supy.util._spinup.get_spinup_state``."""

from __future__ import annotations

from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest

from supy.util._spinup import get_spinup_state

pytestmark = pytest.mark.api


def _make_dummy_state() -> pd.DataFrame:
    cols = pd.MultiIndex.from_tuples(
        [("sfr_surf", f"({i},)") for i in range(7)]
        + [("lai_id", f"({i},)") for i in range(3)]
        + [("soilstore_surf", f"({i},)") for i in range(7)]
        + [("state_surf", f"({i},)") for i in range(7)]
        + [("alb", f"({i},)") for i in range(7)]
        + [("porosity_id", "0"), ("decidcap_id", "0"), ("pormax_dec", "0"), ("pormin_dec", "0")]
    )
    data = np.zeros((1, len(cols)), dtype=float)
    df = pd.DataFrame(data, index=pd.Index([1], name="grid"), columns=cols)
    df.loc[:, "sfr_surf"] = [[0.2, 0.2, 0.1, 0.1, 0.2, 0.1, 0.1]]
    return df


def _make_forcing(days: int) -> pd.DataFrame:
    idx = pd.date_range("2020-01-01", periods=days * 24 + 1, freq="1h")
    return pd.DataFrame({"Tair": np.full(len(idx), 10.0)}, index=idx)


def test_get_spinup_state_rejects_short_forcing() -> None:
    # ARRANGE
    df_state = _make_dummy_state()
    df_forcing = _make_forcing(300)

    # ACT / ASSERT
    with pytest.raises(ValueError, match="shorter than one year"):
        get_spinup_state(df_state, df_forcing, start_analysis="2020-10-01")


@pytest.mark.parametrize(
    ("days", "start_analysis"),
    [
        (400, "2021-01-01"),
        (730, "2021-06-01"),
        (800, "2022-01-01"),
    ],
)
def test_get_spinup_state_handles_multiyear_forcing(
    monkeypatch: pytest.MonkeyPatch, days: int, start_analysis: str
) -> None:
    # ARRANGE
    df_state = _make_dummy_state()
    df_forcing = _make_forcing(days)
    captured: dict[str, pd.DataFrame] = {}

    class _FakeSimulation:
        def __init__(self, state_init: pd.DataFrame) -> None:
            self.state_init = state_init
            self.forcing_df: pd.DataFrame | None = None

        @classmethod
        def from_state(cls, state_init: pd.DataFrame) -> "_FakeSimulation":
            return cls(state_init)

        def update_forcing(self, forcing_df: pd.DataFrame) -> None:
            self.forcing_df = forcing_df
            captured["forcing"] = forcing_df

        def run(self) -> SimpleNamespace:
            assert self.forcing_df is not None
            idx_final = self.forcing_df.index[-1] + self.forcing_df.index.freq
            n_grids = len(self.state_init)
            multi_idx = pd.MultiIndex.from_product(
                [[idx_final], range(n_grids)], names=["datetime", "grid"]
            )
            state_final = pd.DataFrame(
                np.ones((n_grids, len(self.state_init.columns)), dtype=float),
                index=multi_idx,
                columns=self.state_init.columns,
            )
            # Mark per-surface diagonals with distinct finite values
            for var, size in [
                ("lai_id", 3),
                ("soilstore_surf", 7),
                ("state_surf", 7),
                ("alb", 7),
            ]:
                vals = np.zeros((n_grids, size), dtype=float)
                for i in range(size):
                    vals[i, i] = float(i + 2)
                state_final[var] = vals
            state_final.loc[(idx_final, 3), ("porosity_id", "0")] = 0.77
            state_final.loc[(idx_final, 3), ("decidcap_id", "0")] = 0.88
            return SimpleNamespace(df=pd.DataFrame(), state_final=state_final)

    import supy.suews_sim as suews_sim_mod

    monkeypatch.setattr(suews_sim_mod, "SUEWSSimulation", _FakeSimulation)

    # ACT
    result = get_spinup_state(
        df_state, df_forcing, start_analysis=start_analysis, spinup_days=365
    )

    # ASSERT
    assert "forcing" in captured
    assert len(captured["forcing"]) > 0
    assert captured["forcing"].index.max() == pd.Timestamp(start_analysis)
    assert captured["forcing"].index.min() == pd.Timestamp(start_analysis) - pd.Timedelta(days=365)
    np.testing.assert_allclose(result["lai_id"].iloc[0].to_numpy(), [2.0, 3.0, 4.0])
    np.testing.assert_allclose(
        result["soilstore_surf"].iloc[0].to_numpy(), [2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
    )
    assert float(result[("porosity_id", "0")].iloc[0]) == pytest.approx(0.77)
    assert float(result[("decidcap_id", "0")].iloc[0]) == pytest.approx(0.88)
