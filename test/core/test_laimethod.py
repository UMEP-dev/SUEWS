"""Regression tests for laimethod (issue #1291).

The `model.physics.laimethod` option controls whether SUEWS computes LAI
internally via GDD/SDD thresholds (laimethod=1, default) or uses observed
values from the `lai` forcing column (laimethod=0). Issue #1291 reported
that the forcing column was silently ignored because the dailystate routine
hard-coded the local switch to `LAICalcYes = 1`.

These tests guard against regression in:
  * Validator plumbing (FORCING_REQUIREMENTS entry).
  * Fortran/Python wiring (the switch actually routes `forcing%LAI_obs`
    through to `LAI_id_next`).
"""

import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy._check import FORCING_REQUIREMENTS, check_forcing
from supy.data_model.core.model import LAIMethod

from conftest import TIMESTEPS_PER_DAY


def _base_forcing_df(n_timesteps: int = 24) -> pd.DataFrame:
    """Build a minimal hourly forcing DataFrame (mirrors the pattern in
    `test_wind_speed_validation.TestPhysicsSpecificValidation`)."""
    dates = pd.date_range("2021-01-01", periods=n_timesteps, freq="h")
    return pd.DataFrame(
        {
            "iy": dates.year,
            "id": dates.dayofyear,
            "it": dates.hour,
            "imin": dates.minute,
            "qn": -999,
            "qh": -999,
            "qe": -999,
            "qs": -999,
            "qf": -999,
            "U": 2.0,
            "RH": 60,
            "Tair": 20,
            "pres": 1013,
            "rain": 0,
            "kdown": 100,
            "snow": -999,
            "ldown": -999,
            "fcld": -999,
            "Wuh": -999,
            "xsmd": -999,
            "lai": -999,
            "kdiff": -999,
            "kdir": -999,
            "wdir": -999,
            "isec": 0,
        },
        index=dates,
    )


class TestLAIMethodValidator:
    """Unit tests for the `laimethod=0 -> lai column required` validator entry."""

    def test_requirements_entry_exists(self):
        """FORCING_REQUIREMENTS must include the laimethod entry for #1291."""
        assert ("laimethod", 0) in FORCING_REQUIREMENTS
        assert FORCING_REQUIREMENTS[("laimethod", 0)] == ["lai"]

    def test_laimethod_0_requires_lai(self):
        """laimethod=0 with an all-missing lai column emits an issue."""
        df_forcing = _base_forcing_df()
        issues = check_forcing(df_forcing, fix=False, physics={"laimethod": 0})
        assert any(
            "laimethod=0" in issue and "lai" in issue for issue in issues
        )

    def test_laimethod_0_passes_with_valid_lai(self):
        """laimethod=0 with a valid lai column passes the requirement check."""
        df_forcing = _base_forcing_df()
        df_forcing["lai"] = 3.5
        issues = check_forcing(df_forcing, fix=False, physics={"laimethod": 0})
        if issues:
            assert not any(
                "laimethod=0" in issue and "lai" in issue for issue in issues
            )

    def test_laimethod_default_skips_requirement(self):
        """Default laimethod=1 does not require the lai column."""
        df_forcing = _base_forcing_df()
        issues = check_forcing(df_forcing, fix=False, physics={"laimethod": 1})
        if issues:
            assert not any(
                "laimethod=0" in issue and "lai" in issue for issue in issues
            )

    def test_handles_dict_with_value_key(self):
        """Validator unwraps YAML-style {'value': 0} physics structure."""
        df_forcing = _base_forcing_df()
        issues = check_forcing(
            df_forcing, fix=False, physics={"laimethod": {"value": 0}}
        )
        assert any(
            "laimethod=0" in issue and "lai" in issue for issue in issues
        )


@pytest.mark.core
class TestLAIMethodRuntime:
    """End-to-end test that the switch actually gates the observed-LAI override.

    Runs the sample-data simulation twice over ~30 days with a seasonally-varying
    forcing `lai` column: once with laimethod=0 (expect daily LAI outputs to track
    the forcing) and once with laimethod=1 (default; expect outputs NOT to track).
    """

    N_DAYS = 30
    LAI_OBS_LOW = 1.0
    LAI_OBS_HIGH = 6.0

    def _seasonal_lai(self, index: pd.DatetimeIndex) -> np.ndarray:
        """Build a seasonal LAI curve spanning LAI_OBS_LOW .. LAI_OBS_HIGH."""
        doy = index.dayofyear.values
        phase = 2 * np.pi * (doy - 1) / 365.0
        amp = (self.LAI_OBS_HIGH - self.LAI_OBS_LOW) / 2
        mid = (self.LAI_OBS_HIGH + self.LAI_OBS_LOW) / 2
        return mid - amp * np.cos(phase)

    def _run_sim(
        self, laimethod_value: int, lai_series: np.ndarray
    ) -> pd.DataFrame:
        """Run a short simulation with the chosen laimethod and supplied lai column.

        Returns the DailyState sub-frame keyed by calendar date for easy alignment.
        """
        sim = SUEWSSimulation.from_sample_data()

        df_forcing = sim.forcing.df.copy()
        end_index = TIMESTEPS_PER_DAY * self.N_DAYS - 1
        df_forcing = df_forcing.iloc[: end_index + 1].copy()
        df_forcing["lai"] = lai_series

        sim._config.model.physics.laimethod = LAIMethod(laimethod_value)
        sim._df_state_init = sim._config.to_df_state()

        sim.update_forcing(df_forcing)
        results = sim.run(end_date=df_forcing.index[-1])

        df_dailystate = results.xs("DailyState", level="group", axis=1)
        # DailyState has a (grid, datetime) MultiIndex; LAI values are written at
        # 23:55 of each day. Drop the grid level and re-key by date to align with
        # the forcing daily mean.
        df_single_grid = df_dailystate.xs(1, level="grid")
        df_lai_daily = df_single_grid[["LAI_EveTr", "LAI_DecTr", "LAI_Grass"]].dropna(
            how="all"
        )
        df_lai_daily.index = df_lai_daily.index.normalize()
        return df_lai_daily

    def test_laimethod_runtime_switch(self):
        """Observed LAI tracks forcing only when laimethod=0."""
        sim = SUEWSSimulation.from_sample_data()
        end_index = TIMESTEPS_PER_DAY * self.N_DAYS - 1
        index = sim.forcing.df.index[: end_index + 1]
        lai_series = self._seasonal_lai(index)

        df_obs = self._run_sim(laimethod_value=0, lai_series=lai_series)
        df_calc = self._run_sim(laimethod_value=1, lai_series=lai_series)

        # Daily mean of forcing, keyed by normalised date.
        ser_forcing_daily = pd.Series(lai_series, index=index).resample("1D").mean()
        ser_forcing_daily.index = ser_forcing_daily.index.normalize()

        for column in ("LAI_EveTr", "LAI_DecTr", "LAI_Grass"):
            aligned = pd.concat(
                [df_obs[column], df_calc[column], ser_forcing_daily],
                axis=1,
                join="inner",
            ).dropna()
            aligned.columns = ["obs", "calc", "forcing"]
            assert len(aligned) >= self.N_DAYS - 2, (
                f"{column}: expected at least {self.N_DAYS - 2} aligned days, got {len(aligned)}"
            )

            rmse_obs = float(
                np.sqrt(((aligned["obs"] - aligned["forcing"]) ** 2).mean())
            )
            rmse_calc = float(
                np.sqrt(((aligned["calc"] - aligned["forcing"]) ** 2).mean())
            )

            assert rmse_obs < 0.5, (
                f"{column}: laimethod=0 should track forcing (RMSE < 0.5), got {rmse_obs:.3f}"
            )
            assert rmse_calc > rmse_obs, (
                f"{column}: laimethod=1 output (RMSE {rmse_calc:.3f}) should diverge "
                f"from forcing more than laimethod=0 ({rmse_obs:.3f})"
            )
