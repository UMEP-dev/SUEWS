"""Regression tests for laimethod (issues #1291, #1296).

The `model.physics.laimethod` option controls whether SUEWS computes LAI
internally via GDD/SDD thresholds (laimethod=1, default) or uses observed
values from the `lai` forcing column (laimethod=0). Issue #1291 reported
that the forcing column was silently ignored because the dailystate routine
hard-coded the local switch to `LAICalcYes = 1`.

Issue #1296 tightened the contract for the observed path: under
`laimethod=0` every row of the `lai` forcing column must be
**non-negative** (`lai >= 0`). Zero observations are valid (and clamped
to `laimin` when the site configuration retains a positive floor);
**negative** values — including the `-999` missing sentinel — are
rejected at pre-flight. Do not reintroduce sentinel-based fallback
tests for the observed path.

These tests guard against regression in:
  * Validator plumbing (FORCING_REQUIREMENTS entry, non-negative check).
  * Fortran/Python wiring (the switch actually routes `forcing%LAI_obs`
    through to `LAI_id_next`).
"""

import numpy as np
import pandas as pd
import pytest
import supy as sp

from supy import SUEWSSimulation
from supy._check import FORCING_REQUIREMENTS, check_forcing
from supy.data_model.core.model import LAIMethod

from conftest import TIMESTEPS_PER_DAY

pytestmark = pytest.mark.physics


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


class TestLAIMethodNegativeRejected:
    """Validator rejects every negative ``lai`` value under ``laimethod=0``.

    Issue #1296 contract: the observed path requires a non-negative
    observation (``lai >= 0``) at every timestep. Zero is valid (and
    clamped to ``laimin`` at runtime when the site configuration retains
    a positive floor). Sentinels (``-999``) and other negative values are
    all rejected at pre-flight so the user sees a clear error before the
    run starts.
    """

    def test_validator_accepts_lai_zero(self):
        """``lai == 0`` across all rows is a valid observation."""
        df_forcing = _base_forcing_df()
        df_forcing["lai"] = 0.0
        issues = check_forcing(
            df_forcing, fix=False, physics={"laimethod": 0}
        )
        if issues:
            assert not any(
                "non-negative" in issue for issue in issues
            ), issues

    def test_validator_rejects_small_negative_lai(self):
        """Small negative values (above the -900 threshold) are rejected."""
        df_forcing = _base_forcing_df()
        df_forcing["lai"] = -5.0
        issues = check_forcing(
            df_forcing, fix=False, physics={"laimethod": 0}
        )
        assert any(
            "laimethod=0" in issue and "non-negative" in issue
            for issue in issues
        ), issues

    def test_validator_rejects_missing_sentinel(self):
        """``lai == -999`` is not a permitted fallback under laimethod=0."""
        df_forcing = _base_forcing_df()  # lai column already set to -999
        issues = check_forcing(
            df_forcing, fix=False, physics={"laimethod": 0}
        )
        # Exactly one non-negative issue is emitted for this column — the
        # generic all-missing check must be suppressed to avoid duplicates.
        matching = [
            issue for issue in issues
            if "laimethod=0" in issue and "non-negative" in issue
        ]
        assert len(matching) == 1, (
            f"Expected one non-negative issue; got {len(matching)}: {issues}"
        )
        assert "missing sentinel" in matching[0], matching[0]

    def test_validator_rejects_partial_missing(self):
        """A mix of valid observations and -999 fill-ins is still rejected."""
        df_forcing = _base_forcing_df()
        df_forcing["lai"] = 3.5
        df_forcing.iloc[3:6, df_forcing.columns.get_loc("lai")] = -999.0
        issues = check_forcing(
            df_forcing, fix=False, physics={"laimethod": 0}
        )
        matching = [
            issue for issue in issues
            if "laimethod=0" in issue and "non-negative" in issue
        ]
        assert matching, issues
        # Invalid-row count in the message should reflect the 3 sentinels.
        assert "3" in matching[0], matching[0]

    def test_validator_rejects_nan_gap(self):
        """A NaN gap still breaks the every-timestep observed-LAI contract."""
        df_forcing = _base_forcing_df()
        df_forcing["lai"] = 3.5
        df_forcing.iloc[3, df_forcing.columns.get_loc("lai")] = np.nan
        issues = check_forcing(
            df_forcing, fix=False, physics={"laimethod": 0}
        )
        matching = [
            issue for issue in issues
            if "laimethod=0" in issue and "missing/NaN" in issue
        ]
        assert matching, issues

    def test_validator_accepts_all_positive_lai(self):
        """Strictly positive ``lai`` values pass the non-negative check."""
        df_forcing = _base_forcing_df()
        df_forcing["lai"] = 3.5
        issues = check_forcing(
            df_forcing, fix=False, physics={"laimethod": 0}
        )
        if issues:
            assert not any(
                "non-negative" in issue for issue in issues
            ), issues


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

        # Widen per-class LAI envelope so the seasonal forcing curve is not
        # clipped at runtime — this test is about the laimethod switch, not
        # about bound enforcement.
        for surf_type in ("evetr", "dectr", "grass"):
            surf = getattr(sim._config.sites[0].properties.land_cover, surf_type)
            surf.lai.laimin = 0.0
            surf.lai.laimax = 10.0

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

    def test_run_rejects_all_missing_lai(self):
        """SUEWSSimulation.run() must surface the `lai`-required validator
        error when `laimethod=0` and the forcing column is all -999."""
        sim = SUEWSSimulation.from_sample_data()

        end_index = TIMESTEPS_PER_DAY * self.N_DAYS - 1
        df_forcing = sim.forcing.df.copy().iloc[: end_index + 1].copy()
        df_forcing["lai"] = -999.0  # all-missing sentinel

        sim._config.model.physics.laimethod = LAIMethod.OBSERVED
        sim._df_state_init = sim._config.to_df_state()
        sim.update_forcing(df_forcing)

        with pytest.raises(ValueError, match="laimethod=0"):
            sim.run(end_date=df_forcing.index[-1])

    @staticmethod
    def _set_lai_bounds(sim, laimin: float, laimax: float) -> None:
        """Override LAImin / LAImax for every vegetation class in the config."""
        for surf_type in ("evetr", "dectr", "grass"):
            surf = getattr(sim._config.sites[0].properties.land_cover, surf_type)
            surf.lai.laimin = laimin
            surf.lai.laimax = laimax

    def test_zero_lai_observation_honoured(self):
        """A genuine zero observation drives LAI to zero when ``laimin = 0``.

        Issue #1296 keeps zero as a valid observation: users who expect
        complete canopy dieback (winter, browning) can set ``laimin`` to
        zero in the site configuration and the observation is honoured.
        """
        sim = SUEWSSimulation.from_sample_data()
        self._set_lai_bounds(sim, laimin=0.0, laimax=10.0)

        end_index = TIMESTEPS_PER_DAY * self.N_DAYS - 1
        df_forcing = sim.forcing.df.copy().iloc[: end_index + 1].copy()
        df_forcing["lai"] = 0.0

        sim._config.model.physics.laimethod = LAIMethod.OBSERVED
        sim._df_state_init = sim._config.to_df_state()
        sim.update_forcing(df_forcing)
        results = sim.run(end_date=df_forcing.index[-1])

        df_dailystate = results.xs("DailyState", level="group", axis=1)
        df_lai = df_dailystate.xs(1, level="grid")[[
            "LAI_EveTr",
            "LAI_DecTr",
            "LAI_Grass",
        ]].dropna(how="all")
        # Skip the first day (initial state inherited from the config).
        df_lai_tail = df_lai.iloc[1:]
        np.testing.assert_allclose(
            df_lai_tail.values, 0.0, atol=1e-9,
            err_msg="lai=0.0 observation should drive LAI outputs to zero",
        )

    def test_zero_lai_observation_clamped_to_laimin(self):
        """Zero observations are clamped up to ``laimin`` when the site
        configuration retains a positive floor. This is the default
        behaviour of the sample config (``laimin`` > 0 for all classes).
        """
        sim = SUEWSSimulation.from_sample_data()

        df_state = sim._config.to_df_state()
        laimin_series = df_state["laimin"].iloc[0]
        laimin_values = [float(laimin_series[f"({iv},)"]) for iv in range(3)]
        # Pre-condition: sample site has laimin > 0 for every class.
        assert all(v > 0 for v in laimin_values)

        end_index = TIMESTEPS_PER_DAY * self.N_DAYS - 1
        df_forcing = sim.forcing.df.copy().iloc[: end_index + 1].copy()
        df_forcing["lai"] = 0.0

        sim._config.model.physics.laimethod = LAIMethod.OBSERVED
        sim._df_state_init = sim._config.to_df_state()
        sim.update_forcing(df_forcing)
        results = sim.run(end_date=df_forcing.index[-1])

        df_dailystate = results.xs("DailyState", level="group", axis=1)
        df_lai = df_dailystate.xs(1, level="grid")[[
            "LAI_EveTr",
            "LAI_DecTr",
            "LAI_Grass",
        ]].dropna(how="all")
        df_lai_tail = df_lai.iloc[1:]
        for column, laimin in zip(
            ["LAI_EveTr", "LAI_DecTr", "LAI_Grass"], laimin_values
        ):
            np.testing.assert_allclose(
                df_lai_tail[column].values,
                laimin,
                atol=1e-9,
                err_msg=f"{column} should be clamped up to laimin={laimin}",
            )

    def test_observed_lai_clamped_to_envelope(self):
        """Observations outside each vegetation class's [LAImin, LAImax]
        envelope must be clamped per-class. Upper-bound clamping keeps the
        downstream LAI/LAImax ratios bounded; lower-bound clamping keeps the
        observation consistent with the site-specific canopy floor."""
        sim = SUEWSSimulation.from_sample_data()

        # Extract per-class LAImax from the df_state_init (columns like
        # ``(laimax, (iv,))`` where iv indexes EveTr/DecTr/Grass).
        df_state = sim._config.to_df_state()
        laimax_series = df_state["laimax"].iloc[0]
        laimax_values = [float(laimax_series[f"({iv},)"]) for iv in range(3)]
        max_laimax = max(laimax_values)

        end_index = TIMESTEPS_PER_DAY * self.N_DAYS - 1
        df_forcing = sim.forcing.df.copy().iloc[: end_index + 1].copy()
        # Force a constant observation well above every class's LAImax.
        df_forcing["lai"] = max_laimax + 1.0

        sim._config.model.physics.laimethod = LAIMethod.OBSERVED
        sim._df_state_init = sim._config.to_df_state()
        sim.update_forcing(df_forcing)
        results = sim.run(end_date=df_forcing.index[-1])

        df_dailystate = results.xs("DailyState", level="group", axis=1)
        df_lai = df_dailystate.xs(1, level="grid")[[
            "LAI_EveTr",
            "LAI_DecTr",
            "LAI_Grass",
        ]].dropna(how="all")

        # Skip the first day (initial state inherited from the config).
        df_lai_tail = df_lai.iloc[1:]
        for column, laimax in zip(
            ["LAI_EveTr", "LAI_DecTr", "LAI_Grass"], laimax_values
        ):
            assert (df_lai_tail[column] <= laimax + 1e-9).all(), (
                f"{column} exceeded LAImax={laimax}: max observed "
                f"{df_lai_tail[column].max()}"
            )

    def test_negative_sentinel_rejected(self):
        """Non-canonical sentinels (e.g. ``-950``) are still rejected.

        Issue #1296 contract: under ``laimethod=0`` every ``lai`` row
        must be non-negative (``>= 0``). Any strictly negative value —
        the canonical ``-999`` sentinel and defensive variants such as
        ``-950`` alike — is refused, because the observed path does not
        permit falling back to the calculated branch on a per-timestep
        basis.
        """
        sim = SUEWSSimulation.from_sample_data()

        end_index = TIMESTEPS_PER_DAY * self.N_DAYS - 1
        df_forcing = sim.forcing.df.copy().iloc[: end_index + 1].copy()
        df_forcing["lai"] = -950.0

        sim._config.model.physics.laimethod = LAIMethod.OBSERVED
        sim._df_state_init = sim._config.to_df_state()
        sim.update_forcing(df_forcing)

        with pytest.raises(ValueError, match="laimethod=0"):
            sim.run(end_date=df_forcing.index[-1])

    def test_nan_lai_gap_rejected(self):
        """Missing ``lai`` values are rejected before the run starts."""
        sim = SUEWSSimulation.from_sample_data()

        end_index = TIMESTEPS_PER_DAY * self.N_DAYS - 1
        df_forcing = sim.forcing.df.copy().iloc[: end_index + 1].copy()
        df_forcing["lai"] = 3.5
        df_forcing.iloc[0, df_forcing.columns.get_loc("lai")] = np.nan

        sim._config.model.physics.laimethod = LAIMethod.OBSERVED
        sim._df_state_init = sim._config.to_df_state()
        sim.update_forcing(df_forcing)

        with pytest.raises(ValueError, match="laimethod=0"):
            sim.run(end_date=df_forcing.index[-1])

    def test_legacy_run_supy_unchecked_rejects_nan_gap(self):
        """Legacy ``run_supy(..., check_input=False)`` still rejects NaN ``lai``."""
        with pytest.deprecated_call():
            df_state_init, df_forcing = sp.load_sample_data()

        end_index = TIMESTEPS_PER_DAY * 2 - 1
        df_forcing = df_forcing.iloc[: end_index + 1].copy()
        df_forcing["lai"] = 3.5
        df_forcing.iloc[0, df_forcing.columns.get_loc("lai")] = np.nan
        df_state_init.loc[:, ("laimethod", "0")] = 0

        with pytest.raises(Exception, match="laimethod=0"):
            sp.run_supy(df_forcing, df_state_init, check_input=False)


class TestLAIBoundsPreflightWarning:
    """Validator warns when forcing LAI will be clamped to [LAImin, LAImax]."""

    @pytest.fixture
    def supy_caplog(self, caplog):
        """Capture SuPy logger records. The SuPy logger has ``propagate=False``
        by default, which hides messages from pytest's ``caplog`` — this
        fixture temporarily re-enables propagation for the duration of a test.
        """
        import logging

        supy_logger = logging.getLogger("SuPy")
        original = supy_logger.propagate
        supy_logger.propagate = True
        try:
            with caplog.at_level(logging.WARNING, logger="SuPy"):
                yield caplog
        finally:
            supy_logger.propagate = original

    def test_warn_when_forcing_below_laimin(self, supy_caplog):
        """Forcing values below the highest class LAImin trigger a warning."""
        df_forcing = _base_forcing_df()
        df_forcing["lai"] = 0.5  # below laimin for every class
        lai_bounds = {
            "laimin": [[1.0, 1.0, 1.0]],
            "laimax": [[5.0, 5.0, 5.0]],
        }
        check_forcing(
            df_forcing,
            fix=False,
            physics={"laimethod": 0},
            lai_bounds=lai_bounds,
        )
        messages = [rec.message for rec in supy_caplog.records]
        assert any("clamped at runtime" in msg for msg in messages), messages
        assert any("below" in msg.lower() for msg in messages), messages

    def test_warn_when_forcing_above_laimax(self, supy_caplog):
        """Forcing values above the lowest class LAImax trigger a warning."""
        df_forcing = _base_forcing_df()
        df_forcing["lai"] = 7.0
        lai_bounds = {
            "laimin": [[0.0, 0.0, 0.0]],
            "laimax": [[5.0, 6.0, 6.5]],
        }
        check_forcing(
            df_forcing,
            fix=False,
            physics={"laimethod": 0},
            lai_bounds=lai_bounds,
        )
        messages = [rec.message for rec in supy_caplog.records]
        assert any("clamped at runtime" in msg for msg in messages), messages
        assert any("above" in msg.lower() for msg in messages), messages

    def test_silent_when_forcing_within_bounds(self, supy_caplog):
        """No warning emitted when all forcing values fit inside the envelope."""
        df_forcing = _base_forcing_df()
        df_forcing["lai"] = 3.0
        lai_bounds = {
            "laimin": [[1.0, 1.0, 1.0]],
            "laimax": [[5.0, 5.0, 5.0]],
        }
        check_forcing(
            df_forcing,
            fix=False,
            physics={"laimethod": 0},
            lai_bounds=lai_bounds,
        )
        messages = [rec.message for rec in supy_caplog.records]
        assert not any("clamped at runtime" in msg for msg in messages)


class TestLAIBoundsYAMLExtraction:
    """Phase A validation pulls LAI bounds from the YAML and forwards them."""

    def test_extract_lai_bounds_from_sample_config(self):
        """The sample YAML is parsed into a bounds dict matching df_state."""
        from pathlib import Path

        import yaml
        from supy.data_model.validation.pipeline.phase_a import (
            _extract_lai_bounds_from_user_data,
        )

        sample_yaml = (
            Path(__file__).parent.parent / "fixtures" / "benchmark1" / "benchmark1.yml"
        )
        with open(sample_yaml) as f:
            user_data = yaml.safe_load(f)

        bounds = _extract_lai_bounds_from_user_data(user_data)
        assert bounds is not None
        assert bounds["laimin"] == [[4.0, 1.0, 1.6]]
        assert bounds["laimax"] == [[5.1, 5.5, 5.9]]

    def test_extract_returns_none_when_sites_missing(self):
        """Invalid YAML structure yields None so the caller skips the warning."""
        from supy.data_model.validation.pipeline.phase_a import (
            _extract_lai_bounds_from_user_data,
        )

        assert _extract_lai_bounds_from_user_data({}) is None
        assert _extract_lai_bounds_from_user_data({"sites": []}) is None
        assert _extract_lai_bounds_from_user_data(None) is None


class TestLAIMethodMultiGrid:
    """Multi-grid legacy-path validation (see ``_physics_dict_from_df_state``)."""

    def test_multi_grid_laimethod_triggers_requirement(self):
        """If any grid selects ``laimethod=0``, the forcing requirement must
        apply — even when grid 1 uses the default (``laimethod=1``)."""
        from supy._supy_module import _physics_dict_from_df_state

        sim = SUEWSSimulation.from_sample_data()
        df_state = sim._config.to_df_state()

        # Duplicate the single-grid state to simulate a two-grid run with
        # grid 1 using the default and grid 2 selecting observed LAI.
        df_state_multi = pd.concat([df_state, df_state.copy()], ignore_index=True)
        df_state_multi.loc[0, "laimethod"] = 1
        df_state_multi.loc[1, "laimethod"] = 0

        physics_dict = _physics_dict_from_df_state(df_state_multi)
        # Multi-grid: value should be a list spanning both grids' choices.
        assert physics_dict.get("laimethod") == [0, 1]

        df_forcing = _base_forcing_df()
        issues = check_forcing(df_forcing, fix=False, physics=physics_dict)
        assert any(
            "laimethod=0" in issue and "lai" in issue for issue in issues
        ), (
            "Expected validator to require `lai` column when any grid sets "
            f"laimethod=0; got issues: {issues}"
        )
