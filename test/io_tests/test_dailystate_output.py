"""Test DailyState output and LAI phenology functionality."""

import math
import tempfile

from conftest import TIMESTEPS_PER_DAY
import numpy as np
import pandas as pd
import pytest

import supy as sp

from supy._supy_module import _save_supy
from supy.data_model.core.model import OutputControl
from supy.data_model.core.type import RefValue

pytestmark = [
    pytest.mark.physics,
    pytest.mark.contract(
        "C-CLOCK", "C-RESPONSE", "C-OUTPUT", "C-SAVE", "C-RESAMPLE", "C-NATIVE-ERROR"
    ),
]


def _unwrap(val) -> float:
    """Return numeric float from either a RefValue or bare float."""
    return float(val.value if isinstance(val, RefValue) else val)


def _set_ref(obj, attr: str, value: float) -> None:
    """Set a FlexibleRefValue attribute whether it holds a RefValue or bare float."""
    cur = getattr(obj, attr)
    if isinstance(cur, RefValue):
        cur.value = value
    else:
        setattr(obj, attr, RefValue(value))


def _reset_days_for_lat(latitude: float) -> tuple[int, int]:
    """Return (summer_reset_day, winter_reset_day) matching Fortran get_reset_days."""
    if latitude >= 0.0:
        return 196, 15
    return 15, 196


def _season_masks(doy: pd.Index, latitude: float) -> tuple[np.ndarray, np.ndarray]:
    """Return (growth_mask, senescence_mask) matching Fortran update_growth_state."""
    summer_reset_day, winter_reset_day = _reset_days_for_lat(latitude)
    if latitude >= 0.0:
        senescence = (doy >= summer_reset_day) | (doy < winter_reset_day)
    else:
        senescence = (doy >= summer_reset_day) & (doy < winter_reset_day)
    return ~senescence, senescence


def _make_constant_temp_window(
    df_forcing_base: pd.DataFrame,
    start: str,
    n_days: int,
    temp_c: float,
) -> pd.DataFrame:
    """Create a multi-day 5-minute forcing slice with constant air temperature."""
    n_steps = TIMESTEPS_PER_DAY * n_days
    template = df_forcing_base.iloc[:n_steps].copy()
    idx = pd.date_range(start, periods=n_steps, freq="5min")
    template.index = idx
    template["iy"] = idx.year
    template["id"] = idx.day_of_year
    template["it"] = idx.hour
    template["imin"] = idx.minute
    template["Tair"] = temp_c
    return template


class TestDailyStateOutput:
    """Test suite for DailyState output handling and LAI phenology."""

    def test_dailystate_no_resampling(self, sample_data_loaded, sample_run_cached):
        """Test that DailyState output is not resampled and preserves daily values."""
        n_steps = TIMESTEPS_PER_DAY * 3  # 3 days of 5-min data
        _, df_forcing = sample_data_loaded
        df_forcing_multi_day = df_forcing.iloc[:n_steps]

        df_output, _ = sample_run_cached(n_steps)

        assert "DailyState" in df_output.columns.get_level_values("group").unique()
        df_dailystate = df_output.loc[:, "DailyState"]
        df_dailystate_clean = df_dailystate.dropna(how="all")

        n_days = len(
            pd.date_range(
                df_forcing_multi_day.index[0], df_forcing_multi_day.index[-1], freq="D"
            )
        )
        assert len(df_dailystate_clean) <= n_days
        assert len(df_dailystate_clean) > 0

        hours = df_dailystate_clean.index.get_level_values("datetime").hour
        assert all(h >= 23 for h in hours), (
            "DailyState should only have values at end of day"
        )

    def test_dailystate_save_output(self, sample_run_cached):
        """Test that DailyState data is correctly saved to file."""
        df_output, df_state_final = sample_run_cached(TIMESTEPS_PER_DAY * 3)

        with tempfile.TemporaryDirectory() as dir_temp:
            list_files = _save_supy(df_output, df_state_final, path_dir_save=dir_temp)

            dailystate_files = [f for f in list_files if "DailyState" in f.name]
            assert len(dailystate_files) > 0, "DailyState file should be created"

            for ds_file in dailystate_files:
                df_saved = pd.read_csv(ds_file, sep="\t")
                assert len(df_saved) > 0, "DailyState file should not be empty"

                data_cols = [
                    c
                    for c in df_saved.columns
                    if c not in ["Year", "DOY", "Hour", "Min", "Dectime"]
                ]
                assert len(data_cols) > 0, "Should have data columns"
                assert any(any(df_saved[col] != -999) for col in data_cols), (
                    "DailyState should contain actual data values"
                )

                doy_values = df_saved["DOY"].values
                expected_doys = list(range(2, len(doy_values) + 2))
                assert list(doy_values) == expected_doys, (
                    f"Missing or mislabelled days in output. Got DOYs: {list(doy_values)}, expected: {expected_doys}"
                )
                assert (df_saved["Hour"] == 0).all()
                assert (df_saved["Min"] == 0).all()

    def test_dailystate_different_output_frequencies(self, sample_run_cached):
        """Test DailyState output with different resampling frequencies."""
        df_output, df_state_final = sample_run_cached(TIMESTEPS_PER_DAY * 2)

        for freq_s in [300, 1800, 3600]:
            with tempfile.TemporaryDirectory() as dir_temp:
                list_files = _save_supy(
                    df_output, df_state_final, path_dir_save=dir_temp, freq_s=freq_s
                )
                dailystate_files = [f for f in list_files if "DailyState" in f.name]
                assert len(dailystate_files) > 0
                df_ds = pd.read_csv(dailystate_files[0], sep="\t")
                assert len(df_ds) > 0, f"DailyState should have data at freq={freq_s}"

    def test_dailystate_only_output_config_saves_file(self, sample_run_cached):
        """Test that requesting only DailyState writes a populated output file."""
        df_output, df_state_final = sample_run_cached(TIMESTEPS_PER_DAY)

        with tempfile.TemporaryDirectory() as dir_temp:
            list_files = _save_supy(
                df_output,
                df_state_final,
                path_dir_save=dir_temp,
                output_config=OutputControl(groups=["DailyState"]),
            )

            assert [f.name for f in list_files if "SUEWS" in f.name] == []
            dailystate_files = [f for f in list_files if "DailyState" in f.name]
            assert len(dailystate_files) == 1

            df_saved = pd.read_csv(dailystate_files[0], sep="\t")
            data_cols = [
                c
                for c in df_saved.columns
                if c not in ["Year", "DOY", "Hour", "Min", "Dectime"]
            ]

            assert len(df_saved) == 1
            assert (df_saved[data_cols] != -999).any().any()

    def test_dailystate_lai_responds_to_gdd_sdd(
        self, sample_dailystate_full_year, sample_config_loaded
    ):
        """LAI grows once GDD >= gdd_base and declines once SDD <= sdd_base."""
        config_lai_dectr = sample_config_loaded.sites[0].properties.land_cover.dectr.lai
        gdd_base = _unwrap(config_lai_dectr.gdd_base)
        sdd_base = _unwrap(config_lai_dectr.sdd_base)

        df_dailystate = sample_dailystate_full_year
        lai = df_dailystate["LAI_DecTr"]
        gdd = df_dailystate["GDD_DecTr"]
        sdd = df_dailystate["SDD_DecTr"]
        lai_change = lai.diff()

        # Unconditional assertions (never vacuous `if` guards)
        assert (gdd >= gdd_base).any(), (
            "Full-year simulation must reach gdd_base during the growth season"
        )
        assert (sdd <= sdd_base).any(), (
            "Full-year simulation must reach sdd_base during the senescence season"
        )
        assert ((gdd >= gdd_base) & (lai_change > 0)).any(), (
            "No LAI growth found despite accumulated GDD passing gdd_base"
        )
        assert ((sdd <= sdd_base) & (lai_change < 0)).any(), (
            "No LAI decline found despite accumulated SDD passing sdd_base"
        )

    def test_lai_remains_bounded_and_physical(
        self, sample_dailystate_full_year, sample_config_loaded
    ):
        """LAI for every vegetated surface stays finite, non-negative, and within bounds."""
        lc = sample_config_loaded.sites[0].properties.land_cover
        df_dailystate = sample_dailystate_full_year

        for col, surf_cfg in (
            ("LAI_EveTr", lc.evetr.lai),
            ("LAI_DecTr", lc.dectr.lai),
            ("LAI_Grass", lc.grass.lai),
        ):
            lai = df_dailystate[col]
            lai_min = float(surf_cfg.lai_min.value)
            lai_max = float(surf_cfg.lai_max.value)
            assert np.isfinite(lai).all(), f"{col} contains non-finite values"
            assert (lai >= 0.0).all(), f"{col} fell below 0"
            assert (lai >= lai_min - 1e-12).all(), f"{col} fell below lai_min={lai_min}"
            assert (lai <= lai_max + 1e-12).all(), f"{col} exceeded lai_max={lai_max}"

    def test_lai_only_grows_during_growth_season_and_declines_during_senescence(
        self, sample_dailystate_full_year, sample_config_loaded
    ):
        """LAI does not decline in growth season and does not grow in senescence season."""
        latitude = float(sample_config_loaded.sites[0].properties.lat.value)
        df_dailystate = sample_dailystate_full_year
        doy = df_dailystate.index.get_level_values("datetime").day_of_year
        growth_mask, senescence_mask = _season_masks(doy, latitude)

        assert growth_mask.any(), "Expected non-empty growth season mask"
        assert senescence_mask.any(), "Expected non-empty senescence season mask"

        lai = df_dailystate["LAI_DecTr"]
        delta_lai = lai.diff().iloc[1:]
        growth_after = growth_mask[1:]
        senescence_after = senescence_mask[1:]

        assert (delta_lai[growth_after] >= 0.0).all(), (
            "LAI decreased during the growth season"
        )
        assert (delta_lai[senescence_after] <= 0.0).all(), (
            "LAI increased during the senescence season"
        )

    def test_gdd_sdd_accumulates_in_correct_season_and_resets_before_accumulation(
        self, sample_dailystate_full_year, sample_config_loaded
    ):
        """GDD/SDD reset before accumulation on reset days and accumulate only in season."""
        latitude = float(sample_config_loaded.sites[0].properties.lat.value)
        summer_reset_day, winter_reset_day = _reset_days_for_lat(latitude)
        lai_cfg = sample_config_loaded.sites[0].properties.land_cover.dectr.lai
        base_t_gdd = float(lai_cfg.base_temperature.value)
        base_t_sdd = float(lai_cfg.base_temperature_senescence.value)

        df_dailystate = sample_dailystate_full_year
        doy = df_dailystate.index.get_level_values("datetime").day_of_year
        growth_mask, senescence_mask = _season_masks(doy, latitude)

        gdd = df_dailystate["GDD_DecTr"]
        sdd = df_dailystate["SDD_DecTr"]
        tmean = 0.5 * (df_dailystate["Tmin"] + df_dailystate["Tmax"])

        summer_reset_mask = doy == summer_reset_day
        winter_reset_mask = doy == winter_reset_day
        assert summer_reset_mask.any(), "Simulation must contain summer_reset_day"
        assert winter_reset_mask.any(), "Simulation must contain winter_reset_day"

        # On winter_reset_day (start of growth season): reset happens BEFORE
        # accumulation, so SDD == 0 and GDD equals today's non-negative increment.
        expected_winter_gdd = (tmean[winter_reset_mask] - base_t_gdd).clip(lower=0.0)
        assert (sdd[winter_reset_mask] == 0.0).all()
        assert np.allclose(gdd[winter_reset_mask], expected_winter_gdd, atol=1e-6)

        # On summer_reset_day (start of senescence season): reset happens BEFORE
        # accumulation, so GDD == 0 and SDD equals today's non-positive increment.
        expected_summer_sdd = (tmean[summer_reset_mask] - base_t_sdd).clip(upper=0.0)
        assert (gdd[summer_reset_mask] == 0.0).all()
        assert np.allclose(sdd[summer_reset_mask], expected_summer_sdd, atol=1e-6)

        # Off-season accumulators stay at zero throughout the opposing season
        # (starting from the reset day that opens each season).
        assert (sdd[growth_mask] == 0.0).all(), "SDD must remain 0 in growth season"
        # After summer_reset_day, GDD must remain 0 through year-end senescence.
        post_summer_senescence = senescence_mask & (doy >= summer_reset_day)
        assert post_summer_senescence.any()
        assert (gdd[post_summer_senescence] == 0.0).all(), (
            "GDD must remain 0 in post-summer senescence season"
        )

        # Within each season (excluding the opening reset day), active accumulator is monotonic.
        delta_gdd = gdd.diff()
        delta_sdd = sdd.diff()
        non_reset_growth = growth_mask & (~winter_reset_mask)
        non_reset_senescence = senescence_mask & (~summer_reset_mask)
        assert (delta_gdd[non_reset_growth].dropna() >= 0.0).all(), (
            "GDD decreased on a non-reset growth day"
        )
        assert (delta_sdd[non_reset_senescence].dropna() <= 0.0).all(), (
            "SDD increased on a non-reset senescence day"
        )

    @pytest.mark.parametrize("latitude", [51.5, 0.0, -33.9])
    def test_hemisphere_and_equator_reset_day_boundaries(
        self, sample_data_loaded, sample_yaml_path, latitude
    ):
        """NH, equator (NH dates), and SH swap reset days and reset before accumulating."""
        _, df_forcing_base = sample_data_loaded
        summer_reset_day, winter_reset_day = _reset_days_for_lat(latitude)

        # Test both DOY 14-16 (Jan 14-16) and DOY 195-197 (Jul 13-15 in leap year 2012)
        for start_date, temp_c in (("2012-01-14", 15.0), ("2012-07-13", 15.0)):
            sim = sp.SUEWSSimulation(str(sample_yaml_path))
            site = sim._config.sites[0]
            site.properties.lat.value = latitude
            dectr_lai = site.properties.land_cover.dectr.lai
            _set_ref(dectr_lai, "base_temperature", 5.0)
            _set_ref(dectr_lai, "base_temperature_senescence", 25.0)
            _set_ref(dectr_lai, "gdd_base", 0.0)
            _set_ref(dectr_lai, "sdd_base", 0.0)
            _set_ref(dectr_lai, "lai_min", 0.5)
            _set_ref(dectr_lai, "lai_max", 5.0)
            _set_ref(dectr_lai.lai_power, "growth_lai", 0.0)
            _set_ref(dectr_lai.lai_power, "growth_gdd", 0.02)
            _set_ref(dectr_lai.lai_power, "senescence_lai", 0.0)
            _set_ref(dectr_lai.lai_power, "senescence_sdd", 0.02)
            _set_ref(site.initial_states.dectr, "lai_id", 2.5)
            _set_ref(site.initial_states.dectr, "gdd_id", 50.0)
            _set_ref(site.initial_states.dectr, "sdd_id", -50.0)

            df_forcing = _make_constant_temp_window(
                df_forcing_base, start_date, n_days=3, temp_c=temp_c
            )
            sim.update_forcing(df_forcing)
            sim.run(start_date=df_forcing.index[0], end_date=df_forcing.index[-1])

            ds = sim.output.loc[:, "DailyState"].dropna(how="all")
            doys = list(ds.index.get_level_values("datetime").day_of_year)
            assert len(doys) == 3

            # Middle day is the boundary day (DOY 15 or DOY 196)
            boundary_doy = doys[1]
            gdd_b = float(ds["GDD_DecTr"].iloc[1])
            sdd_b = float(ds["SDD_DecTr"].iloc[1])
            tmean_b = 0.5 * (float(ds["Tmin"].iloc[1]) + float(ds["Tmax"].iloc[1]))
            lai_prev = float(ds["LAI_DecTr"].iloc[0])
            lai_b = float(ds["LAI_DecTr"].iloc[1])

            if boundary_doy == winter_reset_day:
                # Reset to 0 first, then accumulate delta_gdd = tmean - base_t_gdd
                expected_gdd = max(0.0, tmean_b - 5.0)
                assert expected_gdd > 0.0
                assert sdd_b == pytest.approx(0.0)
                assert gdd_b == pytest.approx(expected_gdd, abs=1e-6)
                assert lai_b > lai_prev
            elif boundary_doy == summer_reset_day:
                # Reset to 0 first, then accumulate delta_sdd = tmean - base_t_sdd
                expected_sdd = min(0.0, tmean_b - 25.0)
                assert expected_sdd < 0.0
                assert gdd_b == pytest.approx(0.0)
                assert sdd_b == pytest.approx(expected_sdd, abs=1e-6)
                assert lai_b < lai_prev
            else:
                pytest.fail(f"Unexpected boundary DOY {boundary_doy} for lat={latitude}")

    def test_reset_day_preserves_increment_with_nonzero_gdd_and_sdd_base(
        self, sample_data_loaded, sample_yaml_path
    ):
        """Reset-before-accumulate ensures day 15 / 196 increment counts toward gdd_base/sdd_base."""
        _, df_forcing_base = sample_data_loaded

        # Day 15 (winter_reset_day in NH): T=15 C, base_t_gdd=5 C -> delta_gdd=10 >= gdd_base=5
        sim_g = sp.SUEWSSimulation(str(sample_yaml_path))
        site = sim_g._config.sites[0]
        site.properties.lat.value = 51.5
        lai_cfg = site.properties.land_cover.dectr.lai
        _set_ref(lai_cfg, "base_temperature", 5.0)
        _set_ref(lai_cfg, "base_temperature_senescence", 25.0)
        _set_ref(lai_cfg, "gdd_base", 5.0)
        _set_ref(lai_cfg, "sdd_base", -5.0)
        _set_ref(lai_cfg, "lai_min", 0.5)
        _set_ref(lai_cfg, "lai_max", 5.0)
        _set_ref(lai_cfg.lai_power, "growth_lai", 0.0)
        _set_ref(lai_cfg.lai_power, "growth_gdd", 0.05)
        _set_ref(lai_cfg.lai_power, "senescence_lai", 0.0)
        _set_ref(lai_cfg.lai_power, "senescence_sdd", 0.05)
        _set_ref(site.initial_states.dectr, "lai_id", 2.0)
        _set_ref(site.initial_states.dectr, "gdd_id", 99.0)
        _set_ref(site.initial_states.dectr, "sdd_id", -99.0)

        df_jan = _make_constant_temp_window(
            df_forcing_base, "2012-01-14", n_days=3, temp_c=15.0
        )
        sim_g.update_forcing(df_jan)
        sim_g.run(start_date=df_jan.index[0], end_date=df_jan.index[-1])
        ds_jan = sim_g.output.loc[:, "DailyState"].dropna(how="all")
        tmean_jan_15 = 0.5 * (float(ds_jan["Tmin"].iloc[1]) + float(ds_jan["Tmax"].iloc[1]))
        expected_gdd_15 = tmean_jan_15 - 5.0
        assert expected_gdd_15 >= 5.0
        assert float(ds_jan["GDD_DecTr"].iloc[1]) == pytest.approx(expected_gdd_15, abs=1e-6)
        assert float(ds_jan["SDD_DecTr"].iloc[1]) == pytest.approx(0.0, abs=1e-6)
        # Day 15 grew by delta_gdd * 0.05 from day 14's LAI
        assert float(ds_jan["LAI_DecTr"].iloc[1]) == pytest.approx(
            float(ds_jan["LAI_DecTr"].iloc[0]) + expected_gdd_15 * 0.05, abs=1e-6
        )

        # Day 196 (summer_reset_day in NH, 2012-07-14 in leap year): delta_sdd <= sdd_base=-5
        sim_s = sp.SUEWSSimulation(str(sample_yaml_path))
        site = sim_s._config.sites[0]
        site.properties.lat.value = 51.5
        lai_cfg = site.properties.land_cover.dectr.lai
        _set_ref(lai_cfg, "base_temperature", 5.0)
        _set_ref(lai_cfg, "base_temperature_senescence", 25.0)
        _set_ref(lai_cfg, "gdd_base", 5.0)
        _set_ref(lai_cfg, "sdd_base", -5.0)
        _set_ref(lai_cfg, "lai_min", 0.5)
        _set_ref(lai_cfg, "lai_max", 5.0)
        _set_ref(lai_cfg.lai_power, "growth_lai", 0.0)
        _set_ref(lai_cfg.lai_power, "growth_gdd", 0.05)
        _set_ref(lai_cfg.lai_power, "senescence_lai", 0.0)
        _set_ref(lai_cfg.lai_power, "senescence_sdd", 0.05)
        _set_ref(site.initial_states.dectr, "lai_id", 3.0)
        _set_ref(site.initial_states.dectr, "gdd_id", 99.0)
        _set_ref(site.initial_states.dectr, "sdd_id", -99.0)

        df_jul = _make_constant_temp_window(
            df_forcing_base, "2012-07-13", n_days=3, temp_c=15.0
        )
        sim_s.update_forcing(df_jul)
        sim_s.run(start_date=df_jul.index[0], end_date=df_jul.index[-1])
        ds_jul = sim_s.output.loc[:, "DailyState"].dropna(how="all")
        tmean_jul_196 = 0.5 * (float(ds_jul["Tmin"].iloc[1]) + float(ds_jul["Tmax"].iloc[1]))
        expected_sdd_196 = tmean_jul_196 - 25.0
        assert expected_sdd_196 <= -5.0
        assert float(ds_jul["GDD_DecTr"].iloc[1]) == pytest.approx(0.0, abs=1e-6)
        assert float(ds_jul["SDD_DecTr"].iloc[1]) == pytest.approx(expected_sdd_196, abs=1e-6)
        assert float(ds_jul["LAI_DecTr"].iloc[1]) == pytest.approx(
            float(ds_jul["LAI_DecTr"].iloc[0]) + expected_sdd_196 * 0.05, abs=1e-6
        )

    @pytest.mark.parametrize("latitude", [51.5, -33.9])
    def test_leap_year_doy_365_366_wraparound(
        self, sample_data_loaded, sample_yaml_path, latitude
    ):
        """Leap-year DOY 365 -> 366 -> DOY 1 accumulates without spurious reset."""
        _, df_forcing_base = sample_data_loaded
        sim = sp.SUEWSSimulation(str(sample_yaml_path))
        site = sim._config.sites[0]
        site.properties.lat.value = latitude
        lai_cfg = site.properties.land_cover.dectr.lai
        _set_ref(lai_cfg, "base_temperature", 5.0)
        _set_ref(lai_cfg, "base_temperature_senescence", 25.0)
        _set_ref(lai_cfg, "gdd_base", 0.0)
        _set_ref(lai_cfg, "sdd_base", 0.0)
        _set_ref(site.initial_states.dectr, "gdd_id", 0.0)
        _set_ref(site.initial_states.dectr, "sdd_id", 0.0)

        df_wrap = _make_constant_temp_window(
            df_forcing_base, "2012-12-30", n_days=4, temp_c=15.0
        )
        sim.update_forcing(df_wrap)
        sim.run(start_date=df_wrap.index[0], end_date=df_wrap.index[-1])

        ds = sim.output.loc[:, "DailyState"].dropna(how="all")
        doys = list(ds.index.get_level_values("datetime").day_of_year)
        assert doys == [365, 366, 1, 2]
        assert np.isfinite(ds["LAI_DecTr"]).all()
        tmean = 0.5 * (ds["Tmin"] + ds["Tmax"])
        if latitude >= 0.0:
            # NH Dec-Jan is senescence: SDD accumulates continuously across DOY 366 -> 1
            expected_sdd = np.cumsum((tmean - 25.0).clip(upper=0.0).to_numpy())
            assert (expected_sdd < 0.0).all()
            assert np.allclose(ds["SDD_DecTr"].to_numpy(), expected_sdd, atol=1e-6)
            assert (ds["GDD_DecTr"] == 0.0).all()
        else:
            # SH Dec-Jan is growth: GDD accumulates continuously across DOY 366 -> 1
            expected_gdd = np.cumsum((tmean - 5.0).clip(lower=0.0).to_numpy())
            assert (expected_gdd > 0.0).all()
            assert np.allclose(ds["GDD_DecTr"].to_numpy(), expected_gdd, atol=1e-6)
            assert (ds["SDD_DecTr"] == 0.0).all()

    def test_zero_lai_cold_day_short_circuits_without_nan(
        self, sample_data_loaded, sample_yaml_path
    ):
        """lai_id_prev == 0 with negative exponent and delta_gdd == 0 short-circuits cleanly."""
        _, df_forcing_base = sample_data_loaded
        sim = sp.SUEWSSimulation(str(sample_yaml_path))
        site = sim._config.sites[0]
        site.properties.lat.value = 51.5
        lai_cfg = site.properties.land_cover.dectr.lai
        _set_ref(lai_cfg, "base_temperature", 10.0)
        _set_ref(lai_cfg, "gdd_base", 0.0)
        _set_ref(lai_cfg, "lai_min", 0.0)
        _set_ref(lai_cfg, "lai_max", 5.0)
        _set_ref(lai_cfg.lai_power, "growth_lai", -0.02)
        _set_ref(lai_cfg.lai_power, "growth_gdd", 0.04)
        _set_ref(site.initial_states.dectr, "lai_id", 0.0)
        _set_ref(site.initial_states.dectr, "gdd_id", 0.0)

        # Cold spring day (T = 2 C < base_t = 10 C -> delta_gdd == 0)
        df_cold = _make_constant_temp_window(
            df_forcing_base, "2012-03-01", n_days=2, temp_c=2.0
        )
        sim.update_forcing(df_cold)
        sim.run(start_date=df_cold.index[0], end_date=df_cold.index[-1])

        ds = sim.output.loc[:, "DailyState"].dropna(how="all")
        assert np.isfinite(ds["LAI_DecTr"]).all()
        assert (ds["LAI_DecTr"] == 0.0).all()

    @pytest.mark.parametrize(
        ("start_date", "temp_c", "branch"),
        [
            ("2012-03-01", 20.0, "growth"),
            ("2012-10-01", 5.0, "senescence"),
        ],
    )
    def test_zero_lai_with_negative_exponent_and_nonzero_increment_raises_code_117(
        self, sample_data_loaded, sample_yaml_path, start_date, temp_c, branch
    ):
        """Zero LAI under a negative exponent with active increment raises Fortran error 117."""
        _, df_forcing_base = sample_data_loaded
        sim = sp.SUEWSSimulation(str(sample_yaml_path))
        site = sim._config.sites[0]
        site.properties.lat.value = 51.5
        lai_cfg = site.properties.land_cover.dectr.lai
        _set_ref(lai_cfg, "base_temperature", 5.0)
        _set_ref(lai_cfg, "base_temperature_senescence", 20.0)
        _set_ref(lai_cfg, "gdd_base", 0.0)
        _set_ref(lai_cfg, "sdd_base", 0.0)
        _set_ref(lai_cfg, "lai_min", 0.0)
        _set_ref(lai_cfg, "lai_max", 5.0)
        _set_ref(lai_cfg.lai_power, "growth_lai", -0.5)
        _set_ref(lai_cfg.lai_power, "growth_gdd", 0.04)
        _set_ref(lai_cfg.lai_power, "senescence_lai", -1.0)
        _set_ref(lai_cfg.lai_power, "senescence_sdd", 0.02)
        _set_ref(site.initial_states.dectr, "lai_id", 0.0)
        _set_ref(site.initial_states.dectr, "gdd_id", 0.0)
        _set_ref(site.initial_states.dectr, "sdd_id", 0.0)

        df_win = _make_constant_temp_window(
            df_forcing_base, start_date, n_days=2, temp_c=temp_c
        )
        sim.update_forcing(df_win)
        with pytest.raises(RuntimeError, match="code 117"):
            sim.run(start_date=df_win.index[0], end_date=df_win.index[-1])

    def test_zero_lai_with_positive_exponent_is_absorbing_zero_state(
        self, sample_data_loaded, sample_yaml_path
    ):
        """lai_id_prev == 0 with positive growth exponent stays finite at 0."""
        _, df_forcing_base = sample_data_loaded
        sim = sp.SUEWSSimulation(str(sample_yaml_path))
        site = sim._config.sites[0]
        site.properties.lat.value = 51.5
        lai_cfg = site.properties.land_cover.dectr.lai
        _set_ref(lai_cfg, "base_temperature", 5.0)
        _set_ref(lai_cfg, "gdd_base", 0.0)
        _set_ref(lai_cfg, "lai_min", 0.0)
        _set_ref(lai_cfg, "lai_max", 5.0)
        _set_ref(lai_cfg.lai_power, "growth_lai", 0.5)
        _set_ref(lai_cfg.lai_power, "growth_gdd", 0.04)
        _set_ref(site.initial_states.dectr, "lai_id", 0.0)
        _set_ref(site.initial_states.dectr, "gdd_id", 0.0)

        df_warm = _make_constant_temp_window(
            df_forcing_base, "2012-03-01", n_days=2, temp_c=20.0
        )
        sim.update_forcing(df_warm)
        sim.run(start_date=df_warm.index[0], end_date=df_warm.index[-1])

        ds = sim.output.loc[:, "DailyState"].dropna(how="all")
        assert np.isfinite(ds["LAI_DecTr"]).all()
        assert (ds["LAI_DecTr"] == 0.0).all()

    def test_equal_lai_min_max_update_veg_avoids_div_by_zero(
        self, sample_data_loaded, sample_yaml_path
    ):
        """update_Veg stays finite when lai_max == lai_min and LAI_id changes."""
        _, df_forcing_base = sample_data_loaded
        sim = sp.SUEWSSimulation(str(sample_yaml_path))
        site = sim._config.sites[0]
        for surf in ("evetr", "dectr", "grass"):
            lai_cfg = getattr(site.properties.land_cover, surf).lai
            _set_ref(lai_cfg, "lai_min", 2.0)
            _set_ref(lai_cfg, "lai_max", 2.0)
            _set_ref(lai_cfg, "gdd_base", 0.0)
            # Start above lai_max so limit_lai moves LAI_id from 3.0 -> 2.0 on day 1
            _set_ref(getattr(site.initial_states, surf), "lai_id", 3.0)

        df_win = _make_constant_temp_window(
            df_forcing_base, "2012-03-01", n_days=2, temp_c=15.0
        )
        sim.update_forcing(df_win)
        sim.run(start_date=df_win.index[0], end_date=df_win.index[-1])

        ds = sim.output.loc[:, "DailyState"].dropna(how="all")
        for col in (
            "LAI_EveTr",
            "LAI_DecTr",
            "LAI_Grass",
            "AlbEveTr",
            "AlbDecTr",
            "AlbGrass",
            "Porosity",
            "DecidCap",
        ):
            assert np.isfinite(ds[col]).all(), f"{col} became non-finite with lai_min==lai_max"
        assert np.allclose(ds["LAI_DecTr"], 2.0, atol=1e-12)

    def test_reset_day_checkpoint_split_matches_uninterrupted_run(
        self, sample_data_loaded, sample_yaml_path
    ):
        """Splitting a simulation at a reset-day boundary reproduces phenology bit-for-bit."""
        _, df_forcing_base = sample_data_loaded
        df_win = _make_constant_temp_window(
            df_forcing_base, "2012-01-13", n_days=6, temp_c=12.0
        )
        split_idx = TIMESTEPS_PER_DAY * 2  # Split right after DOY 14 -> DOY 15 boundary

        def _make_sim():
            s = sp.SUEWSSimulation(str(sample_yaml_path))
            site = s._config.sites[0]
            site.properties.lat.value = 51.5
            dectr = site.properties.land_cover.dectr.lai
            _set_ref(dectr, "base_temperature", 5.0)
            _set_ref(dectr, "base_temperature_senescence", 22.0)
            _set_ref(dectr, "gdd_base", 0.0)
            _set_ref(dectr, "sdd_base", 0.0)
            _set_ref(site.initial_states.dectr, "gdd_id", 0.0)
            _set_ref(site.initial_states.dectr, "sdd_id", -50.0)
            return s

        sim_full = _make_sim()
        sim_full.update_forcing(df_win)
        out_full = sim_full.run(start_date=df_win.index[0], end_date=df_win.index[-1])

        sim_part1 = _make_sim()
        sim_part1.update_forcing(df_win.iloc[:split_idx])
        out_p1 = sim_part1.run(
            start_date=df_win.index[0], end_date=df_win.index[split_idx - 1]
        )

        sim_part2 = sp.SUEWSSimulation.from_checkpoint(sim_part1.config, out_p1.checkpoint)
        sim_part2.update_forcing(df_win.iloc[split_idx:])
        out_p2 = sim_part2.run(
            start_date=df_win.index[split_idx], end_date=df_win.index[-1]
        )

        ds_full = out_full.df.loc[:, "DailyState"].dropna(how="all")
        ds_split = pd.concat([out_p1.df, out_p2.df]).loc[:, "DailyState"].dropna(how="all")
        for col in ("LAI_EveTr", "LAI_DecTr", "LAI_Grass", "GDD_DecTr", "SDD_DecTr"):
            assert np.array_equal(ds_full[col].to_numpy(), ds_split[col].to_numpy()), (
                f"Checkpoint split diverged on {col}"
            )

    @pytest.mark.parametrize("bad_lai", [-1.0, math.inf])
    def test_dailystate_lai_observed_invalid_fortran_guard(
        self, sample_data_loaded, sample_yaml_path, monkeypatch, bad_lai
    ):
        """Invalid observed LAI (negative or +Inf) reaching Fortran raises error 105."""
        sim = sp.SUEWSSimulation(str(sample_yaml_path))

        sim._config.model.physics.laimethod.value = 0
        sim._df_state_init = sim._config.to_df_state()

        _, df_forcing = sample_data_loaded
        df_forcing = df_forcing.copy()

        first_day = df_forcing.index.normalize() == df_forcing.index[0].normalize()
        df_forcing.loc[first_day, "lai"] = bad_lai

        sim.update_forcing(df_forcing)

        monkeypatch.setattr(
            sp._check,
            "_check_observed_lai_nonneg",
            lambda *a, **k: False,
        )

        with pytest.raises(RuntimeError, match="code 105"):
            sim.run(_validate_forcing=False)
