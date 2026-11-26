"""Daylight saving time (DLS) Northern and Southern Hemisphere physics tests.

``SUEWS_cal_DLS`` in ``src/suews/src/suews_util_time.f95`` sets ``timer%DLS``
from local-standard day-of-year ``timer_st%id`` and the site window
``(startdls, enddls)``. When ``DLS == 1``, ``cal_profile_hour(it, dls)``
shifts diurnal profile lookups forward by +1 hour in forcing time.

These end-to-end ``SUEWSSimulation`` tests verify:
- Southern Hemisphere year-wrapping windows (``startdls > enddls``) activate
  DLS after ``startdls`` through year-end and from DOY 1 up to ``enddls``,
  and stay inactive between ``enddls`` and ``startdls``.
- Northern Hemisphere intra-year windows (``startdls < enddls``) preserve
  strict ``startdls < id < enddls`` boundaries.
- Disabled windows (``None/None`` and ``startdls == enddls``) keep ``DLS = 0``.
"""

from __future__ import annotations

import copy

from conftest import TIMESTEPS_PER_DAY
from layer5_checks import assert_simulation_output_finiteness
import numpy as np
import pandas as pd
import pytest

from supy import SUEWSSimulation
from supy.data_model import SUEWSConfig

pytestmark = [
    pytest.mark.physics,
    pytest.mark.contract("C-CLOCK", "C-BIND"),
]

# Midday 3-hour plateau in ahprof_24hr (identical on working days and holidays)
# so a +1 h DLS shift moves the QF pulse unambiguously by 12 five-minute steps.
_MIDDAY_PROFILE: dict[str, float] = {
    str(hour): (5.0 if 10 <= hour <= 12 else 0.0) for hour in range(1, 25)
}
_STEPS_PER_HOUR = 12


def _make_repeated_forcing(
    base_day: pd.DataFrame, start_date: str, n_days: int
) -> pd.DataFrame:
    """Repeat one 5-minute forcing day across ``n_days`` calendar days."""
    start_ts = pd.Timestamp(start_date) + pd.Timedelta(minutes=5)
    index = pd.date_range(start_ts, periods=TIMESTEPS_PER_DAY * n_days, freq="300s")
    df_forcing = pd.concat(
        [base_day.reset_index(drop=True) for _ in range(n_days)],
        ignore_index=True,
    )
    df_forcing.index = index
    df_forcing["iy"] = index.year
    df_forcing["id"] = index.day_of_year
    df_forcing["it"] = index.hour
    df_forcing["imin"] = index.minute
    df_forcing["isec"] = index.second
    return df_forcing


def _build_dls_qf_config(
    base_cfg: SUEWSConfig,
    startdls: int | None,
    enddls: int | None,
) -> SUEWSConfig:
    """Return a copy of ``base_cfg`` with a midday QF pulse and chosen DLS bounds."""
    cfg = copy.deepcopy(base_cfg)
    anthro = cfg.sites[0].properties.anthropogenic_emissions
    anthro.startdls = startdls
    anthro.enddls = enddls
    heat = anthro.heat
    heat.qf_a.working_day = 0.60
    heat.qf_a.holiday = 0.60
    heat.qf_b.working_day = 0.0
    heat.qf_b.holiday = 0.0
    heat.qf_c.working_day = 0.0
    heat.qf_c.holiday = 0.0
    heat.ahprof_24hr.working_day = dict(_MIDDAY_PROFILE)
    heat.ahprof_24hr.holiday = dict(_MIDDAY_PROFILE)
    return cfg


def _run_qf(config: SUEWSConfig, df_forcing: pd.DataFrame) -> pd.Series:
    """Run ``config`` on ``df_forcing`` and return finite ``SUEWS.QF``."""
    sim = SUEWSSimulation()
    sim.update_config(config, auto_load_forcing=False)
    sim.update_forcing(df_forcing)
    output = sim.run(
        start_date=df_forcing.index[0],
        end_date=df_forcing.index[-1],
        n_jobs=1,
    )
    assert_simulation_output_finiteness(
        output.df,
        expected_grids=(1,),
        expected_timestamps=df_forcing.index,
        forcing=sim.forcing.df,
        tstep=300,
    )
    return output.df.loc[1, ("SUEWS", "QF")]


def _day_midday_slice(ser_qf: pd.Series, date_str: str) -> np.ndarray:
    """Extract 04:05..22:00 (strictly interior to calendar day ``date_str``)."""
    start = pd.Timestamp(f"{date_str} 04:05:00")
    end = pd.Timestamp(f"{date_str} 22:00:00")
    values = ser_qf.loc[start:end].to_numpy(dtype=float)
    assert len(values) == 18 * _STEPS_PER_HOUR
    return values


def _assert_dls_state(
    ser_qf: pd.Series,
    baseline_no_dls: np.ndarray,
    date_str: str,
    *,
    expect_active: bool,
) -> None:
    """Assert whether ``date_str`` matches unshifted ``DLS=0`` or +1 h shifted ``DLS=1``."""
    day_vals = _day_midday_slice(ser_qf, date_str)
    unshifted_diff = float(np.max(np.abs(day_vals - baseline_no_dls)))
    shifted_diff = float(
        np.max(np.abs(day_vals[_STEPS_PER_HOUR:] - baseline_no_dls[:-_STEPS_PER_HOUR]))
    )
    # Confirm the midday pulse actually separates DLS=0 from DLS=1.
    pulse_separation = float(
        np.max(
            np.abs(
                baseline_no_dls[_STEPS_PER_HOUR:] - baseline_no_dls[:-_STEPS_PER_HOUR]
            )
        )
    )
    assert pulse_separation > 1.0, (
        f"Midday QF pulse too flat to distinguish DLS shift ({pulse_separation:.3f})"
    )
    if expect_active:
        assert shifted_diff < 1e-10, (
            f"{date_str}: expected DLS=1 (+1 h shift), max diff vs shifted={shifted_diff:.3e}"
        )
        assert unshifted_diff > 1.0, (
            f"{date_str}: expected DLS=1, but QF matched unshifted baseline"
        )
    else:
        assert unshifted_diff < 1e-10, (
            f"{date_str}: expected DLS=0 (unshifted), max diff vs baseline={unshifted_diff:.3e}"
        )
        assert shifted_diff > 1.0, (
            f"{date_str}: expected DLS=0, but QF matched shifted baseline"
        )


@pytest.fixture(scope="module")
def dls_base(
    short_sample_yaml_path,
) -> tuple[SUEWSConfig, pd.DataFrame, np.ndarray]:
    """Validated config, one controlled forcing day, and its DLS=0 midday QF template."""
    sim = SUEWSSimulation(str(short_sample_yaml_path))
    base_day = sim.forcing.df.iloc[:TIMESTEPS_PER_DAY].copy()
    base_day["rain"] = 0.0
    base_day["Tair"] = 18.0
    base_day["RH"] = 60.0
    base_day["U"] = 3.0
    base_cfg = SUEWSConfig.from_yaml(str(short_sample_yaml_path))

    # Build a single-day unshifted (DLS disabled) midday QF baseline.
    df_ref = _make_repeated_forcing(base_day, "2012-06-15", n_days=1)
    qf_ref = _run_qf(_build_dls_qf_config(base_cfg, None, None), df_ref)
    baseline = _day_midday_slice(qf_ref, "2012-06-15")
    return base_cfg, base_day, baseline


def test_southern_hemisphere_dls_wraps_across_year_boundary(dls_base):
    """Southern Hemisphere DLS (startdls > enddls) activates across year-end and stays off in winter."""
    base_cfg, base_day, baseline = dls_base
    # Typical Southern Hemisphere window (e.g. Oct..Apr): startdls=275, enddls=95.
    cfg_sh = _build_dls_qf_config(base_cfg, startdls=275, enddls=95)

    # 1. Spring transition in leap year 2012: 2012-10-01 is DOY 275 (off), 2012-10-02 is DOY 276 (on).
    df_spring = _make_repeated_forcing(base_day, "2012-10-01", n_days=2)
    qf_spring = _run_qf(cfg_sh, df_spring)
    _assert_dls_state(qf_spring, baseline, "2012-10-01", expect_active=False)
    _assert_dls_state(qf_spring, baseline, "2012-10-02", expect_active=True)

    # 2. Year boundary wrap: 2012-12-31 (DOY 366 > 275) and 2013-01-01 (DOY 1 < 95) are both active.
    df_wrap = _make_repeated_forcing(base_day, "2012-12-31", n_days=2)
    qf_wrap = _run_qf(cfg_sh, df_wrap)
    _assert_dls_state(qf_wrap, baseline, "2012-12-31", expect_active=True)
    _assert_dls_state(qf_wrap, baseline, "2013-01-01", expect_active=True)

    # 3. Autumn transition in leap year 2012: 2012-04-03 is DOY 94 (on), 2012-04-04 is DOY 95 (off).
    df_autumn = _make_repeated_forcing(base_day, "2012-04-03", n_days=2)
    qf_autumn = _run_qf(cfg_sh, df_autumn)
    _assert_dls_state(qf_autumn, baseline, "2012-04-03", expect_active=True)
    _assert_dls_state(qf_autumn, baseline, "2012-04-04", expect_active=False)

    # 4. Austral winter (2012-06-15, DOY 167, between 95 and 275): inactive.
    df_winter = _make_repeated_forcing(base_day, "2012-06-15", n_days=1)
    qf_winter = _run_qf(cfg_sh, df_winter)
    _assert_dls_state(qf_winter, baseline, "2012-06-15", expect_active=False)


def test_northern_hemisphere_and_disabled_dls_windows_unchanged(dls_base):
    """Northern Hemisphere strict (startdls, enddls) and disabled windows keep exact behaviour."""
    base_cfg, base_day, baseline = dls_base

    # Northern Hemisphere: startdls=86 (2012-03-26, off -> 2012-03-27 DOY 87, on),
    # enddls=303 (2012-10-28 DOY 302, on -> 2012-10-29 DOY 303, off).
    cfg_nh = _build_dls_qf_config(base_cfg, startdls=86, enddls=303)
    qf_nh_spring = _run_qf(
        cfg_nh, _make_repeated_forcing(base_day, "2012-03-26", n_days=2)
    )
    _assert_dls_state(qf_nh_spring, baseline, "2012-03-26", expect_active=False)
    _assert_dls_state(qf_nh_spring, baseline, "2012-03-27", expect_active=True)

    qf_nh_autumn = _run_qf(
        cfg_nh, _make_repeated_forcing(base_day, "2012-10-28", n_days=2)
    )
    _assert_dls_state(qf_nh_autumn, baseline, "2012-10-28", expect_active=True)
    _assert_dls_state(qf_nh_autumn, baseline, "2012-10-29", expect_active=False)

    # Equal bounds (startdls == enddls) disable DLS on every day.
    cfg_eq = _build_dls_qf_config(base_cfg, startdls=100, enddls=100)
    qf_eq = _run_qf(cfg_eq, _make_repeated_forcing(base_day, "2012-04-08", n_days=3))
    for date_str in ("2012-04-08", "2012-04-09", "2012-04-10"):
        _assert_dls_state(qf_eq, baseline, date_str, expect_active=False)
