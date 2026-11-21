"""Resolution-to-kernel default policy witnesses across supported intervals (W2-4c).

Verifies end-to-end how YAML forcing resolution, kernel timestep (``tstep``),
and output frequency (``output.freq``) interact through ``SUEWSSimulation``
(YAML -> Rust engine -> saved output):

- Omitting ``model.control.tstep`` and ``model.control.output.freq`` applies
  the documented defaults (``tstep = 300`` s, saved cadence ``3600`` s /
  ``_60.txt``) and propagates ``tstep = 300`` into the native kernel timer.
- Across all supported divisors of hourly forcing (``300, 600, 900, 1800,
  3600`` s), forcing is aligned cleanly (with zero interpolation when
  ``tstep == forcing_resolution``), daily rainfall mass is conserved to
  machine precision, SUEWS outputs are finite, water balance closes,
  ``DailyState`` writes one row per day, and the kernel checkpoint records the
  configured ``tstep``.
- Explicit ``model.control.output.freq`` settings write matching ``_SUEWS_<min>``
  output files and ``.units.json`` intervals, conserving daily rainfall totals.
- Incompatible settings (``output.freq`` not a multiple of ``tstep``, or
  forcing resolution not divisible by ``tstep``) fail loudly.
"""

from __future__ import annotations

import json
from pathlib import Path

from layer5_checks import (
    assert_simulation_output_finiteness,
    expected_dailystate_mask,
    strict_sum,
    water_balance_residual_mm,
)
import numpy as np
import pandas as pd
import pytest
import yaml

from supy import SUEWSSimulation
from supy.data_model import SUEWSConfig

pytestmark = [pytest.mark.physics, pytest.mark.contract("C-FORCING", "C-CLOCK", "C-BALANCE")]

WATER_CLOSURE_TOLERANCE_MM = 1e-4
N_DAYS = 2
N_HOURLY_ROWS = 24 * N_DAYS
SUPPORTED_TSTEPS_S = (300, 600, 900, 1800, 3600)


def _write_yaml_case(
    base_yaml_dict: dict,
    path_out: Path,
    *,
    forcing_path: Path,
    tstep: int | None,
    output_freq: int | None,
) -> Path:
    """Write a self-contained YAML config with controlled timestep/output keys."""
    cfg_dict = yaml.safe_load(yaml.safe_dump(base_yaml_dict))
    control = cfg_dict["model"]["control"]
    control.pop("start_time", None)
    control.pop("end_time", None)
    control["forcing"]["file"] = str(forcing_path)
    if tstep is None:
        control.pop("tstep", None)
    else:
        control["tstep"] = tstep
    if output_freq is None:
        control.setdefault("output", {}).pop("freq", None)
    else:
        control.setdefault("output", {})["freq"] = output_freq
    path_out.write_text(yaml.safe_dump(cfg_dict, sort_keys=False), encoding="utf-8")
    return path_out


@pytest.fixture(scope="module")
def two_day_hourly_fixture(short_sample_yaml_path, tmp_path_factory):
    """Two-day hourly forcing slice with known rainfall plus parsed base YAML dict."""
    work_dir = tmp_path_factory.mktemp("resolution_policy")
    raw_yaml = yaml.safe_load(short_sample_yaml_path.read_text(encoding="utf-8"))
    source_forcing = short_sample_yaml_path.parent / "Kc_2012_data_60_jan.txt"
    lines = source_forcing.read_text(encoding="utf-8").splitlines()
    # Header + 48 hourly rows (2 full days, containing non-zero rain).
    forcing_2d = work_dir / "forcing_2d_60.txt"
    forcing_2d.write_text(
        "\n".join(lines[: 1 + N_HOURLY_ROWS]) + "\n", encoding="utf-8"
    )
    df_raw = pd.read_csv(forcing_2d, sep=r"\s+")
    raw_rain_total = strict_sum(df_raw["rain"], label="raw_forcing.rain")
    assert raw_rain_total > 0.0, "Two-day hourly fixture must contain rainfall"
    return raw_yaml, forcing_2d, df_raw, raw_rain_total


@pytest.mark.core
def test_default_tstep_and_output_freq_policy_when_omitted_from_yaml(
    two_day_hourly_fixture, tmp_path
):
    """Omitting ``tstep`` and ``output.freq`` defaults to 300 s kernel and 3600 s save."""
    # ARRANGE
    raw_yaml, forcing_2d, _, raw_rain_total = two_day_hourly_fixture
    cfg_path = _write_yaml_case(
        raw_yaml,
        tmp_path / "default_resolution.yml",
        forcing_path=forcing_2d,
        tstep=None,
        output_freq=None,
    )

    # ACT
    sim = SUEWSSimulation(str(cfg_path))
    output = sim.run(n_jobs=1)
    saved_paths = sim.save(tmp_path / "saved_default")

    # ASSERT
    tstep_cfg = sim.config.model.control.tstep
    assert int(getattr(tstep_cfg, "value", tstep_cfg)) == 300
    assert sim.config.model.control.output.freq is None

    expected_timestamps = pd.date_range(
        pd.Timestamp("2012-01-01") + pd.Timedelta(seconds=300),
        periods=N_DAYS * (86400 // 300),
        freq="300s",
    )
    assert_simulation_output_finiteness(
        output.df,
        expected_grids=(1,),
        expected_timestamps=expected_timestamps,
        tstep=300,
    )
    suews = output.df.loc[1, "SUEWS"]
    assert len(suews) == len(expected_timestamps)
    assert int((suews.index[1] - suews.index[0]).total_seconds()) == 300
    assert strict_sum(suews["Rain"], label="default.SUEWS.Rain") == pytest.approx(
        raw_rain_total, rel=1e-10, abs=1e-10
    )

    timer = json.loads(sim.checkpoint.grid_states[1])["timer"]
    assert timer["tstep"] == 300
    assert timer["dt_since_start"] == N_DAYS * 86400 + 300

    suews_txt = [p for p in saved_paths if "_SUEWS_" in p.name and p.suffix == ".txt"]
    assert len(suews_txt) == 1
    assert suews_txt[0].name.endswith("_SUEWS_60.txt")
    units_meta = json.loads(
        (tmp_path / "saved_default" / f"{suews_txt[0].name}.units.json").read_text(
            encoding="utf-8"
        )
    )
    assert units_meta["columns"]["Rain"]["interval"] == "1 hour"


@pytest.mark.core
@pytest.mark.parametrize("tstep", SUPPORTED_TSTEPS_S, ids=lambda s: f"tstep-{s}s")
def test_supported_tstep_intervals_conserve_rain_and_close_water_balance(
    two_day_hourly_fixture, tmp_path, tstep
):
    """Every hourly-divisor ``tstep`` must align forcing, conserve rain, and close water."""
    # ARRANGE
    raw_yaml, forcing_2d, df_raw, raw_rain_total = two_day_hourly_fixture
    cfg_path = _write_yaml_case(
        raw_yaml,
        tmp_path / f"cfg_{tstep}.yml",
        forcing_path=forcing_2d,
        tstep=tstep,
        output_freq=None,
    )

    # ACT
    sim = SUEWSSimulation(str(cfg_path))
    output = sim.run(n_jobs=1)

    # ASSERT: timestep cadence and non-interpolation when tstep == forcing interval
    expected_rows = N_DAYS * (86400 // tstep)
    expected_timestamps = pd.date_range(
        pd.Timestamp("2012-01-01") + pd.Timedelta(seconds=tstep),
        periods=expected_rows,
        freq=f"{tstep}s",
    )
    assert sim.forcing.df.index.equals(expected_timestamps)
    assert len(sim.forcing.df) == expected_rows
    forcing_dt = (sim.forcing.df.index[1] - sim.forcing.df.index[0]).total_seconds()
    assert int(forcing_dt) == tstep

    if tstep == 3600:
        np.testing.assert_allclose(
            sim.forcing.df["kdown"].to_numpy(),
            df_raw["kdown"].to_numpy(),
            rtol=0.0,
            atol=1e-12,
        )
        np.testing.assert_allclose(
            sim.forcing.df["Tair"].to_numpy(),
            df_raw["Tair"].to_numpy(),
            rtol=0.0,
            atol=1e-12,
        )
        np.testing.assert_allclose(
            sim.forcing.df["rain"].to_numpy(),
            df_raw["rain"].to_numpy(),
            rtol=0.0,
            atol=1e-12,
        )

    # ASSERT: active-variable & EOD finiteness, rain conservation, water closure, timer
    assert_simulation_output_finiteness(
        output.df,
        expected_grids=(1,),
        expected_timestamps=expected_timestamps,
        tstep=tstep,
    )
    suews = output.df.loc[1, "SUEWS"]
    assert len(suews) == expected_rows
    assert int((suews.index[1] - suews.index[0]).total_seconds()) == tstep

    assert strict_sum(suews["Rain"], label=f"tstep-{tstep}.Rain") == pytest.approx(
        raw_rain_total, rel=1e-10, abs=1e-10
    )
    residual = water_balance_residual_mm(
        output.df, sim.state_init, expected_timestamps=expected_timestamps
    )
    assert residual < WATER_CLOSURE_TOLERANCE_MM, (
        f"tstep={tstep}s water balance residual {residual:.3e} mm"
    )

    eod_mask = expected_dailystate_mask(output.df.loc[1, "DailyState"].index, tstep)
    assert int(eod_mask.sum()) == N_DAYS

    timer = json.loads(sim.checkpoint.grid_states[1])["timer"]
    assert timer["tstep"] == tstep
    assert timer["dt_since_start"] == expected_rows * tstep + tstep


@pytest.mark.core
@pytest.mark.parametrize(
    ("tstep", "output_freq", "expected_suffix", "expected_interval"),
    [
        (300, 300, "_5", "5 minutes"),
        (600, 1800, "_30", "30 minutes"),
        (1800, 3600, "_60", "1 hour"),
        (3600, 3600, "_60", "1 hour"),
    ],
    ids=["300-to-300", "600-to-1800", "1800-to-3600", "3600-to-3600"],
)
def test_output_freq_policy_writes_configured_cadence_and_conserves_rain(
    two_day_hourly_fixture,
    tmp_path,
    tstep,
    output_freq,
    expected_suffix,
    expected_interval,
):
    """Explicit ``output.freq`` writes the configured file cadence and conserves rainfall."""
    # ARRANGE
    raw_yaml, forcing_2d, _, raw_rain_total = two_day_hourly_fixture
    cfg_path = _write_yaml_case(
        raw_yaml,
        tmp_path / f"cfg_{tstep}_{output_freq}.yml",
        forcing_path=forcing_2d,
        tstep=tstep,
        output_freq=output_freq,
    )

    # ACT
    sim = SUEWSSimulation(str(cfg_path))
    sim.run(n_jobs=1)
    out_dir = tmp_path / f"out_{tstep}_{output_freq}"
    saved_paths = sim.save(out_dir)

    # ASSERT
    suews_txt = [p for p in saved_paths if "_SUEWS_" in p.name and p.suffix == ".txt"]
    assert len(suews_txt) == 1
    assert suews_txt[0].name.endswith(f"_SUEWS{expected_suffix}.txt")

    units_meta = json.loads(
        (out_dir / f"{suews_txt[0].name}.units.json").read_text(encoding="utf-8")
    )
    assert units_meta["columns"]["Rain"]["interval"] == expected_interval

    df_saved = pd.read_csv(suews_txt[0], sep=r"\s+")
    assert len(df_saved) == N_DAYS * (86400 // output_freq)
    # Saved Rain is an interval rate [mm s-1]; integrate over output_freq to recover mm.
    saved_rain_total = (
        strict_sum(df_saved["Rain"], label=f"saved_{tstep}_{output_freq}.Rain")
        * output_freq
    )
    assert saved_rain_total == pytest.approx(raw_rain_total, rel=1e-3, abs=1e-2)


@pytest.mark.core
def test_incompatible_resolution_settings_are_rejected(
    two_day_hourly_fixture, tmp_path
):
    """Non-multiple ``output.freq`` and non-divisible ``tstep`` must fail loudly."""
    # ARRANGE
    raw_yaml, forcing_2d, _, _ = two_day_hourly_fixture

    # ACT / ASSERT 1: output.freq not an integer multiple of tstep
    bad_freq_path = _write_yaml_case(
        raw_yaml,
        tmp_path / "bad_output_freq.yml",
        forcing_path=forcing_2d,
        tstep=600,
        output_freq=900,
    )
    with pytest.raises(ValueError, match="multiple of timestep"):
        SUEWSConfig.from_yaml(str(bad_freq_path))

    # ACT / ASSERT 2: hourly forcing resolution (3600 s) not divisible by tstep (420 s)
    bad_tstep_path = _write_yaml_case(
        raw_yaml,
        tmp_path / "bad_tstep.yml",
        forcing_path=forcing_2d,
        tstep=420,
        output_freq=420,
    )
    sim = SUEWSSimulation()
    sim.update_config(
        SUEWSConfig.from_yaml(str(bad_tstep_path)), auto_load_forcing=False
    )
    with pytest.raises(RuntimeError, match="not divisible"):
        sim.update_forcing(str(forcing_2d))
