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
  output files and ``.units.json`` intervals, conserving daily rainfall totals
  and preserving single-grid saved ``Fcld`` timestamps, missing ``-999.0000``
  sentinels, and ``%.4f`` rounding on disk.
- Targeted writer-layer damage controls verify that saved-file ``Fcld`` checks
  reject writer-only mutations even while in-memory output frames remain intact.
- Incompatible settings (``output.freq`` not a multiple of ``tstep``, or
  forcing resolution not divisible by ``tstep``) fail loudly.
"""

from __future__ import annotations

import json
from pathlib import Path

from layer5_checks import (
    assert_dailystate_group_occurrence,
    assert_forcing_cloud,
    assert_output_group_occurrence,
    assert_saved_suews_txt_cloud,
    assert_simulation_output_finiteness,
    expected_dailystate_mask,
    strict_sum,
    water_balance_residual_mm,
)
import numpy as np
import pandas as pd
import pytest
import yaml

from supy import (
    SUEWSSimulation,
    _save as supy_save,  # ruff: ignore[import-private-name]
)
from supy.data_model import SUEWSConfig

pytestmark = [
    pytest.mark.physics,
    pytest.mark.contract("C-FORCING", "C-CLOCK", "C-BALANCE"),
]

WATER_CLOSURE_TOLERANCE_MM = 1e-4
N_DAYS = 2
N_HOURLY_ROWS = 24 * N_DAYS
SUPPORTED_TSTEPS_S = (300, 600, 900, 1800, 3600)

_NATIVE_5MIN_WINDOW_STEPS = 36  # 3 hours crossing midnight (23:05..02:00)
_NATIVE_5MIN_MIXED_CLOUD: tuple[float, ...] = (
    # 23:05..23:30 -> mean 1.6 / 6 = 0.266666... -> saved 0.2667 (rounds up)
    0.1,
    0.2,
    0.5,
    0.1,
    0.2,
    0.5,
    # 23:35..00:00 -> partial missing, mean 1.6 / 4 = 0.4000 (midnight/DOY rollover)
    0.0,
    -999.0,
    0.17,
    -999.0,
    0.43,
    1.0,
    # 00:05..00:30 -> all missing -> saved -999.0000
    -999.0,
    -999.0,
    -999.0,
    -999.0,
    -999.0,
    -999.0,
    # 00:35..01:00 -> exact zero -> saved 0.0000
    0.0,
    0.0,
    0.0,
    0.0,
    0.0,
    0.0,
    # 01:05..01:30 -> partial missing, mean 0.4 / 3 = 0.133333... -> saved 0.1333
    0.1,
    0.1,
    0.2,
    -999.0,
    -999.0,
    -999.0,
    # 01:35..02:00 -> exact one -> saved 1.0000
    1.0,
    1.0,
    1.0,
    1.0,
    1.0,
    1.0,
)
_NATIVE_5MIN_MIXED_EXPECTED_30M_TOKENS: tuple[str, ...] = (
    "0.2667",
    "0.4000",
    "-999.0000",
    "0.0000",
    "0.1333",
    "1.0000",
)


def _read_raw_forcing_cloud_frame(forcing_path: Path) -> pd.DataFrame:
    """Parse timestamps and raw ``fcld`` directly from a SUEWS forcing text file."""
    df_raw = pd.read_csv(forcing_path, sep=r"\s+")
    stamps = [
        pd.Timestamp(year=int(y), month=1, day=1)
        + pd.Timedelta(days=int(d) - 1, hours=int(h), minutes=int(m))
        for y, d, h, m in zip(
            df_raw["iy"], df_raw["id"], df_raw["it"], df_raw["imin"], strict=True
        )
    ]
    return pd.DataFrame(
        {"fcld": df_raw["fcld"].to_numpy(dtype=float)},
        index=pd.DatetimeIndex(stamps, name="datetime"),
    )


def _write_native_5min_forcing(
    base_raw: pd.DataFrame, path_out: Path, mode: str
) -> Path:
    """Write a 3-hour midnight-crossing 5-minute forcing file with known ``fcld``."""
    # Rows 23..25 cover 2012-01-01 23:00..2012-01-02 02:00 (interval ends 00:00..02:00).
    three_hours = base_raw.iloc[23:26].copy()
    expanded = three_hours.loc[three_hours.index.repeat(12)].reset_index(drop=True)
    stamps = pd.date_range(
        "2012-01-01 23:05:00", periods=_NATIVE_5MIN_WINDOW_STEPS, freq="300s"
    )
    expanded["iy"] = stamps.year
    expanded["id"] = stamps.day_of_year
    expanded["it"] = stamps.hour
    expanded["imin"] = stamps.minute
    expanded["rain"] = expanded["rain"].astype(float) / 12.0
    if mode == "missing":
        expanded["fcld"] = -999.0
    elif mode == "finite":
        expanded["fcld"] = np.linspace(0.05, 0.95, _NATIVE_5MIN_WINDOW_STEPS)
    elif mode == "mixed":
        expanded["fcld"] = np.asarray(_NATIVE_5MIN_MIXED_CLOUD, dtype=float)
    else:
        raise ValueError(f"Unsupported cloud mode: {mode!r}")
    expanded.to_csv(path_out, sep=" ", index=False)
    return path_out


def _saved_suews_txt_path(saved_paths: list[Path], expected_suffix: str) -> Path:
    """Locate the single saved ``_SUEWS_<min>.txt`` path and verify its suffix."""
    suews_txt = [p for p in saved_paths if "_SUEWS_" in p.name and p.suffix == ".txt"]
    assert len(suews_txt) == 1
    assert suews_txt[0].name.endswith(f"_SUEWS{expected_suffix}.txt")
    return suews_txt[0]


def _mutate_writer_cloud(df_year: pd.DataFrame, kind: str) -> pd.DataFrame:
    """Apply one named writer-layer ``Fcld`` or clock mutation to a copy."""
    mutated = df_year.copy()
    if kind == "overwrite_all_01234":
        mutated["Fcld"] = 0.1234
    elif kind == "fill_missing_zero":
        mutated.loc[pd.Timestamp("2012-01-02 00:30:00"), "Fcld"] = 0.0
    elif kind == "erase_finite_to_nan":
        mutated.loc[pd.Timestamp("2012-01-01 23:30:00"), "Fcld"] = np.nan
    elif kind == "off_by_one_4dp_ulp":
        mutated.loc[pd.Timestamp("2012-01-01 23:30:00"), "Fcld"] = 0.2666
    elif kind == "swap_two_rows":
        first = pd.Timestamp("2012-01-01 23:30:00")
        other = pd.Timestamp("2012-01-02 01:00:00")
        mutated.loc[first, "Fcld"], mutated.loc[other, "Fcld"] = (
            mutated.loc[other, "Fcld"],
            mutated.loc[first, "Fcld"],
        )
    elif kind == "shift_timestamps_plus_30m":
        mutated.index += pd.Timedelta(minutes=30)
    else:
        raise ValueError(f"Unsupported writer mutation kind: {kind!r}")
    return mutated


def _corrupt_saved_txt_file(src: Path, dst: Path, kind: str) -> Path:
    """Create a post-write damaged copy of an intact saved ``_SUEWS_*.txt`` file."""
    lines = src.read_text(encoding="utf-8").splitlines()
    headers = lines[0].split()
    fcld_idx = headers.index("Fcld")
    dectime_idx = headers.index("Dectime")
    out_lines = [lines[0]]
    for row_idx, line in enumerate(lines[1:]):
        fields = line.split()
        if kind == "literal_nan_token" and row_idx == 2:
            fields[fcld_idx] = "nan"
        elif kind == "truncated_3dp_token" and row_idx == 0:
            fields[fcld_idx] = "0.267"
        elif kind == "corrupted_dectime" and row_idx == 1:
            fields[dectime_idx] = "1.5000"
        elif kind == "dropped_fcld_column":
            del fields[fcld_idx]
        elif kind == "dropped_missing_row" and row_idx == 2:
            continue
        out_lines.append("\t".join(fields))
    if kind == "dropped_fcld_column":
        del headers[fcld_idx]
        out_lines[0] = "\t".join(headers)
    dst.write_text("\n".join(out_lines) + "\n", encoding="utf-8")
    return dst


def _assert_writer_mutation_caught(
    sim: SUEWSSimulation,
    output,
    raw_forcing: pd.DataFrame,
    intact_text: str,
    out_dir: Path,
    kind: str,
    monkeypatch,
) -> None:
    """Prove writer-layer damage fails saved-file checks while memory stays intact."""
    original_writer = supy_save.save_df_grid_group
    hits = 0

    def damaged_writer(df_year, grid, group, *args, **kwargs):
        nonlocal hits
        if group == "SUEWS" and "Fcld" in df_year:
            hits += 1
            df_year = _mutate_writer_cloud(df_year, kind)
        return original_writer(df_year, grid, group, *args, **kwargs)

    with monkeypatch.context() as scoped_patch:
        scoped_patch.setattr(supy_save, "save_df_grid_group", damaged_writer)
        damaged_paths = sim.save(out_dir)
    assert hits == 1, f"Writer mutation hook did not fire for {kind}"
    damaged_txt = _saved_suews_txt_path(damaged_paths, "_30")
    assert damaged_txt.read_text(encoding="utf-8") != intact_text, (
        f"Writer mutation {kind} left saved text identical to intact file"
    )
    # In-memory native and resampled frames remain completely intact.
    assert_forcing_cloud(output.df.loc[1], raw_forcing, tstep=300)
    assert_forcing_cloud(
        output.resample(freq="1800s").df.loc[1],
        raw_forcing,
        tstep=300,
        output_step=1800,
        native=output.df.loc[1],
    )
    with pytest.raises(AssertionError, match="Fcld"):
        assert_saved_suews_txt_cloud(
            damaged_txt, raw_forcing, tstep=300, output_step=1800
        )


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


@pytest.fixture(scope="module", params=["missing", "finite", "mixed"])
def two_day_hourly_cloud_fixture(two_day_hourly_fixture, tmp_path_factory, request):
    """Two-day hourly forcing across missing, finite, and mixed cloud modes."""
    raw_yaml, base_forcing, base_raw, raw_rain_total = two_day_hourly_fixture
    if request.param == "missing":
        return raw_yaml, base_forcing, base_raw, raw_rain_total
    work_dir = tmp_path_factory.mktemp(f"resolution_policy_{request.param}")
    forcing_2d = work_dir / f"forcing_2d_60_{request.param}.txt"
    df_raw = base_raw.copy()
    df_raw["fcld"] = np.linspace(0.1, 0.9, len(df_raw))
    if request.param == "mixed":
        df_raw.loc[df_raw.index[::3], "fcld"] = -999.0
    df_raw.to_csv(forcing_2d, sep=" ", index=False)
    return raw_yaml, forcing_2d, df_raw, raw_rain_total


@pytest.mark.core
def test_default_tstep_and_output_freq_policy_when_omitted_from_yaml(
    two_day_hourly_cloud_fixture, tmp_path
):
    """Omitting ``tstep`` and ``output.freq`` defaults to 300 s kernel and 3600 s save."""
    # ARRANGE
    raw_yaml, forcing_2d, _, raw_rain_total = two_day_hourly_cloud_fixture
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
        forcing=sim.forcing.df,
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

    suews_txt = _saved_suews_txt_path(saved_paths, "_60")
    units_meta = json.loads(
        (tmp_path / "saved_default" / f"{suews_txt.name}.units.json").read_text(
            encoding="utf-8"
        )
    )
    assert units_meta["columns"]["Rain"]["interval"] == "1 hour"
    assert_saved_suews_txt_cloud(suews_txt, sim.forcing.df, tstep=300, output_step=3600)


@pytest.mark.core
@pytest.mark.parametrize("tstep", SUPPORTED_TSTEPS_S, ids=lambda s: f"tstep-{s}s")
def test_supported_tstep_intervals_conserve_rain_and_close_water_balance(
    two_day_hourly_cloud_fixture, tmp_path, tstep
):
    """Every hourly-divisor ``tstep`` must align forcing, conserve rain, and close water."""
    # ARRANGE
    raw_yaml, forcing_2d, df_raw, raw_rain_total = two_day_hourly_cloud_fixture
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
        np.testing.assert_allclose(
            sim.forcing.df["fcld"].to_numpy(),
            df_raw["fcld"].to_numpy(),
            rtol=0.0,
            atol=1e-12,
        )

    # ASSERT: active-variable & EOD finiteness, rain conservation, water closure, timer
    assert_simulation_output_finiteness(
        output.df,
        expected_grids=(1,),
        expected_timestamps=expected_timestamps,
        forcing=sim.forcing.df,
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


def _assert_aggregated_overlap_erasure_rejected(
    aggregated_grid: pd.DataFrame,
    native_grid: pd.DataFrame,
    forcing: pd.DataFrame,
    *,
    tstep: int,
    output_freq: int,
) -> None:
    """Prove erasing day-2 DailyState or BEERS fails while Fcld and Rain stay intact."""
    overlap_ts = pd.Timestamp("2012-01-03 00:00:00") - pd.Timedelta(seconds=output_freq)
    for group_name in ("DailyState", "BEERS"):
        erased = aggregated_grid.copy()
        for col in erased[group_name].columns:
            erased.at[overlap_ts, (group_name, col)] = np.nan
        with pytest.raises(AssertionError, match=group_name):
            assert_forcing_cloud(
                erased,
                forcing,
                tstep=tstep,
                output_step=output_freq,
                native=native_grid,
            )
        with pytest.raises(AssertionError, match=group_name):
            assert_output_group_occurrence(
                erased,
                native_grid,
                tstep=tstep,
                output_step=output_freq,
            )
        if group_name == "DailyState":
            with pytest.raises(AssertionError, match="DailyState"):
                assert_dailystate_group_occurrence(
                    erased,
                    native_grid,
                    tstep=tstep,
                    output_step=output_freq,
                )


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
    two_day_hourly_cloud_fixture,
    tmp_path,
    tstep,
    output_freq,
    expected_suffix,
    expected_interval,
):
    """Explicit ``output.freq`` writes the configured file cadence and conserves rainfall."""
    # ARRANGE
    raw_yaml, forcing_2d, _, raw_rain_total = two_day_hourly_cloud_fixture
    cfg_path = _write_yaml_case(
        raw_yaml,
        tmp_path / f"cfg_{tstep}_{output_freq}.yml",
        forcing_path=forcing_2d,
        tstep=tstep,
        output_freq=output_freq,
    )

    # ACT
    sim = SUEWSSimulation(str(cfg_path))
    output = sim.run(n_jobs=1)
    native_grid = output.df.loc[1]
    aggregated_grid = output.resample(freq=f"{output_freq}s").df.loc[1]
    assert_forcing_cloud(
        aggregated_grid,
        sim.forcing.df,
        tstep=tstep,
        output_step=output_freq,
        native=native_grid,
    )
    assert_dailystate_group_occurrence(
        aggregated_grid,
        native_grid,
        tstep=tstep,
        output_step=output_freq,
    )
    assert_output_group_occurrence(
        aggregated_grid,
        native_grid,
        tstep=tstep,
        output_step=output_freq,
    )
    if output_freq > tstep:
        _assert_aggregated_overlap_erasure_rejected(
            aggregated_grid,
            native_grid,
            sim.forcing.df,
            tstep=tstep,
            output_freq=output_freq,
        )
    out_dir = tmp_path / f"out_{tstep}_{output_freq}"
    saved_paths = sim.save(out_dir)

    # ASSERT
    suews_txt = _saved_suews_txt_path(saved_paths, expected_suffix)

    units_meta = json.loads(
        (out_dir / f"{suews_txt.name}.units.json").read_text(encoding="utf-8")
    )
    assert units_meta["columns"]["Rain"]["interval"] == expected_interval

    assert_saved_suews_txt_cloud(
        suews_txt, sim.forcing.df, tstep=tstep, output_step=output_freq
    )
    if tstep == 3600:
        assert_saved_suews_txt_cloud(
            suews_txt,
            _read_raw_forcing_cloud_frame(forcing_2d),
            tstep=tstep,
            output_step=output_freq,
        )

    df_saved = pd.read_csv(suews_txt, sep=r"\s+")
    assert len(df_saved) == N_DAYS * (86400 // output_freq)
    # Saved Rain is an interval rate [mm s-1]; integrate over output_freq to recover mm.
    saved_rain_total = (
        strict_sum(df_saved["Rain"], label=f"saved_{tstep}_{output_freq}.Rain")
        * output_freq
    )
    assert saved_rain_total == pytest.approx(raw_rain_total, rel=1e-3, abs=1e-2)


@pytest.mark.core
@pytest.mark.parametrize(
    ("mode", "tstep", "output_freq", "expected_suffix"),
    [
        ("mixed", 300, 300, "_5"),
        ("mixed", 300, 1800, "_30"),
        ("missing", 300, 1800, "_30"),
        ("finite", 300, 1800, "_30"),
    ],
    ids=[
        "mixed-native-300-to-300",
        "mixed-agg-300-to-1800",
        "missing-agg-300-to-1800",
        "finite-agg-300-to-1800",
    ],
)
def test_saved_suews_txt_cloud_equivalence_classes_from_native_forcing_file(
    two_day_hourly_fixture, tmp_path, mode, tstep, output_freq, expected_suffix
):
    """Saved ``_SUEWS_<min>.txt`` preserves Fcld directly from raw native forcing files."""
    raw_yaml, _, base_raw, _ = two_day_hourly_fixture
    forcing_path = _write_native_5min_forcing(
        base_raw, tmp_path / f"forcing_5m_{mode}.txt", mode
    )
    raw_forcing = _read_raw_forcing_cloud_frame(forcing_path)
    cfg_path = _write_yaml_case(
        raw_yaml,
        tmp_path / f"cfg_native_{mode}_{output_freq}.yml",
        forcing_path=forcing_path,
        tstep=tstep,
        output_freq=output_freq,
    )

    sim = SUEWSSimulation(str(cfg_path))
    output = sim.run(n_jobs=1)
    saved_paths = sim.save(tmp_path / f"saved_{mode}_{output_freq}")

    np.testing.assert_allclose(
        sim.forcing.df["fcld"].to_numpy(),
        raw_forcing["fcld"].to_numpy(),
        rtol=0.0,
        atol=0.0,
    )
    assert_forcing_cloud(output.df.loc[1], raw_forcing, tstep=tstep)
    saved_txt = _saved_suews_txt_path(saved_paths, expected_suffix)
    assert_saved_suews_txt_cloud(
        saved_txt, raw_forcing, tstep=tstep, output_step=output_freq
    )

    if mode == "mixed" and output_freq == 1800:
        lines = saved_txt.read_text(encoding="utf-8").splitlines()
        fcld_idx = lines[0].split().index("Fcld")
        tokens = tuple(line.split()[fcld_idx] for line in lines[1:] if line.strip())
        assert tokens == _NATIVE_5MIN_MIXED_EXPECTED_30M_TOKENS


@pytest.mark.core
def test_saved_suews_txt_cloud_catches_writer_damage_with_intact_memory(
    two_day_hourly_fixture, tmp_path, monkeypatch
):
    """Saved-file checks pass intact output and reject writer-only and format damage."""
    raw_yaml, _, base_raw, _ = two_day_hourly_fixture
    forcing_path = _write_native_5min_forcing(
        base_raw, tmp_path / "forcing_5m_mixed.txt", "mixed"
    )
    raw_forcing = _read_raw_forcing_cloud_frame(forcing_path)
    cfg_path = _write_yaml_case(
        raw_yaml,
        tmp_path / "cfg_writer_control.yml",
        forcing_path=forcing_path,
        tstep=300,
        output_freq=1800,
    )

    sim = SUEWSSimulation(str(cfg_path))
    output = sim.run(n_jobs=1)

    # Intact positive control: both memory and saved text file pass.
    intact_paths = sim.save(tmp_path / "intact")
    intact_txt = _saved_suews_txt_path(intact_paths, "_30")
    assert_forcing_cloud(output.df.loc[1], raw_forcing, tstep=300)
    assert_forcing_cloud(
        output.resample(freq="1800s").df.loc[1],
        raw_forcing,
        tstep=300,
        output_step=1800,
        native=output.df.loc[1],
    )
    assert_saved_suews_txt_cloud(intact_txt, raw_forcing, tstep=300, output_step=1800)
    intact_text = intact_txt.read_text(encoding="utf-8")

    for kind in (
        "overwrite_all_01234",
        "fill_missing_zero",
        "erase_finite_to_nan",
        "off_by_one_4dp_ulp",
        "swap_two_rows",
        "shift_timestamps_plus_30m",
    ):
        _assert_writer_mutation_caught(
            sim,
            output,
            raw_forcing,
            intact_text,
            tmp_path / f"writer_{kind}",
            kind,
            monkeypatch,
        )

    for kind in (
        "literal_nan_token",
        "truncated_3dp_token",
        "corrupted_dectime",
        "dropped_fcld_column",
        "dropped_missing_row",
    ):
        corrupted = _corrupt_saved_txt_file(
            intact_txt, tmp_path / f"format_{kind}.txt", kind
        )
        with pytest.raises(AssertionError, match="Fcld"):
            assert_saved_suews_txt_cloud(
                corrupted, raw_forcing, tstep=300, output_step=1800
            )


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
