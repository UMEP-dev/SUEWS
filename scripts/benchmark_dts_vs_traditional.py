#!/usr/bin/env python3
"""
Benchmark traditional (run_supy) vs DTS (run_dts) execution paths.

This script reports two comparison modes:
1) user-feels: end-to-end wall time, including config/state setup and output DataFrames
2) kernel-only: Fortran kernel time only (no DataFrame packing/unpacking)

Notes:
- user-feels includes SUEWSConfig.from_df_state() for DTS by default.
- kernel-only times exclude Python-side forcing/state preparation.
"""

from __future__ import annotations

import argparse
import copy
import time
from typing import Iterable, Tuple, List

import numpy as np
import pandas as pd

from supy import load_SampleData, run_supy
from supy.data_model import SUEWSConfig
from supy.dts import run_dts

# Internal helpers for kernel-only timings (dev/benchmark use)
from supy._load import list_var_input_multitsteps
from supy._run import pack_grid_dict, _reset_supy_error, _check_supy_error
from supy.supy_driver import suews_driver as sd

from supy.dts._core import (
    create_suews_config,
    create_suews_site,
    create_suews_state,
    create_suews_forcing,
    create_suews_timer,
)
from supy.dts._populate import (
    populate_config_from_pydantic,
    populate_site_from_pydantic,
    populate_state_from_pydantic,
    populate_storedrainprm,
    populate_roughnessstate,
    populate_ohmstate_defaults,
    populate_forcing_from_row,
    populate_atmstate,
    populate_timer_from_datetime,
)
from supy.supy_driver import suews_driver as drv
from supy.supy_driver import module_ctrl_const_allocate as alloc


def _ensure_freq(df_forcing: pd.DataFrame) -> pd.DataFrame:
    """Ensure DatetimeIndex has freq set (run_supy relies on it)."""
    if df_forcing.index.freq is not None:
        return df_forcing
    df = df_forcing.copy()
    freq = pd.infer_freq(df.index)
    if freq is None:
        if len(df.index) > 1:
            delta = df.index[1] - df.index[0]
            freq = pd.tseries.frequencies.to_offset(delta)
        else:
            freq = pd.tseries.frequencies.to_offset("1H")
    df.index = pd.DatetimeIndex(df.index, freq=freq)
    return df


def _ensure_time_cols(df: pd.DataFrame) -> pd.DataFrame:
    """Ensure required time columns exist for traditional forcing."""
    out = df.copy()
    if "iy" not in out.columns:
        out["iy"] = out.index.year
    if "id" not in out.columns:
        out["id"] = out.index.dayofyear
    if "it" not in out.columns:
        out["it"] = out.index.hour
    if "imin" not in out.columns:
        out["imin"] = out.index.minute
    if "isec" not in out.columns:
        out["isec"] = out.index.second
    return out


def _prepare_forcing_traditional(df_forcing: pd.DataFrame) -> pd.DataFrame:
    """Match run_supy forcing preprocessing for kernel-only inputs."""
    df = _ensure_time_cols(df_forcing)
    df = df.assign(
        metforcingdata_grid=0,
        ts5mindata_ir=0,
    ).rename(
        columns={
            "%" + "iy": "iy",
            "id": "id",
            "it": "it",
            "imin": "imin",
            "qn": "qn1_obs",
            "qh": "qh_obs",
            "qe": "qe",
            "qs": "qs_obs",
            "qf": "qf_obs",
            "U": "avu1",
            "RH": "avrh",
            "Tair": "temp_c",
            "pres": "press_hpa",
            "rain": "precip",
            "kdown": "kdown",
            "snow": "snowfrac_obs",
            "ldown": "ldown_obs",
            "fcld": "fcld_obs",
            "Wuh": "wu_m3",
            "xsmd": "xsmd",
            "lai": "lai_obs",
            "kdiff": "kdiff",
            "kdir": "kdir",
            "wdir": "wdir",
        }
    )
    list_var_forcing = [
        "iy",
        "id",
        "it",
        "imin",
        "qn1_obs",
        "qh_obs",
        "qe",
        "qs_obs",
        "qf_obs",
        "avu1",
        "avrh",
        "temp_c",
        "press_hpa",
        "precip",
        "kdown",
        "snowfrac_obs",
        "ldown_obs",
        "fcld_obs",
        "wu_m3",
        "xsmd",
        "lai_obs",
        "kdiff",
        "kdir",
        "wdir",
        "isec",
        "metforcingdata_grid",
        "ts5mindata_ir",
    ]
    for col in list_var_forcing:
        if col not in df.columns:
            df[col] = 0.0
    return df.loc[:, list_var_forcing]


def _normalize_df_state_init(df_state_init: pd.DataFrame) -> pd.DataFrame:
    df_init = df_state_init.copy()
    if df_init.index.nlevels > 1:
        idx_dt = df_init.index.get_level_values("datetime").unique()
        dt_last = idx_dt.max()
        df_init = df_init.loc[dt_last]
    return df_init


def _prepare_traditional_multitsteps_input(
    df_state_init: pd.DataFrame, df_forcing: pd.DataFrame
) -> dict:
    df_init = _normalize_df_state_init(df_state_init)
    grid_id = df_init.index[0]
    ser_state_init = df_init.loc[grid_id]
    dict_state_start = pack_grid_dict(ser_state_init)

    df_forcing_prepped = _prepare_forcing_traditional(df_forcing)
    metforcingblock = np.array(
        df_forcing_prepped.drop(
            columns=["metforcingdata_grid", "ts5mindata_ir", "isec"]
        ),
        order="F",
    )

    dict_input = copy.deepcopy(dict_state_start)
    dict_input.update(
        {
            "metforcingblock": metforcingblock,
            "ts5mindata_ir": np.array(df_forcing_prepped["ts5mindata_ir"], order="F"),
            "len_sim": int(df_forcing_prepped.shape[0]),
            "flag_test": False,
        }
    )
    dict_input = {k: dict_input[k] for k in list_var_input_multitsteps}
    return dict_input


def _prepare_dts_objects(
    config: SUEWSConfig,
    df_forcing: pd.DataFrame,
    nlayer: int,
    ndepth: int,
    site_index: int = 0,
):
    model = config.model
    site = config.sites[site_index] if hasattr(config, "sites") else config.site
    initial_states = site.initial_states

    if len(df_forcing) > 1:
        tstep_s = int((df_forcing.index[1] - df_forcing.index[0]).total_seconds())
    else:
        tstep_s = 3600

    config_dts = create_suews_config()
    site_dts = create_suews_site(nlayer=nlayer, ndepth=ndepth)
    state_dts = create_suews_state(nlayer=nlayer, ndepth=ndepth)
    forcing_dts = create_suews_forcing()
    timer_dts = create_suews_timer()

    populate_config_from_pydantic(config_dts, model)
    populate_site_from_pydantic(site_dts, site, model)
    land_cover = site.properties.land_cover
    populate_state_from_pydantic(
        state_dts, initial_states, nlayer, ndepth, land_cover=land_cover
    )
    populate_storedrainprm(state_dts, land_cover)
    site_dts.cal_surf(config_dts)
    populate_roughnessstate(state_dts, site_dts)
    populate_ohmstate_defaults(state_dts)

    # Initialize atmospheric state from first timestep
    first_row = df_forcing.iloc[0]
    populate_forcing_from_row(forcing_dts, first_row)
    populate_atmstate(state_dts, forcing_dts)

    return config_dts, site_dts, state_dts, forcing_dts, timer_dts, tstep_s


def _measure_user_feels_traditional(
    df_forcing: pd.DataFrame, df_state_init: pd.DataFrame
) -> float:
    t0 = time.perf_counter()
    run_supy(df_forcing, df_state_init)
    return time.perf_counter() - t0


def _infer_nlayer(config: SUEWSConfig, site_index: int = 0) -> int:
    """Infer nlayer from config's vertical_layers."""
    site = config.sites[site_index] if hasattr(config, "sites") else config.site
    vl = site.properties.vertical_layers
    nlayer = vl.nlayer
    # Handle RefValue wrapper if present
    if hasattr(nlayer, "value"):
        return int(nlayer.value)
    return int(nlayer)


def _measure_user_feels_dts(
    df_forcing: pd.DataFrame, df_state_init: pd.DataFrame, nlayer: int | None, ndepth: int
) -> float:
    t0 = time.perf_counter()
    config = SUEWSConfig.from_df_state(df_state_init)
    if nlayer is None:
        nlayer = _infer_nlayer(config)
    run_dts(df_forcing, config, nlayer=nlayer, ndepth=ndepth)
    return time.perf_counter() - t0


def _measure_kernel_traditional(dict_input: dict) -> float:
    _reset_supy_error()
    t0 = time.perf_counter()
    sd.suews_cal_multitsteps(**dict_input)
    t_kernel = time.perf_counter() - t0
    _check_supy_error()
    return t_kernel


def _prepare_dts_forcing_block(df_forcing: pd.DataFrame) -> np.ndarray:
    """Prepare forcing block array for batch DTS (same format as traditional)."""
    df = _prepare_forcing_traditional(df_forcing)
    # Drop the extra columns not in the 21-column MetForcingBlock
    # The block excludes: metforcingdata_grid, ts5mindata_ir, isec
    cols_21 = [
        "iy", "id", "it", "imin",
        "qn1_obs", "qh_obs", "qe", "qs_obs", "qf_obs",
        "avu1", "avrh", "temp_c", "press_hpa", "precip", "kdown",
        "snowfrac_obs", "ldown_obs", "fcld_obs", "wu_m3", "xsmd", "lai_obs",
    ]
    return np.asfortranarray(df[cols_21].values, dtype=np.float64)


def _measure_kernel_dts(
    df_forcing: pd.DataFrame,
    config: SUEWSConfig,
    nlayer: int | None,
    ndepth: int,
) -> float:
    """Measure DTS kernel time using batch suews_cal_multitsteps_dts."""
    if nlayer is None:
        nlayer = _infer_nlayer(config)

    # Prepare DTS objects (setup - not timed)
    (
        config_dts,
        site_dts,
        state_dts,
        forcing_dts,
        timer_dts,
        tstep_s,
    ) = _prepare_dts_objects(config, df_forcing, nlayer, ndepth)

    # Prepare forcing block and output array (setup - not timed)
    len_sim = len(df_forcing)
    metforcingblock = _prepare_dts_forcing_block(df_forcing)
    ncols_out = int(alloc.ncolumnsdataoutsuews)
    dataoutblock = np.zeros((len_sim, ncols_out), dtype=np.float64, order="F")

    # Initialize timer with first timestep info
    dt_start = df_forcing.index[0]
    populate_timer_from_datetime(
        timer_dts,
        dt_start,
        tstep_s,
        dt_since_start=0,
        startdls=int(site_dts.anthroemis.startdls),
        enddls=int(site_dts.anthroemis.enddls),
        lat=site_dts.lat,
    )

    # Time only the batch Fortran call
    _reset_supy_error()
    t0 = time.perf_counter()
    drv.suews_cal_multitsteps_dts(
        timer_dts,
        metforcingblock,
        len_sim,
        config_dts,
        site_dts,
        state_dts,
        dataoutblock,
    )
    t_kernel = time.perf_counter() - t0
    _check_supy_error()

    return t_kernel


def _repeat(fn, repeats: int, warmup: int) -> List[float]:
    for _ in range(warmup):
        fn()
    return [fn() for _ in range(repeats)]


def _summarize(times: Iterable[float]) -> Tuple[float, float, float]:
    arr = np.array(list(times), dtype=float)
    if arr.size == 0:
        return float("nan"), float("nan"), float("nan")
    if arr.size == 1:
        return float(arr[0]), float(arr[0]), float(arr[0])
    p25, p50, p75 = np.percentile(arr, [25, 50, 75])
    return float(p50), float(p25), float(p75)


def _format_summary(med: float, p25: float, p75: float) -> str:
    if np.isnan(med):
        return "n/a"
    if med == p25 == p75:
        return f"{med:.4f}"
    return f"{med:.4f} (p25 {p25:.4f}, p75 {p75:.4f})"


def _print_table(title: str, rows: List[Tuple]) -> None:
    print("\n" + title)
    headers = ("Timesteps", "Traditional (s)", "DTS (s)", "Speedup")
    data = [headers] + rows
    widths = [max(len(str(row[i])) for row in data) for i in range(len(headers))]
    for i, row in enumerate(data):
        line = " | ".join(f"{str(row[j]):<{widths[j]}}" for j in range(len(headers)))
        print(line)
        if i == 0:
            print("-" * len(line))


def _max_abs_diff(df_a: pd.DataFrame, df_b: pd.DataFrame) -> float:
    a, b = df_a.align(df_b, join="inner", axis=1)
    a, b = a.align(b, join="inner", axis=0)
    if a.empty or b.empty:
        return float("nan")
    a_num = a.select_dtypes(include=[np.number])
    b_num = b.select_dtypes(include=[np.number])
    if a_num.empty or b_num.empty:
        return float("nan")
    diff = (a_num - b_num).to_numpy()
    return float(np.nanmax(np.abs(diff)))


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Benchmark traditional vs DTS paths (user-feels and kernel-only)."
    )
    parser.add_argument(
        "--timesteps",
        type=int,
        nargs="+",
        default=[48, 288, 1000],
        help="List of timesteps to benchmark.",
    )
    parser.add_argument("--repeats", type=int, default=5, help="Number of repeats.")
    parser.add_argument("--warmup", type=int, default=1, help="Warmup runs.")
    parser.add_argument(
        "--nlayer",
        type=int,
        default=None,
        help="Number of layers (default: infer from config).",
    )
    parser.add_argument("--ndepth", type=int, default=5, help="Number of depths.")
    parser.add_argument(
        "--mode",
        choices=["both", "user-feels", "kernel-only"],
        default="both",
        help="Which benchmarks to run.",
    )
    parser.add_argument(
        "--skip-parity",
        action="store_true",
        help="Skip output parity check.",
    )
    args = parser.parse_args()

    df_state_init, df_forcing_full = load_SampleData()

    results_user = []
    results_kernel = []

    for n in args.timesteps:
        df_forcing = _ensure_freq(df_forcing_full.iloc[:n].copy())

        if args.mode in ("both", "user-feels"):
            def _bench_trad():
                return _measure_user_feels_traditional(df_forcing, df_state_init)

            def _bench_dts():
                return _measure_user_feels_dts(
                    df_forcing, df_state_init, args.nlayer, args.ndepth
                )

            trad_times = _repeat(_bench_trad, args.repeats, args.warmup)
            dts_times = _repeat(_bench_dts, args.repeats, args.warmup)

            trad_med, trad_p25, trad_p75 = _summarize(trad_times)
            dts_med, dts_p25, dts_p75 = _summarize(dts_times)
            speedup = trad_med / dts_med if dts_med > 0 else float("nan")

            results_user.append(
                (
                    n,
                    _format_summary(trad_med, trad_p25, trad_p75),
                    _format_summary(dts_med, dts_p25, dts_p75),
                    f"{speedup:.2f}x" if np.isfinite(speedup) else "n/a",
                )
            )

        if args.mode in ("both", "kernel-only"):
            # Prepare once per repeat (outside timed kernel)
            def _bench_trad_kernel():
                dict_input = _prepare_traditional_multitsteps_input(
                    df_state_init, df_forcing
                )
                return _measure_kernel_traditional(dict_input)

            def _bench_dts_kernel():
                config = SUEWSConfig.from_df_state(df_state_init)
                return _measure_kernel_dts(
                    df_forcing, config, args.nlayer, args.ndepth
                )

            trad_times = _repeat(_bench_trad_kernel, args.repeats, args.warmup)
            dts_times = _repeat(_bench_dts_kernel, args.repeats, args.warmup)

            trad_med, trad_p25, trad_p75 = _summarize(trad_times)
            dts_med, dts_p25, dts_p75 = _summarize(dts_times)
            speedup = trad_med / dts_med if dts_med > 0 else float("nan")

            results_kernel.append(
                (
                    n,
                    _format_summary(trad_med, trad_p25, trad_p75),
                    _format_summary(dts_med, dts_p25, dts_p75),
                    f"{speedup:.2f}x" if np.isfinite(speedup) else "n/a",
                )
            )

        if not args.skip_parity and args.mode in ("both", "user-feels"):
            # Parity check (single run)
            df_trad, _ = run_supy(df_forcing, df_state_init)
            config = SUEWSConfig.from_df_state(df_state_init)
            nlayer_eff = args.nlayer if args.nlayer is not None else _infer_nlayer(config)
            df_dts, _ = run_dts(df_forcing, config, nlayer=nlayer_eff, ndepth=args.ndepth)
            max_diff = _max_abs_diff(df_trad, df_dts)
            print(f"\nParity check (n={n}): max abs diff = {max_diff:.3e}")

    if args.mode in ("both", "user-feels"):
        _print_table("User-feels (end-to-end)", results_user)

    if args.mode in ("both", "kernel-only"):
        _print_table("Kernel-only (Fortran calls only)", results_kernel)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
