"""Vendored RSL air-temperature statistics for the SUEWS multi-version benchmark.

Companion to bench_stats.py (energy-balance fluxes). This module scores the
modelled near-surface air-temperature PROFILE against tower observations at the
measurement heights, following the esmae method: the model's RSL vertical
profile (z_1.., T_1.. columns) is linearly interpolated to each observation
height, then MAE/MBE are computed against the observed T at that height.

Definitions match bench_stats (model minus observation; MISSING = -999 -> NaN;
a sample counts only when both model and obs are finite).

Intervals (RSL obs cover two 2013 measurement windows):
    full   : one row, all valid pairs in both windows pooled.
    window : per measurement window (the two campaigns below).

Output is a tidy DataFrame [height, interval, bucket, stat, value] rounded to
2 dp, serialised deterministically and SHA-256 hashed for the fingerprint.
Dependency-light (numpy + pandas); do NOT import supy here.
"""
from __future__ import annotations

import hashlib
import json

import numpy as np
import pandas as pd

MISSING = -999.0
# observation heights (m) scored, per Ward/KCL setup
HEIGHTS = [6.5, 12.5, 16.0]
# the two 2013 measurement windows (inclusive)
WINDOWS = {
    "Jan-Mar": ("2013-01-01", "2013-03-18"),
    "Apr-Jun": ("2013-04-28", "2013-06-27"),
}
INTERVALS = ["full", "window"]


def interpolate_heights(rsl: pd.DataFrame, heights=HEIGHTS, var: str = "T") -> pd.DataFrame:
    """Per-row linear interpolation of the model T-profile to target heights.

    Port of esmae.tools.interpolate_heights: z-levels may vary per row. `rsl`
    has columns z_1.., <var>_1.. (the RSL output group). Returns a DataFrame
    indexed like `rsl` with one column per target height.
    """
    z_cols = sorted([c for c in rsl.columns if str(c).startswith("z_")],
                    key=lambda x: int(str(x).split("_")[1]))
    var_cols = [f"{var}_{int(str(c).split('_')[1])}" for c in z_cols]
    z = rsl[z_cols].to_numpy(dtype=float)
    v = rsl[var_cols].to_numpy(dtype=float)
    n_rows, n_levels = z.shape
    out = pd.DataFrame(index=rsl.index)
    for h in heights:
        col = np.full(n_rows, np.nan, dtype=float)
        iu = np.array([np.searchsorted(z[i], h, side="right") for i in range(n_rows)])
        il = iu - 1
        ok = np.where((il >= 0) & (iu < n_levels))[0]
        if ok.size:
            zl, zh = z[ok, il[ok]], z[ok, iu[ok]]
            vl, vh = v[ok, il[ok]], v[ok, iu[ok]]
            col[ok] = vl + (h - zl) * (vh - vl) / (zh - zl)
        out[h] = col
    return out


def load_obs_T(h5_path: str, heights=HEIGHTS) -> pd.DataFrame:
    """Observed air temperature at the measurement heights from df_Ta_comb.h5.

    Columns are a (height, {'obs','sim'}) MultiIndex; we take the 'obs' side.
    """
    obs = pd.read_hdf(h5_path)
    return pd.DataFrame({h: obs[(h, "obs")] for h in heights})


def _pair_metrics(model: pd.Series, obs: pd.Series):
    m = pd.to_numeric(model, errors="coerce").mask(lambda s: s == MISSING)
    o = pd.to_numeric(obs, errors="coerce").mask(lambda s: s == MISSING)
    mask = np.isfinite(m) & np.isfinite(o)
    n = int(mask.sum())
    if n == 0:
        return float("nan"), float("nan"), 0
    err = (m[mask] - o[mask]).to_numpy()
    return float(np.mean(np.abs(err))), float(np.mean(err)), n


def _window_mask(idx: pd.DatetimeIndex, win) -> np.ndarray:
    a, b = win
    return (idx >= a) & (idx <= f"{b} 23:59:59")


def compute_rsl_stats(modT: pd.DataFrame, obsT: pd.DataFrame) -> pd.DataFrame:
    """modT/obsT: hourly DataFrames indexed by time, one column per height.

    Returns tidy DataFrame [height, interval, bucket, stat, value] (2 dp).
    """
    common = modT.index.intersection(obsT.index)
    modT, obsT = modT.loc[common], obsT.loc[common]
    rows = []
    for h in HEIGHTS:
        m, o = modT[h], obsT[h]
        any_win = np.zeros(len(common), dtype=bool)
        for name, win in WINDOWS.items():
            sel = _window_mask(common, win)
            any_win |= sel
            mae, mbe, n = _pair_metrics(m[sel], o[sel])
            rows.append((h, "window", name, "MAE", mae))
            rows.append((h, "window", name, "MBE", mbe))
            rows.append((h, "window", name, "n", float(n)))
        mae, mbe, n = _pair_metrics(m[any_win], o[any_win])
        rows.append((h, "full", "all", "MAE", mae))
        rows.append((h, "full", "all", "MBE", mbe))
        rows.append((h, "full", "all", "n", float(n)))
    tab = pd.DataFrame(rows, columns=["height", "interval", "bucket", "stat", "value"])
    tab["value"] = tab["value"].round(2)
    tab = tab.sort_values(["height", "interval", "bucket", "stat"]).reset_index(drop=True)
    return tab


def fingerprint(tab: pd.DataFrame) -> str:
    records = []
    for _, r in tab.iterrows():
        v = r["value"]
        vs = "nan" if (isinstance(v, float) and np.isnan(v)) else f"{float(v):.2f}"
        records.append([f"{float(r['height']):.1f}", r["interval"], r["bucket"], r["stat"], vs])
    records.sort()
    blob = json.dumps(records, separators=(",", ":"), ensure_ascii=True)
    return hashlib.sha256(blob.encode("ascii")).hexdigest()


def to_nested_json(tab: pd.DataFrame) -> dict:
    """Nested dict keyed height(str) -> interval -> bucket -> {MAE, MBE, n}."""
    out: dict = {}
    for _, r in tab.iterrows():
        v = r["value"]
        v = None if (isinstance(v, float) and np.isnan(v)) else float(v)
        if r["stat"] == "n" and v is not None:
            v = int(v)
        key = f"{float(r['height']):.1f}"
        out.setdefault(key, {}).setdefault(r["interval"], {}).setdefault(r["bucket"], {})[r["stat"]] = v
    return out
