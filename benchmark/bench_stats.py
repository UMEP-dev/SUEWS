"""Vendored, version-neutral statistics for the SUEWS multi-version benchmark.

ONE shared definition of MAE, MBE and n, used identically by every supy
version under test so the statistic definitions are fixed and cannot drift.
Deliberately tiny and dependency-light (pandas + numpy only). Do NOT import
supy here -- this module must behave identically regardless of which pinned
supy environment imports it.

Definitions (model minus observation convention):
    error_i = model_i - obs_i
    MAE = mean(|error_i|)
    MBE = mean(error_i)            (positive => model over-predicts)
    n   = count of valid paired samples

Missing handling:
    - Observation missing flag is -999 (SUEWS convention); treated as NaN.
    - A sample contributes only when BOTH model and obs are finite.

Intervals (all over the FIXED evaluation period passed in by the driver):
    full      : one row, all valid pairs pooled.
    seasonal  : meteorological seasons DJF, MAM, JJA, SON.
    monthly   : calendar month 1..12 (pooled across years).
    daily     : day-of-year 1..366 (pooled across years).

The output table is a tidy DataFrame with columns:
    [flux, interval, bucket, stat, value]
rounded to 2 dp, then serialised deterministically and SHA-256 hashed to give
the reproducibility fingerprint.
"""
from __future__ import annotations

import hashlib
import json

import numpy as np
import pandas as pd

MISSING = -999.0
FLUXES = ["Kup", "Lup", "QN", "QH", "QE"]
INTERVALS = ["full", "seasonal", "monthly", "daily"]

# obs column name (lower-case) for each model flux name
OBS_COL = {"Kup": "kup", "Lup": "lup", "QN": "qn", "QH": "qh", "QE": "qe"}

_SEASON = {12: "DJF", 1: "DJF", 2: "DJF",
           3: "MAM", 4: "MAM", 5: "MAM",
           6: "JJA", 7: "JJA", 8: "JJA",
           9: "SON", 10: "SON", 11: "SON"}


def _pair_metrics(model: pd.Series, obs: pd.Series) -> tuple[float, float, int]:
    """MAE, MBE, n for one aligned (model, obs) pair, NaN-safe."""
    m = pd.to_numeric(model, errors="coerce")
    o = pd.to_numeric(obs, errors="coerce")
    o = o.mask(o == MISSING)
    m = m.mask(m == MISSING)
    mask = np.isfinite(m) & np.isfinite(o)
    n = int(mask.sum())
    if n == 0:
        return float("nan"), float("nan"), 0
    err = (m[mask] - o[mask]).to_numpy()
    mae = float(np.mean(np.abs(err)))
    mbe = float(np.mean(err))
    return mae, mbe, n


def _bucket_keys(idx: pd.DatetimeIndex, interval: str):
    if interval == "full":
        return pd.Index(["all"] * len(idx))
    if interval == "seasonal":
        return pd.Index([_SEASON[m] for m in idx.month])
    if interval == "monthly":
        return pd.Index([f"{m:02d}" for m in idx.month])
    if interval == "daily":
        return pd.Index([f"{d:03d}" for d in idx.dayofyear])
    raise ValueError(f"unknown interval {interval!r}")


def compute_stats(df_pairs: pd.DataFrame) -> pd.DataFrame:
    """df_pairs: DatetimeIndex, columns '<flux>_mod' and '<flux>_obs' per flux.

    Returns tidy DataFrame [flux, interval, bucket, stat, value] (2 dp).
    """
    idx = df_pairs.index
    rows = []
    for flux in FLUXES:
        mod = df_pairs[f"{flux}_mod"]
        obs = df_pairs[f"{flux}_obs"]
        for interval in INTERVALS:
            keys = _bucket_keys(idx, interval)
            for bucket in sorted(pd.unique(keys)):
                sel = keys == bucket
                mae, mbe, n = _pair_metrics(mod[sel], obs[sel])
                rows.append((flux, interval, str(bucket), "MAE", mae))
                rows.append((flux, interval, str(bucket), "MBE", mbe))
                rows.append((flux, interval, str(bucket), "n", float(n)))
    tab = pd.DataFrame(rows, columns=["flux", "interval", "bucket", "stat", "value"])
    # round to 2 dp; n stays integral but is stored as float for uniformity
    tab["value"] = tab["value"].round(2)
    tab = tab.sort_values(["flux", "interval", "bucket", "stat"]).reset_index(drop=True)
    return tab


def fingerprint(tab: pd.DataFrame) -> str:
    """Deterministic SHA-256 of the rounded(2dp) stats table.

    Serialised as a canonical sorted list of records with fixed float
    formatting so the hash is byte-identical across runs and platforms.
    NaN is rendered as the literal string 'nan'.
    """
    records = []
    for _, r in tab.iterrows():
        v = r["value"]
        vs = "nan" if (isinstance(v, float) and np.isnan(v)) else f"{float(v):.2f}"
        records.append([r["flux"], r["interval"], r["bucket"], r["stat"], vs])
    records.sort()
    blob = json.dumps(records, separators=(",", ":"), ensure_ascii=True)
    return hashlib.sha256(blob.encode("ascii")).hexdigest()


def to_nested_json(tab: pd.DataFrame) -> dict:
    """Nested dict keyed flux -> interval -> bucket -> {MAE, MBE, n}."""
    out: dict = {}
    for _, r in tab.iterrows():
        v = r["value"]
        v = None if (isinstance(v, float) and np.isnan(v)) else float(v)
        if r["stat"] == "n" and v is not None:
            v = int(v)
        out.setdefault(r["flux"], {}).setdefault(r["interval"], {}).setdefault(
            r["bucket"], {}
        )[r["stat"]] = v
    return out
