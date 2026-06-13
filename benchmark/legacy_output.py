"""Parse a legacy SUEWS ``*_SUEWS_<res>.txt`` output and summarise it.

Pure, dependency-light (pandas + numpy), no supy import, no network. The legacy
SUEWS binaries emit a whitespace-delimited table whose first columns are always
``Year DOY Hour Min Dectime`` followed by named flux columns. Column *count*
drifted across the lineage (73 columns in the oldest era, 87 by ``2020a`` as
fluxes were added), so this module selects fluxes **by header name**, never by
position -- the same code then handles every era.

Two products:

  * a **raw per-flux summary** (mean / min / max / n) for ``QN, QH, QE, Kup,
    Lup``, computed full-period and per meteorological season (DJF/MAM/JJA/SON).
    This needs no observations -- it just characterises what the binary
    produced.
  * an **output fingerprint**: a SHA-256 over the canonical numeric content of
    the parsed table (NaN-normalised, fixed float formatting), so two runs of
    the same binary on the same input can be checked bit-identical regardless
    of incidental whitespace.

An optional obs-based path (``stats_against_obs``) reuses ``bench_stats`` to
emit the modern ``stats.json`` MAE/MBE/n structure when an observation frame is
supplied. The Phase-A legacy pass leaves obs absent (the KCL observations are
on a restricted store and the legacy forcing carries -999 obs columns), so the
raw summary + fingerprint is the deliverable; the obs path exists for later
reuse.

Fortran invalid tokens (``******``, ``NaN``, ``Inf``) and the SUEWS -999
missing sentinel are all coerced to NaN before summarising.
"""
from __future__ import annotations

import hashlib
import json
from pathlib import Path

import numpy as np
import pandas as pd

# Canonical time columns (after renaming) that lead every legacy output.
TIME_COLS = ["Year", "DOY", "Hour", "Min"]

# Flux columns to summarise, in the modern benchmark's flux vocabulary. The
# later-era headers already use exactly these names (Kup, Lup, QN, QH, QE); the
# 2016a-era header uses lowercase (kup, lup, qn, ...) -- both map here.
SUMMARY_FLUXES = ["QN", "QH", "QE", "Kup", "Lup"]

# Header-vocabulary normalisation. Two output formats exist across the lineage:
#   * 2016a-era (80-col, '%'-prefixed lowercase header): time = iy/id/it/imin,
#     fluxes = kdown/kup/ldown/lup/qn/qh/qe.
#   * 2017b+ era (73/87-col): time = Year/DOY/Hour/Min, fluxes = QN/QH/QE/Kup/Lup.
# We map every recognised legacy spelling onto the canonical names above so the
# summary code is era-agnostic. Matching is case-insensitive (see _canon_columns).
_TIME_ALIASES = {
    "year": "Year", "iy": "Year",
    "doy": "DOY", "id": "DOY", "dy": "DOY",
    "hour": "Hour", "it": "Hour",
    "min": "Min", "imin": "Min",
}
_FLUX_ALIASES = {
    "qn": "QN", "qstar": "QN", "q*": "QN",
    "qh": "QH", "h": "QH",
    "qe": "QE", "e": "QE",
    "kup": "Kup",
    "lup": "Lup",
    "kdown": "Kdown", "kdn": "Kdown",
    "ldown": "Ldown", "ldn": "Ldown",
}

MISSING = -999.0

_SEASON = {12: "DJF", 1: "DJF", 2: "DJF",
           3: "MAM", 4: "MAM", 5: "MAM",
           6: "JJA", 7: "JJA", 8: "JJA",
           9: "SON", 10: "SON", 11: "SON"}


def _clean_numeric(df: pd.DataFrame) -> pd.DataFrame:
    """Coerce every column to float, mapping Fortran junk + -999 to NaN.

    ``pd.read_csv`` already maps the listed ``na_values`` to NaN on read; this
    is the belt-and-braces pass that also catches the -999 sentinel and any
    stray non-numeric token (e.g. a stringified ``******`` that slipped
    through) so downstream stats never see a sentinel as a real value.
    """
    out = df.apply(pd.to_numeric, errors="coerce")
    return out.mask(out == MISSING)


def _canon_columns(columns: list[str]) -> list[str]:
    """Map a raw legacy header onto canonical time + flux names.

    Strips a leading ``%`` (the 2016a header marker), then renames any
    recognised legacy spelling (case-insensitively) to its canonical form.
    Unrecognised columns are passed through verbatim (still available by their
    original name), so this only *adds* canonical aliases for the columns we
    summarise -- it never drops information.
    """
    out: list[str] = []
    seen: set[str] = set()
    for raw in columns:
        name = raw.strip().lstrip("%").strip()
        key = name.lower()
        canon = _TIME_ALIASES.get(key) or _FLUX_ALIASES.get(key) or name
        # Guard against a header that already carries both spellings (e.g. a
        # later format with a stray lowercase duplicate): keep the first.
        if canon in seen and canon != name:
            canon = name
        seen.add(canon)
        out.append(canon)
    return out


def read_legacy_output(path: str | Path) -> pd.DataFrame:
    """Read a legacy SUEWS output table into a DatetimeIndex-ed frame.

    Handles both lineage formats by header *name*, never position:
      * 2016a-era ``<code><grid>_<year>_<res>.txt`` ('%'-prefixed lowercase
        header, time = iy/id/it/imin, fluxes = kdown/kup/.../qn/qh/qe);
      * 2017b+ ``*_SUEWS_<res>.txt`` (time = Year/DOY/Hour/Min, fluxes =
        QN/QH/QE/Kup/Lup).
    The index is reconstructed from Year + day-of-year + Hour + Min (SUEWS
    writes day-of-year, not calendar month/day). Flux and time columns are
    numeric; ``******`` / ``NaN`` / ``Inf`` / -999 become NaN.
    """
    path = Path(path)
    # whitespace-delimited; Fortran overflow/invalid tokens -> NaN on read.
    df = pd.read_csv(
        path,
        sep=r"\s+",
        na_values=["******", "*******", "********", "NaN", "Inf", "-Inf", "Infinity"],
        engine="python",
    )
    df.columns = _canon_columns(list(df.columns))
    missing_time = [c for c in TIME_COLS if c not in df.columns]
    if missing_time:
        raise ValueError(
            f"{path.name}: missing expected time column(s) {missing_time}; "
            f"header was {list(df.columns)[:8]}..."
        )
    df = _clean_numeric(df)

    # Build a datetime index from Year + day-of-year + Hour + Min.
    year = df["Year"].astype("Int64")
    doy = df["DOY"].astype("Int64")
    hour = df["Hour"].astype("Int64")
    minute = df["Min"].astype("Int64")
    if year.isna().any() or doy.isna().any():
        raise ValueError(f"{path.name}: non-integer Year/DOY in time columns")
    idx = (
        pd.to_datetime(year.astype(int).astype(str) + "-01-01")
        + pd.to_timedelta(doy.astype(int) - 1, unit="D")
        + pd.to_timedelta(hour.astype(int), unit="h")
        + pd.to_timedelta(minute.astype(int), unit="m")
    )
    df.index = pd.DatetimeIndex(idx, name="datetime")
    return df


def _flux_block(series: pd.Series) -> dict:
    """mean / min / max / n for one flux series, NaN-safe."""
    s = pd.to_numeric(series, errors="coerce")
    valid = s[np.isfinite(s)]
    n = int(valid.shape[0])
    if n == 0:
        return {"mean": None, "min": None, "max": None, "n": 0}
    return {
        "mean": round(float(valid.mean()), 4),
        "min": round(float(valid.min()), 4),
        "max": round(float(valid.max()), 4),
        "n": n,
    }


def raw_flux_summary(df: pd.DataFrame) -> dict:
    """Raw per-flux summary: full-period + seasonal mean/min/max/n.

    Structure:
        {flux: {"full": {...}, "seasonal": {"DJF": {...}, "MAM": {...}, ...}}}

    Only fluxes present in the frame are summarised; a missing flux column is
    silently skipped (older eras may lack a few of the five).
    """
    seasons = np.array([_SEASON[m] for m in df.index.month])
    out: dict = {}
    for flux in SUMMARY_FLUXES:
        if flux not in df.columns:
            continue
        block = {"full": _flux_block(df[flux]), "seasonal": {}}
        for season in ["DJF", "MAM", "JJA", "SON"]:
            sel = seasons == season
            if bool(sel.any()):
                block["seasonal"][season] = _flux_block(df[flux].to_numpy()[sel])
        out[flux] = block
    return out


def output_fingerprint(df: pd.DataFrame) -> str:
    """Deterministic SHA-256 over the parsed numeric content.

    Every cell is rendered with fixed formatting (NaN -> ``'nan'``, finite ->
    ``%.6f``) in a canonical column order, then hashed. Two binary runs that
    differ only by trailing whitespace or float-printing width still hash
    identically; any genuine numeric difference changes the hash.
    """
    cols = sorted(df.columns)
    h = hashlib.sha256()
    h.update((";".join(cols) + "\n").encode("ascii"))
    for ts, row in zip(df.index, df[cols].to_numpy()):
        parts = [str(int(ts.value))]  # ns since epoch, integer + exact
        for v in row:
            parts.append("nan" if not np.isfinite(v) else f"{float(v):.6f}")
        h.update((",".join(parts) + "\n").encode("ascii"))
    return h.hexdigest()


def file_sha256(path: str | Path) -> str:
    """SHA-256 of the raw output file bytes (byte-level determinism check)."""
    h = hashlib.sha256()
    h.update(Path(path).read_bytes())
    return h.hexdigest()


def summarise_output(path: str | Path) -> dict:
    """Parse one legacy output and return the raw summary + fingerprints.

    Returns a JSON-serialisable dict:
        {
          "fluxes": [...present...],
          "period": {"start": ..., "end": ...},
          "n_rows": int,
          "raw_summary": {flux: {"full":..., "seasonal":...}},
          "output_fingerprint": <sha256 over parsed numerics>,
          "file_sha256": <sha256 over raw bytes>,
        }
    """
    df = read_legacy_output(path)
    summary = raw_flux_summary(df)
    return {
        "fluxes": list(summary.keys()),
        "period": {
            "start": df.index.min().strftime("%Y-%m-%d %H:%M"),
            "end": df.index.max().strftime("%Y-%m-%d %H:%M"),
        },
        "n_rows": int(df.shape[0]),
        "raw_summary": summary,
        "output_fingerprint": output_fingerprint(df),
        "file_sha256": file_sha256(path),
    }


# --- optional obs-based path (reuses bench_stats; unused in the Phase-A pass) --

def stats_against_obs(df: pd.DataFrame, obs: pd.DataFrame) -> dict:
    """Obs-based MAE/MBE/n in the modern ``stats.json`` shape.

    ``obs`` must be a DatetimeIndex-ed frame carrying observation columns named
    per ``bench_stats.OBS_COL`` (e.g. 'qh', 'qe', ...). Returns the nested
    flux -> interval -> bucket -> {MAE, MBE, n} dict plus a stats fingerprint.
    Only called when real observations are available; the legacy Phase-A pass
    does NOT call this (obs are unavailable -> raw summary is emitted instead).
    """
    import bench_stats  # local import: keep the pure path import-light

    pairs = pd.DataFrame(index=df.index)
    aligned_obs = obs.reindex(df.index)
    for flux in bench_stats.FLUXES:
        obs_col = bench_stats.OBS_COL[flux]
        if flux not in df.columns or obs_col not in aligned_obs.columns:
            pairs[f"{flux}_mod"] = np.nan
            pairs[f"{flux}_obs"] = np.nan
            continue
        pairs[f"{flux}_mod"] = pd.to_numeric(df[flux], errors="coerce")
        pairs[f"{flux}_obs"] = pd.to_numeric(aligned_obs[obs_col], errors="coerce")
    tab = bench_stats.compute_stats(pairs)
    return {
        "stats": bench_stats.to_nested_json(tab),
        "fingerprint": bench_stats.fingerprint(tab),
    }


def to_json(summary: dict, path: str | Path) -> None:
    """Write a summary dict to ``path`` as pretty, sorted JSON."""
    Path(path).write_text(
        json.dumps(summary, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
