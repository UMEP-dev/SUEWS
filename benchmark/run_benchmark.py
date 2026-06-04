"""Driver for the SUEWS multi-version reproducibility benchmark proof.

Runs ONE pinned supy version (whichever interpreter executes this script):
  1. Load the KCL/Ward config (schema-appropriate; see provenance).
  2. Attach the shared canonical forcing.
  3. Run the FIXED period 2011-01-01..2013-12-31.
  4. Resample model output (5-min) to hourly aligned to the obs (end-of-hour).
  5. Compute MAE/MBE/n vs obs for Kup,Lup,QN,QH,QE over four intervals using
     the single vendored stats module (bench_stats).
  6. Run TWICE; assert the rounded-stats SHA-256 fingerprint matches.
  7. Write stats.json, provenance.json, freeze.txt under results/<version>/.

Determinism env (single-threaded BLAS) is pinned by the caller. Obs come from
a local resolved copy (verified byte-identical to restricted Zenodo 508375).

Usage:
  <venv>/bin/python run_benchmark.py --version <ver> --obs <path> \
        --forcing inputs/Kc1_forcing_canonical.txt \
        --config inputs/UK-LO-KCL_Ward.yaml --freeze <freeze.txt>
"""
from __future__ import annotations

import argparse
import hashlib
import json
import platform
import sys
import traceback
import warnings
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import pandas as pd

import bench_stats
import bench_rsl

warnings.filterwarnings("ignore")

PERIOD_START = "2011-01-01"
PERIOD_END = "2013-12-31"


def sha256_file(path: str) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def load_obs(path: str) -> pd.DataFrame:
    obs = pd.read_csv(path, parse_dates=["DateTime"]).set_index("DateTime")
    obs = obs.loc[PERIOD_START:f"{PERIOD_END} 23:59:59"]
    return obs


def _run_via_engine(sim) -> pd.DataFrame:
    """Bypass a broken SUEWSSimulation.run() wrapper by calling the engine
    directly. run_supy_ser returns a 2-, 3- or 4-tuple across releases; the
    model output is always the first element."""
    from supy._run import run_supy_ser

    out = run_supy_ser(sim._df_forcing, sim._df_state_init)
    return out[0] if isinstance(out, tuple) else out


def run_once(config_path: str, forcing_path: str) -> pd.DataFrame:
    """Run supy once, return model hourly DataFrame indexed by end-of-hour."""
    from supy import SUEWSSimulation

    sim = SUEWSSimulation(config_path)
    sim.update_forcing(forcing_path)
    df = None
    try:
        # newer releases accept an explicit date range
        sim.run(start_date=PERIOD_START, end_date=PERIOD_END)
    except TypeError:
        # older releases (e.g. 2025.7.x) have no date kwargs; the forcing already
        # spans exactly the fixed period, so a plain run() is equivalent.
        try:
            sim.run()
        except ValueError:
            df = _run_via_engine(sim)
    except ValueError:
        # 2025.7.6's SUEWSSimulation.run() unpacks 2 values from run_supy_ser,
        # which returns 4 in that release -- a wrapper bug, not an engine fault.
        # Bypass the wrapper and call the engine directly.
        df = _run_via_engine(sim)
    if df is None:
        res = sim.results
        df = res.to_dataframe() if hasattr(res, "to_dataframe") else res
    # take the SUEWS output group, single grid
    suews = df.xs("SUEWS", axis=1, level=0)
    if df.index.nlevels > 1:
        grid0 = df.index.get_level_values(0).unique()[0]
        suews = suews.xs(grid0, axis=0, level=0)
    suews = suews[bench_stats.FLUXES]
    # 5-min -> hourly, end-of-hour label to match obs (timestamp = period end)
    hourly = suews.resample("1h", label="right", closed="right").mean()
    return hourly, df


def rsl_model_T(df: pd.DataFrame, heights) -> pd.DataFrame:
    """Model air temperature interpolated from the RSL profile to the obs
    heights, hourly mean. Additive to the EB path; does not affect EB stats."""
    rsl = df.xs("RSL", axis=1, level=0)
    if df.index.nlevels > 1:
        grid0 = df.index.get_level_values(0).unique()[0]
        rsl = rsl.xs(grid0, axis=0, level=0)
    return bench_rsl.interpolate_heights(rsl, heights).resample("1h").mean()


def build_pairs(model_hourly: pd.DataFrame, obs: pd.DataFrame) -> pd.DataFrame:
    """Align model and obs on common timestamps; return <flux>_mod/<flux>_obs."""
    common = model_hourly.index.intersection(obs.index)
    pairs = pd.DataFrame(index=common)
    for flux in bench_stats.FLUXES:
        pairs[f"{flux}_mod"] = model_hourly.loc[common, flux]
        pairs[f"{flux}_obs"] = obs.loc[common, bench_stats.OBS_COL[flux]]
    return pairs


def compute_fingerprint(config_path, forcing_path, obs, rsl_obs=None):
    """Returns (eb_fp, eb_tab, rsl_fp, rsl_tab). RSL is computed from the SAME
    model run when rsl_obs is given; otherwise rsl_fp/rsl_tab are None."""
    model_hourly, df = run_once(config_path, forcing_path)
    pairs = build_pairs(model_hourly, obs)
    tab = bench_stats.compute_stats(pairs)
    fp = bench_stats.fingerprint(tab)
    rsl_fp = rsl_tab = None
    if rsl_obs is not None:
        # RSL is additive: a failure here (e.g. an older release with a different
        # RSL output shape) must NOT invalidate the energy-balance result.
        try:
            modT = rsl_model_T(df, bench_rsl.HEIGHTS)
            rsl_tab = bench_rsl.compute_rsl_stats(modT, rsl_obs)
            rsl_fp = bench_rsl.fingerprint(rsl_tab)
        except Exception:
            rsl_fp = rsl_tab = None
    return fp, tab, rsl_fp, rsl_tab


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--version", required=True)
    ap.add_argument("--config", required=True)
    ap.add_argument("--forcing", required=True)
    ap.add_argument("--obs", required=True)
    ap.add_argument("--obs-source", default="zenodo:508375/Kc1_Obs.csv (restricted sandbox)")
    ap.add_argument("--rsl-obs", default=None,
                    help="optional df_Ta_comb.h5 (RSL air-temperature obs); enables the RSL axis")
    ap.add_argument("--freeze", required=True)
    ap.add_argument("--outdir", required=True)
    args = ap.parse_args()

    outdir = Path(args.outdir)
    outdir.mkdir(parents=True, exist_ok=True)

    import supy

    schema_version = None
    try:
        from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION
        schema_version = CURRENT_SCHEMA_VERSION
    except Exception:
        pass

    obs_hash = sha256_file(args.obs)
    forcing_hash = sha256_file(args.forcing)
    config_hash = sha256_file(args.config)
    stats_mod_hash = sha256_file(str(Path(bench_stats.__file__)))

    plat = {
        "os": platform.platform(),
        "machine": platform.machine(),
        "python": platform.python_version(),
        "numpy": np.__version__,
        "pandas": pd.__version__,
        "supy": supy.__version__,
    }

    prov = {
        "version": args.version,
        "supy_version": supy.__version__,
        "schema_version": schema_version,
        "config_schema_on_load": None,
        "obs_source": args.obs_source,
        "obs_hash": obs_hash,
        "forcing_hash": forcing_hash,
        "forcing_note": "converted to 24-col canonical SUEWS forcing (3 LAI cols -> 1, all -999; isec dropped)",
        "config_hash": config_hash,
        "stats_module_hash": stats_mod_hash,
        "period": {"start": PERIOD_START, "end": PERIOD_END},
        "platform": plat,
        "run_timestamp_utc": datetime.now(timezone.utc).isoformat(),
        "status": None,
        "fingerprint": None,
        "fingerprint_rerun": None,
        "reproducible": None,
        "rsl_obs_hash": None,
        "rsl_fingerprint": None,
        "rsl_fingerprint_rerun": None,
        "rsl_reproducible": None,
        "error": None,
    }

    try:
        obs = load_obs(args.obs)
        rsl_obs = None
        if args.rsl_obs:
            _obs_T = bench_rsl.load_obs_T(args.rsl_obs)
            # Mask the -999 sentinel BEFORE the hourly mean: resampling would
            # otherwise average it with valid half-hour samples into a bogus
            # finite temperature that the exact-(-999) pair mask later misses.
            _obs_T = _obs_T.where(_obs_T != bench_rsl.MISSING)
            rsl_obs = _obs_T.resample("1h").mean()
            prov["rsl_obs_hash"] = sha256_file(args.rsl_obs)
            prov["rsl_stats_module_hash"] = sha256_file(str(Path(bench_rsl.__file__)))
        # record schema recorded on the loaded config
        try:
            from supy import SUEWSSimulation
            _sim = SUEWSSimulation(args.config)
            prov["config_schema_on_load"] = getattr(_sim.config, "schema_version", None)
            del _sim
        except Exception:
            pass

        fp1, tab1, rfp1, rtab1 = compute_fingerprint(args.config, args.forcing, obs, rsl_obs)
        fp2, tab2, rfp2, rtab2 = compute_fingerprint(args.config, args.forcing, obs, rsl_obs)

        prov["fingerprint"] = fp1
        prov["fingerprint_rerun"] = fp2
        prov["reproducible"] = bool(fp1 == fp2)
        prov["rsl_fingerprint"] = rfp1
        prov["rsl_fingerprint_rerun"] = rfp2
        # An asymmetric outcome (one run yields an RSL fingerprint, the
        # other does not) is itself non-reproducible: count the axis as
        # produced if EITHER run yielded a fingerprint, so the rsl_ok check
        # below catches the mismatch instead of silently passing.
        rsl_produced = (rsl_obs is not None) and (rfp1 is not None or rfp2 is not None)
        rsl_ok = (rfp1 == rfp2) if rsl_produced else True
        prov["rsl_reproducible"] = bool(rfp1 == rfp2) if rsl_produced else None
        if rsl_obs is not None and not rsl_produced:
            prov["rsl_note"] = "RSL obs supplied but no RSL stats produced (no usable RSL profile in this release's output)"
        prov["status"] = "ok" if (fp1 == fp2 and rsl_ok) else "failed"
        if fp1 != fp2 or not rsl_ok:
            prov["error"] = "fingerprint mismatch between two runs in the same pinned env"

        stats_json = {
            "version": args.version,
            "supy_version": supy.__version__,
            "period": {"start": PERIOD_START, "end": PERIOD_END},
            "fluxes": bench_stats.FLUXES,
            "intervals": bench_stats.INTERVALS,
            "fingerprint": fp1,
            "stats": bench_stats.to_nested_json(tab1),
        }
        if rtab1 is not None:
            stats_json["rsl"] = {
                "heights": bench_rsl.HEIGHTS,
                "windows": bench_rsl.WINDOWS,
                "fingerprint": rfp1,
                "stats": bench_rsl.to_nested_json(rtab1),
            }
        (outdir / "stats.json").write_text(json.dumps(stats_json, indent=2, sort_keys=True))
        tab1.to_csv(outdir / "stats_table.csv", index=False)
        if rtab1 is not None:
            rtab1.to_csv(outdir / "rsl_table.csv", index=False)

    except Exception as e:
        prov["status"] = "failed"
        prov["error"] = f"{type(e).__name__}: {e}\n{traceback.format_exc()}"

    (outdir / "provenance.json").write_text(json.dumps(prov, indent=2, sort_keys=True))
    # freeze is written by the caller; just confirm path here
    print(f"[{args.version}] status={prov['status']} "
          f"eb_fp={(prov['fingerprint'] or '-')[:12]} eb_repro={prov['reproducible']} "
          f"rsl_fp={(prov['rsl_fingerprint'] or '-')[:12]} rsl_repro={prov['rsl_reproducible']}")
    if prov["error"]:
        print(prov["error"][:600], file=sys.stderr)
    return 0 if prov["status"] == "ok" else 1


if __name__ == "__main__":
    raise SystemExit(main())
