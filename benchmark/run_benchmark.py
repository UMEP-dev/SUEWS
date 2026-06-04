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
    return hourly


def build_pairs(model_hourly: pd.DataFrame, obs: pd.DataFrame) -> pd.DataFrame:
    """Align model and obs on common timestamps; return <flux>_mod/<flux>_obs."""
    common = model_hourly.index.intersection(obs.index)
    pairs = pd.DataFrame(index=common)
    for flux in bench_stats.FLUXES:
        pairs[f"{flux}_mod"] = model_hourly.loc[common, flux]
        pairs[f"{flux}_obs"] = obs.loc[common, bench_stats.OBS_COL[flux]]
    return pairs


def compute_fingerprint(config_path, forcing_path, obs) -> tuple[str, pd.DataFrame]:
    model_hourly = run_once(config_path, forcing_path)
    pairs = build_pairs(model_hourly, obs)
    tab = bench_stats.compute_stats(pairs)
    fp = bench_stats.fingerprint(tab)
    return fp, tab


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--version", required=True)
    ap.add_argument("--config", required=True)
    ap.add_argument("--forcing", required=True)
    ap.add_argument("--obs", required=True)
    ap.add_argument("--obs-source", default="zenodo:508375/Kc1_Obs.csv (restricted sandbox)")
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
        "error": None,
    }

    try:
        obs = load_obs(args.obs)
        # record schema recorded on the loaded config
        try:
            from supy import SUEWSSimulation
            _sim = SUEWSSimulation(args.config)
            prov["config_schema_on_load"] = getattr(_sim.config, "schema_version", None)
            del _sim
        except Exception:
            pass

        fp1, tab1 = compute_fingerprint(args.config, args.forcing, obs)
        fp2, tab2 = compute_fingerprint(args.config, args.forcing, obs)

        prov["fingerprint"] = fp1
        prov["fingerprint_rerun"] = fp2
        prov["reproducible"] = bool(fp1 == fp2)
        prov["status"] = "ok" if fp1 == fp2 else "failed"
        if fp1 != fp2:
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
        (outdir / "stats.json").write_text(json.dumps(stats_json, indent=2, sort_keys=True))
        tab1.to_csv(outdir / "stats_table.csv", index=False)

    except Exception as e:
        prov["status"] = "failed"
        prov["error"] = f"{type(e).__name__}: {e}\n{traceback.format_exc()}"

    (outdir / "provenance.json").write_text(json.dumps(prov, indent=2, sort_keys=True))
    # freeze is written by the caller; just confirm path here
    print(f"[{args.version}] status={prov['status']} "
          f"fp={prov['fingerprint']} rerun={prov['fingerprint_rerun']} "
          f"reproducible={prov['reproducible']}")
    if prov["error"]:
        print(prov["error"][:600], file=sys.stderr)
    return 0 if prov["status"] == "ok" else 1


if __name__ == "__main__":
    raise SystemExit(main())
