"""GH-1292 PR1 diagnostic harness for moisture-aware LAI scaffolding.

Usage
-----
    uv run python scripts/verify/moisture_phenology_site.py --site AU-ASM

Purpose
-------
Runs SUEWS at a FLUXNET2015 site for all three vegetation ``LAItype``
values (0 = thermal original, 1 = thermal high-latitude, 2 = moisture-
aware scaffolding) and writes a diagnostic under
``.context/gh1292/<site>/``:

* ``lai_timeseries.png`` -- simulated LAI per variant, plus the observed
  MODIS LAI carried in the forcing file's ``lai`` column.
* ``metrics.json`` -- RMSE (vs MODIS), seasonal amplitude, green-up and
  brown-down day-of-year estimates per variant and per vegetation
  surface.
* ``summary.txt`` -- one-line verdict against the design-note
  acceptance criteria (V2.4). In PR1 the V0 and V2 traces are expected
  to match bit-identically; PR2 will replace the no-op and this script
  becomes the scientific acceptance harness.

The forcing text file is read from Ting's local FLUXNET2015 archive
(``/Users/tingsun/Dropbox (Personal)/6.Repos/SUEWS-FLUXNET2015/``,
read-only per the ``ref_dropbox-legacy-repos`` memory); the script
copies the file into ``.context/gh1292/<site>/`` on first run and uses
the cached copy thereafter.

This script is intentionally kept out of pytest: it depends on external
data that does not ship with the repo and is meant to be run by hand
while developing PR2.
"""

from __future__ import annotations

import argparse
import json
import shutil
import sys
from pathlib import Path
from typing import Dict

import numpy as np
import pandas as pd

FLUXNET_ARCHIVE = Path(
    "/Users/tingsun/Dropbox (Personal)/6.Repos/SUEWS-FLUXNET2015/data"
)
DEFAULT_SITE = "AU-ASM"
LAI_TYPE_VARIANTS = [0, 1, 2]
LAI_TYPE_COLS = [
    ("laitype", "(0,)"),
    ("laitype", "(1,)"),
    ("laitype", "(2,)"),
]
VEG_SURFACE_NAMES = ["evetr", "dectr", "grass"]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--site",
        default=DEFAULT_SITE,
        help="FLUXNET2015 site identifier (e.g. AU-ASM, AU-DaS, US-MMS)",
    )
    parser.add_argument(
        "--year",
        type=int,
        default=None,
        help="Optional year filter; default uses the first full year in the forcing",
    )
    parser.add_argument(
        "--archive",
        type=Path,
        default=FLUXNET_ARCHIVE,
        help="FLUXNET2015 archive root (defaults to Ting's local Dropbox path)",
    )
    return parser.parse_args()


def ensure_cached_forcing(site: str, archive: Path) -> Path:
    """Copy the FLUXNET forcing text file into .context/gh1292/<site>/ if not cached."""

    source = archive / "suews_forcing" / f"FLUXNET2015_LST_{site}.txt"
    if not source.exists():
        raise FileNotFoundError(
            f"Expected FLUXNET forcing file missing: {source}"
        )
    cache_dir = Path(".context/gh1292") / site
    cache_dir.mkdir(parents=True, exist_ok=True)
    dest = cache_dir / source.name
    if not dest.exists():
        shutil.copy2(source, dest)
    return dest


def load_forcing(path: Path, year: int | None) -> pd.DataFrame:
    """Read the FLUXNET forcing file using SuPy's datetime-aware parser."""

    from supy.util import read_forcing

    df = read_forcing(str(path), tstep_mod=300).copy()
    if year is not None:
        df = df[df.index.year == year]
    if df.empty:
        raise ValueError(
            "forcing file has no rows for the requested year; check --year value"
        )
    return df


def run_scenario(
    laitype: int,
    forcing: pd.DataFrame,
    start_date: pd.Timestamp,
    end_date: pd.Timestamp,
) -> pd.DataFrame:
    """Run SUEWS with the requested forcing after overriding laitype on all veg surfaces."""

    # Imported lazily so this script can surface a clean error if supy is missing.
    from supy import SUEWSSimulation

    sim = SUEWSSimulation.from_sample_data()
    state = sim.state_init.copy()
    for col in LAI_TYPE_COLS:
        state.loc[:, col] = laitype
    scenario = SUEWSSimulation.from_state(state)
    scenario.update_forcing(forcing)
    output = scenario.run(start_date=start_date, end_date=end_date)
    df = output.SUEWS[["LAI"]].copy()
    df.columns = ["LAI"]
    return df


def compute_metrics(lai_sim: pd.Series, lai_obs: pd.Series) -> Dict[str, float]:
    """Compute RMSE vs observed and seasonal amplitude / onset / offset DOY."""

    aligned = pd.concat([lai_sim, lai_obs], axis=1).dropna()
    if aligned.empty:
        return {
            "rmse_vs_obs": float("nan"),
            "amplitude": float("nan"),
            "green_up_doy": float("nan"),
            "brown_down_doy": float("nan"),
        }
    sim = aligned.iloc[:, 0]
    obs = aligned.iloc[:, 1]
    rmse = float(np.sqrt(((sim - obs) ** 2).mean()))

    daily = sim.resample("1D").mean()
    if daily.empty:
        return {"rmse_vs_obs": rmse, "amplitude": float("nan"),
                "green_up_doy": float("nan"), "brown_down_doy": float("nan")}
    peak = float(daily.max())
    trough = float(daily.min())
    amplitude = peak - trough
    threshold = trough + 0.5 * amplitude
    above = daily >= threshold
    if above.any():
        green_doy = int(above.idxmax().dayofyear)
        brown_doy = int(above[::-1].idxmax().dayofyear)
    else:
        green_doy = float("nan")
        brown_doy = float("nan")
    return {
        "rmse_vs_obs": rmse,
        "amplitude": amplitude,
        "green_up_doy": float(green_doy),
        "brown_down_doy": float(brown_doy),
    }


def main() -> int:
    args = parse_args()
    archive = args.archive
    forcing_path = ensure_cached_forcing(args.site, archive)
    forcing = load_forcing(forcing_path, args.year)
    start_date = forcing.index.min()
    end_date = forcing.index.max()
    obs_lai = forcing["lai"].replace(-999.0, np.nan)
    obs_lai = obs_lai[~obs_lai.index.duplicated(keep="first")].dropna()

    results: Dict[str, pd.Series] = {}
    metrics: Dict[str, Dict[str, float]] = {}
    for laitype in LAI_TYPE_VARIANTS:
        lai_df = run_scenario(laitype, forcing, start_date, end_date)
        lai_series = lai_df["LAI"]
        # Drop pandas multi-index grid level if present so we can align with observations.
        if isinstance(lai_series.index, pd.MultiIndex):
            lai_series = lai_series.droplevel(0)
        lai_series = lai_series[~lai_series.index.duplicated(keep="first")]
        results[f"laitype_{laitype}"] = lai_series
        metrics[f"laitype_{laitype}"] = compute_metrics(lai_series, obs_lai)

    out_dir = Path(".context/gh1292") / args.site
    out_dir.mkdir(parents=True, exist_ok=True)

    metrics["observed"] = {
        "rmse_vs_obs": 0.0,
        "amplitude": float(obs_lai.max() - obs_lai.min()),
        "green_up_doy": float("nan"),
        "brown_down_doy": float("nan"),
    }
    (out_dir / "metrics.json").write_text(
        json.dumps(metrics, indent=2), encoding="utf-8"
    )

    try:
        import matplotlib

        matplotlib.use("Agg")
        import matplotlib.pyplot as plt

        fig, ax = plt.subplots(figsize=(10, 5))
        obs_daily = obs_lai.resample("1D").mean()
        ax.plot(obs_daily.index, obs_daily.values, label="Observed (MODIS LAI)", color="k", lw=1.2)
        colours = {0: "tab:blue", 1: "tab:orange", 2: "tab:green"}
        for laitype in LAI_TYPE_VARIANTS:
            series = results[f"laitype_{laitype}"].resample("1D").mean()
            ax.plot(
                series.index,
                series.values,
                label=f"Simulated (LAIType={laitype})",
                color=colours[laitype],
                lw=0.9,
                alpha=0.8,
            )
        ax.set_xlabel("Date")
        ax.set_ylabel("LAI")
        ax.set_title(f"GH-1292 moisture-aware phenology diagnostic -- {args.site}")
        ax.legend(loc="upper right")
        fig.tight_layout()
        fig.savefig(out_dir / "lai_timeseries.png", dpi=160)
        plt.close(fig)
    except ImportError:
        print("matplotlib not available; skipping plot", file=sys.stderr)

    lai0 = results["laitype_0"]
    lai2 = results["laitype_2"]
    aligned = pd.concat([lai0, lai2], axis=1, join="inner").dropna()
    max_abs_diff = float(np.max(np.abs(aligned.iloc[:, 0].values - aligned.iloc[:, 1].values)))
    verdict = (
        "PR1 contract OK: LAIType=2 reproduces LAIType=0 bit-identically."
        if max_abs_diff == 0.0
        else f"PR1 contract FAILED: max |LAI0 - LAI2| = {max_abs_diff:.6e}."
    )
    (out_dir / "summary.txt").write_text(verdict + "\n", encoding="utf-8")
    print(f"[{args.site}] {verdict}")
    print(f"Outputs under: {out_dir}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
