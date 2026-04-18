"""GH-1292 PR3 parameter-sweep tool for moisture-aware LAI calibration.

Usage
-----
    uv run python scripts/verify/moisture_phenology_sweep.py \\
        --param w_opt --values 0.3 0.5 0.7 0.9 --dry-start

    # All-parameter sensitivity scan at the bundled sample with depleted soil:
    uv run python scripts/verify/moisture_phenology_sweep.py --all --dry-start

Purpose
-------
Scans one of the six moisture-aware ``LAI_PRM`` parameters (``w_wilt``,
``w_opt``, ``f_shape``, ``w_on``, ``w_off``, ``tau_w``) over a
user-provided value list, runs SUEWS for each, and emits a dose-response
summary alongside the existing ``LAIType = 0`` baseline. Outputs go to
``.context/gh1292/<site>/sweep_<param>.{json,png}`` and are the
foundation for eventual FLUXNET-driven calibration.

The tool is a calibration **scaffold**, not a full parameter fit:

* Single-site, single-parameter-per-invocation (serialised; nested
  scans are available via ``--all`` or repeated calls).
* Uses the bundled Swindon sample by default; ``--dry-start`` depletes
  the vegetation soil store on day 1 so the moisture gate is actually
  engaged during the year (Swindon is otherwise well-watered).
* Reports RMSE and mean-difference against the ``LAIType = 0``
  baseline, plus seasonal amplitude and estimated green-up DOY.

A proper per-IGBP calibration against FLUXNET2015 requires dedicated
per-site SUEWS configurations (latitude, land-cover fractions,
soil-store capacity, thermal fits from Omidvar et al. 2022 Table S1).
Those configs are not shipped with this tool; see the "Roadmap for
full calibration" appendix in
``dev-ref/design-notes/gh1292-moisture-phenology.md``.
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence

import numpy as np
import pandas as pd

DEFAULT_SITE = "Swindon"  # placeholder tag for the bundled sample
PARAM_NAMES = ("w_wilt", "w_opt", "f_shape", "w_on", "w_off", "tau_w")
DEFAULT_SWEEP_VALUES: Dict[str, Sequence[float]] = {
    "w_wilt": (0.05, 0.15, 0.30, 0.50, 0.70),
    "w_opt": (0.25, 0.40, 0.55, 0.70, 0.90),
    "f_shape": (0.5, 1.0, 1.5, 2.0, 3.0),
    "w_on": (0.15, 0.30, 0.45, 0.60, 0.80),
    "w_off": (0.05, 0.10, 0.20, 0.30, 0.45),
    "tau_w": (3.0, 7.0, 15.0, 30.0, 60.0),
}
LAI_TYPE_COLS = [("laitype", f"({i},)") for i in range(3)]


@dataclass(frozen=True)
class SweepResult:
    param: str
    value: float
    mean_lai: float
    rmse_vs_baseline: float
    amplitude: float
    green_up_doy: Optional[float]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--param",
        choices=PARAM_NAMES,
        help="Single parameter to sweep (overrides --all)",
    )
    parser.add_argument(
        "--values",
        type=float,
        nargs="+",
        help="Override default sweep values for the selected parameter",
    )
    parser.add_argument(
        "--all",
        action="store_true",
        help="Sweep every parameter using default value ranges",
    )
    parser.add_argument(
        "--site",
        default=DEFAULT_SITE,
        help="Site tag used in output paths (default Swindon, i.e. the bundled sample)",
    )
    parser.add_argument(
        "--dry-start",
        action="store_true",
        help=(
            "Deplete soilstore_surf on vegetation indices (2, 3, 4) to 10 mm on "
            "day 1 so the moisture gate engages at well-watered sites"
        ),
    )
    args = parser.parse_args()
    if not args.all and not args.param:
        parser.error("either --param or --all is required")
    return args


def _apply_dry_start(state: pd.DataFrame) -> pd.DataFrame:
    for veg_idx in (2, 3, 4):
        state.loc[:, ("soilstore_surf", f"({veg_idx},)")] = 10.0
    return state


def _set_laitype(state: pd.DataFrame, value: int) -> pd.DataFrame:
    for col in LAI_TYPE_COLS:
        state.loc[:, col] = value
    return state


def _set_param(state: pd.DataFrame, param: str, value: float) -> pd.DataFrame:
    for i in range(3):
        state.loc[:, (param, f"({i},)")] = value
    return state


def _co_adjust(param_overrides: Dict[str, float]) -> Dict[str, float]:
    """Preserve the LAIParams validators (w_opt > w_wilt, w_on > w_off) during a sweep.

    When sweeping ``w_wilt`` we may exceed the default ``w_opt`` (0.40); similarly
    when sweeping ``w_off`` we may exceed the default ``w_on`` (0.35). The validator
    fires under laitype=2 (as designed), so we lift the paired threshold by a small
    margin whenever the swept value would violate the constraint.
    """

    adjusted = dict(param_overrides)
    margin = 0.05
    # w_wilt sweep: lift w_opt if necessary so w_opt > w_wilt.
    if "w_wilt" in adjusted and adjusted["w_wilt"] + margin >= 0.40:
        adjusted.setdefault("w_opt", min(0.98, adjusted["w_wilt"] + margin))
    # w_off sweep: lift w_on if necessary so w_on > w_off.
    if "w_off" in adjusted and adjusted["w_off"] + margin >= 0.35:
        adjusted.setdefault("w_on", min(0.98, adjusted["w_off"] + margin))
    # w_on sweep (low values): drop w_off if necessary so w_off < w_on.
    if "w_on" in adjusted and adjusted["w_on"] - margin <= 0.20:
        adjusted.setdefault("w_off", max(0.02, adjusted["w_on"] - margin))
    # w_opt sweep (low values): drop w_wilt if necessary so w_wilt < w_opt.
    if "w_opt" in adjusted and adjusted["w_opt"] - margin <= 0.15:
        adjusted.setdefault("w_wilt", max(0.02, adjusted["w_opt"] - margin))
    return adjusted


def _run_sample_scenario(
    laitype: int,
    param_overrides: Optional[Dict[str, float]] = None,
    dry_start: bool = False,
) -> pd.Series:
    from supy import SUEWSSimulation

    sim = SUEWSSimulation.from_sample_data()
    state = sim.state_init.copy()
    _set_laitype(state, laitype)
    if dry_start:
        _apply_dry_start(state)
    if param_overrides:
        for p, v in _co_adjust(param_overrides).items():
            _set_param(state, p, v)

    scenario = SUEWSSimulation.from_state(state)
    scenario.update_forcing(sim.forcing)
    output = scenario.run()
    return output.SUEWS["LAI"].copy()


def _compute_metrics(lai: pd.Series, baseline: pd.Series) -> Dict[str, float]:
    diff = lai.values - baseline.values
    rmse = float(np.sqrt(np.mean(diff**2)))
    mean = float(lai.mean())
    amplitude = float(lai.max() - lai.min())

    green_up = float("nan")
    # Daily aggregation; detect first DOY where LAI exceeds trough + 0.5 * amplitude.
    try:
        daily = lai.groupby(lai.index.get_level_values("datetime").dayofyear).mean()
        if len(daily) > 0 and amplitude > 1.0e-6:
            threshold = float(daily.min()) + 0.5 * amplitude
            above = daily >= threshold
            if above.any():
                green_up = float(above.idxmax())
    except Exception:  # noqa: BLE001 - robust to missing index layers
        pass

    return {
        "rmse_vs_baseline": rmse,
        "mean_lai": mean,
        "amplitude": amplitude,
        "green_up_doy": green_up,
    }


def sweep_parameter(
    param: str,
    values: Sequence[float],
    baseline: pd.Series,
    dry_start: bool,
) -> List[SweepResult]:
    results: List[SweepResult] = []
    for value in values:
        lai = _run_sample_scenario(
            laitype=2, param_overrides={param: float(value)}, dry_start=dry_start
        )
        metrics = _compute_metrics(lai, baseline)
        results.append(
            SweepResult(
                param=param,
                value=float(value),
                mean_lai=metrics["mean_lai"],
                rmse_vs_baseline=metrics["rmse_vs_baseline"],
                amplitude=metrics["amplitude"],
                green_up_doy=metrics["green_up_doy"],
            )
        )
    return results


def _write_sweep_outputs(
    out_dir: Path, param: str, results: Sequence[SweepResult], baseline_mean: float
) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)
    payload = {
        "parameter": param,
        "baseline_mean_lai": baseline_mean,
        "points": [
            {
                "value": r.value,
                "mean_lai": r.mean_lai,
                "rmse_vs_baseline": r.rmse_vs_baseline,
                "amplitude": r.amplitude,
                "green_up_doy": r.green_up_doy,
            }
            for r in results
        ],
    }
    (out_dir / f"sweep_{param}.json").write_text(
        json.dumps(payload, indent=2), encoding="utf-8"
    )

    try:
        import matplotlib

        matplotlib.use("Agg")
        import matplotlib.pyplot as plt

        fig, ax_left = plt.subplots(figsize=(7, 4))
        x = [r.value for r in results]
        ax_left.plot(x, [r.mean_lai for r in results], "o-", color="tab:blue", label="mean LAI")
        ax_left.axhline(baseline_mean, color="k", lw=0.7, ls="--", label="LAIType=0 baseline")
        ax_left.set_xlabel(param)
        ax_left.set_ylabel("mean LAI [m2/m2]", color="tab:blue")
        ax_left.tick_params(axis="y", labelcolor="tab:blue")

        ax_right = ax_left.twinx()
        ax_right.plot(x, [r.rmse_vs_baseline for r in results], "s-", color="tab:orange", label="RMSE vs baseline")
        ax_right.set_ylabel("RMSE vs LAIType=0 [m2/m2]", color="tab:orange")
        ax_right.tick_params(axis="y", labelcolor="tab:orange")

        ax_left.set_title(f"GH-1292 moisture-aware sensitivity: {param}")
        lines1, labels1 = ax_left.get_legend_handles_labels()
        lines2, labels2 = ax_right.get_legend_handles_labels()
        ax_left.legend(lines1 + lines2, labels1 + labels2, loc="best", fontsize=8)
        fig.tight_layout()
        fig.savefig(out_dir / f"sweep_{param}.png", dpi=150)
        plt.close(fig)
    except ImportError:
        print("matplotlib not available; skipping plot", file=sys.stderr)


def main() -> int:
    args = parse_args()
    out_dir = Path(".context/gh1292") / args.site

    baseline_series = _run_sample_scenario(laitype=0, dry_start=args.dry_start)
    baseline_mean = float(baseline_series.mean())
    print(f"[{args.site}] LAIType=0 baseline mean LAI = {baseline_mean:.4f}")

    params_to_sweep: Iterable[str]
    if args.all:
        params_to_sweep = PARAM_NAMES
    else:
        params_to_sweep = [args.param]

    for param in params_to_sweep:
        values = args.values if args.values and not args.all else DEFAULT_SWEEP_VALUES[param]
        print(f"  Sweeping {param} over {list(values)}")
        results = sweep_parameter(param, values, baseline_series, dry_start=args.dry_start)
        _write_sweep_outputs(out_dir, param, results, baseline_mean)
        for r in results:
            green_str = f"{r.green_up_doy:.0f}" if r.green_up_doy == r.green_up_doy else "nan"
            print(
                f"    {param}={r.value:<6g}  meanLAI={r.mean_lai:.4f}  "
                f"RMSE={r.rmse_vs_baseline:.4f}  ampl={r.amplitude:.3f}  greenDOY={green_str}"
            )

    print(f"Outputs under: {out_dir}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
