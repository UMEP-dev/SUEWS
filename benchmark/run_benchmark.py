"""Full benchmark runner (Stage 2).

Resolves the evaluation observations from a swappable source (`local:` or a
restricted `zenodo:` record), runs the esmae benchmark for the configured site,
writes the HTML report, and returns a non-zero exit code on regression when
``--fail-on-regression`` is set.

This encodes the flow validated locally against the Ward-2016 KCL case. It is
wired into CI by the (currently disabled) ``full-benchmark`` job once the real
restricted production record and data-governance sign-off are in place. The
site config + benchmark setup YAML are added under ``benchmark/configs`` and
``benchmark/setups`` in the Stage-2 enablement PR.

Several esmae dev-branch quirks are routed around here rather than patched
upstream (tracked for report to the esmae maintainer):
  * BenchmarkInfo lacks save_stats/load_stats on the recompute path;
  * BenchmarkWebsite.save_site re-saves model forcing and crashes when supy
    could not load the (wider-than-expected) forcing file -> write HTML only.
"""
from __future__ import annotations

import argparse
import sys
import tempfile
from pathlib import Path

import yaml

sys.path.insert(0, str(Path(__file__).resolve().parent))
from data_supply import fetch_eval_data  # noqa: E402

HERE = Path(__file__).resolve().parent
DEFAULT_SETUP = HERE / "setups" / "benchmark_kcl.yaml"


def _shim_benchmark_info() -> None:
    """Add attrs esmae's recompute path reads but its BenchmarkInfo omits."""
    from esmae._benchmark._core import BenchmarkInfo as BI

    for attr in ("save_stats", "load_stats", "compare_stats",
                 "save_plots", "load_plots", "compare_plots"):
        if not hasattr(BI, attr):
            setattr(BI, attr, False)


def main() -> int:
    ap = argparse.ArgumentParser(description="Run the SUEWS regression benchmark")
    ap.add_argument("--obs-source", required=True,
                    help="local:/path/to.csv or zenodo:<recid>/<file>")
    ap.add_argument("--setup", default=str(DEFAULT_SETUP),
                    help="benchmark setup YAML")
    ap.add_argument("--out", default="site", help="output site directory")
    ap.add_argument("--title", default=None, help="model-version label / results subdir")
    ap.add_argument("--fail-on-regression", action="store_true",
                    help="exit non-zero if the benchmark status is a regression")
    args = ap.parse_args()

    import esmae as em
    from esmae._quicklook import WebsiteBase

    _shim_benchmark_info()

    setup = Path(args.setup)
    if not setup.exists():
        print(f"ERROR: setup YAML not found: {setup}\n"
              "Add the site config + setup under benchmark/ (Stage-2 enablement).",
              file=sys.stderr)
        return 2

    # Resolve the (sensitive) observations into a temp dir and point the setup at it.
    tmp = tempfile.mkdtemp(prefix="obs_")
    resolved = fetch_eval_data(args.obs_source, tmp)
    print(f"[supply] {args.obs_source} -> {resolved}")

    cfg = yaml.safe_load(setup.read_text())
    cfg.setdefault("evaluation", {}).setdefault("energy_balance", {})["path"] = resolved
    run_setup = Path(tmp) / "setup_resolved.yaml"
    run_setup.write_text(yaml.safe_dump(cfg, sort_keys=False))

    title = args.title or f"v{__import__('supy').__version__}"
    site = em.BenchmarkWebsite(title=title, exist_okay=True, working_dir=args.out)
    site.benchmarker.register(benchmark_cls=em.presets.CustomBase, name="benchmark")
    site.benchmark(name="benchmark", benchmarkID=cfg.get("info", {}).get("benchmarkID", "bm"),
                   setup_path=str(run_setup), save=False)

    bm = site.benchmarker.get_benchmark("benchmark")
    status = bool(getattr(bm, "_status", False))
    print(f"[benchmark] status (passed)={status}")

    # Write HTML only (avoid esmae save_all's forcing re-save crash).
    try:
        site.save_site()
    except AttributeError:
        WebsiteBase.save_site(site, save_dir=Path(site.info.website_dir))

    if args.fail_on_regression and not status:
        print("REGRESSION: benchmark status did not pass.", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
