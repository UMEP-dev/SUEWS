"""Replay the literal sample prefix, then report water closure without a tolerance.

This optional diagnostic never changes the acceptance verdict of a guide journey.
Use the wheel-only environment prepared by that journey, not an editable install.
"""

import argparse
import json
import os
from pathlib import Path
import tempfile

from scripts.suews.guide_journeys.extractor import Document
from scripts.suews.guide_journeys.manifest import check, load
from scripts.suews.guide_journeys.python_session import PythonSession
from scripts.suews.guide_journeys.runner import read_page, resolve_steps

# Native in-memory water fluxes are amounts in mm per model timestep.
# See tutorial_05_results_analysis.py:158-190 and _output_metadata.py:357-358.
# Observe only after the last replayed statement; no later reader uses this state.
WATER_OBSERVATION = """
(lambda water: (lambda residual: {
    'formula': 'Rain + Irr - Evap - RO - TotCh',
    'units': 'mm per model timestep (native in-memory amounts)',
    'rows': len(water),
    'first_index': str(water.index[0]),
    'last_index': str(water.index[-1]),
    'all_finite': bool(np.isfinite(water.to_numpy()).all()),
    'totals_mm': water.sum().to_dict(),
    'residual_mm': {
        'sum': float(residual.sum()),
        'mean': float(residual.mean()),
        'std': float(residual.std()),
        'min': float(residual.min()),
        'max': float(residual.max()),
        'max_abs': float(residual.abs().max()),
        'quantiles': {str(q): float(v) for q, v in
                      residual.quantile([0, .01, .25, .5, .75, .99, 1]).items()},
        'absolute_quantiles': {str(q): float(v) for q, v in
                      residual.abs().quantile([.5, .9, .95, .99, 1]).items()},
        'daily_totals': {str(k): float(v) for k, v in
                        residual.groupby(residual.index.get_level_values('datetime')
                                         .normalize()).sum().items()},
    },
})(water.Rain + water.Irr - water.Evap - water.RO - water.TotCh))(
    pd.DataFrame({name: sim.get_variable(name, group='SUEWS').iloc[:, 0]
                  for name in ['Rain', 'Irr', 'Evap', 'RO', 'TotCh']})
)
"""


def main() -> int:
    """Execute a source-bound prefix and retain process and residual evidence."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--revision", required=True)
    parser.add_argument("--venv", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument(
        "--manifest",
        type=Path,
        default=Path("test/docs/journeys/getting_started.yml"),
    )
    args = parser.parse_args()
    manifest = load(args.manifest)
    document = Document(
        read_page(Path.cwd(), args.revision, manifest["guide"]), manifest["title"]
    )
    stale = check(document, manifest, args.revision)
    if stale:
        raise ValueError(stale)
    specs = resolve_steps(document, manifest, manifest["journeys"][0])
    stop = next(i for i, spec in enumerate(specs) if spec["id"] == "sample-run")
    record = {
        "kind": "optional diagnostic replay; not acceptance",
        "revision": args.revision,
        "source_blob": document.source_blob,
        "manifest_sha256": manifest["_manifest_sha256"],
        "steps": [],
    }
    env = dict(os.environ, VIRTUAL_ENV=str(args.venv.resolve()), MPLBACKEND="Agg")
    with tempfile.TemporaryDirectory() as temporary:
        session = PythonSession(Path(temporary), env)
        try:
            for spec in specs[: stop + 1]:
                process = session.execute(spec)
                record["steps"].append({
                    "id": spec["id"],
                    "source_lines": spec["source_lines"],
                    **process,
                })
                if process["exit_status"]:
                    break
            else:
                record["water_balance"] = session.observe(WATER_OBSERVATION)
        finally:
            session.close()
    args.output.write_text(json.dumps(record, indent=2) + "\n", encoding="utf-8")
    return int(record.get("water_balance", {}).get("exit_status", 1) != 0)


if __name__ == "__main__":
    raise SystemExit(main())
