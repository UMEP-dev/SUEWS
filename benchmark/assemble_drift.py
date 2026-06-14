"""Assemble the cross-version drift report from a benchmark results index.

Reads ``<results_dir>/index.json`` and writes:
  * ``<results_dir>/drift_report.json`` -- full record list + metadata
  * ``<results_dir>/drift_table.csv``   -- tidy long table for the site view

Run from the benchmark directory:  python3 assemble_drift.py
"""

from __future__ import annotations

import csv
import json
from dataclasses import asdict
from pathlib import Path
from typing import Optional

from bench_drift import compute_drift

_CSV_HEADER = [
    "from_tag",
    "to_tag",
    "framing",
    "flux",
    "interval",
    "bucket",
    "metric",
    "value",
]


def _default_baseline() -> Optional[str]:
    """Pinned baseline from the shipped registry, if supy is importable."""
    try:
        from supy._model_registry import drift_baseline_tag

        return drift_baseline_tag()
    except Exception:
        return None


def assemble_drift(
    results_dir: Path | str = "results", baseline_tag: Optional[str] = None
) -> dict:
    results_dir = Path(results_dir)
    index = json.loads((results_dir / "index.json").read_text(encoding="utf-8"))

    baseline = baseline_tag or _default_baseline()
    records = compute_drift(index, baseline_tag=baseline)

    report = {
        "site": index.get("site"),
        "baseline_tag": baseline,
        "framings": ["consecutive", "baseline"],
        "records": [asdict(r) for r in records],
    }
    (results_dir / "drift_report.json").write_text(
        json.dumps(report, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )

    with (results_dir / "drift_table.csv").open("w", newline="", encoding="utf-8") as fh:
        writer = csv.writer(fh)
        writer.writerow(_CSV_HEADER)
        for r in records:
            for metric, value in (("delta_mae", r.delta_mae), ("delta_mbe", r.delta_mbe)):
                writer.writerow(
                    [r.from_tag, r.to_tag, r.framing, r.flux, r.interval, r.bucket, metric, value]
                )

    return report


if __name__ == "__main__":
    out = assemble_drift()
    print(f"[OK] wrote drift report for site {out['site']} ({len(out['records'])} records)")
