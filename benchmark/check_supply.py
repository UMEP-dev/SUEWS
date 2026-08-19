"""CI proof of the restricted-Zenodo data supply.

Fetches the (restricted) evaluation dataset from Zenodo using a bearer token
(GitHub Actions secret mapped to $ZENODO_TOKEN), validates that it has the
expected energy-balance observation schema, and writes a small NON-SENSITIVE
summary artefact (column list + row counts only -- never the raw data).

Exit non-zero on any failure so the workflow fails loudly. This proves the
secret -> restricted Zenodo record -> benchmark-input path that the full
regression job depends on, without using any real sensitive data (the test
record holds a fabricated, same-format synthetic dataset).
"""
from __future__ import annotations

import json
import os
import sys
import tempfile
from pathlib import Path

import pandas as pd

from data_supply import BASE, fetch_eval_data, get_token

# Energy-balance obs columns the benchmark consumes (Kup/Lup/QN/QH/QE <- kup/lup/qn/qh/qe).
REQUIRED = ["DateTime", "qn", "qh", "qe", "kdown", "ldown", "kup", "lup", "Tair"]


def main() -> int:
    source = os.environ.get("OBS_SOURCE")
    if not source:
        print("ERROR: OBS_SOURCE not set (e.g. zenodo:<recid>/synthetic_eb_obs.csv)", file=sys.stderr)
        return 2

    out_dir = Path(os.environ.get("BENCHMARK_OUT", "benchmark_supply_out"))
    out_dir.mkdir(parents=True, exist_ok=True)

    token = get_token()
    if source.startswith("zenodo:") and not token:
        print("ERROR: zenodo: source needs a token in $ZENODO_TOKEN", file=sys.stderr)
        return 2

    print(f"[supply] base={BASE} source={source} token={'present' if token else 'none'}")
    with tempfile.TemporaryDirectory() as tmp:
        path = fetch_eval_data(source, tmp, token=token)
        df = pd.read_csv(path)
        n = len(df)
        cols = list(df.columns)
        missing = [c for c in REQUIRED if c not in cols]

        summary = {
            "source": source if not source.startswith("zenodo:") else source.split("/")[0] + "/<file>",
            "base": BASE,
            "rows": int(n),
            "n_columns": len(cols),
            "required_present": not missing,
            "missing_required": missing,
        }
        (out_dir / "supply_summary.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
        print("[supply] summary:", json.dumps(summary))

        if missing:
            print(f"ERROR: fetched dataset missing required columns: {missing}", file=sys.stderr)
            return 1
        if n <= 0:
            print("ERROR: fetched dataset has no rows", file=sys.stderr)
            return 1

    print("[supply] OK: restricted Zenodo dataset fetched and schema validated.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
