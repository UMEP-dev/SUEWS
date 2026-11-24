"""Generate compact method-5 references in the CI-pinned manylinux image.

Usage: python scripts/suews/gen_ehc_reference.py [--annual-dir DIRECTORY]
The optional directory retains the full implicit outputs for supplementary
same-build annual comparisons using SUEWS_EHC_IMPLICIT_REFERENCE.
"""

import argparse
from datetime import UTC, datetime
import hashlib
import json
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "test" / "fixtures"))
sys.path.insert(0, str(ROOT / "test" / "fixtures" / "data_test"))
from ehc_reference_io import (  # noqa: E402 - repository fixture helpers
    CASES,
    REFERENCE_DIR,
    WINDOWS,
    build_sim,
    reference_csv,
    select_reference,
)
from sample_output_io import build_identity, platform_identity  # noqa: E402


def main():
    """Write both reference windows and their build provenance."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--annual-dir", type=Path)
    args = parser.parse_args()
    REFERENCE_DIR.mkdir(exist_ok=True)
    if args.annual_dir:
        args.annual_dir.mkdir(parents=True, exist_ok=True)
    provenance = {
        "format_version": 1,
        "generated_utc": datetime.now(UTC).isoformat(),
        "float_format": "%.7g",
        "build": build_identity(ROOT),
        "platform": platform_identity(),
        "windows_exclusive_end": WINDOWS,
        "cases": {},
    }
    for case in CASES:
        sim = build_sim(case)
        full = sim.run(n_jobs=1).df
        selected = select_reference(full)
        payload = reference_csv(selected).encode("ascii")
        (REFERENCE_DIR / f"{case}.csv").write_bytes(payload)
        provenance["cases"][case] = {
            "sha256": hashlib.sha256(payload).hexdigest(),
            "rows": len(selected), "columns": list(selected.columns),
            "annual_rows": len(full),
            "negative_qf_air_steps": int((selected["EHC|QF_Air"] < 0).sum()),
            "capped_steps": int((selected["debug|iter"] >= 60).sum()),
            "config_sha256": hashlib.sha256(sim.config.model_dump_json().encode()).hexdigest(),
        }
        if args.annual_dir:
            full.columns = ["|".join(column) for column in full.columns]
            full.to_parquet(args.annual_dir / f"{case}.parquet")
        print(f"Wrote {case}: {len(selected)} rows", flush=True)
    (REFERENCE_DIR / "provenance.json").write_text(
        json.dumps(provenance, indent=2, sort_keys=True) + "\n", encoding="ascii"
    )


if __name__ == "__main__":
    main()
