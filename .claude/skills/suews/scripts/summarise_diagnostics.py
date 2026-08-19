#!/usr/bin/env python3
"""Read a SUEWS run diagnostics.json and print a short plain-English summary.

Pure stdlib so the Skill never pulls supy as a dependency.

Usage:
    python summarise_diagnostics.py <run_dir>

Exit codes: 0 = all checks passed; 1 = warnings present; 2 = failures.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path


def summarise(run_dir: Path) -> int:
    path = run_dir / "diagnostics.json"
    if not path.exists():
        print(f"diagnostics.json not found in {run_dir}", file=sys.stderr)
        print(
            f"Run: suews diagnose {run_dir} --format json > {path}",
            file=sys.stderr,
        )
        return 2

    payload = json.loads(path.read_text(encoding="utf-8"))
    data = payload.get("data") or payload
    summary = data.get("summary") or {}
    n_pass = summary.get("n_pass", 0)
    n_warn = summary.get("n_warn", 0)
    n_fail = summary.get("n_fail", 0)
    checks = data.get("checks") or []

    print(f"Run: {run_dir}")
    print(f"Checks: {n_pass} passed, {n_warn} warnings, {n_fail} failed")

    fail_names = [c.get("name") for c in checks if c.get("severity") == "fail"]
    warn_names = [c.get("name") for c in checks if c.get("severity") == "warning"]
    if fail_names:
        print(f"Failed: {', '.join(fail_names)}")
    if warn_names:
        print(f"Warnings: {', '.join(warn_names)}")
    if not fail_names and not warn_names:
        print("All diagnostic checks passed.")

    if n_fail:
        return 2
    if n_warn:
        return 1
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Read a SUEWS run diagnostics.json and print a short "
            "plain-English summary. Pure stdlib so the Skill never pulls "
            "supy as a dependency."
        ),
    )
    parser.add_argument(
        "run_dir",
        type=Path,
        help="Path to a SUEWS run directory containing diagnostics.json.",
    )
    args = parser.parse_args()
    return summarise(args.run_dir)


if __name__ == "__main__":
    sys.exit(main())
