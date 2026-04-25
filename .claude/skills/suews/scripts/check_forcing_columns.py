#!/usr/bin/env python3
"""Check the header of a SUEWS forcing CSV against the canonical column list.

Pure stdlib so the Skill never pulls supy.

Usage:
    python check_forcing_columns.py <forcing.csv>

Exit codes: 0 = all required columns present; 1 = missing columns.
"""

from __future__ import annotations

import csv
import sys
from pathlib import Path

REQUIRED = ["Tair", "RH", "Press", "U", "Kdown", "rain"]
OPTIONAL = ["Ldown", "fcld", "qn1_obs"]


def check(path_csv: Path) -> int:
    with path_csv.open("r", encoding="utf-8") as fh:
        reader = csv.reader(fh)
        try:
            header = next(reader)
        except StopIteration:
            print(f"{path_csv}: empty file", file=sys.stderr)
            return 1

    header_set = {h.strip() for h in header}
    missing = [c for c in REQUIRED if c not in header_set]
    found_optional = [c for c in OPTIONAL if c in header_set]

    print(f"File: {path_csv}")
    print(f"Header columns: {len(header_set)}")
    if missing:
        print(f"Missing required: {', '.join(missing)}")
    else:
        print("All required columns present.")
    if found_optional:
        print(f"Optional present: {', '.join(found_optional)}")
    return 1 if missing else 0


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: check_forcing_columns.py <forcing.csv>", file=sys.stderr)
        sys.exit(2)
    sys.exit(check(Path(sys.argv[1])))
