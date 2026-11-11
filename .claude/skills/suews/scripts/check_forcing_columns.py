#!/usr/bin/env python3
"""Check the header of a SUEWS forcing file against the canonical column list.

Supports both comma-separated (.csv) and whitespace-delimited (.txt) SUEWS
forcing headers and mirrors ``src/supy/data_model/forcing/variables.py``.
Pure stdlib so the Skill never pulls supy.

Usage:
    python .claude/skills/suews/scripts/check_forcing_columns.py <forcing_file>

Exit codes: 0 = all baseline columns present; 1 = missing baseline columns.
"""

from __future__ import annotations

import argparse
import csv
from pathlib import Path
import sys

TIME_COORDS = ["iy", "id", "it", "imin"]
REQUIRED_DRIVERS = ["U", "RH", "Tair", "pres", "rain", "kdown"]
REQUIRED = TIME_COORDS + REQUIRED_DRIVERS

OPTIONAL = [
    "qn",
    "qh",
    "qe",
    "qs",
    "qf",
    "snow",
    "ldown",
    "fcld",
    "Wuh",
    "xsmd",
    "lai",
    "kdiff",
    "kdir",
    "wdir",
    "lai_evetr",
    "lai_dectr",
    "lai_grass",
    "wuh_paved",
    "wuh_bldgs",
    "wuh_evetr",
    "wuh_dectr",
    "wuh_grass",
    "wuh_bsoil",
    "wuh_water",
]

# Legacy file-header aliases accepted by src/supy/data_model/forcing/variables.py
# (plus lowercase 'wuh' used in legacy 24-column sample text files).
FILE_ALIASES: dict[str, tuple[str, ...]] = {
    "Tair": ("temp_c",),
    "qn": ("qn1_obs",),
    "qs": ("qs_obs",),
    "qf": ("qf_obs",),
    "snow": ("snowfrac",),
    "Wuh": ("wu_mm", "wuh"),
    "wuh_paved": ("wu_mm_paved",),
    "wuh_bldgs": ("wu_mm_bldgs",),
    "wuh_evetr": ("wu_mm_evetr",),
    "wuh_dectr": ("wu_mm_dectr",),
    "wuh_grass": ("wu_mm_grass",),
    "wuh_bsoil": ("wu_mm_bsoil",),
    "wuh_water": ("wu_mm_water",),
}

COMMON_SPELL_TRAPS: dict[str, str] = {
    "press": "pres",
    "pressure": "pres",
    "wind_speed": "U",
    "relative_humidity": "RH",
    "air_temperature": "Tair",
    "shortwave_down": "kdown",
    "precipitation": "rain",
}


def _parse_header(first_line: str) -> list[str]:
    stripped = first_line.strip()
    if not stripped:
        return []
    if "," in stripped:
        row = next(csv.reader([stripped]))
        cols = [col.strip() for col in row if col.strip()]
    else:
        cols = stripped.split()
    if cols and cols[0].startswith("%"):
        cols[0] = cols[0].lstrip("%").strip()
        if not cols[0]:
            cols = cols[1:]
    return cols


def _present_or_alias(
    col: str, header_by_fold: dict[str, str]
) -> tuple[bool, str | None]:
    exact = header_by_fold.get(col.casefold())
    if exact is not None:
        return True, (None if exact == col else exact)
    for alias in FILE_ALIASES.get(col, ()):
        matched = header_by_fold.get(alias.casefold())
        if matched is not None:
            return True, matched
    return False, None


def check(path_forcing: Path) -> int:
    """Validate forcing header columns and report missing or aliased names."""
    with path_forcing.open("r", encoding="utf-8-sig") as fh:
        first_line = fh.readline()
    if not first_line:
        print(f"{path_forcing}: empty file", file=sys.stderr)
        return 1

    header = _parse_header(first_line)
    if not header:
        print(f"{path_forcing}: empty header line", file=sys.stderr)
        return 1

    header_by_fold = {h.casefold(): h for h in header}
    missing: list[str] = []
    used_aliases: list[str] = []

    for col in REQUIRED:
        ok, alias = _present_or_alias(col, header_by_fold)
        if not ok:
            missing.append(col)
        elif alias is not None:
            used_aliases.append(f"{alias} -> {col}")

    found_optional: list[str] = []
    for col in OPTIONAL:
        ok, alias = _present_or_alias(col, header_by_fold)
        if ok:
            found_optional.append(col if alias is None else f"{col} (via {alias})")

    spell_hints = [
        f"{header_by_fold[bad]} (use {good})"
        for bad, good in COMMON_SPELL_TRAPS.items()
        if bad in header_by_fold and good.casefold() not in header_by_fold
    ]

    print(f"File: {path_forcing}")
    print(f"Header columns: {len(header)}")
    if missing:
        print(f"[X] Missing baseline columns: {', '.join(missing)}")
        if spell_hints:
            print(f"    Header hints: {', '.join(spell_hints)}")
    else:
        print(
            "[OK] All baseline columns present (iy, id, it, imin, U, RH, Tair, pres, rain, kdown)."
        )
    if used_aliases:
        print(f"Case/file aliases matched: {', '.join(used_aliases)}")
    if found_optional:
        print(f"Optional/conditional present: {', '.join(found_optional)}")
    return 1 if missing else 0


def main() -> int:
    """Parse CLI arguments and run the forcing-header check."""
    parser = argparse.ArgumentParser(
        description=(
            "Check the header of a SUEWS forcing file (.csv or .txt) against "
            "the canonical column list in supy.data_model.forcing.variables. "
            "Pure stdlib so the Skill never pulls supy."
        ),
    )
    parser.add_argument(
        "forcing_file",
        type=Path,
        help="Path to the SUEWS forcing CSV or whitespace-delimited TXT file.",
    )
    args = parser.parse_args()
    return check(args.forcing_file)


if __name__ == "__main__":
    sys.exit(main())
