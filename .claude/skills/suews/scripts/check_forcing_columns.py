#!/usr/bin/env python3
"""Check the header of a SUEWS forcing file against the canonical column list.

SUEWS forcing files are space- or tab-delimited text; a comma-separated header
is rejected, as the SUEWS loaders reject it. Headers are matched
case-insensitively, mirroring ``src/supy/data_model/forcing/variables.py``.
Pure stdlib so the Skill never pulls supy.

Usage:
    python .claude/skills/suews/scripts/check_forcing_columns.py <forcing_file>

Exit codes: 0 = all baseline columns present; 1 = missing baseline columns
or a comma-separated header.
"""

from __future__ import annotations

import argparse
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
    "sm",
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
# The legacy soil column 'xsmd' is deliberately not an alias of 'sm': supy
# accepts it only as an entirely missing placeholder, and values in it have a
# different meaning and unit, so they must go through 'suews convert --forcing'.
FILE_ALIASES: dict[str, tuple[str, ...]] = {
    "Tair": ("temp_c",),
    "qn": ("qn1_obs",),
    "qs": ("qs_obs",),
    "qf": ("qf_obs",),
    "snow": ("snowfrac",),
    "Wuh": ("wu_mm",),
    "wuh_paved": ("wu_mm_paved",),
    "wuh_bldgs": ("wu_mm_bldgs",),
    "wuh_evetr": ("wu_mm_evetr",),
    "wuh_dectr": ("wu_mm_dectr",),
    "wuh_grass": ("wu_mm_grass",),
    "wuh_bsoil": ("wu_mm_bsoil",),
    "wuh_water": ("wu_mm_water",),
}

# Baseline accessor aliases from variables.py plus the common 'press' typo.
# These are hints only: the file loaders do not accept them as headers.
COMMON_SPELL_TRAPS: dict[str, str] = {
    "press": "pres",
    "pressure": "pres",
    "air_pressure": "pres",
    "p": "pres",
    "wind_speed": "U",
    "wind": "U",
    "relative_humidity": "RH",
    "humidity": "RH",
    "temperature": "Tair",
    "air_temperature": "Tair",
    "temp": "Tair",
    "t_air": "Tair",
    "ta": "Tair",
    "shortwave_down": "kdown",
    "solar_radiation": "kdown",
    "sw_down": "kdown",
    "k_down": "kdown",
    "precipitation": "rain",
    "rainfall": "rain",
    "precip": "rain",
}

LEGACY_SOIL = ("xsmd", "smd")


def _parse_header(header_line: str) -> list[str]:
    stripped = header_line.split("!", 1)[0].strip()
    cols = [col.lstrip("%") for col in stripped.split()]
    return [col for col in cols if col]


def _read_header_line(path_forcing: Path) -> str:
    """Return the first line that is neither blank nor a '!' comment."""
    with path_forcing.open("r", encoding="utf-8-sig") as fh:
        for line in fh:
            if line.strip() and not line.lstrip().startswith("!"):
                return line
    return ""


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
    first_line = _read_header_line(path_forcing)
    if not first_line:
        print(f"{path_forcing}: empty file", file=sys.stderr)
        return 1
    if "," in first_line:
        print(
            f"[X] {path_forcing}: comma-separated header; SUEWS forcing files must "
            "be space- or tab-delimited text. Convert the file before running.",
            file=sys.stderr,
        )
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
    legacy_soil = [header_by_fold[c] for c in LEGACY_SOIL if c in header_by_fold]
    if legacy_soil and "sm" not in header_by_fold:
        print(
            f"Deprecated soil column: {', '.join(legacy_soil)} is not an alias of sm. "
            "Rename it to sm only if it is entirely missing (-999); otherwise "
            "convert with 'suews convert --forcing'."
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
            "Check the header of a SUEWS forcing file (space- or tab-delimited "
            "text) against "
            "the canonical column list in supy.data_model.forcing.variables. "
            "Pure stdlib so the Skill never pulls supy."
        ),
    )
    parser.add_argument(
        "forcing_file",
        type=Path,
        help="Path to the space- or tab-delimited SUEWS forcing file.",
    )
    args = parser.parse_args()
    return check(args.forcing_file)


if __name__ == "__main__":
    sys.exit(main())
