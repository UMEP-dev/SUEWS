#!/usr/bin/env python3
"""Static ratchet: physical-constant literals in Fortran can only go down.

The kernel writes physical constants and unit conversions as bare literals:
``273.15`` alone appears in about 180 places beside a named ``C2K`` that
already exists, and Stefan-Boltzmann, pi and the degree-radian factors each
carry several spellings and values. The clean-up replaces literals with named
constants, first at their current value and precision so output does not move,
in a series of separate PRs. This lint stops new literals arriving while that
happens.

It counts, per file and per kind, the literals listed in ``GATED_KINDS`` in
every ``*.f95`` and ``*.f90`` under ``src/suews/src`` (comments and quoted
strings stripped) except the constants module in ``CONSTANT_MODULES``, where
the named definitions live, and compares the counts with the committed baseline
``scripts/lint/fortran_literals_baseline.json``:

- a count above its baseline fails: use or add a named constant instead;
- a count below its baseline also fails, so the baseline cannot drift above
  the tree: lower it in the same diff with ``--write-baseline``.

``REPORTED_KINDS`` (factors of 1000, 0.001 and 1E6) are listed by
``--report`` but never gated, because too many of their members are not unit
conversions. Matching is by numeric value, so ``273.15``, ``273.150`` and
``273.15D0`` are the same literal, while ``1273.15`` and ``x273`` are not.

Exits 0 when every count equals its baseline, 1 otherwise.
"""

from __future__ import annotations

import argparse
from collections.abc import Callable
import json
import math
from pathlib import Path
import re
import sys

BASELINE_PATH = Path("scripts") / "lint" / "fortran_literals_baseline.json"

# Modules whose job is to define the named constants. Literals there are the
# definitions the rest of the kernel should use, so they are not counted.
CONSTANT_MODULES = frozenset({"suews_ctrl_const.f95"})

REMEDIATION = """\
Physical constants and unit conversions belong in named PARAMETERs (see
suews_ctrl_const.f95), not in new literals.

  - count went up    ->  use the named constant (for example C2K for 273.15);
                         if none exists yet, add one at the literal's current
                         value and precision.
  - count went down  ->  thank you; lower the baseline in this diff:
                         python scripts/lint/check_fortran_literals.py --write-baseline\
"""


def _near(target: float, rel: float) -> Callable[[float], bool]:
    """Match a value within a relative tolerance of ``target``."""
    return lambda value: math.isclose(value, target, rel_tol=rel)


def _one_of(*targets: float) -> Callable[[float], bool]:
    """Match a value exactly equal to one of ``targets``."""
    return lambda value: value in targets


# Each kind maps a parsed literal value to a match decision. Tolerances are
# wide enough to admit every spelling in use (5.67E-8 and 5.67051E-8; pi to
# five or eleven digits) and narrow enough to exclude unrelated numbers.
GATED_KINDS: dict[str, Callable[[float], bool]] = {
    "kelvin_offset": _one_of(273.15, 273.16),
    "stefan_boltzmann": _near(5.67e-8, 1e-3),
    "pi": _near(math.pi, 1e-5),
    "degree_radian": lambda v: (
        math.isclose(v, math.pi / 180, rel_tol=1e-5)
        or math.isclose(v, 180 / math.pi, rel_tol=1e-5)
    ),
    "seconds_per_hour": _one_of(3600.0),
    "seconds_per_day": _one_of(86400.0),
    "minutes_per_day": _one_of(1440.0),
    "reference_pressure": _one_of(1013.0, 1013.25, 101.325, 101325.0),
    "specific_heat_air": _one_of(1005.0, 1006.38),
    "latent_heat_vaporisation": _one_of(2264.705e3, 2.501e6, 2500.25),
}

REPORTED_KINDS: dict[str, Callable[[float], bool]] = {
    "factor_1000": _one_of(1000.0, 1.0e-3),
    "factor_1e6": _one_of(1.0e6, 1.0e-6),
}

# Expressions that spell a gated constant without its literal: seconds per
# day as a product, and the degree-radian factor as a ratio with pi. Each is
# counted under its kind and removed before tokenising, so its factors (3600,
# 60, 180) are not counted again.
_NUM_SUFFIX = r"(?:\.0*)?(?:[dDeE][+-]?0+)?(?:_\w+)?"
EXPRESSION_KINDS: dict[str, re.Pattern[str]] = {
    "seconds_per_day": re.compile(
        rf"(?<![\w.])24{_NUM_SUFFIX}\s*\*\s*"
        rf"(?:60{_NUM_SUFFIX}\s*\*\s*60|3600){_NUM_SUFFIX}(?![\w.])"
    ),
    "degree_radian": re.compile(
        rf"\bpi\s*/\s*180{_NUM_SUFFIX}(?![\w.])"
        rf"|(?<![\w.])180{_NUM_SUFFIX}\s*/\s*pi\b",
        re.I,
    ),
}
# A numeric literal: digits with an optional fraction and exponent, and an
# optional `_kind` suffix. Not part of an identifier (`x273`), not the tail of
# a longer number, and not glued to a dotted operator such as `.AND.`.
_NUMBER = re.compile(
    r"(?<![\w.])(?P<mantissa>\d+\.\d*|\.\d+|\d+)"
    r"(?:(?P<marker>[eEdD])(?P<exponent>[+-]?\d+))?(?:_\w+)?"
    r"(?![\w]|\.(?![a-zA-Z]+\.))"
)


def _strip_comment_and_strings(line: str) -> str:
    """Drop a trailing ``!`` comment and blank out quoted strings."""
    out, quote = [], None
    for char in line:
        if quote:
            if char == quote:
                quote = None
            out.append(" ")
        elif char in "'\"":
            quote = char
            out.append(" ")
        elif char == "!":
            break
        else:
            out.append(char)
    return "".join(out)


def _code_lines(source: str) -> list[str]:
    """Return executable and declaration text, one entry per physical line."""
    lines = []
    for raw in source.splitlines():
        if raw.lstrip().startswith("#"):  # preprocessor directive
            continue
        lines.append(_strip_comment_and_strings(raw))
    return lines


def _literal_values(line: str) -> list[float]:
    """Parse every numeric literal on one line of code."""
    values = []
    for match in _NUMBER.finditer(line):
        text = match.group("mantissa")
        if match.group("exponent") is not None:
            text += "e" + match.group("exponent")
        values.append(float(text))
    return values


def count_kinds(
    source: str, kinds: dict[str, Callable[[float], bool]]
) -> dict[str, int]:
    """Count literals of each kind in one Fortran source."""
    counts = dict.fromkeys(kinds, 0)
    for line in _code_lines(source):
        code = line
        for kind, pattern in EXPRESSION_KINDS.items():
            if kind in kinds:
                counts[kind] += len(pattern.findall(code))
                code = pattern.sub(" ", code)
        for value in _literal_values(code):
            for kind, matches in kinds.items():
                if matches(value):
                    counts[kind] += 1
    return counts


def scan_tree(
    repo_root: Path, kinds: dict[str, Callable[[float], bool]]
) -> dict[str, dict[str, int]]:
    """Return non-zero counts keyed by repo-relative file, then kind."""
    src_root = repo_root / "src" / "suews" / "src"
    result: dict[str, dict[str, int]] = {}
    for path in sorted([*src_root.rglob("*.f95"), *src_root.rglob("*.f90")]):
        if path.name in CONSTANT_MODULES:
            continue
        source = path.read_text(encoding="utf-8", errors="replace")
        counts = {k: n for k, n in sorted(count_kinds(source, kinds).items()) if n}
        if counts:
            result[path.relative_to(repo_root).as_posix()] = counts
    return result


def compare(
    actual: dict[str, dict[str, int]], baseline: dict[str, dict[str, int]]
) -> tuple[list[str], list[str]]:
    """Return (increases, decreases) as readable lines."""
    increases, decreases = [], []
    for file in sorted(set(actual) | set(baseline)):
        found, allowed = actual.get(file, {}), baseline.get(file, {})
        for kind in sorted(set(found) | set(allowed)):
            now, then = found.get(kind, 0), allowed.get(kind, 0)
            if now > then:
                increases.append(f"{file}: {kind} {then} -> {now}")
            elif now < then:
                decreases.append(f"{file}: {kind} {then} -> {now}")
    return increases, decreases


def _totals(counts: dict[str, dict[str, int]]) -> dict[str, int]:
    totals: dict[str, int] = {}
    for per_file in counts.values():
        for kind, n in per_file.items():
            totals[kind] = totals.get(kind, 0) + n
    return dict(sorted(totals.items()))


def _write_baseline(path: Path, counts: dict[str, dict[str, int]]) -> None:
    payload = {
        "description": (
            "Per-file counts of gated Fortran literal kinds; see "
            "scripts/lint/check_fortran_literals.py. Counts may only go down."
        ),
        "totals": _totals(counts),
        "files": counts,
    }
    path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def _print_report(repo_root: Path) -> None:
    gated = scan_tree(repo_root, GATED_KINDS)
    reported = scan_tree(repo_root, REPORTED_KINDS)
    print("Gated kinds (totals):")
    for kind, n in _totals(gated).items():
        print(f"  {kind:26s} {n:5d}")
    print("Reported, not gated (totals):")
    for kind, n in _totals(reported).items():
        print(f"  {kind:26s} {n:5d}")
    print("Per file:")
    for file in sorted(set(gated) | set(reported)):
        merged = {**gated.get(file, {}), **reported.get(file, {})}
        items = ", ".join(f"{k} {n}" for k, n in sorted(merged.items()))
        print(f"  {file}: {items}")


def main(argv: list[str]) -> int:
    """Compare the tree against the baseline, or rewrite or report it."""
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("repo_root", nargs="?", default=".")
    parser.add_argument(
        "--write-baseline",
        action="store_true",
        help="rewrite the baseline from the current tree",
    )
    parser.add_argument(
        "--report",
        action="store_true",
        help="print gated and report-only counts, then exit 0",
    )
    args = parser.parse_args(argv[1:])

    repo_root = Path(args.repo_root).resolve()
    if not (repo_root / "src" / "suews" / "src").is_dir():
        print(f"[X] src/suews/src not found under {repo_root}", file=sys.stderr)
        return 1
    if args.report:
        _print_report(repo_root)
        return 0

    actual = scan_tree(repo_root, GATED_KINDS)
    baseline_path = repo_root / BASELINE_PATH
    if args.write_baseline:
        _write_baseline(baseline_path, actual)
        print(f"[OK] wrote {BASELINE_PATH.as_posix()}: {_totals(actual)}")
        return 0
    if not baseline_path.is_file():
        print(f"[X] baseline {BASELINE_PATH.as_posix()} not found", file=sys.stderr)
        return 1

    baseline = json.loads(baseline_path.read_text(encoding="utf-8"))["files"]
    increases, decreases = compare(actual, baseline)
    if increases or decreases:
        if increases:
            print("[X] new Fortran literal constants:", file=sys.stderr)
            for line in increases:
                print(f"  - {line}", file=sys.stderr)
        if decreases:
            print("[X] baseline is above the tree (lower it):", file=sys.stderr)
            for line in decreases:
                print(f"  - {line}", file=sys.stderr)
        print("", file=sys.stderr)
        print(REMEDIATION, file=sys.stderr)
        return 1

    total = sum(_totals(actual).values())
    print(
        f"[OK] Fortran literal constants match the baseline "
        f"({total} gated literals in {len(actual)} files)."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
