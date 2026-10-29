#!/usr/bin/env python3
"""Static ratchet: physical-constant literals in Fortran can only go down.

Shared definitions must use double precision, and physics modules must import
them directly. Changing a value or precision requires physics-change evidence.

It counts, per file and per kind, the literals listed in ``GATED_KINDS`` in
every ``*.f95`` and ``*.f90`` under ``src/suews/src`` (free-form continuations
joined, comments and quoted strings stripped) except ``CONSTANT_MODULES``, where
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
                         if none exists yet, add one from an authoritative
                         source at double precision.
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


def _strip_comment_and_strings(line: str, quote: str | None) -> tuple[str, str | None]:
    """Blank character data, retaining quote state across continuations."""
    out = []
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
    return "".join(out), quote


def _code_lines(source: str) -> list[str]:
    """Return logical lines of code, joining free-form continuations."""
    lines = []
    pending, quote = "", None
    for raw in source.splitlines():
        # Comments and preprocessor directives can interrupt a continuation.
        if not raw.strip() or raw.lstrip().startswith(("#", "!")):
            continue
        line = raw.lstrip()
        if line.startswith("&"):
            line = line[1:]
        code, quote = _strip_comment_and_strings(line, quote)
        code = code.rstrip()
        if code.endswith("&"):
            pending += code[:-1]
        elif quote:
            pending += code
        else:
            lines.append(pending + code)
            pending = ""
    if pending:
        lines.append(pending)
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
        # Dotted operators delimit adjacent numbers just like symbolic ones.
        code = re.sub(r"\.[a-zA-Z]+\.", " ", line)
        for kind, pattern in EXPRESSION_KINDS.items():
            if kind in kinds:
                counts[kind] += len(pattern.findall(code))
                code = pattern.sub(" ", code)
        for value in _literal_values(code):
            for kind, matches in kinds.items():
                if matches(value):
                    counts[kind] += 1
    return counts


def _declarations(source: str):
    """Yield (attributes, name, initialiser) for free-form numeric declarations.

    Split at top-level commas only: dimensions and function arguments may
    contain commas. Comments, strings, continuations and semicolons are handled
    before declarations are examined.
    """
    for logical in _code_lines(source):
        for statement in logical.split(";"):
            match = re.match(
                r"\s*(real\b|double\s+precision\b|integer\b)(.*)::(.*)", statement, re.I
            )
            if not match:
                continue
            attributes, entities = match[1] + match[2], match[3]
            depth, start = 0, 0
            parts = []
            for pos, char in enumerate(entities):
                depth += (char == "(") - (char == ")")
                if char == "," and depth == 0:
                    parts.append(entities[start:pos])
                    start = pos + 1
            parts.append(entities[start:])
            for part in parts:
                entity = re.match(r"\s*(\w+)(?:\s*\([^=]*\))?\s*(?:=\s*(.*))?$", part)
                if entity:
                    yield (
                        attributes.lower(),
                        entity[1].lower(),
                        (entity[2] or "").strip().lower(),
                    )


def _quantity(name: str, value: float) -> str | None:
    """Classify a definition, separating inverse factors and scheme coefficients.

    The legacy cp/Lv intercepts are coefficients of different empirical
    formulae, not alternative definitions of a universal constant. Their
    names identify those formulae; new unqualified definitions remain gated.
    """
    scheme_coefficients = {
        "cp_air_anohm": "anohm_specific_heat",
        "lv_anohm": "anohm_latent_heat",
        "lv_wetbulb": "wetbulb_latent_heat_intercept",
    }
    if name in scheme_coefficients:
        return scheme_coefficients[name]
    if name == "arcsec_per_deg":
        return "arcseconds_per_degree"
    for kind, matches in GATED_KINDS.items():
        if matches(value):
            if kind == "degree_radian":
                return "degrees_per_radian" if value > 1 else "radians_per_degree"
            return kind
    return None


def _check_definitions(path: Path) -> tuple[set[str], list[str]]:
    """Collect shared names and validate the central definitions."""
    source = path.read_text(encoding="utf-8")
    errors = []
    shared = set()
    quantities = {}
    for attributes, name, expression in _declarations(source):
        if "parameter" not in attributes:
            continue
        # Only physical/math definitions, not allocation indices, are shared.
        numbers = list(_NUMBER.finditer(expression))
        kinds = [_quantity(name, value) for value in _literal_values(expression)]
        gated = any(kind is not None for kind in kinds)
        if gated or name in {"deg2rad", "rad2deg", "jtoumolpar", "kdntopar", "eps_fp"}:
            shared.add(name)
        if not gated:
            continue
        if not attributes.startswith("integer"):
            if any(number["marker"] not in {"d", "D"} for number in numbers):
                errors.append(f"{path.name}: {name}: use double-precision D literals")
            if not re.search(
                r"kind\s*\(\s*1d0\s*\)|real64|kind\s*=\s*8|real\s*\(\s*8\s*\)|double\s+precision",
                attributes,
            ):
                errors.append(f"{path.name}: {name}: declare double precision")
        # A single literal defines a quantity; expressions such as pi/180
        # have their own units and must not be classified by the denominator.
        if len(numbers) != 1 or not _NUMBER.fullmatch(expression):
            continue
        value = _literal_values(expression)[0]
        quantity = _quantity(name, value)
        if quantity == "reference_pressure":
            value *= 100 if 200 < value < 2000 else 1000 if value < 200 else 1
        if quantity:
            previous = quantities.get(quantity)
            if previous and previous[1] != value:
                errors.append(
                    f"{path.name}: {name} conflicts with {previous[0]} for {quantity}"
                )
            quantities[quantity] = (name, value)

    return shared, errors


def _check_physics_copies(src: Path, shared: set[str]) -> list[str]:
    """Reject declarations and immutable copies that hide shared constants."""
    errors = []
    for path in sorted([*src.rglob("*.f95"), *src.rglob("*.f90")]):
        if not path.name.startswith("suews_phys_"):
            continue
        source = path.read_text(encoding="utf-8")
        names = set(shared)
        # USE renaming is legal, but copying the renamed import is not.
        for line in _code_lines(source):
            if re.match(r"\s*use\b", line, re.I):
                for local, remote in re.findall(r"(\w+)\s*=>\s*(\w+)", line.lower()):
                    if remote in names:
                        names.add(local)
        declared = set()
        for _, name, expression in _declarations(source):
            declared.add(name)
            if name in names:
                errors.append(f"{path.name}: {name}: redeclares a shared constant")
            if expression.strip("() ") in names:
                errors.append(
                    f"{path.name}: {name}: copies shared constant {expression}"
                )
        # Also catch a separately declared module/local variable assigned the
        # shared constant later (component assignments are model state).
        assignments = re.findall(
            r"(?im)^\s*(\w+)\s*=(?!=|>)", "\n".join(_code_lines(source))
        )
        for line in _code_lines(source):
            for statement in line.split(";"):
                match = re.fullmatch(r"\s*(\w+)\s*=\s*\(?\s*(\w+)\s*\)?\s*", statement)
                if (
                    match
                    and match[1].lower() in declared
                    and match[2].lower() in names
                    and sum(n.lower() == match[1].lower() for n in assignments) <= 1
                ):
                    errors.append(
                        f"{path.name}: {match[1]}: copies shared constant {match[2]}"
                    )
    return errors


def check_shared_constants(repo_root: Path) -> list[str]:
    """Enforce precision, unique quantities and direct use of shared constants."""
    src = repo_root / "src" / "suews" / "src"
    path = src / "suews_ctrl_const.f95"
    if not path.is_file():
        return []  # Small fixture trees can exercise the literal ratchet alone.
    shared, errors = _check_definitions(path)
    return errors + _check_physics_copies(src, shared)


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

    return _check_baseline(repo_root, args.write_baseline)


def _check_baseline(repo_root: Path, write_baseline: bool) -> int:
    """Validate definitions before comparing or updating the literal ratchet."""
    definition_errors = check_shared_constants(repo_root)
    if definition_errors:
        for error in definition_errors:
            print(f"[X] {error}", file=sys.stderr)
        return 1

    actual = scan_tree(repo_root, GATED_KINDS)
    baseline_path = repo_root / BASELINE_PATH
    if write_baseline:
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
