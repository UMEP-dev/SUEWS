#!/usr/bin/env python3
"""Static ratchet: physical-constant literals in SuPy Python code can only go down.

The Python layer centralises physical, mathematical, and unit-conversion
constants in ``src/supy/constants.py``, aligned with
``src/suews/src/suews_ctrl_const.f95``. This AST-based lint prevents new bare
constant literals from entering ``src/supy/`` while remaining baseline entries
are retired.

It parses every ``*.py`` file under ``src/supy`` (excluding
``CONSTANT_MODULES`` and generated ``EXCLUDED_FILES``), counts occurrences of
``GATED_KINDS`` (both standalone numeric literals and multiplicative/divisive
expressions such as ``24 * 3600``, ``24 * 60 * 60``, ``pi / 180``, and
``180 / pi``), and compares the per-file counts against the committed baseline
``scripts/lint/python_literals_baseline.json``:

- a count above its baseline fails: import the named constant from
  ``supy.constants`` instead;
- a count below its baseline also fails, so the baseline cannot drift above
  the tree: lower it in the same diff with ``--write-baseline``.

Because the scan operates on Python AST nodes, comments, docstrings, string
literals, version strings, booleans, identifiers, and bare integer ``Enum``
member codes (such as error codes in ``ErrorCode``) are never matched; float
``Enum`` members, ``Enum`` expressions, and ``Enum`` methods are still checked.
``REPORTED_KINDS`` (factors of 1000, 0.001, 1e6, and 1e-6) are printed by
``--report`` and never gated.

Exits 0 when every count equals its baseline, 1 otherwise.
"""

from __future__ import annotations

import argparse
import ast
from collections.abc import Callable
import json
import math
from pathlib import Path
import sys

BASELINE_PATH = Path("scripts") / "lint" / "python_literals_baseline.json"
SOURCE_ROOT = Path("src") / "supy"

# Modules whose job is to define the named constants. Literals there are the
# canonical definitions the rest of SuPy imports, so they are not counted.
CONSTANT_MODULES = frozenset({"src/supy/constants.py"})

# Generated build/SCM files matching .ruff.toml exclusions.
EXCLUDED_FILES = frozenset({
    "src/supy/_version.py",
    "src/supy/_suews_driver.py",
})

_ENUM_BASE_NAMES = frozenset({"Enum", "Flag", "IntEnum", "IntFlag", "ReprEnum"})

REMEDIATION = """\
Physical constants and unit conversions belong in src/supy/constants.py
(aligned with src/suews/src/suews_ctrl_const.f95), not in new literals.

  - count went up    ->  import the named constant from supy.constants
                         (for example C2K for 273.15, SEC_PER_HOUR for 3600,
                         SEC_PER_DAY for 86400, DEG2RAD for pi / 180); if none
                         exists yet, add one aligned with suews_ctrl_const.f95.
  - count went down  ->  thank you; lower the baseline in this diff:
                         python scripts/lint/check_python_literals.py --write-baseline\
"""


def _near(target: float, rel: float) -> Callable[[float], bool]:
    """Match a value within a relative tolerance of ``target``."""
    return lambda value: math.isclose(value, target, rel_tol=rel)


def _one_of(*targets: float) -> Callable[[float], bool]:
    """Match a value exactly equal to one of ``targets``."""
    return lambda value: value in targets


# Each kind maps a parsed numeric literal value to a match decision. This set
# mirrors scripts/lint/check_fortran_literals.py so Python and Fortran enforce
# the same constant-literal ratchet.
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


def _constant_float(node: ast.AST) -> float | None:
    """Return finite float value if ``node`` is an int or float Constant."""
    if not isinstance(node, ast.Constant):
        return None
    value = node.value
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return None
    try:
        numeric = float(value)
    except OverflowError:
        return None
    return numeric if math.isfinite(numeric) else None


def _is_name_chain(node: ast.AST) -> bool:
    """Return True when ``node`` is a plain identifier or dotted attribute chain."""
    while isinstance(node, ast.Attribute):
        node = node.value
    return isinstance(node, ast.Name)


def _is_pi_ref(node: ast.AST) -> bool:
    """Return True when ``node`` refers to symbolic pi (e.g. ``pi``, ``np.pi``)."""
    if isinstance(node, ast.Name):
        return node.id in {"pi", "PI"}
    if isinstance(node, ast.Attribute):
        return node.attr in {"pi", "PI"} and _is_name_chain(node.value)
    return False


def _is_enum_class(node: ast.ClassDef) -> bool:
    """Return True when ``node`` inherits directly from a stdlib ``Enum`` base."""
    return any(
        (isinstance(base, ast.Name) and base.id in _ENUM_BASE_NAMES)
        or (isinstance(base, ast.Attribute) and base.attr in _ENUM_BASE_NAMES)
        for base in node.bases
    )


def _is_bare_int_member(stmt: ast.stmt) -> bool:
    """Return True when ``stmt`` assigns a bare integer literal directly."""
    if isinstance(stmt, ast.Assign) or (
        isinstance(stmt, ast.AnnAssign) and stmt.value is not None
    ):
        value = stmt.value
        return isinstance(value, ast.Constant) and type(value.value) is int
    return False


def _flatten_mul_div(
    node: ast.AST,
    numerators: list[ast.AST],
    denominators: list[ast.AST],
    *,
    in_denominator: bool = False,
) -> None:
    """Decompose a maximal ``*`` / ``/`` tree into numerator and denominator factors."""
    while isinstance(node, ast.UnaryOp) and isinstance(node.op, (ast.UAdd, ast.USub)):
        node = node.operand
    if isinstance(node, ast.BinOp) and isinstance(node.op, ast.Mult):
        _flatten_mul_div(
            node.left, numerators, denominators, in_denominator=in_denominator
        )
        _flatten_mul_div(
            node.right, numerators, denominators, in_denominator=in_denominator
        )
    elif isinstance(node, ast.BinOp) and isinstance(node.op, ast.Div):
        _flatten_mul_div(
            node.left, numerators, denominators, in_denominator=in_denominator
        )
        _flatten_mul_div(
            node.right, numerators, denominators, in_denominator=not in_denominator
        )
    elif in_denominator:
        denominators.append(node)
    else:
        numerators.append(node)


def _find_index(
    factors: list[ast.AST], predicate: Callable[[ast.AST], bool]
) -> int | None:
    """Return index of the first factor matching ``predicate``, else None."""
    for index, node in enumerate(factors):
        if predicate(node):
            return index
    return None


def _is_exact_value(target: float) -> Callable[[ast.AST], bool]:
    """Return a predicate matching numeric Constant nodes equal to ``target``."""
    return lambda node: _constant_float(node) == target


def _consume_degree_radian(
    numerators: list[ast.AST], denominators: list[ast.AST]
) -> list[tuple[int, str]]:
    """Consume ``pi / 180`` and ``180 / pi`` pairs across numerator/denominator."""
    found: list[tuple[int, str]] = []
    is_180 = _is_exact_value(180.0)
    for left, right in ((numerators, denominators), (denominators, numerators)):
        while True:
            idx_pi = _find_index(left, _is_pi_ref)
            idx_deg = _find_index(right, is_180)
            if idx_pi is None or idx_deg is None:
                break
            pi_node = left.pop(idx_pi)
            deg_node = right.pop(idx_deg)
            lineno = min(
                getattr(pi_node, "lineno", 0),
                getattr(deg_node, "lineno", 0),
            )
            found.append((lineno, "degree_radian"))
    return found


def _consume_seconds_per_day(factors: list[ast.AST]) -> list[tuple[int, str]]:
    """Consume ``24 * 3600`` and ``24 * 60 * 60`` products on one side of a ratio."""
    found: list[tuple[int, str]] = []
    is_24 = _is_exact_value(24.0)
    is_60 = _is_exact_value(60.0)
    is_3600 = _is_exact_value(3600.0)
    while True:
        idx_24 = _find_index(factors, is_24)
        idx_3600 = _find_index(factors, is_3600)
        if idx_24 is None or idx_3600 is None:
            break
        n24, n3600 = factors[idx_24], factors[idx_3600]
        for idx in sorted((idx_24, idx_3600), reverse=True):
            factors.pop(idx)
        lineno = min(getattr(n24, "lineno", 0), getattr(n3600, "lineno", 0))
        found.append((lineno, "seconds_per_day"))
    while True:
        idx_24 = _find_index(factors, is_24)
        sixty_indices = [i for i, n in enumerate(factors) if is_60(n)]
        if idx_24 is None or len(sixty_indices) < 2:
            break
        picked = (idx_24, sixty_indices[0], sixty_indices[1])
        nodes = [factors[i] for i in picked]
        for idx in sorted(picked, reverse=True):
            factors.pop(idx)
        lineno = min(getattr(n, "lineno", 0) for n in nodes)
        found.append((lineno, "seconds_per_day"))
    return found


class _LiteralVisitor(ast.NodeVisitor):
    """Collect gated/reported literal and expression occurrences from a Python AST."""

    def __init__(self, kinds: dict[str, Callable[[float], bool]]) -> None:
        self.kinds = kinds
        self.occurrences: list[tuple[int, str]] = []

    def visit_ClassDef(self, node: ast.ClassDef) -> None:
        """Skip bare integer member codes on Enum classes while checking methods."""
        if not _is_enum_class(node):
            self.generic_visit(node)
            return
        for child in (*node.bases, *node.keywords, *node.decorator_list):
            self.visit(child)
        for stmt in node.body:
            if _is_bare_int_member(stmt):
                continue
            self.visit(stmt)

    def visit_BinOp(self, node: ast.BinOp) -> None:
        """Inspect multiplicative/divisive chains before visiting remaining factors."""
        if not isinstance(node.op, (ast.Mult, ast.Div)):
            self.generic_visit(node)
            return
        numerators: list[ast.AST] = []
        denominators: list[ast.AST] = []
        _flatten_mul_div(node, numerators, denominators)
        if "degree_radian" in self.kinds:
            self.occurrences.extend(_consume_degree_radian(numerators, denominators))
        if "seconds_per_day" in self.kinds:
            self.occurrences.extend(_consume_seconds_per_day(numerators))
            self.occurrences.extend(_consume_seconds_per_day(denominators))
        for factor in (*numerators, *denominators):
            self.visit(factor)

    def visit_Constant(self, node: ast.Constant) -> None:
        """Record standalone numeric literals matching any configured kind."""
        value = _constant_float(node)
        if value is None:
            return
        for kind, matches in self.kinds.items():
            if matches(value):
                self.occurrences.append((node.lineno, kind))


def find_occurrences(
    source: str,
    kinds: dict[str, Callable[[float], bool]],
    *,
    filename: str = "<string>",
) -> list[tuple[int, str]]:
    """Return sorted ``(lineno, kind)`` occurrences in one Python source string."""
    tree = ast.parse(source, filename=filename)
    visitor = _LiteralVisitor(kinds)
    visitor.visit(tree)
    return sorted(visitor.occurrences)


def count_kinds(
    source: str,
    kinds: dict[str, Callable[[float], bool]],
    *,
    filename: str = "<string>",
) -> dict[str, int]:
    """Count literals and expressions of each kind in one Python source string."""
    counts = dict.fromkeys(kinds, 0)
    for _, kind in find_occurrences(source, kinds, filename=filename):
        counts[kind] += 1
    return counts


def _iter_python_sources(repo_root: Path) -> list[Path]:
    """Return sorted ``src/supy/**/*.py`` paths subject to the lint."""
    src_root = repo_root / SOURCE_ROOT
    paths: list[Path] = []
    for path in sorted(src_root.rglob("*.py")):
        if "__pycache__" in path.parts:
            continue
        rel = path.relative_to(repo_root).as_posix()
        if rel in CONSTANT_MODULES or rel in EXCLUDED_FILES:
            continue
        paths.append(path)
    return paths


def scan_tree(
    repo_root: Path, kinds: dict[str, Callable[[float], bool]]
) -> dict[str, dict[str, int]]:
    """Return non-zero counts keyed by repo-relative file, then kind."""
    result: dict[str, dict[str, int]] = {}
    for path in _iter_python_sources(repo_root):
        rel = path.relative_to(repo_root).as_posix()
        source = path.read_text(encoding="utf-8")
        counts = {
            k: n
            for k, n in sorted(count_kinds(source, kinds, filename=rel).items())
            if n
        }
        if counts:
            result[rel] = counts
    return result


def compare(
    actual: dict[str, dict[str, int]], baseline: dict[str, dict[str, int]]
) -> tuple[list[str], list[str]]:
    """Return ``(increases, decreases)`` as human-readable diagnostic lines."""
    increases: list[str] = []
    decreases: list[str] = []
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
            "Per-file counts of gated Python literal kinds; see "
            "scripts/lint/check_python_literals.py. Counts may only go down."
        ),
        "totals": _totals(counts),
        "files": counts,
    }
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def _load_baseline(path: Path) -> dict[str, dict[str, int]]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict) or not isinstance(payload.get("files"), dict):
        raise ValueError(
            f"baseline {path} must be a JSON object with a 'files' mapping"
        )
    validated: dict[str, dict[str, int]] = {}
    for rel, counts in payload["files"].items():
        if not isinstance(rel, str) or not isinstance(counts, dict):
            raise ValueError(f"invalid baseline entry for {rel!r}")
        validated[rel] = {}
        for kind, count in counts.items():
            if (
                not isinstance(kind, str)
                or isinstance(count, bool)
                or not isinstance(count, int)
                or count < 0
            ):
                raise ValueError(f"invalid count for {rel}:{kind}: {count!r}")
            validated[rel][kind] = count
    return validated


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


def _check_against_baseline(
    actual: dict[str, dict[str, int]], baseline_path: Path
) -> int:
    if not baseline_path.is_file():
        print(f"[X] baseline {BASELINE_PATH.as_posix()} not found", file=sys.stderr)
        return 1
    try:
        baseline = _load_baseline(baseline_path)
    except (OSError, UnicodeError, json.JSONDecodeError, ValueError) as exc:
        print(f"[X] failed to read baseline: {exc}", file=sys.stderr)
        return 1

    increases, decreases = compare(actual, baseline)
    if increases or decreases:
        if increases:
            print("[X] new Python literal constants:", file=sys.stderr)
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
        f"[OK] Python literal constants match the baseline "
        f"({total} gated literals in {len(actual)} files)."
    )
    return 0


def main(argv: list[str] | None = None) -> int:
    """Compare the tree against the baseline, or rewrite or report it."""
    args_list = sys.argv[1:] if argv is None else argv[1:]
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
    args = parser.parse_args(args_list)

    repo_root = Path(args.repo_root).resolve()
    if not (repo_root / SOURCE_ROOT).is_dir():
        print(
            f"[X] {SOURCE_ROOT.as_posix()} not found under {repo_root}",
            file=sys.stderr,
        )
        return 1

    try:
        if args.report:
            _print_report(repo_root)
            return 0
        actual = scan_tree(repo_root, GATED_KINDS)
    except (OSError, UnicodeError, SyntaxError) as exc:
        print(f"[X] failed to scan Python sources: {exc}", file=sys.stderr)
        return 1

    baseline_path = repo_root / BASELINE_PATH
    if args.write_baseline:
        _write_baseline(baseline_path, actual)
        print(f"[OK] wrote {BASELINE_PATH.as_posix()}: {_totals(actual)}")
        return 0
    return _check_against_baseline(actual, baseline_path)


if __name__ == "__main__":
    sys.exit(main(sys.argv))
