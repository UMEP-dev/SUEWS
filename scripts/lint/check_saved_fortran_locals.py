#!/usr/bin/env python3
"""Static lint: Fortran procedure locals must not be implicitly SAVEd.

In Fortran an initialiser in a declaration (``INTEGER :: n = 0``, or
``REAL, POINTER :: p => NULL()``) gives the variable the SAVE attribute. It
is then static: its value survives from one call to the next, and every grid
running on a Rust bridge worker thread reads and writes the same copy.
``-frecursive`` does not change that; it only moves locals *without* an
initialiser onto the stack.

This let one grid's wet-bulb iteration in ``Lat_vap`` take a step size set
by another grid, so parallel multi-grid output differed from serial output
and from one parallel run to the next (gh#1741).

What it flags, inside a SUBROUTINE or FUNCTION body only:

- a non-PARAMETER declaration carrying an initialiser;
- a SAVE attribute or a SAVE statement.

Derived-type component defaults (inside ``TYPE ... END TYPE``) are default
initialisation, not SAVE, and are not flagged. Module-level variables are
static too but are out of scope here: they are legacy globals tracked
separately.

Exits 0 when the tree is clean, 1 otherwise.
"""

from __future__ import annotations

from pathlib import Path
import re
import sys

REMEDIATION = """\
A declaration initialiser makes a Fortran local static (implicit SAVE): it
keeps its value between calls and is shared by every grid thread.

  - constant value      ->  add PARAMETER:  INTEGER, PARAMETER :: from = 2
  - value set per call  ->  drop the initialiser and assign it at the top of
                            the executable part:  LOGICAL :: flag ... flag = .FALSE.

Before adding PARAMETER, check that no callee assigns to the dummy argument
it is passed to (a dummy without INTENT can be written), or the write will
hit read-only storage. Genuine per-grid persistence belongs in the model
state (SUEWS_STATE), not in a SAVEd local.\
"""

# Known offenders awaiting a dedicated fix. Each needs per-grid state, not a
# runtime assignment: they are deliberate "first call" counters.
ALLOWED: dict[tuple[str, str], str] = {
    ("suews_phys_estm.f95", "estmstart"): "ESTM first-call counter keyed on Gridiv == 1",
    ("suews_phys_estm.f95", "tair2set"): "ESTM first-call counter keyed on Gridiv == 1",
}

_TYPE_WORDS = r"(?:INTEGER|REAL|LOGICAL|CHARACTER|DOUBLE\s+PRECISION|COMPLEX|TYPE\s*\(|CLASS\s*\()"
_DECL = re.compile(rf"^{_TYPE_WORDS}", re.I)
_PROC_START = re.compile(
    r"^(?:(?:PURE|ELEMENTAL|RECURSIVE|IMPURE|MODULE)\s+)*"
    r"(?:(?:INTEGER|REAL|LOGICAL|CHARACTER|DOUBLE\s+PRECISION|TYPE\s*\([^)]*\))"
    r"(?:\s*\([^)]*\))?\s+)?"
    r"(?:SUBROUTINE|FUNCTION)\s+\w+",
    re.I,
)
_BLOCK_END = re.compile(r"^END\s*(?:SUBROUTINE|FUNCTION|MODULE|TYPE|INTERFACE|PROGRAM)?\b", re.I)
_CONTROL_END = re.compile(r"^END\s*(?:IF|DO|SELECT|WHERE|ASSOCIATE|BLOCK|FORALL|ENUM)\b", re.I)


def _strip_comment(line: str) -> str:
    """Drop a trailing ``!`` comment, respecting quoted strings."""
    quote = None
    for idx, char in enumerate(line):
        if quote:
            if char == quote:
                quote = None
        elif char in "'\"":
            quote = char
        elif char == "!":
            return line[:idx]
    return line


def _logical_lines(source: str) -> list[tuple[int, str]]:
    """Join ``&`` continuations; return (first physical line, statement)."""
    statements: list[tuple[int, str]] = []
    buffer, start = "", None
    for lineno, raw in enumerate(source.splitlines(), 1):
        text = _strip_comment(raw).strip()
        if text.startswith("#"):  # preprocessor directive
            continue
        if start is None:
            start = lineno
        if text.startswith("&"):
            text = text[1:].lstrip()
        if text.endswith("&"):
            buffer += text[:-1] + " "
            continue
        buffer += text
        # `;` separates statements on one line
        for part in buffer.split(";"):
            statements.append((start, part.strip()))
        buffer, start = "", None
    return statements


def _entities(entity_list: str) -> list[str]:
    """Split a declaration entity list on top-level commas."""
    parts, depth, current = [], 0, ""
    for char in entity_list:
        if char in "([":
            depth += 1
        elif char in ")]":
            depth -= 1
        if char == "," and depth == 0:
            parts.append(current)
            current = ""
        else:
            current += char
    parts.append(current)
    return parts


def find_hits(source: str) -> list[tuple[int, str, str]]:
    """Return (line, variable, statement) for each implicitly SAVEd local."""
    hits: list[tuple[int, str, str]] = []
    stack: list[str] = []
    for lineno, stmt in _logical_lines(source):
        if not stmt:
            continue
        upper = stmt.upper()
        if re.match(r"^MODULE\s+(?!PROCEDURE\b)\w+$", upper):
            stack.append("MODULE")
            continue
        if re.match(r"^(?:ABSTRACT\s+)?INTERFACE\b", upper):
            stack.append("INTERFACE")
            continue
        if re.match(r"^TYPE\s*(?:,[^:]*)?::\s*\w+$", upper) or re.match(r"^TYPE\s+\w+$", upper):
            stack.append("TYPE")
            continue
        if re.match(r"^PROGRAM\s+\w+", upper) or (
            _PROC_START.match(stmt) and not upper.startswith("END")
        ):
            stack.append("PROC")
            continue
        if _BLOCK_END.match(stmt) and not _CONTROL_END.match(stmt):
            if stack:
                stack.pop()
            continue
        if not stack or stack[-1] != "PROC":
            continue
        if re.match(r"^SAVE\b", upper):
            hits.append((lineno, "SAVE", stmt))
            continue
        if not (_DECL.match(stmt) and "::" in stmt):
            continue
        attrs, _, entity_list = stmt.partition("::")
        if re.search(r"\bPARAMETER\b", attrs, re.I):
            continue
        explicit_save = re.search(r"\bSAVE\b", attrs, re.I) is not None
        for entity in _entities(entity_list):
            name = re.match(r"\s*(\w+)", entity)
            if name is None:
                continue
            # `=` at top level of the entity (not inside a dimension spec)
            depth, has_init = 0, False
            for char in entity:
                if char in "([":
                    depth += 1
                elif char in ")]":
                    depth -= 1
                elif char in "=" and depth == 0:
                    has_init = True
            if has_init or explicit_save:
                hits.append((lineno, name.group(1), stmt))
    return hits


def main(argv: list[str]) -> int:
    """Check every Fortran source under src/suews/src."""
    repo_root = Path(argv[1]).resolve() if len(argv) > 1 else Path.cwd()
    src_root = repo_root / "src" / "suews" / "src"
    if not src_root.is_dir():
        print(f"[X] src/suews/src not found under {repo_root}", file=sys.stderr)
        return 1

    offenders: list[str] = []
    checked = 0
    for path in sorted([*src_root.rglob("*.f95"), *src_root.rglob("*.f90")]):
        checked += 1
        source = path.read_text(encoding="utf-8", errors="replace")
        for lineno, name, stmt in find_hits(source):
            if (path.name, name.lower()) in ALLOWED:
                continue
            rel = path.relative_to(repo_root).as_posix()
            offenders.append(f"{rel}:{lineno}: {name}  [{stmt}]")

    if offenders:
        print("[X] implicitly SAVEd Fortran procedure locals:", file=sys.stderr)
        for offender in offenders:
            print(f"  - {offender}", file=sys.stderr)
        print("", file=sys.stderr)
        print(REMEDIATION, file=sys.stderr)
        return 1

    print(
        f"[OK] {checked} Fortran files under src/suews/src have no implicitly "
        f"SAVEd procedure locals ({len(ALLOWED)} allowlisted)."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
