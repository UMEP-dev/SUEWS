#!/usr/bin/env python3
"""Flag pathlib ``read_text`` / ``write_text`` calls that omit ``encoding=``.

This complements ruff ``PLW1514`` (``unspecified-encoding``). PLW1514 only
flags calls whose receiver type it can resolve -- builtin ``open()`` and
``Path(...).read_text()`` where the ``Path`` is directly constructed. It
silently skips the very common form where the receiver is a variable or a
chained expression, e.g.::

    p = some_dir / "x.json"
    p.write_text(payload)            # ruff does NOT flag this
    cfg = resource.read_text()       # nor this

On Windows those inherit the cp1252 default encoding and raise
``UnicodeEncodeError`` on Unicode content -- the exact failure class that
hit the 2026-06-06 nightly (gh#1097, gh#902). Because ``read_text`` /
``write_text`` are pathlib-specific method names, flagging *every* such
call without ``encoding=`` is high precision (virtually no non-Path class
defines them), so this AST check needs no type inference.

Usage::

    python scripts/lint/check_pathlib_encoding.py [PATH ...]

With no PATH, scans the repository tree from the current directory.
Exits 1 if any offending call is found.
"""

from __future__ import annotations

import argparse
import ast
import sys
from pathlib import Path

METHODS = {"read_text", "write_text"}

# Characters that make up a file-mode string (e.g. "r", "w", "rb", "a+").
_MODE_CHARS = set("rwxabt+")

# Directories that never contain first-party source to lint.
EXCLUDE_DIRS = {
    ".git",
    ".ruff_cache",
    ".venv",
    "venv",
    "__pycache__",
    "build",
    "dist",
    "docs/_build",
    ".claude/worktrees",
}

# Generated files (mirror the ruff exclude list).
EXCLUDE_FILES = {"_version.py", "_suews_driver.py"}


def _has_encoding(call: ast.Call) -> bool:
    for kw in call.keywords:
        # kw.arg is None for **kwargs splat -- treat as "cannot prove missing"
        # and accept it, to avoid false positives on dynamic kwargs.
        if kw.arg in (None, "encoding"):
            return True
    return False


def _looks_like_mode(s: str) -> bool:
    """True if ``s`` looks like a file-mode string (``"r"``, ``"w"``, ``"rb"``)."""
    return bool(s) and all(c in _MODE_CHARS for c in s)


def _open_is_unencoded_text(call: ast.Call) -> bool:
    """Decide whether a ``.open(...)`` call is an unencoded *text-mode* open.

    Returns True only when we can prove the call opens in text mode without an
    explicit encoding. This deliberately skips:
      - binary modes (``"rb"``/``"wb"``) -- encoding is illegal there;
      - calls whose first arg is a non-mode string (``zf.open("x.txt")``) or a
        non-constant -- almost certainly ``zipfile``/``tarfile``/``importlib``
        style, not ``pathlib.Path.open``, so flagging would be a false positive.
    """
    if _has_encoding(call):
        return False

    mode = None
    if call.args:
        first = call.args[0]
        if isinstance(first, ast.Constant) and isinstance(first.value, str):
            mode = first.value
        else:
            # non-constant / non-str first arg -> can't prove text; skip.
            return False
    else:
        for kw in call.keywords:
            if kw.arg == "mode":
                if isinstance(kw.value, ast.Constant) and isinstance(
                    kw.value.value, str
                ):
                    mode = kw.value.value
                else:
                    return False
        if mode is None:
            mode = "r"  # pathlib default

    if not _looks_like_mode(mode):
        return False  # e.g. "data.csv" -> zipfile-style name, not a mode
    return "b" not in mode


def check_file(path: Path) -> list[tuple[int, str]]:
    try:
        tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    except (SyntaxError, UnicodeDecodeError):
        return []
    findings: list[tuple[int, str]] = []
    for node in ast.walk(tree):
        if not (isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)):
            continue
        attr = node.func.attr
        if attr in METHODS and not _has_encoding(node):
            findings.append((node.lineno, attr))
        elif attr == "open" and _open_is_unencoded_text(node):
            findings.append((node.lineno, "open"))
    return findings


def _iter_py_files(root: Path):
    for p in root.rglob("*.py"):
        rel = p.relative_to(root).as_posix()
        if any(part in EXCLUDE_DIRS for part in p.parts):
            continue
        if any(rel.startswith(d + "/") or rel == d for d in EXCLUDE_DIRS):
            continue
        if p.name in EXCLUDE_FILES:
            continue
        yield p


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("paths", nargs="*", help="files or dirs to scan")
    args = parser.parse_args(argv)

    targets: list[Path] = []
    if args.paths:
        for raw in args.paths:
            p = Path(raw)
            if p.is_dir():
                targets.extend(_iter_py_files(p))
            elif p.suffix == ".py":
                targets.append(p)
    else:
        targets.extend(_iter_py_files(Path(".")))

    total = 0
    for path in sorted(set(targets)):
        for lineno, method in check_file(path):
            total += 1
            print(
                f"{path}:{lineno}: `{method}` without explicit `encoding=` "
                f"(add encoding=\"utf-8\")"
            )

    if total:
        print(
            f"\n[X] {total} pathlib text-I/O call(s) missing encoding. "
            "Add encoding=\"utf-8\". See .claude/rules/python/conventions.md (rule 6)."
        )
        return 1
    print("[OK] All pathlib read_text/write_text calls specify an encoding.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
