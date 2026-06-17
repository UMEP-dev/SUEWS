#!/usr/bin/env python3
"""Resolve the CPython build/test matrix from pyproject ``requires-python``.

For an abi3 / limited-API wheel the build floor is the *lowest* supported
CPython -- the lower bound of ``project.requires-python``. One wheel built
there covers every newer CPython, so the floor is the only build target we
need. Deriving the matrix from ``requires-python`` keeps ``pyproject.toml`` as
the single source of truth and stops the CI build matrix drifting from the
declared support floor.

Background (gh#1553): raising ``requires-python`` to ``>=3.12`` while
``determine-matrix.sh`` still hardcoded a ``cp39`` build target handed
cibuildwheel ``cp39`` together with ``>=3.12``. The intersection was empty and
the build aborted with ``cibuildwheel: No build identifiers selected``. This
helper removes that second, drift-prone source of truth.

This script reads the pyproject as DATA only -- nothing in it is executed --
so it is safe to point at a pull-request's pyproject from a base-trusted
caller.

Modes (exactly one):
  --floor-tag           print the floor build tag, e.g. ``cp312``
  --clamp   '<json>'    print the cpXY JSON list with entries below the floor dropped
  --bookend '<json>'    print ``["cp<floor>", "cp<newest-after-clamp>"]``

All cpXY tags are assumed to be CPython 3.x (``cp3YY``); a non-3.x tag is a
hard error rather than a silent guess.
"""

from __future__ import annotations

import argparse
import json
import re
import sys

try:
    import tomllib
except ModuleNotFoundError:  # tomllib is stdlib only on Python >= 3.11
    tomllib = None


def _die(msg: str) -> None:
    # Emit a GitHub Actions error annotation and fail closed.
    sys.exit(f"::error::clamp_python_floor: {msg}")


def _read_requires_python(path: str) -> str:
    try:
        with open(path, encoding="utf-8") as handle:
            text = handle.read()
    except OSError as exc:
        _die(f"cannot read {path}: {exc}")

    # Prefer a real TOML parse; fall back to a line regex if tomllib is absent
    # or the file is malformed (matches classify_pyproject.py's defensiveness).
    if tomllib is not None:
        try:
            value = tomllib.loads(text).get("project", {}).get("requires-python")
            if value:
                return value
        except (tomllib.TOMLDecodeError, AttributeError, TypeError):
            pass

    match = re.search(
        r'^\s*requires-python\s*=\s*["\']([^"\']+)["\']', text, re.MULTILINE
    )
    if not match:
        _die(f"no project.requires-python found in {path}")
    return match.group(1)


def _floor_minor(path: str) -> int:
    requires_python = _read_requires_python(path)
    match = re.search(r">=\s*3\.(\d+)", requires_python)
    if not match:
        _die(f"requires-python={requires_python!r} has no '>=3.Y' lower bound")
    return int(match.group(1))


def _tag_minor(tag: str) -> int:
    match = re.fullmatch(r"cp3(\d+)", tag.strip())
    if not match:
        _die(f"not a CPython 3.x cpXY tag: {tag!r}")
    return int(match.group(1))


def _load_candidates(raw: str) -> list[str]:
    try:
        candidates = json.loads(raw)
    except json.JSONDecodeError as exc:
        _die(f"invalid cpXY JSON list {raw!r}: {exc}")
    if not isinstance(candidates, list) or not all(
        isinstance(item, str) for item in candidates
    ):
        _die(f"expected a JSON list of cpXY strings, got {raw!r}")
    return candidates


def main() -> None:
    """Parse arguments and print the requested matrix value."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pyproject", required=True, help="path to pyproject.toml")
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--floor-tag", action="store_true", help="print cp<floor>")
    mode.add_argument("--clamp", metavar="JSON", help="drop tags below the floor")
    mode.add_argument(
        "--bookend", metavar="JSON", help="print [floor, newest] after clamping"
    )
    args = parser.parse_args()

    floor = _floor_minor(args.pyproject)

    if args.floor_tag:
        print(f"cp3{floor}")
        return

    raw = args.clamp if args.clamp is not None else args.bookend
    candidates = _load_candidates(raw)
    kept = [tag for tag in candidates if _tag_minor(tag) >= floor]
    if not kept:
        _die(f"no candidate CPython >= 3.{floor} in {candidates}")

    if args.bookend is not None and len(kept) > 1:
        kept = [kept[0], kept[-1]]

    print(json.dumps(kept))


if __name__ == "__main__":
    main()
