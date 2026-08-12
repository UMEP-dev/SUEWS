#!/usr/bin/env python3
"""Static lint: tests must not reach packaged data through ``<module>.__file__``.

``Path(supy.__file__).parent / "sample_data"`` assumes the package is an
unpacked directory on disk. The import system does not guarantee that, and it
couples the test to supy's internal layout. supy resolves its own data with
``importlib.resources`` (``src/supy/_env.py``: ``trv_supy_module =
files("supy")``); tests should do the same.

This is a supy-free static pass, so CI can run it without building -- the same
reasoning as ``check_test_markers.py``, which this script is modelled on.

What it flags: any ``<something>.__file__`` attribute access under ``test/``.
Bare ``__file__`` (the test module's own location, used for fixture paths) is
an ``ast.Name`` rather than an ``ast.Attribute``, so it is never flagged.

Scope is deliberately ``test/`` only. Do not widen it to ``src/``:
``src/supy/cmd/json_envelope.py`` reads ``supy.__file__`` legitimately, to
find the install root when walking up for a ``.git`` directory. That is
locating the package, not reaching into its data, and it is correct.

Exits 0 when the tree is clean, 1 otherwise.
"""

from __future__ import annotations

import ast
import sys
from pathlib import Path

REMEDIATION = """\
Reach packaged data through the package's own resource handle:

    from supy._env import trv_supy_module

    config = trv_supy_module / "sample_data" / "sample_config.yml"

That returns a Traversable, not a Path, so:

  - existence check  ->  `config.is_file()`, NOT `config.exists()`
  - read text        ->  `config.open(encoding="utf-8")` / `.read_text(...)`
  - a real path (a str() path, subprocess, or any API that opens the file
    itself)          ->  `importlib.resources.as_file(config)`

Every wrong form passes under an editable install, so a green test run is not
evidence the conversion is right.

Full guidance, including how to bind a real path for a test's lifetime:
`.claude/rules/tests/patterns.md` ("Locating packaged data"). Worked examples:
`_sample_yaml_path` / `_copy_sample_data` in `test/cmd/test_validate_config.py`.\
"""


def find_hits(source: str) -> list[tuple[int, str]]:
    """Return (line, expression) for each ``<something>.__file__`` access."""
    try:
        tree = ast.parse(source)
    except SyntaxError:
        # A file that does not parse is not this lint's problem; pytest and
        # ruff both surface it far more usefully.
        return []
    # sorted() because ast.walk is breadth-first, so hits would otherwise be
    # reported in tree order rather than line order.
    return sorted(
        (node.lineno, ast.unparse(node))
        for node in ast.walk(tree)
        if isinstance(node, ast.Attribute) and node.attr == "__file__"
    )


def main(argv: list[str]) -> int:
    repo_root = Path(argv[1]).resolve() if len(argv) > 1 else Path.cwd()
    test_root = repo_root / "test"
    if not test_root.is_dir():
        print(f"[X] test/ not found under {repo_root}", file=sys.stderr)
        return 1

    offenders: list[str] = []
    checked = 0

    # Every .py file, not just `test_*.py` -- conftest.py and test helpers reach
    # for packaged data too, and a glob restricted to `test_*.py` would let a
    # reintroduction land there unseen.
    for path in sorted(test_root.rglob("*.py")):
        rel = path.relative_to(repo_root).as_posix()
        checked += 1
        for lineno, expr in find_hits(path.read_text(encoding="utf-8")):
            offenders.append(f"{rel}:{lineno}: {expr}")

    if offenders:
        print(
            "[X] packaged-data lint: tests reaching package data via `<module>.__file__`:",
            file=sys.stderr,
        )
        for offender in offenders:
            print(f"  - {offender}", file=sys.stderr)
        print("", file=sys.stderr)
        print(REMEDIATION, file=sys.stderr)
        return 1

    print(
        f"[OK] {checked} files under test/ reach packaged data without "
        f"`<module>.__file__`."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
