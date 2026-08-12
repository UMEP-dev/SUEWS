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

Exits 0 when the tree is clean, 1 otherwise.
"""

from __future__ import annotations

import ast
import sys
from pathlib import Path

# Files exempted from the check, each with the reason it is exempt.
#
# Deliberately empty: the sweep was complete, so nothing needs exempting. Keep
# it that way. An allowlist that carries one entry tends to acquire a second,
# and the check is only worth having while it covers the whole tree. If you
# think you need an entry here, the bar is a file another in-flight branch owns
# -- and it comes with a note saying when to delete it.
ALLOWLIST: dict[str, str] = {}

REMEDIATION = """\
Reach packaged data through the package's own resource handle instead:

    from supy._env import trv_supy_module

    sample_dir = trv_supy_module / "sample_data"
    config = sample_dir / "sample_config.yml"

`trv_supy_module` is `importlib.resources.files("supy")`, which returns a
Traversable. The protocol guarantees only `/` (joinpath), `name`, `is_dir`,
`is_file`, `iterdir`, `open`, `read_bytes` and `read_text` -- so:

  - existence check      ->  `config.is_file()`, NOT `config.exists()`
  - read text            ->  `config.open(encoding="utf-8")` / `.read_text(...)`
  - a real path is needed (a str() path, subprocess, or any API that opens the
    file itself) ->  materialise it:

        from importlib.resources import as_file

        with as_file(config) as path_config:
            SUEWSConfig.from_yaml(str(path_config))

    In a unittest.TestCase, `self.enterContext(as_file(config))` binds it for
    the lifetime of the test.

All of the wrong forms happen to work under an editable install, where the
Traversable wraps a real filesystem path -- so a passing test run is NOT
evidence that a conversion is correct.

See `.claude/rules/tests/patterns.md` ("Locating packaged data") and the
existing `_sample_yaml_path` helper in `test/cmd/test_validate_config.py`.\
"""


class PackagedFileVisitor(ast.NodeVisitor):
    """Collect ``<something>.__file__`` attribute accesses."""

    def __init__(self) -> None:
        self.hits: list[tuple[int, str]] = []

    def visit_Attribute(self, node: ast.Attribute) -> None:
        if node.attr == "__file__":
            self.hits.append((node.lineno, ast.unparse(node)))
        self.generic_visit(node)


def find_hits(source: str) -> list[tuple[int, str]]:
    try:
        tree = ast.parse(source)
    except SyntaxError:
        # A file that does not parse is not this lint's problem; pytest and
        # ruff both surface it far more usefully.
        return []
    visitor = PackagedFileVisitor()
    visitor.visit(tree)
    return visitor.hits


def main(argv: list[str]) -> int:
    repo_root = Path(argv[1]).resolve() if len(argv) > 1 else Path.cwd()
    test_root = repo_root / "test"
    if not test_root.is_dir():
        print(f"[X] test/ not found under {repo_root}", file=sys.stderr)
        return 1

    offenders: list[str] = []
    checked = 0
    exempted: list[str] = []

    # Every .py file, not just `test_*.py` -- conftest.py and test helpers reach
    # for packaged data too, and a glob restricted to `test_*.py` would let a
    # reintroduction land there unseen.
    for path in sorted(test_root.rglob("*.py")):
        rel = path.relative_to(repo_root).as_posix()
        if rel in ALLOWLIST:
            exempted.append(rel)
            continue
        checked += 1
        for lineno, expr in find_hits(path.read_text(encoding="utf-8")):
            offenders.append(f"{rel}:{lineno}: {expr}")

    for rel in exempted:
        print(f"[!] EXEMPT {rel} -- {ALLOWLIST[rel]}")

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
        f"`<module>.__file__`; {len(exempted)} exempted."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
