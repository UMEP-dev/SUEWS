#!/usr/bin/env python3
"""Static lint: every test file declares `physics` or `api` (gh#1300).

Complements the pytest_collection_finish hook in test/conftest.py. The
hook is the authoritative runtime check, but it requires importing supy
(which requires a compiled build). This script is a supy-free static
pass so CI can lint marker presence without building — keep CI cheap
for test-only PRs.

Exits 0 when every test file is covered, 1 otherwise.
"""

from __future__ import annotations

import ast
import sys
from pathlib import Path

NATURE_MARKERS = {"physics", "api"}

# Directories whose tests pick up `api` automatically via
# pytest_collection_modifyitems. Files under these prefixes are exempt
# from the static check.
AUTO_APPLIED_DIRS: tuple[str, ...] = ("test/umep/",)


def iter_test_files(test_root: Path):
    for path in sorted(test_root.rglob("test_*.py")):
        yield path


def _flatten_pytestmark(node: ast.AST):
    """Yield attribute-name fragments referenced by pytestmark assignments."""
    if isinstance(node, (ast.List, ast.Tuple)):
        for elt in node.elts:
            yield from _flatten_pytestmark(elt)
    elif isinstance(node, ast.Attribute):
        yield node.attr
    elif isinstance(node, ast.Call):
        yield from _flatten_pytestmark(node.func)


def file_has_nature_marker(source: str) -> bool:
    try:
        tree = ast.parse(source)
    except SyntaxError:
        return False
    for node in tree.body:
        if not isinstance(node, ast.Assign):
            continue
        targets = {t.id for t in node.targets if isinstance(t, ast.Name)}
        if "pytestmark" not in targets:
            continue
        fragments = set(_flatten_pytestmark(node.value))
        if fragments & NATURE_MARKERS:
            return True
    return False


def is_auto_applied(rel: Path) -> bool:
    rel_posix = rel.as_posix()
    return any(rel_posix.startswith(prefix) for prefix in AUTO_APPLIED_DIRS)


def main(argv: list[str]) -> int:
    repo_root = Path(argv[1]).resolve() if len(argv) > 1 else Path.cwd()
    test_root = repo_root / "test"
    if not test_root.is_dir():
        print(f"[X] test/ not found under {repo_root}", file=sys.stderr)
        return 1

    missing: list[str] = []
    checked = 0
    auto_applied = 0
    for path in iter_test_files(test_root):
        rel = path.relative_to(repo_root)
        if is_auto_applied(rel):
            auto_applied += 1
            continue
        checked += 1
        if file_has_nature_marker(path.read_text()):
            continue
        missing.append(rel.as_posix())

    if missing:
        print(
            "[X] gh#1300 marker lint: test files missing both `physics` and `api`:",
            file=sys.stderr,
        )
        for rel in missing:
            print(f"  - {rel}", file=sys.stderr)
        print(
            "Add `pytestmark = pytest.mark.api` (or physics) at module level, "
            "or auto-apply via a conftest.py hook.",
            file=sys.stderr,
        )
        return 1

    print(
        f"[OK] {checked} test files carry physics or api directly; "
        f"{auto_applied} files pick up a marker via conftest auto-apply."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
