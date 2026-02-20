"""Meta test: verify every supy import used in the test suite actually exists.

This catches interface breakages (renamed/removed modules, functions, classes)
before any slow simulation tests run. It is self-maintaining: when new tests
add new imports, this test automatically validates them.
"""

import ast
import importlib
from pathlib import Path

import pytest

# Files to skip when scanning for imports
_SKIP_FILES = {"conftest.py", "debug_utils.py", "__init__.py"}


def _check_import_from(node, test_file, failures):
    """Check a ``from supy.x import y`` statement resolves at runtime."""
    try:
        mod = importlib.import_module(node.module)
    except Exception as exc:
        for alias in node.names:
            name = alias.name
            stmt = f"from {node.module} import {name}"
            failures.append((test_file.name, stmt, str(exc)))
        return

    for alias in node.names:
        name = alias.name
        if name == "*":
            continue
        if not hasattr(mod, name):
            stmt = f"from {node.module} import {name}"
            failures.append(
                (test_file.name, stmt, f"{node.module} has no attribute '{name}'")
            )


def _check_import(module_name, test_file, failures):
    """Check a ``import supy.x`` statement resolves at runtime."""
    try:
        importlib.import_module(module_name)
    except Exception as exc:
        stmt = f"import {module_name}"
        failures.append((test_file.name, stmt, str(exc)))


def _format_report(failures):
    """Format failures grouped by file for readable pytest output."""
    from collections import defaultdict

    by_file = defaultdict(list)
    for filename, stmt, error in failures:
        by_file[filename].append((stmt, error))

    lines = []
    for filename in sorted(by_file):
        lines.append(f"  {filename}:")
        for stmt, error in by_file[filename]:
            lines.append(f"    {stmt}")
            lines.append(f"      -> {error}")
    return "\n".join(lines)


@pytest.mark.smoke
def test_all_test_imports_resolve():
    """Verify every supy import used in tests actually exists."""
    test_root = Path(__file__).parent
    failures = []

    for test_file in sorted(test_root.rglob("*.py")):
        if test_file.name in _SKIP_FILES:
            continue
        if "__pycache__" in test_file.parts:
            continue
        if test_file.name == "test_api_surface.py":
            continue

        try:
            tree = ast.parse(
                test_file.read_text(encoding="utf-8", errors="replace"),
                filename=str(test_file),
            )
        except SyntaxError:
            continue

        for node in ast.walk(tree):
            if isinstance(node, ast.ImportFrom):
                if node.module and node.module.startswith("supy"):
                    _check_import_from(node, test_file, failures)
            elif isinstance(node, ast.Import):
                for alias in node.names:
                    if alias.name.startswith("supy"):
                        _check_import(alias.name, test_file, failures)

    if failures:
        report = _format_report(failures)
        pytest.fail(f"Broken supy imports in test suite:\n\n{report}")
