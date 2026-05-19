"""Meta test: verify every supy import used in the test suite actually exists.

This catches interface breakages (renamed/removed modules, functions, classes)
before any slow simulation tests run. It is self-maintaining: when new tests
add new imports, this test automatically validates them.
"""

import ast
import json
import subprocess
import sys
from pathlib import Path

import pytest

pytestmark = pytest.mark.api

# Files to skip when scanning for imports
_SKIP_FILES = {"conftest.py", "debug_utils.py", "__init__.py"}

_IMPORT_CHECKER = r"""
import importlib
import json
import sys


def purge_supy_modules():
    for module_name in list(sys.modules):
        if module_name == "supy" or module_name.startswith("supy."):
            del sys.modules[module_name]
    importlib.invalidate_caches()


checks = json.loads(sys.stdin.read())
failures = []

for check in checks:
    purge_supy_modules()
    files = check.get("files") or [check["file"]]
    if check["kind"] == "from":
        module_name = check["module"]
        try:
            module = importlib.import_module(module_name)
        except Exception as exc:
            for file_name in files:
                for imported_name in check["names"]:
                    failures.append([
                        file_name,
                        f"from {module_name} import {imported_name}",
                        str(exc),
                    ])
            continue

        for imported_name in check["names"]:
            if imported_name == "*":
                continue
            if not hasattr(module, imported_name):
                for file_name in files:
                    failures.append([
                        file_name,
                        f"from {module_name} import {imported_name}",
                        f"{module_name} has no attribute {imported_name!r}",
                    ])
    else:
        module_name = check["module"]
        try:
            importlib.import_module(module_name)
        except Exception as exc:
            for file_name in files:
                failures.append([file_name, f"import {module_name}", str(exc)])

print(json.dumps(failures))
"""


def _run_import_checks(checks):
    """Check imports in a clean subprocess so pytest collection cannot mask bugs."""
    checks = _dedupe_checks(checks)
    result = subprocess.run(
        [sys.executable, "-c", _IMPORT_CHECKER],
        input=json.dumps(checks),
        text=True,
        capture_output=True,
        check=False,
    )
    if result.returncode != 0:
        pytest.fail(
            "supy import surface checker subprocess failed:\n"
            f"stdout:\n{result.stdout}\n\nstderr:\n{result.stderr}"
        )
    try:
        return json.loads(result.stdout)
    except json.JSONDecodeError as exc:
        pytest.fail(
            "supy import surface checker produced unparsable stdout:\n"
            f"error: {exc}\n\nstdout:\n{result.stdout}\n\nstderr:\n{result.stderr}"
        )


def _dedupe_checks(checks):
    """Collapse repeated imports while preserving per-file failure reports."""
    deduped = {}
    for check in checks:
        key = (check["kind"], check["module"], tuple(check.get("names", ())))
        if key not in deduped:
            deduped[key] = {k: v for k, v in check.items() if k != "file"}
            deduped[key]["files"] = []
        deduped[key]["files"].append(check["file"])
    return list(deduped.values())


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
@pytest.mark.smoke_bridge
def test_all_test_imports_resolve():
    """Verify every supy import used in tests actually exists."""
    test_root = Path(__file__).parent
    checks = []

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
                    checks.append(
                        {
                            "kind": "from",
                            "module": node.module,
                            "names": [alias.name for alias in node.names],
                            "file": test_file.name,
                        }
                    )
            elif isinstance(node, ast.Import):
                for alias in node.names:
                    if alias.name.startswith("supy"):
                        checks.append(
                            {
                                "kind": "import",
                                "module": alias.name,
                                "file": test_file.name,
                            }
                        )

    failures = _run_import_checks(checks)
    if failures:
        report = _format_report(failures)
        pytest.fail(f"Broken supy imports in test suite:\n\n{report}")


@pytest.mark.smoke
def test_import_checks_ignore_parent_process_import_cache():
    """A parent-process submodule import must not mask a missing top-level API."""
    from supy import suews_sim  # noqa: F401, PLC0415 - Deliberately pollute cache.

    failures = _run_import_checks(
        [
            {
                "kind": "from",
                "module": "supy",
                "names": ["suews_sim"],
                "file": "synthetic_test.py",
            }
        ]
    )

    assert failures == [
        [
            "synthetic_test.py",
            "from supy import suews_sim",
            "supy has no attribute 'suews_sim'",
        ]
    ]
