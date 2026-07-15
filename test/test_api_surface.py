"""Meta test: verify every supy import used in the test suite actually exists.

This catches interface breakages (renamed/removed modules, functions, classes)
before any slow simulation tests run. It is self-maintaining: when new tests
add new imports, this test automatically validates them.
"""

import ast
import json
from pathlib import Path
import subprocess
import sys

import conftest as suite_conftest
import pytest

pytestmark = pytest.mark.api


class _CollectedItem:
    """Minimal pytest item surface used to exercise collection ordering."""

    def __init__(self, path, nodeid):
        self.fspath = Path(path)
        self.nodeid = nodeid


@pytest.mark.core
def test_collection_preserves_native_test_relative_order():
    """Only the import-surface probe may move ahead of declared test order."""
    items = [
        _CollectedItem("test/physics/test_other.py", "other"),
        _CollectedItem("test/core/test_sample_output.py", "sample"),
        _CollectedItem(
            "test/core/test_public_api_wrappers.py",
            "TestPublicAPIEquivalence::test_functional_matches_oop",
        ),
        _CollectedItem("test/test_api_surface.py", "api_surface"),
    ]

    suite_conftest.pytest_collection_modifyitems(items)

    assert [item.nodeid for item in items] == [
        "api_surface",
        "other",
        "sample",
        "TestPublicAPIEquivalence::test_functional_matches_oop",
    ]


# Files to skip when scanning for imports
_SKIP_FILES = {"conftest.py", "debug_utils.py", "__init__.py"}

_IMPORT_CHECKER = r"""
import importlib
import json
import sys

checks = json.loads(sys.stdin.read())
failures = []

for check in checks:
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
    # Check the top-level public surface before importing submodules, which can
    # add their names to the parent package. The subprocess itself starts with
    # a clean module cache; repeated purges between checks only re-imported the
    # same package hundreds of times without improving isolation.
    checks.sort(
        key=lambda check: (
            check["module"] != "supy",
            "suews_sim" not in check.get("names", ()),
        )
    )
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
    from supy import suews_sim  # noqa: F401, PLC0415 - Deliberately pollute cache.

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
                    checks.append({
                        "kind": "from",
                        "module": node.module,
                        "names": [alias.name for alias in node.names],
                        "file": test_file.name,
                    })
            elif isinstance(node, ast.Import):
                for alias in node.names:
                    if alias.name.startswith("supy"):
                        checks.append({
                            "kind": "import",
                            "module": alias.name,
                            "file": test_file.name,
                        })

    # The synthetic top-level import must fail in the clean subprocess even
    # though this parent process imported the submodule above. This folds the
    # former second subprocess test into the main surface probe.
    synthetic_file = "synthetic_parent_cache_test.py"
    synthetic_failure = [
        synthetic_file,
        "from supy import suews_sim",
        "supy has no attribute 'suews_sim'",
    ]
    checks.append({
        "kind": "from",
        "module": "supy",
        "names": ["suews_sim"],
        "file": synthetic_file,
    })

    failures = _run_import_checks(checks)
    assert synthetic_failure in failures

    real_failures = [failure for failure in failures if failure[0] != synthetic_file]
    if real_failures:
        report = _format_report(real_failures)
        pytest.fail(f"Broken supy imports in test suite:\n\n{report}")
