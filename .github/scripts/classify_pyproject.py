#!/usr/bin/env python3
"""Classify pyproject.toml changes for CI build detection.

Compares build-relevant, package-relevant, and test-relevant tables between
base and HEAD to decide whether a multiplatform build, a single-platform build,
a standard test-tier run, or no build is needed.

Environment variables
---------------------
BASE_SHA : str
    Git commit SHA of the PR base (or merge-group base).
GITHUB_OUTPUT : str
    Path to the file where GitHub Actions step outputs are written.

Outputs (written to GITHUB_OUTPUT)
-----------------------------------
build_relevant : "true" | "false"
    Whether [build-system], [tool.cibuildwheel], or [tool.meson-python] changed
    (multiplatform build).
package_relevant : "true" | "false"
    Whether [project] dependencies, requires-python, scripts, or
    optional-dependencies changed (single-platform build).
test_relevant : "true" | "false"
    Whether [tool.pytest.ini_options] changed (single-platform build and
    standard test tier).

Falls back conservatively (all true) when tomllib is unavailable or the
base or HEAD file cannot be read.
"""

from __future__ import annotations

from collections.abc import Mapping
import os
import subprocess
import sys
from typing import Any, NamedTuple

try:
    import tomllib
except ImportError:
    tomllib = None  # type: ignore[assignment]


class PyprojectClassification(NamedTuple):
    """Classification flags and human-readable reasons for a pyproject diff."""

    build_relevant: bool
    package_relevant: bool
    test_relevant: bool
    reasons: tuple[str, ...]


def classify_pyproject(
    base: Mapping[str, Any],
    head: Mapping[str, Any],
) -> PyprojectClassification:
    """Classify differences between parsed base and HEAD pyproject.toml dicts."""
    build_relevant = False
    package_relevant = False
    test_relevant = False
    reasons: list[str] = []

    # [build-system] -- affects wheel compilation toolchain
    if base.get("build-system") != head.get("build-system"):
        build_relevant = True
        reasons.append("[build-system] changed -> multiplatform build")

    base_tool = base.get("tool", {})
    head_tool = head.get("tool", {})
    if not isinstance(base_tool, Mapping):
        base_tool = {}
    if not isinstance(head_tool, Mapping):
        head_tool = {}

    # [tool.cibuildwheel] -- affects wheel build configuration
    if base_tool.get("cibuildwheel") != head_tool.get("cibuildwheel"):
        build_relevant = True
        reasons.append("[tool.cibuildwheel] changed -> multiplatform build")

    # [tool.meson-python] (including subtables such as [tool.meson-python.args])
    if base_tool.get("meson-python") != head_tool.get("meson-python"):
        build_relevant = True
        reasons.append("[tool.meson-python] changed -> multiplatform build")

    # [project] runtime metadata, entry points, and optional dependencies
    base_proj = base.get("project", {})
    head_proj = head.get("project", {})
    if not isinstance(base_proj, Mapping):
        base_proj = {}
    if not isinstance(head_proj, Mapping):
        head_proj = {}

    if base_proj.get("dependencies") != head_proj.get("dependencies"):
        package_relevant = True
        reasons.append("[project] dependencies changed -> single-platform build")
    if base_proj.get("requires-python") != head_proj.get("requires-python"):
        package_relevant = True
        reasons.append("[project] requires-python changed -> single-platform build")
    if base_proj.get("scripts") != head_proj.get("scripts"):
        package_relevant = True
        reasons.append("[project.scripts] changed -> single-platform build")
    if base_proj.get("optional-dependencies") != head_proj.get("optional-dependencies"):
        package_relevant = True
        reasons.append(
            "[project.optional-dependencies] changed -> single-platform build"
        )

    # [tool.pytest.ini_options] -- affects test collection, markers, and warnings
    base_pytest = base_tool.get("pytest", {})
    head_pytest = head_tool.get("pytest", {})
    if not isinstance(base_pytest, Mapping):
        base_pytest = {}
    if not isinstance(head_pytest, Mapping):
        head_pytest = {}
    if base_pytest.get("ini_options") != head_pytest.get("ini_options"):
        test_relevant = True
        reasons.append("[tool.pytest.ini_options] changed -> test-suite validation")

    return PyprojectClassification(
        build_relevant=build_relevant,
        package_relevant=package_relevant,
        test_relevant=test_relevant,
        reasons=tuple(reasons),
    )


def _write_outputs(build: bool, package: bool, test: bool) -> None:
    output = os.environ["GITHUB_OUTPUT"]
    with open(output, "a", encoding="utf-8") as f:
        f.write(f"build_relevant={'true' if build else 'false'}\n")
        f.write(f"package_relevant={'true' if package else 'false'}\n")
        f.write(f"test_relevant={'true' if test else 'false'}\n")


def main() -> None:
    if tomllib is None:
        print(
            f"::warning::tomllib unavailable (Python {sys.version}); "
            "needs >= 3.11. Treating as build-relevant."
        )
        _write_outputs(build=True, package=True, test=True)
        return

    base_sha = os.environ.get("BASE_SHA", "")
    if not base_sha:
        print("No base SHA, treating as build-relevant (conservative)")
        _write_outputs(build=True, package=True, test=True)
        return

    # Load base version of pyproject.toml
    try:
        base_text = subprocess.check_output(
            ["git", "show", f"{base_sha}:pyproject.toml"],
            text=True,
            stderr=subprocess.DEVNULL,
        )
        base = tomllib.loads(base_text)
    except Exception as e:
        print(f"Cannot read base pyproject.toml ({e}), treating as build-relevant")
        _write_outputs(build=True, package=True, test=True)
        return

    # Load HEAD version
    try:
        with open("pyproject.toml", "rb") as f:
            head = tomllib.load(f)
    except Exception as e:
        print(f"Cannot read HEAD pyproject.toml ({e}), treating as build-relevant")
        _write_outputs(build=True, package=True, test=True)
        return

    result = classify_pyproject(base, head)
    for reason in result.reasons:
        print(reason)

    if (
        not result.build_relevant
        and not result.package_relevant
        and not result.test_relevant
    ):
        print("pyproject.toml: non-build changes only (tooling/metadata)")

    _write_outputs(
        build=result.build_relevant,
        package=result.package_relevant,
        test=result.test_relevant,
    )


if __name__ == "__main__":
    main()
