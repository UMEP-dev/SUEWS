#!/usr/bin/env python3
"""Classify pyproject.toml changes for CI build detection.

Compares [build-system] and [tool.cibuildwheel] between base and HEAD to
decide whether a multiplatform build is needed, or whether only package
metadata (dependencies, requires-python) changed.

Environment variables
---------------------
BASE_SHA : str
    Git commit SHA of the PR base (or merge-group base).
GITHUB_OUTPUT : str
    Path to the file where GitHub Actions step outputs are written.

Outputs (written to GITHUB_OUTPUT)
-----------------------------------
build_relevant : "true" | "false"
    Whether build-system or cibuildwheel sections changed (multiplatform).
package_relevant : "true" | "false"
    Whether project dependencies or requires-python changed (single-platform).

Falls back conservatively (both true) when tomllib is unavailable or the
base commit cannot be read.
"""

from __future__ import annotations

import os
import subprocess
import sys


def _write_outputs(build: bool, package: bool) -> None:
    output = os.environ["GITHUB_OUTPUT"]
    with open(output, "a") as f:
        f.write(f"build_relevant={'true' if build else 'false'}\n")
        f.write(f"package_relevant={'true' if package else 'false'}\n")


def main() -> None:
    try:
        import tomllib
    except ImportError:
        print(
            f"::warning::tomllib unavailable (Python {sys.version}); "
            "needs >= 3.11. Treating as build-relevant."
        )
        _write_outputs(build=True, package=True)
        return

    base_sha = os.environ.get("BASE_SHA", "")
    if not base_sha:
        print("No base SHA, treating as build-relevant (conservative)")
        _write_outputs(build=True, package=True)
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
        _write_outputs(build=True, package=True)
        return

    # Load HEAD version
    try:
        with open("pyproject.toml", "rb") as f:
            head = tomllib.load(f)
    except Exception as e:
        print(f"Cannot read HEAD pyproject.toml ({e}), treating as build-relevant")
        _write_outputs(build=True, package=True)
        return

    build_relevant = False
    package_relevant = False

    # [build-system] -- affects compilation
    if base.get("build-system") != head.get("build-system"):
        build_relevant = True
        print("[build-system] changed -> multiplatform build")

    # [tool.cibuildwheel] -- affects wheel build config
    if base.get("tool", {}).get("cibuildwheel") != head.get("tool", {}).get(
        "cibuildwheel"
    ):
        build_relevant = True
        print("[tool.cibuildwheel] changed -> multiplatform build")

    # [project] dependencies / requires-python -- wheel metadata only
    base_proj = base.get("project", {})
    head_proj = head.get("project", {})
    if base_proj.get("dependencies") != head_proj.get("dependencies"):
        package_relevant = True
        print("[project] dependencies changed -> single-platform build")
    if base_proj.get("requires-python") != head_proj.get("requires-python"):
        package_relevant = True
        print("[project] requires-python changed -> single-platform build")

    if not build_relevant and not package_relevant:
        print("pyproject.toml: non-build changes only (tooling, metadata, optional deps)")

    _write_outputs(build=build_relevant, package=package_relevant)


if __name__ == "__main__":
    main()
