#!/usr/bin/env python3
"""CI test runner: pytest + bridge manifest check.

Replaces shell-level ``&&`` chaining in CIBW_TEST_COMMAND, which breaks
on Windows cmd.exe when pytest marker expressions contain double quotes
(e.g. ``-m "smoke or cfg"``).
"""

from __future__ import annotations

import subprocess
import sys


def main() -> int:
    if len(sys.argv) < 2:
        print("usage: run_ci_tests.py <project_dir> [pytest_args...]", file=sys.stderr)
        return 2

    project_dir = sys.argv[1]
    pytest_args = sys.argv[2:]

    # Run pytest
    rc = subprocess.call(
        [sys.executable, "-m", "pytest", f"{project_dir}/test"] + pytest_args,
    )
    if rc != 0:
        return rc

    # Run bridge manifest check
    rc = subprocess.call(
        [sys.executable, f"{project_dir}/scripts/check_bridge_manifest.py"],
    )
    return rc


if __name__ == "__main__":
    raise SystemExit(main())
