#!/usr/bin/env python3
"""CI test runner: pytest + bridge manifest check.

Replaces shell-level ``&&`` chaining in CIBW_TEST_COMMAND, which breaks
on Windows cmd.exe when pytest marker expressions contain double quotes
(e.g. ``-m "smoke or cfg"``).
"""

from __future__ import annotations

from collections.abc import Mapping
import os
from pathlib import Path
import subprocess
import sys

if __package__:
    from .ci_phase_metrics import stop_phase
else:
    from ci_phase_metrics import stop_phase


def metrics_paths(
    project_dir: Path,
    environment: Mapping[str, str],
) -> tuple[Path, Path]:
    """Resolve the pytest and phase files shared with the host runner."""
    metrics_dir = Path(
        environment.get("SUEWS_CI_METRICS_DIR", str(project_dir / "ci-metrics"))
    )
    name = environment.get("SUEWS_CI_METRICS_NAME", "physics")
    pytest_path = metrics_dir / f"{name}-pytest.json"
    phases_path = Path(
        environment.get("SUEWS_CI_PHASES", str(metrics_dir / f"{name}-phases.json"))
    )
    return pytest_path, phases_path


def main() -> int:
    if len(sys.argv) < 2:
        print("usage: run_ci_tests.py <project_dir> [pytest_args...]", file=sys.stderr)
        return 2

    project_dir = Path(sys.argv[1]).resolve()
    pytest_args = sys.argv[2:]
    metrics_path, phases_path = metrics_paths(project_dir, os.environ)
    metrics_path.parent.mkdir(parents=True, exist_ok=True)
    if phases_path.exists():
        stop_phase(phases_path, "install")
    pytest_environment = os.environ.copy()
    pytest_environment["PYTHONPATH"] = os.pathsep.join(
        part
        for part in (str(project_dir), pytest_environment.get("PYTHONPATH", ""))
        if part
    )
    pytest_environment["SUEWS_CI_METRICS"] = str(metrics_path)

    # Run pytest
    rc = subprocess.call(
        [
            sys.executable,
            "-m",
            "pytest",
            "-p",
            "scripts.suews.pytest_ci_metrics",
            str(project_dir / "test"),
            *pytest_args,
        ],
        env=pytest_environment,
    )
    if rc != 0:
        return rc

    # Run bridge manifest check
    rc = subprocess.call(
        [sys.executable, str(project_dir / "scripts/suews/check_bridge_manifest.py")],
    )
    return rc


if __name__ == "__main__":
    raise SystemExit(main())
