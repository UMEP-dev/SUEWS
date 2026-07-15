"""Contract tests for the standalone pytest CI metrics plugin."""

from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest

pytestmark = pytest.mark.api

PROJECT_ROOT = Path(__file__).resolve().parents[2]


@pytest.mark.smoke
def test_plugin_writes_parseable_metrics_and_step_summary(tmp_path: Path) -> None:
    """A pytest run records its stable inventory, timings, workers and warnings."""
    test_file = tmp_path / "test_sample.py"
    test_file.write_text(
        """\
import warnings


def test_pass():
    assert True


def test_warn():
    warnings.warn("group me", UserWarning)
""",
        encoding="utf-8",
    )
    metrics_path = tmp_path / "ci-metrics.json"
    summary_path = tmp_path / "step-summary.md"
    env = os.environ.copy()
    env.update({
        "GITHUB_STEP_SUMMARY": str(summary_path),
        "PYTHONPATH": os.pathsep.join(
            part for part in (str(PROJECT_ROOT), env.get("PYTHONPATH", "")) if part
        ),
        "SUEWS_CI_METRICS": str(metrics_path),
    })

    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--confcutdir",
            str(tmp_path),
            "-p",
            "scripts.suews.pytest_ci_metrics",
            str(test_file),
            "-q",
        ],
        cwd=tmp_path,
        env=env,
        check=False,
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, result.stdout + result.stderr
    metrics = json.loads(metrics_path.read_text(encoding="utf-8"))
    node_ids = ["test_sample.py::test_pass", "test_sample.py::test_warn"]
    expected_hash = hashlib.sha256("\n".join(node_ids).encode()).hexdigest()

    assert metrics["schema_version"] == 1
    assert metrics["result"]["exit_code"] == 0
    assert metrics["inventory"] == {
        "node_count": 2,
        "node_id_sha256": expected_hash,
    }
    assert metrics["execution"] == {
        "effective_worker_count": 1,
        "xdist": False,
    }
    assert set(metrics["phases"]) == {"collection", "session", "tests"}
    assert all(phase["duration_seconds"] >= 0 for phase in metrics["phases"].values())
    assert metrics["warnings"] == [
        {
            "category": "UserWarning",
            "count": 1,
            "fingerprint": hashlib.sha256(b"UserWarning\ngroup me").hexdigest(),
            "message": "group me",
        }
    ]

    summary = summary_path.read_text(encoding="utf-8")
    assert "Pytest CI metrics" in summary
    assert expected_hash in summary
    assert "UserWarning: group me" in summary
