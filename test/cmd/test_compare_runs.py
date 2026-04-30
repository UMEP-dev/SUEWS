"""Tests for ``suews compare``."""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest
from click.testing import CliRunner

pytestmark = pytest.mark.api


def _build_run_dir(tmp_path: Path, name: str, seed: int = 1) -> Path:
    """Create a minimal run dir with a parquet output."""
    run_dir = tmp_path / name
    run_dir.mkdir(parents=True)
    rng = np.random.default_rng(seed)
    n = 144  # 12 hours at 5-min steps
    idx = pd.date_range("2024-06-01", periods=n, freq="5min", name="datetime")
    df = pd.DataFrame(
        {
            "QH": rng.normal(loc=80.0, scale=10.0, size=n),
            "QE": rng.normal(loc=40.0, scale=5.0, size=n),
            "QN": rng.normal(loc=200.0, scale=20.0, size=n),
        },
        index=idx,
    )
    df.to_parquet(run_dir / "df_output.parquet")
    return run_dir


def test_compare_run_against_itself_zero_rmse(tmp_path: Path) -> None:
    """Comparing a run against itself: rmse=0, bias=0, r=1."""
    from supy.cmd.compare_runs import compare_runs_cmd

    run_dir = _build_run_dir(tmp_path, "run01")

    runner = CliRunner()
    result = runner.invoke(
        compare_runs_cmd,
        [str(run_dir), str(run_dir), "--format", "json"],
    )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] in {"success", "warning"}

    per_var = envelope["data"]["per_variable"]
    for var in ("QH", "QE", "QN"):
        assert var in per_var
        assert per_var[var]["rmse"] == pytest.approx(0.0, abs=1e-9)
        assert per_var[var]["bias"] == pytest.approx(0.0, abs=1e-9)
        # r is undefined when both inputs are identical and trivially
        # constant; for non-constant identical series it is 1.
        assert per_var[var]["r"] == pytest.approx(1.0, abs=1e-9)


def test_compare_two_distinct_runs_has_overlap(tmp_path: Path) -> None:
    """Two independent runs sharing a time index must report joint overlap."""
    from supy.cmd.compare_runs import compare_runs_cmd

    run_a = _build_run_dir(tmp_path, "runA", seed=1)
    run_b = _build_run_dir(tmp_path, "runB", seed=2)

    runner = CliRunner()
    result = runner.invoke(
        compare_runs_cmd,
        [str(run_a), str(run_b), "--format", "json"],
    )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    overlap = envelope["data"]["time_axis_overlap"]
    assert overlap["n"] > 0
    assert overlap["start"] is not None
    assert overlap["end"] is not None


def test_compare_unknown_metric_rejected(tmp_path: Path) -> None:
    """Passing an unknown metric must yield a structured error."""
    from supy.cmd.compare_runs import compare_runs_cmd

    run_dir = _build_run_dir(tmp_path, "runX")

    runner = CliRunner()
    result = runner.invoke(
        compare_runs_cmd,
        [
            str(run_dir),
            str(run_dir),
            "--metrics",
            "rmse,bogus",
            "--format",
            "json",
        ],
    )
    assert result.exit_code != 0
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert "bogus" in envelope["errors"][0]["message"].lower()
