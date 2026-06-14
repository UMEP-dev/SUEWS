"""Tests for ``suews compare``."""

from __future__ import annotations

import json
from pathlib import Path

from click.testing import CliRunner
import numpy as np
import pandas as pd
import pytest

from supy.cmd.compare_runs import compare_runs_cmd

pytestmark = pytest.mark.api


def _suews_columns(names: list[str]) -> pd.MultiIndex:
    return pd.MultiIndex.from_product([["SUEWS"], names], names=["group", "var"])


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


def test_compare_text_metric_subset_does_not_require_all_metrics(
    tmp_path: Path,
) -> None:
    run_a = _build_run_dir(tmp_path, "subsetA", seed=1)
    run_b = _build_run_dir(tmp_path, "subsetB", seed=1)

    runner = CliRunner()
    result = runner.invoke(
        compare_runs_cmd,
        [
            str(run_a),
            str(run_b),
            "--variables",
            "QH",
            "--metrics",
            "rmse",
        ],
    )

    assert result.exit_code == 0, result.output
    assert "rmse=" in result.output
    assert "bias=" not in result.output
    assert "r=" not in result.output


def test_compare_reads_real_parquet_output_name_and_columns(tmp_path: Path) -> None:
    idx = pd.MultiIndex.from_product(
        [[1], pd.date_range("2024-06-01", periods=4, freq="h")],
        names=["grid", "datetime"],
    )

    for name, offset in (("realA", 0.0), ("realB", 1.0)):
        run_dir = tmp_path / name
        run_dir.mkdir(parents=True)
        df = pd.DataFrame(
            np.array([
                [10.0 + offset, 20.0, 30.0],
                [11.0 + offset, 21.0, 31.0],
                [12.0 + offset, 22.0, 32.0],
                [13.0 + offset, 23.0, 33.0],
            ]),
            columns=_suews_columns(["QH", "QE", "QN"]),
            index=idx,
        )
        df.to_parquet(run_dir / "SUEWS_output.parquet")

    runner = CliRunner()
    result = runner.invoke(
        compare_runs_cmd,
        [
            str(tmp_path / "realA"),
            str(tmp_path / "realB"),
            "--variables",
            "QH",
            "--format",
            "json",
        ],
    )

    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert "QH" in envelope["data"]["per_variable"]
    assert envelope["data"]["per_variable"]["QH"]["bias"] == pytest.approx(1.0)


def test_compare_reads_legacy_text_run_outputs(tmp_path: Path) -> None:
    for name, offset in (("txtA", 0.0), ("txtB", 2.0)):
        run_dir = tmp_path / name
        run_dir.mkdir(parents=True)
        df = pd.DataFrame({
            "datetime": pd.date_range("2024-06-01", periods=4, freq="h"),
            "QH": [10.0 + offset, 11.0 + offset, 12.0 + offset, 13.0 + offset],
            "QE": [20.0, 21.0, 22.0, 23.0],
            "QN": [30.0, 31.0, 32.0, 33.0],
        })
        df.to_csv(run_dir / "Kc1_2024_SUEWS_60.txt", sep="\t", index=False)

    runner = CliRunner()
    result = runner.invoke(
        compare_runs_cmd,
        [
            str(tmp_path / "txtA"),
            str(tmp_path / "txtB"),
            "--variables",
            "QH",
            "--format",
            "json",
        ],
    )

    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["data"]["per_variable"]["QH"]["bias"] == pytest.approx(2.0)
