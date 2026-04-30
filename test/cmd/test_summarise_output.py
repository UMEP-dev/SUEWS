"""Tests for ``suews summarise``."""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest
from click.testing import CliRunner

pytestmark = pytest.mark.api


def _build_run_dir(tmp_path: Path) -> Path:
    run_dir = tmp_path / "run01"
    run_dir.mkdir(parents=True)
    n = 240
    idx = pd.date_range("2024-06-01", periods=n, freq="h", name="datetime")
    rng = np.random.default_rng(seed=42)
    df = pd.DataFrame(
        {
            "QH": rng.normal(loc=70.0, scale=15.0, size=n),
            "QE": rng.normal(loc=40.0, scale=10.0, size=n),
            "QN": rng.normal(loc=180.0, scale=30.0, size=n),
        },
        index=idx,
    )
    df.to_csv(run_dir / "df_output.csv")
    return run_dir


def test_summarise_returns_non_nan_means(tmp_path: Path) -> None:
    """Default invocation summarises every numeric column and gets non-NaN means."""
    from supy.cmd.summarise_output import summarise_output_cmd

    run_dir = _build_run_dir(tmp_path)

    runner = CliRunner()
    result = runner.invoke(
        summarise_output_cmd, [str(run_dir), "--format", "json"]
    )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] in {"success", "warning"}

    data = envelope["data"]
    assert data["run_dir"] == str(run_dir)
    assert data["n_steps"] > 0
    assert data["variables"], "variables list must be non-empty"
    list_names = {var["name"] for var in data["variables"]}
    assert {"QH", "QE", "QN"} <= list_names
    for var in data["variables"]:
        assert var["mean"] is not None, "mean should be defined for synthetic data"
        assert not np.isnan(var["mean"])


def test_summarise_with_explicit_variables(tmp_path: Path) -> None:
    """Restricting --variables narrows the summary to those columns."""
    from supy.cmd.summarise_output import summarise_output_cmd

    run_dir = _build_run_dir(tmp_path)

    runner = CliRunner()
    result = runner.invoke(
        summarise_output_cmd,
        [str(run_dir), "--variables", "QH,QE", "--format", "json"],
    )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    list_names = {var["name"] for var in envelope["data"]["variables"]}
    assert list_names == {"QH", "QE"}


def test_summarise_text_format(tmp_path: Path) -> None:
    from supy.cmd.summarise_output import summarise_output_cmd

    run_dir = _build_run_dir(tmp_path)

    runner = CliRunner()
    result = runner.invoke(summarise_output_cmd, [str(run_dir)])
    assert result.exit_code == 0, result.output
    assert "Variable summary" in result.output
