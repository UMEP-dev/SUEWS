"""Tests for ``suews summarise``."""

from __future__ import annotations

import json
from pathlib import Path

from click.testing import CliRunner
import numpy as np
import pandas as pd
import pytest

from supy.cmd.summarise_output import summarise_output_cmd

pytestmark = pytest.mark.api


def _suews_columns(names: list[str]) -> pd.MultiIndex:
    return pd.MultiIndex.from_product([["SUEWS"], names], names=["group", "var"])


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
    run_dir = _build_run_dir(tmp_path)

    runner = CliRunner()
    result = runner.invoke(summarise_output_cmd, [str(run_dir), "--format", "json"])
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
    run_dir = _build_run_dir(tmp_path)

    runner = CliRunner()
    result = runner.invoke(summarise_output_cmd, [str(run_dir)])
    assert result.exit_code == 0, result.output
    assert "Variable summary" in result.output


def test_summarise_reads_real_parquet_output_name_and_columns(tmp_path: Path) -> None:
    run_dir = tmp_path / "parquet-run"
    run_dir.mkdir(parents=True)
    idx = pd.MultiIndex.from_product(
        [[1], pd.date_range("2024-06-01", periods=4, freq="h")],
        names=["grid", "datetime"],
    )
    df = pd.DataFrame(
        np.array([
            [10.0, 20.0, 30.0],
            [11.0, 21.0, 31.0],
            [12.0, 22.0, 32.0],
            [13.0, 23.0, 33.0],
        ]),
        columns=_suews_columns(["QH", "QE", "QN"]),
        index=idx,
    )
    df.to_parquet(run_dir / "SUEWS_output.parquet")

    runner = CliRunner()
    result = runner.invoke(
        summarise_output_cmd,
        [str(run_dir), "--variables", "QH", "--format", "json"],
    )

    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    variables = envelope["data"]["variables"]
    assert [var["name"] for var in variables] == ["QH"]
    assert variables[0]["mean"] == pytest.approx(11.5)
