"""Tests for ``suews summarise``."""

from __future__ import annotations

import json
from pathlib import Path

from click.testing import CliRunner
import numpy as np
import pandas as pd
import pytest

from supy.cmd.summarise_output import summarise_output_cmd

pytestmark = [pytest.mark.api, pytest.mark.contract("C-API")]


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


def _build_multigroup_parquet_run_dir(tmp_path: Path) -> Path:
    run_dir = tmp_path / "multigroup-run"
    run_dir.mkdir(parents=True)
    idx = pd.MultiIndex.from_product(
        [[1], pd.date_range("2024-06-01", periods=4, freq="h")],
        names=["grid", "datetime"],
    )
    columns = pd.MultiIndex.from_tuples(
        [
            ("SUEWS", "Kdown"),
            ("SUEWS", "QS"),
            ("ESTM", "QS"),
            ("SUEWS", "AlbSnow"),
            ("DailyState", "AlbSnow"),
            ("SUEWS", "Ts_Paved"),
            ("EHC", "Ts_Paved"),
            ("debug", "Ts_Paved"),
        ],
        names=["group", "var"],
    )
    data = np.array(
        [
            [100.0, 20.0, np.nan, 0.2, np.nan, 25.0, np.nan, 25.0],
            [200.0, 30.0, np.nan, 0.2, np.nan, 26.0, np.nan, 26.0],
            [300.0, 40.0, np.nan, 0.2, np.nan, 27.0, np.nan, 27.0],
            [400.0, 30.0, np.nan, 0.2, 0.5, 28.0, np.nan, 28.0],
        ],
        dtype=float,
    )
    df = pd.DataFrame(data, columns=columns, index=idx)
    df.to_parquet(run_dir / "selected grid_SUEWS_output.parquet")
    return run_dir


def test_summarise_multigroup_default_preserves_groups_and_nan_fractions(
    tmp_path: Path,
) -> None:
    """Default summary keeps every (group, var) series and its own NaN fraction."""
    run_dir = _build_multigroup_parquet_run_dir(tmp_path)
    runner = CliRunner()

    res_json = runner.invoke(summarise_output_cmd, [str(run_dir), "--format", "json"])
    assert res_json.exit_code == 0, res_json.output
    envelope = json.loads(res_json.stdout)
    assert envelope["status"] == "success"
    assert envelope["data"]["notes"] == []

    list_vars = envelope["data"]["variables"]
    assert [(v["group"], v["name"]) for v in list_vars] == [
        ("SUEWS", "Kdown"),
        ("SUEWS", "QS"),
        ("ESTM", "QS"),
        ("SUEWS", "AlbSnow"),
        ("DailyState", "AlbSnow"),
        ("SUEWS", "Ts_Paved"),
        ("EHC", "Ts_Paved"),
        ("debug", "Ts_Paved"),
    ]
    by_key = {(v["group"], v["name"]): v for v in list_vars}
    assert by_key["SUEWS", "QS"]["mean"] == pytest.approx(30.0)
    assert by_key["SUEWS", "QS"]["nan_pct"] == pytest.approx(0.0)
    assert by_key["ESTM", "QS"]["mean"] is None
    assert by_key["ESTM", "QS"]["nan_pct"] == pytest.approx(100.0)
    assert by_key["DailyState", "AlbSnow"]["mean"] == pytest.approx(0.5)
    assert by_key["DailyState", "AlbSnow"]["nan_pct"] == pytest.approx(75.0)
    assert by_key["EHC", "Ts_Paved"]["mean"] is None
    assert by_key["EHC", "Ts_Paved"]["nan_pct"] == pytest.approx(100.0)
    assert by_key["debug", "Ts_Paved"]["mean"] == pytest.approx(26.5)

    res_text = runner.invoke(summarise_output_cmd, [str(run_dir)])
    assert res_text.exit_code == 0, res_text.output
    assert "group" in res_text.output
    for grp in ("SUEWS", "ESTM", "DailyState", "EHC", "debug"):
        assert grp in res_text.output


def test_summarise_multigroup_qualified_selection(tmp_path: Path) -> None:
    """GROUP.VAR selects the exact colliding series without warnings."""
    run_dir = _build_multigroup_parquet_run_dir(tmp_path)
    runner = CliRunner()

    res_suews = runner.invoke(
        summarise_output_cmd,
        [str(run_dir), "--variables", "SUEWS.QS", "--format", "json"],
    )
    assert res_suews.exit_code == 0, res_suews.output
    env_suews = json.loads(res_suews.stdout)
    assert env_suews["status"] == "success"
    assert env_suews["data"]["notes"] == []
    assert env_suews["data"]["variables"] == [
        {
            "group": "SUEWS",
            "name": "QS",
            "mean": pytest.approx(30.0),
            "min": pytest.approx(20.0),
            "max": pytest.approx(40.0),
            "nan_pct": pytest.approx(0.0),
        }
    ]

    res_both = runner.invoke(
        summarise_output_cmd,
        [str(run_dir), "--variables", "ESTM.QS,DailyState.AlbSnow", "--format", "json"],
    )
    assert res_both.exit_code == 0, res_both.output
    env_both = json.loads(res_both.stdout)
    assert env_both["status"] == "success"
    assert [(v["group"], v["name"]) for v in env_both["data"]["variables"]] == [
        ("ESTM", "QS"),
        ("DailyState", "AlbSnow"),
    ]
    assert env_both["data"]["variables"][0]["mean"] is None
    assert env_both["data"]["variables"][0]["nan_pct"] == pytest.approx(100.0)
    assert env_both["data"]["variables"][1]["mean"] == pytest.approx(0.5)
    assert env_both["data"]["variables"][1]["nan_pct"] == pytest.approx(75.0)


def test_summarise_multigroup_bare_unambiguous_and_colliding_selections(
    tmp_path: Path,
) -> None:
    """Bare unambiguous names succeed cleanly; bare collisions expand with a note."""
    run_dir = _build_multigroup_parquet_run_dir(tmp_path)
    runner = CliRunner()

    res_unique = runner.invoke(
        summarise_output_cmd,
        [str(run_dir), "--variables", "Kdown", "--format", "json"],
    )
    assert res_unique.exit_code == 0, res_unique.output
    env_unique = json.loads(res_unique.stdout)
    assert env_unique["status"] == "success"
    assert env_unique["data"]["notes"] == []
    assert [(v["group"], v["name"]) for v in env_unique["data"]["variables"]] == [
        ("SUEWS", "Kdown")
    ]

    res_qs_json = runner.invoke(
        summarise_output_cmd,
        [str(run_dir), "--variables", "QS", "--format", "json"],
    )
    assert res_qs_json.exit_code == 0, res_qs_json.output
    env_qs = json.loads(res_qs_json.stdout)
    assert env_qs["status"] == "warning"
    assert [(v["group"], v["name"]) for v in env_qs["data"]["variables"]] == [
        ("SUEWS", "QS"),
        ("ESTM", "QS"),
    ]
    assert any(
        "SUEWS.QS" in note and "ESTM.QS" in note for note in env_qs["data"]["notes"]
    )

    res_qs_text = runner.invoke(
        summarise_output_cmd,
        [str(run_dir), "--variables", "QS"],
    )
    assert res_qs_text.exit_code == 0, res_qs_text.output
    assert "SUEWS" in res_qs_text.output
    assert "ESTM" in res_qs_text.output
    assert "SUEWS.QS" in res_qs_text.output
    assert "ESTM.QS" in res_qs_text.output

    res_ts = runner.invoke(
        summarise_output_cmd,
        [str(run_dir), "--variables", "Ts_Paved", "--format", "json"],
    )
    assert res_ts.exit_code == 0, res_ts.output
    env_ts = json.loads(res_ts.stdout)
    assert env_ts["status"] == "warning"
    assert [(v["group"], v["name"]) for v in env_ts["data"]["variables"]] == [
        ("SUEWS", "Ts_Paved"),
        ("EHC", "Ts_Paved"),
        ("debug", "Ts_Paved"),
    ]


def test_summarise_partially_absent_and_missing_variables(tmp_path: Path) -> None:
    """Missing bare or qualified names emit warnings while keeping present hits."""
    run_dir = _build_multigroup_parquet_run_dir(tmp_path)
    runner = CliRunner()

    result = runner.invoke(
        summarise_output_cmd,
        [
            str(run_dir),
            "--variables",
            "Kdown,NO_SUCH_VAR,ESTM.MISSING,UNKNOWN.QS",
            "--format",
            "json",
        ],
    )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "warning"
    assert [(v["group"], v["name"]) for v in envelope["data"]["variables"]] == [
        ("SUEWS", "Kdown")
    ]
    notes = envelope["data"]["notes"]
    assert "variable 'NO_SUCH_VAR' not found in output" in notes
    assert "variable 'ESTM.MISSING' not found in output" in notes
    assert "variable 'UNKNOWN.QS' not found in output" in notes


def test_summarise_deduplicates_repeated_selectors(tmp_path: Path) -> None:
    """Overlapping or repeated selectors emit each column once without crashing."""
    run_dir = _build_multigroup_parquet_run_dir(tmp_path)
    runner = CliRunner()

    result = runner.invoke(
        summarise_output_cmd,
        [str(run_dir), "--variables", "SUEWS.QS,QS", "--format", "json"],
    )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert [(v["group"], v["name"]) for v in envelope["data"]["variables"]] == [
        ("SUEWS", "QS"),
        ("ESTM", "QS"),
    ]


def test_summarise_legacy_text_and_flat_csv_keep_ungrouped_contract(
    tmp_path: Path,
) -> None:
    """Flat CSV and legacy space-delimited text keep the ungrouped output shape."""
    run_dir = tmp_path / "legacy-txt"
    run_dir.mkdir(parents=True)
    header = "Year DOY Hour Min Dectime QH QE QN"
    rows = "\n".join(
        f"2024 153 {hour} 0 {152 + hour / 24:.4f} {50.0 + hour} 25.0 120.0"
        for hour in range(4)
    )
    (run_dir / "Kc1_2024_SUEWS_60.txt").write_text(
        f"{header}\n{rows}\n", encoding="utf-8"
    )

    runner = CliRunner()
    res_json = runner.invoke(
        summarise_output_cmd,
        [str(run_dir), "--variables", "QH,MISSING", "--format", "json"],
    )
    assert res_json.exit_code == 0, res_json.output
    envelope = json.loads(res_json.stdout)
    assert envelope["status"] == "warning"
    assert envelope["data"]["notes"] == ["variable 'MISSING' not found in output"]
    assert envelope["data"]["variables"] == [
        {
            "name": "QH",
            "mean": pytest.approx(51.5),
            "min": pytest.approx(50.0),
            "max": pytest.approx(53.0),
            "nan_pct": pytest.approx(0.0),
        }
    ]
    assert "group" not in envelope["data"]["variables"][0]

    res_text = runner.invoke(summarise_output_cmd, [str(run_dir), "--variables", "QH"])
    assert res_text.exit_code == 0, res_text.output
    header_line = next(
        line for line in res_text.output.splitlines() if "nan_pct" in line
    )
    assert "group" not in header_line


def test_summarise_single_group_parquet_keeps_bare_name_unambiguous(
    tmp_path: Path,
) -> None:
    """Single-group MultiIndex Parquet resolves bare names cleanly with group metadata."""
    run_dir = tmp_path / "single-group"
    run_dir.mkdir(parents=True)
    idx = pd.MultiIndex.from_product(
        [[1], pd.date_range("2024-06-01", periods=3, freq="h")],
        names=["grid", "datetime"],
    )
    df = pd.DataFrame(
        np.array([[100.0, 20.0], [200.0, 30.0], [300.0, 40.0]]),
        columns=_suews_columns(["Kdown", "QS"]),
        index=idx,
    )
    df.to_parquet(run_dir / "SUEWS_output.parquet")

    runner = CliRunner()
    result = runner.invoke(
        summarise_output_cmd,
        [str(run_dir), "--variables", "QS", "--format", "json"],
    )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "success"
    assert envelope["data"]["notes"] == []
    assert envelope["data"]["variables"] == [
        {
            "group": "SUEWS",
            "name": "QS",
            "mean": pytest.approx(30.0),
            "min": pytest.approx(20.0),
            "max": pytest.approx(40.0),
            "nan_pct": pytest.approx(0.0),
        }
    ]


def test_summarise_help_documents_group_qualified_syntax() -> None:
    """CLI help documents both bare and group-qualified variable selection."""
    runner = CliRunner()
    result = runner.invoke(summarise_output_cmd, ["--help"])
    assert result.exit_code == 0, result.output
    assert "--variables" in result.output
    assert "suews summarise path/to/run_dir --variables SUEWS.QS,ESTM.QS" in result.output
