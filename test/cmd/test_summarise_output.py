"""Tests for ``suews summarise``."""

from __future__ import annotations

import json
from pathlib import Path

from click.testing import CliRunner
import numpy as np
import pandas as pd
import pytest

from supy._save import _write_df_txt  # ruff: ignore[import-private-name]
from supy.cmd.compare_runs import compare_runs_cmd
from supy.cmd.summarise_output import summarise_output_cmd
from supy.diagnostics import check_energy_balance_closure, check_nan_proportion

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
    assert (
        "suews summarise path/to/run_dir --variables SUEWS.QS,ESTM.QS" in result.output
    )


def _build_mixed_sentinel_outputs(tmp_path: Path) -> tuple[Path, Path, Path]:
    """Write identical 36-step SUEWS data via ``_write_df_txt`` and Parquet."""
    n_steps = 36
    idx = pd.date_range("2012-01-01 00:05", periods=n_steps, freq="5min")
    # 11 NaN + 7 zeros + 12*0.3 + 6*1.0 -> 25 finite values summing to 9.6 (mean 0.384).
    df_year = pd.DataFrame(
        {
            "Fcld": [np.nan] * 11 + [0.0] * 7 + [0.3] * 12 + [1.0] * 6,
            "Kdown": np.zeros(n_steps),
            "QN": np.linspace(-39.06, -37.99, n_steps),
            "T2": np.linspace(-12.5, 8.5, n_steps),
            "Lob": [-999.0001, -998.9999, -1000.0, -998.0, 999.0, 0.0] * 6,
            "Rain": [np.nan] * 11 + [0.0] * 15 + [1.25e-5] * 10,
        },
        index=idx,
    )

    txt_dir = tmp_path / "txt-run"
    txt_dir.mkdir()
    path_txt = txt_dir / "KCL1_2012_SUEWS_5.txt"
    _write_df_txt(df_year, path_txt, rate_columns=("Rain",))

    pq_dir = tmp_path / "pq-run"
    pq_dir.mkdir()
    df_pq = df_year.copy()
    df_pq.index = pd.MultiIndex.from_product([[1], idx], names=["grid", "datetime"])
    df_pq.columns = _suews_columns(list(df_year.columns))
    df_pq.to_parquet(pq_dir / "KCL_SUEWS_output.parquet")
    return txt_dir, pq_dir, path_txt


def _summarise_json_by_name(
    run_dir: Path, variables: str | None = None
) -> dict[str, dict[str, object]]:
    """Run ``suews summarise --format json`` and index variable records by name."""
    args = [str(run_dir), "--format", "json"]
    if variables is not None:
        args[1:1] = ["--variables", variables]
    result = CliRunner().invoke(summarise_output_cmd, args)
    assert result.exit_code == 0, result.output
    return {v["name"]: v for v in json.loads(result.stdout)["data"]["variables"]}


def test_summarise_decodes_legacy_text_sentinels_from_real_writer(
    tmp_path: Path,
) -> None:
    """Real ``_write_df_txt`` output decodes ``-999`` sentinels to match Parquet.

    Exercises mixed missing/valid ``Fcld`` (11/36 missing, 7 valid zeros,
    mean 0.384), valid night-time ``Kdown`` zeros, valid negative fluxes and
    temperatures, close non-sentinel negative values (``-999.0001`` and
    ``-998.9999``), and a ``%.10e`` water-rate column (``-9.9900000000e+02``).
    """
    txt_dir, pq_dir, path_txt = _build_mixed_sentinel_outputs(tmp_path)

    raw_text = path_txt.read_text(encoding="utf-8")
    assert "-999.0000" in raw_text
    assert "-9.9900000000e+02" in raw_text

    selection = "Fcld,Kdown,QN,T2,Lob,Rain"
    by_txt = _summarise_json_by_name(txt_dir, selection)
    by_pq = _summarise_json_by_name(pq_dir, selection)

    assert by_txt["Fcld"]["mean"] == pytest.approx(0.384, abs=1e-12)
    assert by_txt["Fcld"]["min"] == pytest.approx(0.0, abs=1e-12)
    assert by_txt["Fcld"]["max"] == pytest.approx(1.0, abs=1e-12)
    assert by_txt["Fcld"]["nan_pct"] == pytest.approx(100.0 * 11 / 36, abs=1e-12)
    assert by_txt["Kdown"]["mean"] == pytest.approx(0.0, abs=1e-12)
    assert by_txt["Kdown"]["nan_pct"] == pytest.approx(0.0, abs=1e-12)
    assert by_txt["Lob"]["nan_pct"] == pytest.approx(0.0, abs=1e-12)
    assert by_txt["Lob"]["min"] == pytest.approx(-1000.0, abs=1e-12)
    assert by_txt["Lob"]["max"] == pytest.approx(999.0, abs=1e-12)
    assert by_txt["Rain"]["nan_pct"] == pytest.approx(100.0 * 11 / 36, abs=1e-12)

    for var_name in selection.split(","):
        assert by_txt[var_name]["nan_pct"] == pytest.approx(
            by_pq[var_name]["nan_pct"], abs=1e-12
        )
        for stat in ("mean", "min", "max"):
            assert by_txt[var_name][stat] == pytest.approx(
                by_pq[var_name][stat], abs=5e-5
            )

    by_default = _summarise_json_by_name(txt_dir)
    assert by_default["Fcld"]["mean"] == pytest.approx(0.384, abs=1e-12)
    assert by_default["Fcld"]["nan_pct"] == pytest.approx(100.0 * 11 / 36, abs=1e-12)

    res_text = CliRunner().invoke(
        summarise_output_cmd,
        [str(txt_dir), "--variables", "Fcld,Kdown", "--format", "text"],
    )
    assert res_text.exit_code == 0, res_text.output
    assert "0.384" in res_text.stdout
    assert "30.6%" in res_text.stdout
    assert "-305" not in res_text.stdout


def test_summarise_legacy_text_all_missing_column(tmp_path: Path) -> None:
    """All-missing ``-999`` column reports 100% missing with null stats."""
    n_steps = 36
    idx = pd.date_range("2012-01-01 00:05", periods=n_steps, freq="5min")
    df_year = pd.DataFrame(
        {
            "Fcld": np.full(n_steps, np.nan),
            "Kdown": np.zeros(n_steps),
            "QN": np.full(n_steps, -38.5),
        },
        index=idx,
    )
    txt_dir = tmp_path / "unobserved-txt"
    txt_dir.mkdir()
    _write_df_txt(df_year, txt_dir / "KCL1_2012_SUEWS_5.txt")

    runner = CliRunner()
    for extra_args in (["--variables", "Fcld,Kdown,QN"], []):
        res_json = runner.invoke(
            summarise_output_cmd, [str(txt_dir), *extra_args, "--format", "json"]
        )
        assert res_json.exit_code == 0, res_json.output
        by_name = {
            v["name"]: v for v in json.loads(res_json.stdout)["data"]["variables"]
        }
        assert by_name["Fcld"] == {
            "name": "Fcld",
            "mean": None,
            "min": None,
            "max": None,
            "nan_pct": 100.0,
        }
        assert by_name["Kdown"]["mean"] == pytest.approx(0.0, abs=1e-12)
        assert by_name["QN"]["mean"] == pytest.approx(-38.5, abs=1e-12)

    res_text = runner.invoke(
        summarise_output_cmd,
        [str(txt_dir), "--variables", "Fcld", "--format", "text"],
    )
    assert res_text.exit_code == 0, res_text.output
    assert "n/a" in res_text.stdout
    assert "100.0%" in res_text.stdout
    assert "-999" not in res_text.stdout


def test_summarise_legacy_text_integer_sentinel_and_valid_negatives(
    tmp_path: Path,
) -> None:
    """Integer ``-999`` tokens decode cleanly without affecting close integers."""
    txt_dir = tmp_path / "int-txt"
    txt_dir.mkdir()
    content = (
        "Year\tDOY\tHour\tMin\tDectime\tMixed\tAllMiss\tValidNeg\n"
        "2012\t1\t0\t5\t0.0035\t-999\t-999\t-998\n"
        "2012\t1\t0\t10\t0.0069\t0\t-999\t-1000\n"
        "2012\t1\t0\t15\t0.0104\t2\t-999\t-1\n"
        "2012\t1\t0\t20\t0.0139\t-999\t-999\t999\n"
    )
    (txt_dir / "KCL1_2012_SUEWS_5.txt").write_text(content, encoding="utf-8")

    runner = CliRunner()
    res = runner.invoke(summarise_output_cmd, [str(txt_dir), "--format", "json"])
    assert res.exit_code == 0, res.output
    by_name = {v["name"]: v for v in json.loads(res.stdout)["data"]["variables"]}

    assert by_name["Mixed"]["mean"] == pytest.approx(1.0, abs=1e-12)
    assert by_name["Mixed"]["min"] == pytest.approx(0.0, abs=1e-12)
    assert by_name["Mixed"]["max"] == pytest.approx(2.0, abs=1e-12)
    assert by_name["Mixed"]["nan_pct"] == pytest.approx(50.0, abs=1e-12)

    assert by_name["AllMiss"] == {
        "name": "AllMiss",
        "mean": None,
        "min": None,
        "max": None,
        "nan_pct": 100.0,
    }
    assert by_name["ValidNeg"]["nan_pct"] == pytest.approx(0.0, abs=1e-12)
    assert by_name["ValidNeg"]["min"] == pytest.approx(-1000.0, abs=1e-12)
    assert by_name["ValidNeg"]["max"] == pytest.approx(999.0, abs=1e-12)


def test_summarise_does_not_decode_minus_999_in_csv_or_parquet(tmp_path: Path) -> None:
    """Flat CSV and Parquet files without a SUEWS text sentinel keep ``-999``."""
    idx = pd.date_range("2024-06-01", periods=4, freq="h", name="datetime")
    df = pd.DataFrame({"QH": [-999.0, 1.0, 2.0, 3.0]}, index=idx)

    csv_dir = tmp_path / "csv-run"
    csv_dir.mkdir()
    df.to_csv(csv_dir / "df_output.csv")

    pq_dir = tmp_path / "pq-run"
    pq_dir.mkdir()
    df.to_parquet(pq_dir / "df_output.parquet")

    runner = CliRunner()
    for run_dir in (csv_dir, pq_dir):
        res = runner.invoke(
            summarise_output_cmd,
            [str(run_dir), "--variables", "QH", "--format", "json"],
        )
        assert res.exit_code == 0, res.output
        entry = json.loads(res.stdout)["data"]["variables"][0]
        assert entry["min"] == pytest.approx(-999.0, abs=1e-12)
        assert entry["mean"] == pytest.approx((-999.0 + 6.0) / 4.0, abs=1e-12)
        assert entry["nan_pct"] == pytest.approx(0.0, abs=1e-12)


def test_real_writer_text_consumers_agree(tmp_path: Path) -> None:
    """``summarise``, ``diagnose``, and ``compare`` agree on real ``_write_df_txt`` output."""
    n_steps = 8
    idx = pd.date_range("2012-01-01 01:00", periods=n_steps, freq="h")
    df_year = pd.DataFrame(
        {
            "QN": np.full(n_steps, 200.0),
            "QF": np.full(n_steps, 20.0),
            "QH": np.full(n_steps, 100.0),
            "QE": np.full(n_steps, 80.0),
            "QS": np.full(n_steps, 40.0),
            "Fcld": [np.nan, np.nan, 0.0, 0.2, 0.4, 0.6, 0.8, 1.0],
        },
        index=idx,
    )
    txt_dir = tmp_path / "txt-run"
    txt_dir.mkdir()
    _write_df_txt(df_year, txt_dir / "KCL1_2012_SUEWS_60.txt")

    pq_dir = tmp_path / "pq-run"
    pq_dir.mkdir()
    df_pq = df_year.copy()
    df_pq.index = pd.MultiIndex.from_product([[1], idx], names=["grid", "datetime"])
    df_pq.columns = _suews_columns(list(df_year.columns))
    df_pq.to_parquet(pq_dir / "KCL_SUEWS_output.parquet")

    assert check_nan_proportion(txt_dir).passed
    assert check_energy_balance_closure(txt_dir).passed

    runner = CliRunner()
    res_cmp = runner.invoke(
        compare_runs_cmd,
        [str(txt_dir), str(pq_dir), "--variables", "QH,Fcld", "--format", "json"],
    )
    assert res_cmp.exit_code == 0, res_cmp.output
    per_var = json.loads(res_cmp.stdout)["data"]["per_variable"]
    assert per_var["QH"]["n"] == 8
    assert per_var["QH"]["rmse"] == pytest.approx(0.0, abs=1e-12)
    assert per_var["Fcld"]["n"] == 6
    assert per_var["Fcld"]["rmse"] == pytest.approx(0.0, abs=1e-12)
