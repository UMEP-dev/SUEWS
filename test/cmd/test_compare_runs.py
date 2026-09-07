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


# --- alignment, grid selection and sample counting (gh#1744) ------------------


def _write_native_text(
    run_dir: Path,
    *,
    doy: int,
    qh: list[float],
    year: int = 2024,
    grid: int = 1,
) -> Path:
    """Write a legacy text output carrying the native Year/DOY/Hour/Min clock."""
    run_dir.mkdir(parents=True, exist_ok=True)
    n = len(qh)
    df = pd.DataFrame({
        "Year": [year] * n,
        "DOY": [doy] * n,
        "Hour": list(range(1, n + 1)),
        "Min": [0] * n,
        "Dectime": [doy - 1 + h / 24 for h in range(1, n + 1)],
        "QH": qh,
    })
    path = run_dir / f"Kc{grid}_{year}_SUEWS_60.txt"
    df.to_csv(path, sep="\t", index=False)
    return path


def _invoke_json(*args: str):
    result = CliRunner().invoke(compare_runs_cmd, [*args, "--format", "json"])
    return result, json.loads(result.stdout)


def test_native_text_on_different_days_is_rejected(tmp_path: Path) -> None:
    """Identical values on 1 Jan and 30 Jan must not compare as a perfect match."""
    _write_native_text(tmp_path / "a", doy=1, qh=[10.0, 11.0, 12.0, 13.0])
    _write_native_text(tmp_path / "b", doy=30, qh=[10.0, 11.0, 12.0, 13.0])

    result, envelope = _invoke_json(
        str(tmp_path / "a"), str(tmp_path / "b"), "--variables", "QH"
    )

    assert result.exit_code == 1
    assert envelope["status"] == "error"
    assert "No overlapping timestamps" in envelope["errors"][0]["message"]
    assert envelope["data"]["time_axis_overlap"]["n"] == 0


def test_native_text_clock_is_parsed_and_aligned(tmp_path: Path) -> None:
    _write_native_text(tmp_path / "a", doy=5, qh=[10.0, 11.0, 12.0, 13.0])
    _write_native_text(tmp_path / "b", doy=5, qh=[12.0, 13.0, 14.0, 15.0])

    result, envelope = _invoke_json(
        str(tmp_path / "a"), str(tmp_path / "b"), "--variables", "QH"
    )

    assert result.exit_code == 0, result.output
    data = envelope["data"]
    assert data["alignment"] == "time"
    assert data["time_axis_overlap"]["start"] == "2024-01-05T01:00:00"
    assert data["time_axis_overlap"]["end"] == "2024-01-05T04:00:00"
    assert data["time_axis_overlap"]["n"] == 4
    assert data["per_variable"]["QH"]["bias"] == pytest.approx(2.0)
    assert data["grid"] == {"baseline": "1", "scenario": "1"}


def test_native_text_yearly_files_of_one_grid_are_concatenated(tmp_path: Path) -> None:
    for year in (2024, 2025):
        _write_native_text(
            tmp_path / "a", doy=1, year=year, qh=[10.0, 11.0, 12.0, 13.0]
        )
        _write_native_text(
            tmp_path / "b", doy=1, year=year, qh=[11.0, 12.0, 13.0, 14.0]
        )

    result, envelope = _invoke_json(
        str(tmp_path / "a"), str(tmp_path / "b"), "--variables", "QH"
    )

    assert result.exit_code == 0, result.output
    assert envelope["data"]["time_axis_overlap"]["n"] == 8
    assert envelope["data"]["per_variable"]["QH"]["n"] == 8


def test_native_text_sentinel_is_missing_not_a_value(tmp_path: Path) -> None:
    _write_native_text(tmp_path / "a", doy=1, qh=[10.0, -999.0, 12.0, 13.0, 14.0])
    _write_native_text(tmp_path / "b", doy=1, qh=[10.0, 11.0, 12.0, 13.0, 14.0])

    result, envelope = _invoke_json(
        str(tmp_path / "a"), str(tmp_path / "b"), "--variables", "QH"
    )

    assert result.exit_code == 0, result.output
    entry = envelope["data"]["per_variable"]["QH"]
    assert entry["n"] == 4
    assert entry["rmse"] == pytest.approx(0.0)


def test_disjoint_parquet_time_axes_exit_nonzero(tmp_path: Path) -> None:
    for name, start in (("c", "2024-01-01"), ("d", "2024-02-01")):
        run_dir = tmp_path / name
        run_dir.mkdir()
        pd.DataFrame(
            {"QH": [10.0, 11.0, 12.0, 13.0]},
            index=pd.date_range(start, periods=4, freq="h", name="datetime"),
        ).to_parquet(run_dir / "df_output.parquet")

    result, envelope = _invoke_json(
        str(tmp_path / "c"), str(tmp_path / "d"), "--variables", "QH"
    )

    assert result.exit_code == 1
    assert envelope["status"] == "error"
    assert (
        "2024-01-01T00:00:00 .. 2024-01-01T03:00:00" in envelope["errors"][0]["message"]
    )
    assert envelope["data"]["time_axis_overlap"]["n"] == 0


def test_sample_count_is_finite_pairs(tmp_path: Path) -> None:
    idx = pd.date_range("2024-06-01", periods=5, freq="h", name="datetime")
    run_a = tmp_path / "a"
    run_b = tmp_path / "b"
    run_a.mkdir()
    run_b.mkdir()
    pd.DataFrame({"QH": [1.0, 2.0, 3.0, np.nan, np.nan]}, index=idx).to_parquet(
        run_a / "df_output.parquet"
    )
    pd.DataFrame({"QH": [1.0, 2.0, 3.0, 4.0, 5.0]}, index=idx).to_parquet(
        run_b / "df_output.parquet"
    )

    result, envelope = _invoke_json(
        str(run_a), str(run_b), "--variables", "QH", "--metrics", "rmse"
    )

    assert result.exit_code == 0, result.output
    assert envelope["data"]["time_axis_overlap"]["n"] == 5
    assert envelope["data"]["per_variable"]["QH"]["n"] == 3


def test_no_evaluable_variable_is_an_error(tmp_path: Path) -> None:
    idx = pd.date_range("2024-06-01", periods=4, freq="h", name="datetime")
    run_a = tmp_path / "a"
    run_b = tmp_path / "b"
    run_a.mkdir()
    run_b.mkdir()
    pd.DataFrame({"QH": [np.nan] * 4}, index=idx).to_parquet(
        run_a / "df_output.parquet"
    )
    pd.DataFrame({"QH": [1.0, 2.0, 3.0, 4.0]}, index=idx).to_parquet(
        run_b / "df_output.parquet"
    )

    result, envelope = _invoke_json(str(run_a), str(run_b), "--variables", "QH,QE")

    assert result.exit_code == 1
    assert envelope["status"] == "error"
    assert "No evaluable comparison" in envelope["errors"][0]["message"]
    messages = [
        w["message"] if isinstance(w, dict) else w for w in envelope["warnings"]
    ]
    assert any("'QH' skipped" in m for m in messages)
    assert any("'QE' skipped" in m for m in messages)


def _write_two_grid_parquet(run_dir: Path, offset_grid2: float) -> None:
    run_dir.mkdir(parents=True)
    idx = pd.MultiIndex.from_product(
        [[1, 2], pd.date_range("2024-06-01", periods=4, freq="h")],
        names=["grid", "datetime"],
    )
    qh = np.array(
        [10.0, 11.0, 12.0, 13.0] + [20.0 + offset_grid2 + i for i in range(4)]
    )
    df = pd.DataFrame(
        np.column_stack([qh, qh + 1.0, qh + 2.0]),
        columns=_suews_columns(["QH", "QE", "QN"]),
        index=idx,
    )
    df.to_parquet(run_dir / "SUEWS_output.parquet")


def test_multi_grid_requires_explicit_grid(tmp_path: Path) -> None:
    _write_two_grid_parquet(tmp_path / "a", 0.0)
    _write_two_grid_parquet(tmp_path / "b", 3.0)

    result, envelope = _invoke_json(
        str(tmp_path / "a"), str(tmp_path / "b"), "--variables", "QH"
    )

    assert result.exit_code == 1
    assert envelope["status"] == "error"
    assert "--grid" in envelope["errors"][0]["message"]
    assert "1, 2" in envelope["errors"][0]["message"]


def test_multi_grid_selected_grid_is_compared_and_reported(tmp_path: Path) -> None:
    _write_two_grid_parquet(tmp_path / "a", 0.0)
    _write_two_grid_parquet(tmp_path / "b", 3.0)

    result, envelope = _invoke_json(
        str(tmp_path / "a"), str(tmp_path / "b"), "--variables", "QH", "--grid", "2"
    )

    assert result.exit_code == 0, result.output
    assert envelope["data"]["grid"] == {"baseline": "2", "scenario": "2"}
    assert envelope["data"]["per_variable"]["QH"]["bias"] == pytest.approx(3.0)

    result, envelope = _invoke_json(
        str(tmp_path / "a"), str(tmp_path / "b"), "--variables", "QH", "--grid", "7"
    )
    assert result.exit_code == 1
    assert "grid '7' not present" in envelope["errors"][0]["message"]


def test_differing_single_grid_identities_are_warned(tmp_path: Path) -> None:
    _write_native_text(tmp_path / "a", doy=1, grid=1, qh=[10.0, 11.0, 12.0, 13.0])
    _write_native_text(tmp_path / "b", doy=1, grid=2, qh=[10.0, 11.0, 12.0, 13.0])

    result, envelope = _invoke_json(
        str(tmp_path / "a"), str(tmp_path / "b"), "--variables", "QH"
    )

    assert result.exit_code == 0, result.output
    assert envelope["status"] == "warning"
    assert envelope["data"]["grid"] == {"baseline": "1", "scenario": "2"}
    messages = [
        w["message"] if isinstance(w, dict) else w for w in envelope["warnings"]
    ]
    assert any("grid identities differ" in m for m in messages)


def test_positional_alignment_is_explicit_and_labelled(tmp_path: Path) -> None:
    """Inputs without a time axis are rejected unless --align positional is given."""
    for name, values in (
        ("a", [10.0, 11.0, 12.0, 13.0]),
        ("b", [11.0, 12.0, 13.0, 14.0]),
    ):
        run_dir = tmp_path / name
        run_dir.mkdir()
        pd.DataFrame({"QH": values}).to_csv(run_dir / "df_output.csv", index=False)

    result, envelope = _invoke_json(
        str(tmp_path / "a"), str(tmp_path / "b"), "--variables", "QH"
    )
    assert result.exit_code == 1
    assert (
        "No time axis recoverable for baseline and scenario"
        in envelope["errors"][0]["message"]
    )
    assert "--align positional" in envelope["errors"][0]["message"]

    result, envelope = _invoke_json(
        str(tmp_path / "a"),
        str(tmp_path / "b"),
        "--variables",
        "QH",
        "--align",
        "positional",
    )
    assert result.exit_code == 0, result.output
    assert envelope["status"] == "warning"
    assert envelope["data"]["alignment"] == "positional"
    assert envelope["data"]["time_axis_overlap"] == {"start": None, "end": None, "n": 4}
    assert envelope["data"]["per_variable"]["QH"]["bias"] == pytest.approx(1.0)
    messages = [
        w["message"] if isinstance(w, dict) else w for w in envelope["warnings"]
    ]
    assert any("positional alignment" in m for m in messages)


def test_text_output_labels_alignment_and_grid(tmp_path: Path) -> None:
    _write_native_text(tmp_path / "a", doy=1, qh=[10.0, 11.0, 12.0, 13.0])
    _write_native_text(tmp_path / "b", doy=1, qh=[10.0, 11.0, 12.0, 13.0])

    result = CliRunner().invoke(
        compare_runs_cmd,
        [str(tmp_path / "a"), str(tmp_path / "b"), "--variables", "QH"],
    )

    assert result.exit_code == 0, result.output
    assert "alignment: time" in result.output
    assert "grid     : baseline=1 scenario=1" in result.output
    assert "n=4" in result.output


def test_multi_grid_run_against_observations_csv_with_grid(tmp_path: Path) -> None:
    """--grid selects the run grid while an unlabelled observations CSV is accepted."""
    _write_two_grid_parquet(tmp_path / "run", 0.0)
    path_obs = tmp_path / "observations.csv"
    pd.DataFrame({
        "datetime": pd.date_range("2024-06-01", periods=4, freq="h"),
        "QH": [
            21.0,
            22.0,
            23.0,
            24.0,
        ],  # grid 2 QH is 20..23; bias = scenario - baseline = +1
    }).to_csv(path_obs, index=False)

    result, envelope = _invoke_json(
        str(tmp_path / "run"), str(path_obs), "--variables", "QH", "--grid", "2"
    )

    assert result.exit_code == 0, result.output
    assert envelope["data"]["grid"] == {"baseline": "2", "scenario": None}
    assert envelope["data"]["per_variable"]["QH"]["bias"] == pytest.approx(1.0)
    assert envelope["data"]["per_variable"]["QH"]["n"] == 4

    result, envelope = _invoke_json(
        str(tmp_path / "run"), str(path_obs), "--variables", "QH"
    )
    assert result.exit_code == 1
    assert "--grid" in envelope["errors"][0]["message"]


def test_grid_does_not_disambiguate_several_unlabelled_partitions(
    tmp_path: Path,
) -> None:
    _write_two_grid_parquet(tmp_path / "run", 0.0)
    run_dir = tmp_path / "obs"
    run_dir.mkdir()
    for name in ("df_output_a.csv", "df_output_b.csv"):
        pd.DataFrame({
            "datetime": pd.date_range("2024-06-01", periods=4, freq="h"),
            "QH": [1.0, 2.0, 3.0, 4.0],
        }).to_csv(run_dir / name, index=False)

    result, envelope = _invoke_json(
        str(tmp_path / "run"), str(run_dir), "--variables", "QH", "--grid", "2"
    )

    assert result.exit_code == 1
    assert "scenario: grid '2' not present" in envelope["errors"][0]["message"]
