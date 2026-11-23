"""Integration tests for public SUEWS commands on streamed Parquet datasets."""

from __future__ import annotations

import importlib
import json
from pathlib import Path
import warnings

from click.testing import CliRunner
import numpy as np
import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
import pytest

from supy.cmd.suews_cli import cli
from supy.data_model.core.model import OutputFormat
from supy.diagnostics import check_provenance_present, check_run

_run_output_module = importlib.import_module("supy._run_output")
_save_module = importlib.import_module("supy._save")
StreamedParquetParts = _save_module.StreamedParquetParts
save_df_output_parquet = _save_module.save_df_output_parquet
write_output_parquet = _save_module.write_output_parquet

pytestmark = [
    pytest.mark.api,
    pytest.mark.contract("C-API", "C-OUTPUT", "C-SAVE"),
]

_STEP_S = 300


def _invoke_cli(args: list[str]) -> tuple[int, dict]:
    result = CliRunner().invoke(cli, [*args, "--format", "json"])
    assert result.stdout, f"expected JSON envelope on stdout, got: {result.output!r}"
    return result.exit_code, json.loads(result.stdout)


def _make_grid_frame(
    timestamps: pd.DatetimeIndex,
    grid: int,
    columns: pd.MultiIndex,
    *,
    qh_offset: float = 0.0,
    nan_qh_fraction: float = 0.0,
    break_closure: bool = False,
    include_stebbs: bool = False,
) -> pd.DataFrame:
    """Build one grid's energy-balanced SUEWS frame."""
    n_rows = len(timestamps)
    diurnal = np.linspace(0.0, 2.0 * np.pi, n_rows, endpoint=False)
    qn = 200.0 + 40.0 * np.sin(diurnal) + 5.0 * grid
    qf = 0.1 * qn
    qh = 0.5 * qn + qh_offset
    qe = 0.4 * qn
    qs = 0.2 * qn - qh_offset
    if break_closure:
        qs = 0.2 * qn
        qh += 0.8 * qn

    if nan_qh_fraction > 0.0:
        qh = qh.copy()
        qh[: round(n_rows * nan_qh_fraction)] = np.nan

    cols = [qn, qf, qh, qe, qs]
    if include_stebbs:
        cols.append(15.0 + 2.0 * np.cos(diurnal))

    idx = pd.MultiIndex.from_product([[grid], timestamps], names=["grid", "datetime"])
    return pd.DataFrame(np.column_stack(cols), index=idx, columns=columns)


def _make_balanced_frame(
    timestamps: pd.DatetimeIndex,
    *,
    grids: tuple[int, ...] = (1,),
    qh_offset_by_grid: dict[int, float] | None = None,
    nan_qh_fraction_by_grid: dict[int, float] | None = None,
    break_closure_grids: set[int] | None = None,
    include_stebbs: bool = False,
) -> pd.DataFrame:
    """Build a canonical multi-group SUEWS frame closing QN + QF = QH + QE + QS."""
    qh_offset_by_grid = qh_offset_by_grid or {}
    nan_qh_fraction_by_grid = nan_qh_fraction_by_grid or {}
    break_closure_grids = break_closure_grids or set()

    list_var_tuples: list[tuple[str, str]] = [
        ("SUEWS", "QN"),
        ("SUEWS", "QF"),
        ("SUEWS", "QH"),
        ("SUEWS", "QE"),
        ("SUEWS", "QS"),
    ]
    if include_stebbs:
        list_var_tuples.append(("STEBBS", "QN_bldg_FA"))
    columns = pd.MultiIndex.from_tuples(list_var_tuples, names=["group", "var"])

    list_frames = [
        _make_grid_frame(
            timestamps,
            grid,
            columns,
            qh_offset=qh_offset_by_grid.get(grid, 0.0),
            nan_qh_fraction=nan_qh_fraction_by_grid.get(grid, 0.0),
            break_closure=grid in break_closure_grids,
            include_stebbs=include_stebbs,
        )
        for grid in grids
    ]
    df_out = pd.concat(list_frames, axis=0).sort_index()
    df_out.attrs["supy_output_interval_s"] = _STEP_S
    return df_out


def _state_for_clock(timestamp_reference: str) -> pd.DataFrame:
    if timestamp_reference == "follow":
        return pd.DataFrame()
    return pd.DataFrame({"timezone": [0.0]}, index=[1])


def _write_monolithic_run(
    run_dir: Path,
    df_output: pd.DataFrame,
    *,
    site: str = "selected grid",
    timestamp_reference: str = "follow",
    with_provenance: bool = True,
) -> Path:
    run_dir.mkdir(parents=True, exist_ok=True)
    df_to_write = df_output.copy(deep=False)
    df_to_write.attrs = {**df_output.attrs, "supy_output_interval_s": _STEP_S}
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", FutureWarning)
        list_paths = save_df_output_parquet(
            df_to_write,
            _state_for_clock(timestamp_reference),
            freq_s=_STEP_S,
            site=site,
            path_dir_save=run_dir,
            save_tstep=True,
            save_state=False,
            timestamp_reference=timestamp_reference,
        )
    if with_provenance:
        (run_dir / "provenance.json").write_text(
            json.dumps({"command": "test"}, indent=2), encoding="utf-8"
        )
    return Path(list_paths[0])


def _write_streamed_run(
    run_dir: Path,
    list_chunks: list[tuple[int, int, pd.DataFrame]],
    *,
    site: str = "selected grid",
    timestamp_reference: str = "follow",
    complete: bool = True,
) -> Path:
    run_dir.mkdir(parents=True, exist_ok=True)
    suffix = {
        "follow": "",
        "utc": "_UTC",
        "local_standard_time": "_STANDARD",
        "daylight": "_DAYLIGHT",
    }[timestamp_reference]
    stem = (
        f"{site}_SUEWS_output{suffix}.parquet"
        if site
        else f"SUEWS_output{suffix}.parquet"
    )
    path_dataset = run_dir / stem
    writer = StreamedParquetParts(path_dataset, tstep_s=_STEP_S)
    writer.prepare()

    df_state = _state_for_clock(timestamp_reference)
    for year, chunk, df_chunk in list_chunks:
        writer.start_chunk(year, chunk)
        save_df_output_parquet(
            df_chunk,
            df_state,
            freq_s=_STEP_S,
            site=site,
            path_dir_save=run_dir,
            save_tstep=True,
            save_state=False,
            timestamp_reference=timestamp_reference,
            streamed_parquet=writer,
        )

    if complete:
        writer.finish()
        (run_dir / "provenance.json").write_text(
            json.dumps({"command": "test"}, indent=2), encoding="utf-8"
        )
    return path_dataset


def _plant_dataset_noise(path_dataset: Path, df_decoy: pd.DataFrame) -> None:
    """Populate a dataset directory with temporary, sidecar, and unrelated files."""
    (path_dataset / ".part-2017-003.parquet.tmp").write_bytes(b"incomplete")
    (path_dataset / "part-2017-003.parquet.tmp").write_bytes(b"incomplete")
    (path_dataset / ".part-analysis.tmp").write_bytes(b"user tmp")
    (path_dataset / "notes.txt").write_text("unrelated note\n", encoding="utf-8")
    (path_dataset / "extra.parquet").write_bytes(b"not a committed part")
    (path_dataset / "df_output_stray.csv").write_text("QH\n-999\n", encoding="utf-8")
    (path_dataset / "Site1_2017_SUEWS_60.txt").write_text(
        "Year DOY Hour Min QH\n2017 1 1 0 -999\n", encoding="utf-8"
    )
    (path_dataset / "part-2017-004.parquet").mkdir()
    (path_dataset / "nested").mkdir()
    df_decoy.to_parquet(path_dataset / "nested" / "part-2017-005.parquet")


def _check_map(envelope: dict) -> dict[str, dict]:
    return {entry["name"]: entry for entry in envelope["data"]["checks"]}


def _two_day_pair(
    periods: int = 12, *, include_stebbs: bool = False
) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    day1 = pd.date_range("2017-08-26 00:05", periods=periods, freq="5min")
    day2 = pd.date_range("2017-08-27 00:05", periods=periods, freq="5min")
    df_day1 = _make_balanced_frame(day1, include_stebbs=include_stebbs)
    df_day2 = _make_balanced_frame(day2, include_stebbs=include_stebbs)
    df_whole = pd.concat([df_day1, df_day2], axis=0).sort_index()
    return df_day1, df_day2, df_whole


def _assert_diagnose_matches_monolithic(
    dir_whole: Path, dir_stream: Path, path_dataset: Path, expected_rows: int
) -> None:
    rc_dw, env_dw = _invoke_cli(["diagnose", str(dir_whole)])
    rc_ds, env_ds = _invoke_cli(["diagnose", str(dir_stream)])
    assert rc_dw == 0 and rc_ds == 0
    assert env_ds["status"] == "success"
    checks_w, checks_s = _check_map(env_dw), _check_map(env_ds)
    assert checks_s["output_files_present"]["passed"] is True
    assert checks_s["output_files_present"]["details"]["files"] == [str(path_dataset)]
    assert checks_s["nan_proportion"]["details"] == checks_w["nan_proportion"]["details"]
    assert checks_s["energy_balance_closure"]["details"]["n_rows"] == expected_rows
    assert checks_s["energy_balance_closure"]["details"]["ratio_mean"] == pytest.approx(
        checks_w["energy_balance_closure"]["details"]["ratio_mean"]
    )
    assert (dir_stream / "diagnostics.json").is_file()
    assert not (path_dataset / "diagnostics.json").exists()


def test_completed_streamed_parquet_matches_monolithic_and_ignores_noise(
    tmp_path: Path,
) -> None:
    """Streamed Parquet matches monolithic across diagnose, summarise, and compare."""
    df_day1, df_day2, df_whole = _two_day_pair(12, include_stebbs=True)
    dir_whole, dir_stream = tmp_path / "whole", tmp_path / "streamed"
    _write_monolithic_run(dir_whole, df_whole)
    path_dataset = _write_streamed_run(
        dir_stream, [(2017, 1, df_day1), (2017, 2, df_day2)], complete=True
    )
    _plant_dataset_noise(path_dataset, df_day1)
    (dir_stream / "other.parquet").write_bytes(b"unrelated sibling")

    _assert_diagnose_matches_monolithic(dir_whole, dir_stream, path_dataset, 24)

    rc_sw, env_sw = _invoke_cli(["summarise", str(dir_whole), "--variables", "QH,QE,QN"])
    rc_ss, env_ss = _invoke_cli(["summarise", str(dir_stream), "--variables", "QH,QE,QN"])
    assert rc_sw == 0 and rc_ss == 0
    assert env_ss["data"]["n_steps"] == env_sw["data"]["n_steps"] == 24
    assert env_ss["data"]["period"] == env_sw["data"]["period"]
    assert env_ss["data"]["variables"] == env_sw["data"]["variables"]

    rc_cmp, env_cmp = _invoke_cli(["compare", str(dir_whole), str(dir_stream)])
    assert rc_cmp == 0 and env_cmp["status"] == "success"
    assert env_cmp["data"]["time_axis_overlap"]["n"] == 24
    for var in ("QH", "QE", "QN"):
        assert env_cmp["data"]["per_variable"][var]["n"] == 24
        assert env_cmp["data"]["per_variable"][var]["rmse"] == pytest.approx(0.0, abs=1e-12)
        assert env_cmp["data"]["per_variable"][var]["bias"] == pytest.approx(0.0, abs=1e-12)

    # Restricted output group policy is preserved: non-SUEWS STEBBS stays excluded.
    rc_stebbs, env_stebbs = _invoke_cli([
        "compare",
        str(dir_whole),
        str(dir_stream),
        "--variables",
        "QN_bldg_FA",
        "--metrics",
        "bias,rmse",
    ])
    assert rc_stebbs == 1 and env_stebbs["status"] == "error"
    assert "No evaluable comparison" in env_stebbs["errors"][0]["message"]


def test_later_part_only_signal_is_caught_by_compare_summarise_and_diagnose(
    tmp_path: Path,
) -> None:
    """A signal confined to part 2 must not be masked by a clean first part."""
    day1 = pd.date_range("2017-08-26 00:05", periods=10, freq="5min")
    day2 = pd.date_range("2017-08-27 00:05", periods=10, freq="5min")
    df_base_1 = _make_balanced_frame(day1)
    dir_base = tmp_path / "base"
    _write_monolithic_run(
        dir_base, pd.concat([df_base_1, _make_balanced_frame(day2)]).sort_index()
    )

    # Bias present ONLY in part 2 (+20 W m-2 on QH -> +10 overall across equal parts).
    dir_biased = tmp_path / "biased"
    _write_streamed_run(
        dir_biased,
        [(2017, 1, df_base_1), (2017, 2, _make_balanced_frame(day2, qh_offset_by_grid={1: 20.0}))],
        complete=True,
    )

    rc_cmp, env_cmp = _invoke_cli([
        "compare",
        str(dir_base),
        str(dir_biased),
        "--variables",
        "QH",
    ])
    assert rc_cmp == 0
    assert env_cmp["data"]["time_axis_overlap"]["n"] == 20
    assert env_cmp["data"]["per_variable"]["QH"]["bias"] == pytest.approx(10.0)
    assert env_cmp["data"]["per_variable"]["QH"]["rmse"] > 10.0

    _, env_sum_b = _invoke_cli(["summarise", str(dir_base), "--variables", "QH"])
    rc_sum_s, env_sum_s = _invoke_cli(["summarise", str(dir_biased), "--variables", "QH"])
    assert rc_sum_s == 0 and env_sum_s["data"]["n_steps"] == 20
    assert env_sum_s["data"]["variables"][0]["mean"] == pytest.approx(
        env_sum_b["data"]["variables"][0]["mean"] + 10.0
    )

    # NaN + closure failure present ONLY in part 2 (part 1 is 100% clean).
    dir_broken = tmp_path / "broken_part2"
    _write_streamed_run(
        dir_broken,
        [
            (2017, 1, df_base_1),
            (
                2017,
                2,
                _make_balanced_frame(
                    day2, nan_qh_fraction_by_grid={1: 0.5}, break_closure_grids={1}
                ),
            ),
        ],
        complete=True,
    )

    rc_diag, env_diag = _invoke_cli(["diagnose", str(dir_broken)])
    assert rc_diag == 0 and env_diag["status"] == "warning"
    checks = _check_map(env_diag)
    assert checks["output_files_present"]["passed"] is True
    assert checks["nan_proportion"]["passed"] is False
    assert checks["nan_proportion"]["details"]["fractions"][
        "selected grid_SUEWS_output.parquet[grid=1]"
    ]["QH"] == pytest.approx(0.25)
    assert checks["energy_balance_closure"]["passed"] is False


def test_two_grid_multi_year_streamed_dataset_ordering_and_selection(
    tmp_path: Path,
) -> None:
    """Two grids across two years preserve ordering, grid selection, and per-grid checks."""
    chunks_base = [
        (2011, 2, _make_balanced_frame(pd.date_range("2011-12-31 22:05", periods=6, freq="5min"), grids=(1, 2))),
        (2012, 1, _make_balanced_frame(pd.date_range("2012-01-01 00:05", periods=6, freq="5min"), grids=(1, 2))),
        (2012, 2, _make_balanced_frame(pd.date_range("2012-01-02 00:05", periods=6, freq="5min"), grids=(1, 2))),
    ]
    part_last = pd.date_range("2012-01-02 00:05", periods=6, freq="5min")
    dir_base, dir_scen = tmp_path / "base_2g", tmp_path / "scen_2g"
    _write_monolithic_run(
        dir_base, pd.concat([df for _, _, df in chunks_base]).sort_index(), site="site"
    )
    _write_streamed_run(
        dir_scen,
        [
            chunks_base[0],
            chunks_base[1],
            (2012, 2, _make_balanced_frame(part_last, grids=(1, 2), qh_offset_by_grid={2: 15.0})),
        ],
        site="site",
        complete=True,
    )

    rc_no_grid, env_no_grid = _invoke_cli([
        "compare",
        str(dir_base),
        str(dir_scen),
        "--variables",
        "QH",
    ])
    assert rc_no_grid == 1
    assert "--grid" in env_no_grid["errors"][0]["message"]
    assert "1, 2" in env_no_grid["errors"][0]["message"]

    for grid_id, expected_bias in (("1", 0.0), ("2", 5.0)):
        rc_g, env_g = _invoke_cli([
            "compare",
            str(dir_base),
            str(dir_scen),
            "--variables",
            "QH",
            "--grid",
            grid_id,
        ])
        assert rc_g == 0
        assert env_g["data"]["grid"] == {"baseline": grid_id, "scenario": grid_id}
        assert env_g["data"]["time_axis_overlap"]["n"] == 18
        assert env_g["data"]["time_axis_overlap"]["start"] == "2011-12-31T22:05:00"
        assert env_g["data"]["time_axis_overlap"]["end"] == "2012-01-02T00:30:00"
        assert env_g["data"]["per_variable"]["QH"]["bias"] == pytest.approx(
            expected_bias, abs=1e-12
        )

    rc_sum, env_sum = _invoke_cli(["summarise", str(dir_scen), "--variables", "QH"])
    assert rc_sum == 0
    assert env_sum["data"]["n_steps"] == 36
    assert env_sum["data"]["period"]["start"] == "2011-12-31 22:05:00"
    assert env_sum["data"]["period"]["end"] == "2012-01-02 00:30:00"

    # Grid 2 fails NaN check ONLY in the last 2012 chunk while grid 1 stays clean.
    dir_diag = tmp_path / "diag_2g"
    _write_streamed_run(
        dir_diag,
        [
            chunks_base[0],
            chunks_base[1],
            (2012, 2, _make_balanced_frame(part_last, grids=(1, 2), nan_qh_fraction_by_grid={2: 0.5})),
        ],
        site="site",
        complete=True,
    )
    rc_diag, env_diag = _invoke_cli(["diagnose", str(dir_diag)])
    assert rc_diag == 0
    checks = _check_map(env_diag)
    assert checks["nan_proportion"]["passed"] is False
    assert checks["nan_proportion"]["details"]["n_partitions"] == 2
    fractions = checks["nan_proportion"]["details"]["fractions"]
    assert fractions["site_SUEWS_output.parquet[grid=1]"]["QH"] == pytest.approx(0.0)
    assert fractions["site_SUEWS_output.parquet[grid=2]"]["QH"] == pytest.approx(3.0 / 18.0)


def test_partial_vs_completed_streamed_dataset(tmp_path: Path) -> None:
    """An interrupted streamed dataset is readable before finish() and expands once completed."""
    df_day1, df_day2, df_full = _two_day_pair(8)
    dir_ref, dir_partial, dir_done = (
        tmp_path / "ref",
        tmp_path / "partial",
        tmp_path / "completed",
    )
    _write_monolithic_run(dir_ref, df_full, site="site")
    path_dataset = _write_streamed_run(
        dir_partial, [(2017, 1, df_day1)], site="site", complete=False
    )
    (path_dataset / ".part-2017-002.parquet.tmp").write_bytes(b"corrupted partial write")

    rc_dp, env_dp = _invoke_cli(["diagnose", str(dir_partial)])
    assert rc_dp == 0 and env_dp["status"] == "warning"
    checks_p = _check_map(env_dp)
    assert checks_p["provenance_present"]["passed"] is False
    assert checks_p["output_files_present"]["passed"] is True
    assert checks_p["nan_proportion"]["passed"] is True
    assert checks_p["energy_balance_closure"]["passed"] is True
    assert checks_p["energy_balance_closure"]["details"]["n_rows"] == 8

    assert _invoke_cli(["compare", str(dir_ref), str(dir_partial)])[1]["data"][
        "time_axis_overlap"
    ]["n"] == 8
    assert _invoke_cli(["summarise", str(dir_partial), "--variables", "QH"])[1]["data"][
        "n_steps"
    ] == 8

    _write_streamed_run(
        dir_done,
        [(2017, 1, df_day1), (2017, 2, df_day2)],
        site="site",
        complete=True,
    )
    rc_dc, env_dc = _invoke_cli(["diagnose", str(dir_done)])
    assert rc_dc == 0 and env_dc["status"] == "success"
    checks_c = _check_map(env_dc)
    assert checks_c["provenance_present"]["passed"] is True
    assert checks_c["energy_balance_closure"]["details"]["n_rows"] == 16
    assert _invoke_cli(["compare", str(dir_ref), str(dir_done)])[1]["data"][
        "time_axis_overlap"
    ]["n"] == 16
    assert _invoke_cli(["summarise", str(dir_done), "--variables", "QH"])[1]["data"][
        "n_steps"
    ] == 16


def test_direct_input_standalone_file_vs_canonical_dataset_directory(
    tmp_path: Path,
) -> None:
    """Pointing directly at a canonical dataset dir or standalone file works safely."""
    df_day1, df_day2, df_full = _two_day_pair(8)
    path_file = _write_monolithic_run(tmp_path / "mono", df_full, site="site")
    path_dataset = _write_streamed_run(
        tmp_path / "stream",
        [(2017, 1, df_day1), (2017, 2, df_day2)],
        site="site",
        complete=True,
    )
    assert path_file.is_file() and path_dataset.is_dir()

    for left, right in ((path_file, path_dataset), (path_dataset, path_file)):
        rc_cmp, env_cmp = _invoke_cli(["compare", str(left), str(right)])
        assert rc_cmp == 0 and env_cmp["status"] == "success"
        assert env_cmp["data"]["time_axis_overlap"]["n"] == 16
        assert env_cmp["data"]["per_variable"]["QH"]["rmse"] == pytest.approx(0.0, abs=1e-12)

    _, env_sf = _invoke_cli(["summarise", str(path_file), "--variables", "QH,QE,QN"])
    rc_sd, env_sd = _invoke_cli(["summarise", str(path_dataset), "--variables", "QH,QE,QN"])
    assert rc_sd == 0
    assert env_sd["data"]["n_steps"] == env_sf["data"]["n_steps"] == 16
    assert env_sd["data"]["period"] == env_sf["data"]["period"]
    assert env_sd["data"]["variables"] == env_sf["data"]["variables"]

    for target in (path_file, path_dataset):
        rc_diag, env_diag = _invoke_cli(["diagnose", str(target)])
        assert rc_diag == 0 and env_diag["status"] == "success"
        checks = _check_map(env_diag)
        assert checks["provenance_present"]["passed"] is True
        assert checks["provenance_present"]["details"]["path"] == str(
            target.parent / "provenance.json"
        )
        assert checks["output_files_present"]["passed"] is True
        assert checks["nan_proportion"]["passed"] is True
        assert checks["energy_balance_closure"]["passed"] is True
        assert checks["energy_balance_closure"]["details"]["n_rows"] == 16

    # Direct inputs intentionally omit diagnostics.json (never polluting the
    # Parquet dataset directory or writing incidentally into the parent).
    assert not (path_dataset / "diagnostics.json").exists()
    assert not (path_dataset.parent / "diagnostics.json").exists()
    assert not (path_file.parent / "diagnostics.json").exists()
    assert len(pd.read_parquet(path_dataset)) == 16


def test_empty_and_temporary_only_dataset_directories_fail_cleanly(
    tmp_path: Path,
) -> None:
    """Empty or temp-only canonical Parquet directories never pass as empty runs."""
    day1 = pd.date_range("2017-08-26 00:05", periods=4, freq="5min")
    df_ref = _make_balanced_frame(day1)
    dir_ref = tmp_path / "ref"
    _write_monolithic_run(dir_ref, df_ref, site="site")

    dir_empty = tmp_path / "empty_run"
    ds_empty = dir_empty / "site_SUEWS_output.parquet"
    ds_empty.mkdir(parents=True)
    (dir_empty / "provenance.json").write_text("{}", encoding="utf-8")
    pd.DataFrame([{"site": "site"}]).to_parquet(dir_empty / "site_SUEWS_metadata.parquet")

    dir_tmp_only = tmp_path / "tmp_only_run"
    ds_tmp_only = dir_tmp_only / "site_SUEWS_output.parquet"
    ds_tmp_only.mkdir(parents=True)
    (dir_tmp_only / "provenance.json").write_text("{}", encoding="utf-8")
    _plant_dataset_noise(ds_tmp_only, df_ref)
    df_ref.droplevel("group", axis=1).to_csv(ds_tmp_only / "df_output.csv")

    for target in (dir_empty, ds_empty, dir_tmp_only, ds_tmp_only):
        rc_diag, env_diag = _invoke_cli(["diagnose", str(target)])
        assert rc_diag == 0 and env_diag["status"] == "error"
        assert _check_map(env_diag)["output_files_present"]["passed"] is False

        rc_cmp, env_cmp = _invoke_cli(["compare", str(dir_ref), str(target)])
        assert rc_cmp == 1 and env_cmp["status"] == "error"

        rc_sum, env_sum = _invoke_cli(["summarise", str(target)])
        assert rc_sum == 1 and env_sum["status"] == "error"

    assert not (ds_empty / "diagnostics.json").exists()
    assert not (ds_tmp_only / "diagnostics.json").exists()

    rc_meta, env_meta = _invoke_cli([
        "diagnose",
        str(dir_empty / "site_SUEWS_metadata.parquet"),
    ])
    assert rc_meta == 0 and env_meta["status"] == "error"
    assert _check_map(env_meta)["output_files_present"]["passed"] is False


def test_explicit_clock_suffix_and_format_priority_avoid_double_counting(
    tmp_path: Path,
) -> None:
    """Explicit timestamp-reference Parquet dataset takes priority over CSV and text."""
    df_day1, df_day2, df_full = _two_day_pair(6)
    day1 = df_day1.index.get_level_values("datetime")
    dir_ref, dir_mixed = tmp_path / "ref", tmp_path / "mixed"
    _write_monolithic_run(dir_ref, df_full, site="site", timestamp_reference="utc")
    path_ds = _write_streamed_run(
        dir_mixed,
        [(2017, 1, df_day1), (2017, 2, df_day2)],
        site="site",
        timestamp_reference="utc",
        complete=True,
    )
    assert path_ds.name == "site_SUEWS_output_UTC.parquet"

    pd.DataFrame({
        "datetime": day1,
        "QN": [200.0] * 6,
        "QF": [20.0] * 6,
        "QH": [np.nan] * 6,
        "QE": [80.0] * 6,
        "QS": [40.0] * 6,
    }).to_csv(dir_mixed / "df_output.csv", index=False)
    (dir_mixed / "Site1_2017_SUEWS_60.txt").write_text(
        "Year DOY Hour Min Dectime QN QF QH QE QS\n"
        "2017 238 1 0 238.0 200 20 -999 -999 40\n",
        encoding="utf-8",
    )

    rc_diag, env_diag = _invoke_cli(["diagnose", str(dir_mixed)])
    assert rc_diag == 0 and env_diag["status"] == "success"
    checks = _check_map(env_diag)
    assert checks["nan_proportion"]["details"]["n_partitions"] == 1
    assert list(checks["nan_proportion"]["details"]["fractions"]) == [
        "site_SUEWS_output_UTC.parquet[grid=1]"
    ]
    assert checks["energy_balance_closure"]["details"]["n_rows"] == 12

    rc_cmp, env_cmp = _invoke_cli(["compare", str(dir_ref), str(dir_mixed)])
    assert rc_cmp == 0
    assert env_cmp["data"]["time_axis_overlap"]["n"] == 12
    assert env_cmp["data"]["per_variable"]["QH"]["rmse"] == pytest.approx(0.0, abs=1e-12)

    rc_sum, env_sum = _invoke_cli(["summarise", str(dir_mixed), "--variables", "QH"])
    assert rc_sum == 0
    assert env_sum["data"]["n_steps"] == 12
    assert env_sum["data"]["variables"][0]["nan_pct"] == pytest.approx(0.0)


def _assert_full_summary_matches_monolithic(
    dir_whole: Path, dir_stream: Path
) -> None:
    """Compare full default summaries and direct finite/infinity counts."""
    rc_whole, env_whole = _invoke_cli(["summarise", str(dir_whole)])
    rc_stream, env_stream = _invoke_cli(["summarise", str(dir_stream)])
    assert rc_whole == 0 and env_whole["status"] == "success", env_whole
    assert rc_stream == 0 and env_stream["status"] == "success", env_stream
    assert env_whole["data"]["n_steps"] == 48
    assert env_stream["data"]["n_steps"] == 50
    assert env_stream["data"]["period"] == env_whole["data"]["period"]

    df_whole = _run_output_module._load_run_output_dataframe(dir_whole, flatten=False)
    df_stream = _run_output_module._load_run_output_dataframe(dir_stream, flatten=False)
    whole_by_key = {
        (v.get("group"), v["name"]): v for v in env_whole["data"]["variables"]
    }
    assert len(env_stream["data"]["variables"]) == 160
    for entry_stream in env_stream["data"]["variables"]:
        key = (entry_stream.get("group"), entry_stream["name"])
        entry_whole = whole_by_key[key]
        arr_whole = pd.to_numeric(df_whole[key], errors="coerce").to_numpy(dtype=float)
        arr_stream = pd.to_numeric(df_stream[key], errors="coerce").to_numpy(
            dtype=float
        )
        assert not np.isinf(arr_whole).any(), key
        assert not np.isinf(arr_stream).any(), key
        assert int(np.isfinite(arr_stream).sum()) == int(np.isfinite(arr_whole).sum()), (
            key
        )
        assert entry_whole["nan_pct"] == pytest.approx(
            float(np.isnan(arr_whole).mean() * 100.0), abs=1e-12
        )
        assert entry_stream["nan_pct"] == pytest.approx(
            float(np.isnan(arr_stream).mean() * 100.0), abs=1e-12
        )
        for stat in ("mean", "min", "max"):
            if entry_whole[stat] is None:
                assert entry_stream[stat] is None
            else:
                assert entry_stream[stat] == pytest.approx(
                    entry_whole[stat], rel=1e-12, abs=1e-12
                )


def _assert_streamed_matches_monolithic_summary(
    dir_whole: Path, dir_stream: Path
) -> None:
    """Compare streamed vs monolithic summaries across selected and full columns.

    Streamed output honours ``output_cfg.groups=["SUEWS", "DailyState"]`` and
    stamps the two DailyState rows at native ``23:55:00`` (50 rows = 48 hourly
    SUEWS rows + 2 DailyState-only rows), whereas monolithic ``sim.save()``
    aligns DailyState onto the ``23:00:00`` hourly rows (48 rows) across all
    groups.
    """
    rc_whole_sel, env_whole_sel = _invoke_cli(
        ["summarise", str(dir_whole), "--variables", "QH,QE,QN"]
    )
    rc_stream_sel, env_stream_sel = _invoke_cli(
        ["summarise", str(dir_stream), "--variables", "QH,QE,QN"]
    )
    assert rc_whole_sel == 0 and env_whole_sel["status"] == "success", env_whole_sel
    assert rc_stream_sel == 0 and env_stream_sel["status"] == "success", env_stream_sel
    assert env_whole_sel["data"]["n_steps"] == 48
    assert env_stream_sel["data"]["n_steps"] == 50
    assert env_stream_sel["data"]["period"] == {
        "start": "2012-01-01 01:00:00",
        "end": "2012-01-03 00:00:00",
    }
    assert env_stream_sel["data"]["period"] == env_whole_sel["data"]["period"]
    for entry_stream, entry_whole in zip(
        env_stream_sel["data"]["variables"],
        env_whole_sel["data"]["variables"],
        strict=True,
    ):
        assert entry_stream["group"] == entry_whole["group"] == "SUEWS"
        assert entry_stream["name"] == entry_whole["name"]
        for stat in ("mean", "min", "max"):
            assert entry_stream[stat] == pytest.approx(
                entry_whole[stat], rel=1e-12, abs=1e-12
            )
        assert entry_whole["nan_pct"] == pytest.approx(0.0, abs=1e-12)
        assert entry_stream["nan_pct"] == pytest.approx(4.0, abs=1e-12)

    _assert_full_summary_matches_monolithic(dir_whole, dir_stream)


def test_real_model_streamed_parquet_positive_control(
    short_sample_sim, tmp_path: Path
) -> None:
    """Real SUEWSSimulation streamed Parquet output passes diagnose, summarise, and compare."""
    dir_whole, dir_stream = tmp_path / "model_whole", tmp_path / "model_stream"
    dir_whole.mkdir()
    dir_stream.mkdir()

    def _configure(sim, out_dir: Path, stream_chunk: int | None):
        output_cfg = sim.config.model.control.output
        output_cfg.format = OutputFormat.PARQUET
        output_cfg.dir = str(out_dir)
        output_cfg.freq = 3600
        output_cfg.groups = ["SUEWS", "DailyState"]
        output_cfg.stream_chunk = stream_chunk
        return sim

    sim_whole = _configure(short_sample_sim(), dir_whole, None)
    sim_whole.run(start_date="2012-01-01 00:05", end_date="2012-01-03 00:00")
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", FutureWarning)
        sim_whole.save(dir_whole)

    sim_stream = _configure(short_sample_sim(), dir_stream, 1)
    sim_stream.run(start_date="2012-01-01 00:05", end_date="2012-01-03 00:00")

    [path_dataset] = list(dir_stream.glob("*SUEWS_output*.parquet"))
    assert path_dataset.is_dir()
    assert sorted(p.name for p in path_dataset.iterdir()) == [
        "part-2012-001.parquet",
        "part-2012-002.parquet",
    ]

    rc_diag, env_diag = _invoke_cli(["diagnose", str(dir_stream)])
    assert rc_diag == 0 and env_diag["status"] == "success", env_diag
    checks = _check_map(env_diag)
    for name in (
        "provenance_present",
        "output_files_present",
        "nan_proportion",
        "energy_balance_closure",
    ):
        assert checks[name]["passed"] is True

    rc_cmp, env_cmp = _invoke_cli(["compare", str(dir_whole), str(dir_stream)])
    assert rc_cmp == 0 and env_cmp["status"] == "success"
    for var in ("QH", "QE", "QN"):
        assert env_cmp["data"]["per_variable"][var]["n"] == 48
        assert env_cmp["data"]["per_variable"][var]["rmse"] == pytest.approx(0.0, abs=1e-12)
        assert env_cmp["data"]["per_variable"][var]["bias"] == pytest.approx(0.0, abs=1e-12)

    _assert_streamed_matches_monolithic_summary(dir_whole, dir_stream)


def _write_raw_streamed_parts(
    run_dir: Path,
    list_parts: list[tuple[int, int, pd.DataFrame, int | None]],
    *,
    site: str = "site",
    complete: bool = True,
) -> Path:
    """Write committed streamed Parquet parts directly via StreamedParquetParts."""
    run_dir.mkdir(parents=True, exist_ok=True)
    path_dataset = run_dir / f"{site}_SUEWS_output.parquet"
    writer = StreamedParquetParts(path_dataset, tstep_s=_STEP_S)
    writer.prepare()
    for year, chunk, df_part, interval_s in list_parts:
        writer.start_chunk(year, chunk)
        writer.write(path_dataset, df_part, lambda: [], interval_s=interval_s)
    if complete:
        writer.finish()
        (run_dir / "provenance.json").write_text(
            json.dumps({"command": "test"}, indent=2), encoding="utf-8"
        )
    return path_dataset


def test_short_later_part_missing_required_column_is_rejected_explicitly(
    tmp_path: Path,
) -> None:
    """A 1-row later part missing QH must never pass diagnostics as 1/101 NaN."""
    ts_healthy = pd.date_range("2012-01-01 00:05", periods=100, freq="5min")
    ts_damaged = pd.date_range(
        ts_healthy[-1] + pd.Timedelta(minutes=5), periods=1, freq="5min"
    )
    df_healthy = _make_balanced_frame(ts_healthy)
    df_damaged = _make_balanced_frame(ts_damaged).drop(columns=[("SUEWS", "QH")])

    dir_ref = tmp_path / "ref"
    _write_monolithic_run(dir_ref, df_healthy, site="site")

    dir_bad = tmp_path / "damaged_missing_qh"
    path_ds = _write_raw_streamed_parts(
        dir_bad,
        [(2012, 1, df_healthy, _STEP_S), (2012, 2, df_damaged, _STEP_S)],
        complete=True,
    )

    by_name = {res.name: res for res in check_run(dir_bad)}
    assert by_name["nan_proportion"].passed is False
    assert by_name["energy_balance_closure"].passed is False
    for check_name in ("nan_proportion", "energy_balance_closure"):
        msg = by_name[check_name].message
        assert "part-2012-002.parquet" in msg
        assert "part-2012-001.parquet" in msg
        assert "QH" in msg

    rc_diag, env_diag = _invoke_cli(["diagnose", str(dir_bad)])
    assert rc_diag == 0 and env_diag["status"] != "success"
    checks = _check_map(env_diag)
    assert checks["nan_proportion"]["passed"] is False
    assert checks["energy_balance_closure"]["passed"] is False

    for cmd_args in (
        ["summarise", str(dir_bad), "--variables", "QH"],
        ["summarise", str(path_ds), "--variables", "QH"],
        ["compare", str(dir_ref), str(dir_bad), "--variables", "QH"],
        ["compare", str(dir_ref), str(path_ds), "--variables", "QH"],
    ):
        rc, env = _invoke_cli(cmd_args)
        assert rc == 1 and env["status"] == "error", env
        msg = env["errors"][0]["message"]
        assert "part-2012-002.parquet" in msg
        assert "QH" in msg


def test_extra_column_and_incompatible_index_rejected_explicitly(
    tmp_path: Path,
) -> None:
    """Unexpected columns and incompatible row index structures fail explicitly."""
    df_day1, df_day2, _ = _two_day_pair(6)

    # 1. Unexpected extra column in part 2.
    df_extra = df_day2.copy()
    df_extra["SUEWS", "ExtraFlux"] = 1.0
    dir_extra = tmp_path / "extra_col"
    _write_raw_streamed_parts(
        dir_extra, [(2017, 1, df_day1, _STEP_S), (2017, 2, df_extra, _STEP_S)]
    )
    rc_e, env_e = _invoke_cli(["summarise", str(dir_extra), "--variables", "QH"])
    assert rc_e == 1 and env_e["status"] == "error"
    msg_e = env_e["errors"][0]["message"]
    assert "part-2017-002.parquet" in msg_e and "ExtraFlux" in msg_e

    # 2. Incompatible row index (MultiIndex vs flat DatetimeIndex).
    dir_idx = tmp_path / "bad_index"
    _write_raw_streamed_parts(
        dir_idx,
        [(2017, 1, df_day1, _STEP_S), (2017, 2, df_day2.droplevel("grid"), _STEP_S)],
    )
    rc_i, env_i = _invoke_cli(["summarise", str(dir_idx), "--variables", "QH"])
    assert rc_i == 1 and env_i["status"] == "error"
    msg_i = env_i["errors"][0]["message"]
    assert "part-2017-002.parquet" in msg_i and "index" in msg_i.lower()


def test_incompatible_grid_or_column_dtype_rejected_explicitly(
    tmp_path: Path,
) -> None:
    """Mixed grid ID dtypes and incompatible column dtypes are rejected explicitly."""
    df_day1, df_day2, _ = _two_day_pair(6)

    # 1. Incompatible grid ID dtype (int in part 1 vs string in part 2).
    df_str_grid = df_day2.copy()
    df_str_grid.index = pd.MultiIndex.from_arrays(
        [
            df_str_grid.index.get_level_values("grid").astype(str),
            df_str_grid.index.get_level_values("datetime"),
        ],
        names=["grid", "datetime"],
    )
    dir_grid_dtype = tmp_path / "bad_grid_dtype"
    _write_raw_streamed_parts(
        dir_grid_dtype, [(2017, 1, df_day1, _STEP_S), (2017, 2, df_str_grid, _STEP_S)]
    )
    rc_g, env_g = _invoke_cli(["summarise", str(dir_grid_dtype), "--variables", "QH"])
    assert rc_g == 1 and env_g["status"] == "error"
    assert "grid" in env_g["errors"][0]["message"]

    # 2. Incompatible column dtype (numeric float64 vs string).
    df_bad_dtype = df_day2.copy()
    df_bad_dtype["SUEWS", "QH"] = "corrupt_text"
    dir_dtype = tmp_path / "bad_dtype"
    _write_raw_streamed_parts(
        dir_dtype, [(2017, 1, df_day1, _STEP_S), (2017, 2, df_bad_dtype, _STEP_S)]
    )
    rc_d, env_d = _invoke_cli(["summarise", str(dir_dtype), "--variables", "QH"])
    assert rc_d == 1 and env_d["status"] == "error"
    msg_d = env_d["errors"][0]["message"]
    assert "part-2017-002.parquet" in msg_d and "QH" in msg_d


def test_harmless_column_reordering_preserved(tmp_path: Path) -> None:
    """Harmless column reordering across parts matches monolithic output."""
    ts_part1 = pd.date_range("2012-01-01 00:05", periods=100, freq="5min")
    ts_part2 = pd.date_range(
        ts_part1[-1] + pd.Timedelta(minutes=5), periods=1, freq="5min"
    )
    df_p1 = _make_balanced_frame(ts_part1)
    df_p2 = _make_balanced_frame(ts_part2)

    df_reordered = df_p2[list(reversed(df_p2.columns))].copy()
    df_reordered.attrs = dict(df_p2.attrs)
    dir_mono = tmp_path / "mono_reorder"
    dir_reorder = tmp_path / "stream_reorder"
    _write_monolithic_run(dir_mono, pd.concat([df_p1, df_p2]).sort_index(), site="site")
    _write_raw_streamed_parts(
        dir_reorder, [(2012, 1, df_p1, _STEP_S), (2012, 2, df_reordered, _STEP_S)]
    )
    rc_d, env_d = _invoke_cli(["diagnose", str(dir_reorder)])
    assert rc_d == 0 and env_d["status"] == "success", env_d
    rc_c, env_c = _invoke_cli(
        ["compare", str(dir_mono), str(dir_reorder), "--variables", "QH"]
    )
    assert rc_c == 0 and env_c["data"]["per_variable"]["QH"]["n"] == 101
    assert env_c["data"]["per_variable"]["QH"]["rmse"] == pytest.approx(0.0, abs=1e-12)


def test_present_but_nan_later_field_flows_to_diagnostics(tmp_path: Path) -> None:
    """Present-but-NaN QH in a later part is diagnosed by value, not schema error."""
    ts_part1 = pd.date_range("2012-01-01 00:05", periods=100, freq="5min")
    ts_part2 = pd.date_range(
        ts_part1[-1] + pd.Timedelta(minutes=5), periods=1, freq="5min"
    )
    df_p1 = _make_balanced_frame(ts_part1)

    # 1-row present-but-NaN part 2 (1/101 < 5% -> passes with measured NaN fraction).
    df_nan_small = _make_balanced_frame(ts_part2)
    df_nan_small["SUEWS", "QH"] = np.nan
    dir_nan_small = tmp_path / "present_nan_small"
    _write_raw_streamed_parts(
        dir_nan_small, [(2012, 1, df_p1, _STEP_S), (2012, 2, df_nan_small, _STEP_S)]
    )
    by_name_small = {res.name: res for res in check_run(dir_nan_small)}
    assert by_name_small["nan_proportion"].passed is True
    assert by_name_small["nan_proportion"].details["fractions"][
        "site_SUEWS_output.parquet[grid=1]"
    ]["QH"] == pytest.approx(1.0 / 101.0)
    assert by_name_small["energy_balance_closure"].passed is True

    # 10-row present-but-NaN part 2 (10/110 > 5% -> diagnosed as NaN warning).
    ts_part2_big = pd.date_range(
        ts_part1[-1] + pd.Timedelta(minutes=5), periods=10, freq="5min"
    )
    df_nan_big = _make_balanced_frame(ts_part2_big)
    df_nan_big["SUEWS", "QH"] = np.nan
    dir_nan_big = tmp_path / "present_nan_big"
    _write_raw_streamed_parts(
        dir_nan_big, [(2012, 1, df_p1, _STEP_S), (2012, 2, df_nan_big, _STEP_S)]
    )
    by_name_big = {res.name: res for res in check_run(dir_nan_big)}
    assert by_name_big["nan_proportion"].passed is False
    assert "Could not read" not in by_name_big["nan_proportion"].message
    assert by_name_big["nan_proportion"].details["fractions"][
        "site_SUEWS_output.parquet[grid=1]"
    ]["QH"] == pytest.approx(10.0 / 110.0)


def test_mixed_grids_and_legitimate_metadata_variations_preserved(
    tmp_path: Path,
) -> None:
    """Mixed grids and legitimate per-chunk coverage metadata merge cleanly."""
    ts1 = pd.date_range("2012-01-01 00:05", periods=4, freq="5min")
    ts2 = pd.date_range("2012-01-01 00:25", periods=4, freq="5min")
    df_p1 = _make_balanced_frame(ts1, grids=(1, 2)).drop((2, ts1[1]))
    df_p2 = _make_balanced_frame(ts2, grids=(2,))

    # Legitimate metadata variations: distinct per-chunk coverage dicts and int vs float interval.
    df_p1.attrs = {
        "supy_output_interval_s": 300,
        "supy_output_coverage_s": {"1|2012-01-01T00:05:00": 300.0},
        "supy_output_daily_coverage_s": {"1|2012-01-01T23:55:00": 86400.0},
    }
    df_p2.attrs = {
        "supy_output_interval_s": 300.0,
        "supy_output_coverage_s": {"2|2012-01-01T00:25:00": 150.0},
    }

    dir_unfinished = tmp_path / "mixed_grids_unfinished"
    path_ds = _write_raw_streamed_parts(
        dir_unfinished,
        [(2012, 1, df_p1, _STEP_S), (2012, 2, df_p2, _STEP_S)],
        complete=False,
    )
    df_loaded = _run_output_module._read_output_file(path_ds)
    assert len(df_loaded) == 11
    assert df_loaded.attrs["supy_output_coverage_s"] == {
        "1|2012-01-01T00:05:00": 300.0,
        "2|2012-01-01T00:25:00": 150.0,
    }
    assert df_loaded.attrs["supy_output_daily_coverage_s"] == {
        "1|2012-01-01T23:55:00": 86400.0
    }

    by_name = {res.name: res for res in check_run(dir_unfinished)}
    assert by_name["provenance_present"].passed is False
    assert by_name["nan_proportion"].passed is True
    assert by_name["energy_balance_closure"].passed is True
    assert by_name["energy_balance_closure"].details["n_partitions"] == 2
    assert (
        by_name["energy_balance_closure"].details["partitions"][
            "site_SUEWS_output.parquet[grid=1]"
        ]["n_rows"]
        == 4
    )
    assert (
        by_name["energy_balance_closure"].details["partitions"][
            "site_SUEWS_output.parquet[grid=2]"
        ]["n_rows"]
        == 7
    )


def test_incompatible_invariant_metadata_rejected_explicitly(tmp_path: Path) -> None:
    """Conflicting interval, units, and Arrow field interval metadata are rejected."""
    ts1 = pd.date_range("2012-01-01 00:05", periods=4, freq="5min")
    ts2 = pd.date_range("2012-01-01 00:25", periods=4, freq="5min")

    # 1. Conflicting supy_output_interval_s attr.
    df_part = _make_balanced_frame(ts2)
    df_part.attrs["supy_output_interval_s"] = 3600
    dir_case = tmp_path / "bad_interval"
    _write_raw_streamed_parts(
        dir_case,
        [(2012, 1, _make_balanced_frame(ts1), _STEP_S), (2012, 2, df_part, 3600)],
    )
    rc, env = _invoke_cli(["summarise", str(dir_case), "--variables", "QH"])
    assert rc == 1 and env["status"] == "error"
    assert "supy_output_interval_s" in env["errors"][0]["message"]

    # 2. Conflicting supy_output_units attr.
    df_part = _make_balanced_frame(ts2)
    df_part.attrs["supy_output_units"] = {"SUEWS": {"QH": "kW m-2"}}
    dir_case = tmp_path / "bad_units"
    _write_raw_streamed_parts(
        dir_case,
        [(2012, 1, _make_balanced_frame(ts1), _STEP_S), (2012, 2, df_part, _STEP_S)],
    )
    rc, env = _invoke_cli(["summarise", str(dir_case), "--variables", "QH"])
    assert rc == 1 and env["status"] == "error"
    assert "supy_output_units" in env["errors"][0]["message"]

    # 3. Conflicting Arrow field interval metadata when attrs omit supy_output_interval_s.
    df_no_attr_1 = _make_balanced_frame(ts1)
    df_no_attr_2 = _make_balanced_frame(ts2)
    df_no_attr_1.attrs.pop("supy_output_interval_s", None)
    df_no_attr_2.attrs.pop("supy_output_interval_s", None)
    ds_arrow_int = tmp_path / "bad_arrow_interval" / "site_SUEWS_output.parquet"
    ds_arrow_int.mkdir(parents=True)
    write_output_parquet(df_no_attr_1, ds_arrow_int / "part-2012-001.parquet", 300)
    write_output_parquet(df_no_attr_2, ds_arrow_int / "part-2012-002.parquet", 3600)
    rc, env = _invoke_cli(["summarise", str(ds_arrow_int), "--variables", "QH"])
    assert rc == 1 and env["status"] == "error"
    msg = env["errors"][0]["message"]
    assert "part-2012-002.parquet" in msg and "interval" in msg


def _write_part_with_field_metadata(
    path_part: Path,
    df_part: pd.DataFrame,
    dict_field_meta: dict[bytes, bytes],
) -> None:
    """Write one Parquet part attaching ``dict_field_meta`` to the QH column."""
    table = pa.Table.from_pandas(df_part)
    list_fields = []
    for arrow_field in table.schema:
        if "QH" in arrow_field.name:
            merged_meta = dict(arrow_field.metadata or {})
            merged_meta.update(dict_field_meta)
            list_fields.append(arrow_field.with_metadata(merged_meta))
        else:
            list_fields.append(arrow_field)
    table = table.cast(pa.schema(list_fields, metadata=table.schema.metadata))
    pq.write_table(table, path_part)


def test_later_part_arrow_field_metadata_conflict_rejected_across_three_parts(
    tmp_path: Path,
) -> None:
    """Metadata keys first introduced in part 2 must conflict with part 3."""
    ts1 = pd.date_range("2012-01-01 00:05", periods=4, freq="5min")
    ts2 = pd.date_range("2012-01-01 00:25", periods=4, freq="5min")
    ts3 = pd.date_range("2012-01-01 00:45", periods=4, freq="5min")
    list_frames = [_make_balanced_frame(ts) for ts in (ts1, ts2, ts3)]
    for df_part in list_frames:
        df_part.attrs.clear()

    # Conflict: part 1 declares only units; part 2 introduces interval="5 minutes";
    # part 3 conflicts with interval="1 hour".
    ds_conflict = tmp_path / "three_part_conflict" / "site_SUEWS_output.parquet"
    ds_conflict.mkdir(parents=True)
    _write_part_with_field_metadata(
        ds_conflict / "part-2012-001.parquet", list_frames[0], {b"units": b"W m-2"}
    )
    _write_part_with_field_metadata(
        ds_conflict / "part-2012-002.parquet",
        list_frames[1],
        {b"units": b"W m-2", b"interval": b"5 minutes"},
    )
    _write_part_with_field_metadata(
        ds_conflict / "part-2012-003.parquet",
        list_frames[2],
        {b"units": b"W m-2", b"interval": b"1 hour"},
    )
    rc, env = _invoke_cli(["summarise", str(ds_conflict), "--variables", "QH"])
    assert rc == 1 and env["status"] == "error"
    msg = env["errors"][0]["message"]
    assert "part-2012-002.parquet" in msg
    assert "part-2012-003.parquet" in msg
    assert "interval" in msg

    # Positive control: part 3 agrees with part 2's interval="5 minutes".
    ds_ok = tmp_path / "three_part_ok" / "site_SUEWS_output.parquet"
    ds_ok.mkdir(parents=True)
    _write_part_with_field_metadata(
        ds_ok / "part-2012-001.parquet", list_frames[0], {b"units": b"W m-2"}
    )
    _write_part_with_field_metadata(
        ds_ok / "part-2012-002.parquet",
        list_frames[1],
        {b"units": b"W m-2", b"interval": b"5 minutes"},
    )
    _write_part_with_field_metadata(
        ds_ok / "part-2012-003.parquet",
        list_frames[2],
        {b"units": b"W m-2", b"interval": b"5 minutes"},
    )
    rc_ok, env_ok = _invoke_cli(["summarise", str(ds_ok), "--variables", "QH"])
    assert rc_ok == 0 and env_ok["status"] == "success", env_ok
    assert env_ok["data"]["n_steps"] == 12


def test_direct_provenance_resolution_and_sidecar_collision_safety(
    tmp_path: Path,
) -> None:
    """Direct dataset/file provenance resolves beside target without sidecar collisions."""
    df_day1, df_day2, df_full = _two_day_pair(6)

    # Completed run with existing diagnostics.json from diagnosing the enclosing run dir.
    dir_done = tmp_path / "completed_run"
    path_ds = _write_streamed_run(
        dir_done, [(2017, 1, df_day1), (2017, 2, df_day2)], site="site", complete=True
    )
    sentinel = json.dumps({"sentinel": "enclosing-run-sidecar"})
    (dir_done / "diagnostics.json").write_text(sentinel, encoding="utf-8")

    # Read-only Python APIs resolve sibling provenance and write nothing.
    prov_direct = check_provenance_present(path_ds)
    assert prov_direct.passed is True
    assert prov_direct.details["path"] == str(dir_done / "provenance.json")
    assert all(res.passed for res in check_run(path_ds))
    assert not (path_ds / "diagnostics.json").exists()
    assert (dir_done / "diagnostics.json").read_text(encoding="utf-8") == sentinel

    # CLI diagnose on direct dataset preserves both atomic dataset and parent sidecar.
    rc_ds, env_ds = _invoke_cli(["diagnose", str(path_ds)])
    assert rc_ds == 0 and env_ds["status"] == "success"
    assert not (path_ds / "diagnostics.json").exists()
    assert (dir_done / "diagnostics.json").read_text(encoding="utf-8") == sentinel

    # Missing provenance beside direct dataset/file reports the sibling expected path.
    dir_no_prov = tmp_path / "unprovenanced_run"
    path_ds_unprov = _write_streamed_run(
        dir_no_prov, [(2017, 1, df_day1)], site="site", complete=False
    )
    path_file_unprov = _write_monolithic_run(
        tmp_path / "unprovenanced_mono", df_full, site="site", with_provenance=False
    )
    for target in (path_ds_unprov, path_file_unprov):
        res_prov = check_provenance_present(target)
        assert res_prov.passed is False
        assert res_prov.severity == "warning"
        assert res_prov.details["expected"] == str(target.parent / "provenance.json")
        rc_u, env_u = _invoke_cli(["diagnose", str(target)])
        assert rc_u == 0 and env_u["status"] == "warning"
        assert not (target.parent / "diagnostics.json").exists()
    assert not (path_ds_unprov / "diagnostics.json").exists()
