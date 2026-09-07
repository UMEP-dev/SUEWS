"""Unit tests for ``supy.diagnostics`` check helpers."""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from supy.diagnostics import (
    check_energy_balance_closure,
    check_nan_proportion,
    check_output_files_present,
    check_provenance_present,
    check_run,
)

pytestmark = pytest.mark.api


def _suews_columns(names: list[str]) -> pd.MultiIndex:
    return pd.MultiIndex.from_product([["SUEWS"], names], names=["group", "var"])


def _make_run_dir(
    tmp_path: Path,
    *,
    with_provenance: bool = True,
    with_output: bool = True,
    nan_fraction: float = 0.0,
    closure_offset: float = 0.0,
    n: int = 96,
) -> Path:
    """Construct a synthetic run directory with controlled noise.

    Parameters
    ----------
    tmp_path : Path
        Test temp directory.
    with_provenance : bool
        Write provenance.json sidecar.
    with_output : bool
        Write df_output.csv.
    nan_fraction : float
        Inject this fraction of NaNs into QH.
    closure_offset : float
        Multiply QH by ``(1 + closure_offset)`` so the energy balance
        residual is non-trivial when nonzero.
    n : int
        Number of synthetic timesteps.
    """
    run_dir = tmp_path / "run"
    run_dir.mkdir(parents=True, exist_ok=True)

    rng = np.random.default_rng(seed=1)
    qn = rng.normal(loc=200.0, scale=10.0, size=n)
    # SUEWS identity: QN + QF = QH + QE + QS. With QF = 0.1 QN the sinks
    # must sum to 1.1 QN for the balance to close.
    qf = 0.1 * qn
    qh = 0.5 * qn
    qe = 0.4 * qn
    qs = 0.2 * qn

    if closure_offset:
        qh *= 1.0 + closure_offset

    if nan_fraction > 0.0:
        n_nan = int(n * nan_fraction)
        qh[:n_nan] = np.nan

    if with_output:
        df = pd.DataFrame({"QN": qn, "QH": qh, "QE": qe, "QS": qs, "QF": qf})
        df.to_csv(run_dir / "df_output.csv", index=False)

    if with_provenance:
        (run_dir / "provenance.json").write_text(
            json.dumps({"command": "test"}), encoding="utf-8"
        )

    return run_dir


def test_check_provenance_present_pass(tmp_path: Path) -> None:
    run_dir = _make_run_dir(tmp_path, with_provenance=True)
    res = check_provenance_present(run_dir)
    assert res.passed
    assert res.severity == "pass"
    assert res.name == "provenance_present"


def test_check_provenance_present_warning(tmp_path: Path) -> None:
    run_dir = _make_run_dir(tmp_path, with_provenance=False)
    res = check_provenance_present(run_dir)
    assert not res.passed
    assert res.severity == "warning"


def test_check_output_files_present_pass(tmp_path: Path) -> None:
    run_dir = _make_run_dir(tmp_path, with_output=True)
    res = check_output_files_present(run_dir)
    assert res.passed
    assert res.severity == "pass"
    assert res.details["files"]


def test_check_output_files_present_fail(tmp_path: Path) -> None:
    empty = tmp_path / "empty"
    empty.mkdir(parents=True)
    res = check_output_files_present(empty)
    assert not res.passed
    assert res.severity == "fail"


def test_check_nan_proportion_pass(tmp_path: Path) -> None:
    run_dir = _make_run_dir(tmp_path, nan_fraction=0.0)
    res = check_nan_proportion(run_dir)
    assert res.passed
    assert res.severity == "pass"


def test_check_nan_proportion_warning(tmp_path: Path) -> None:
    run_dir = _make_run_dir(tmp_path, nan_fraction=0.10)
    res = check_nan_proportion(run_dir)
    assert not res.passed
    assert res.severity == "warning"


def test_check_energy_balance_closure_pass(tmp_path: Path) -> None:
    run_dir = _make_run_dir(tmp_path, closure_offset=0.0)
    res = check_energy_balance_closure(run_dir)
    assert res.passed
    assert res.severity == "pass"


def test_check_energy_balance_closure_warning(tmp_path: Path) -> None:
    # 80% offset on QH inflates the residual well beyond the 10% gate.
    run_dir = _make_run_dir(tmp_path, closure_offset=0.8)
    res = check_energy_balance_closure(run_dir)
    assert not res.passed
    assert res.severity == "warning"


def _write_csv(path: Path, **columns: object) -> None:
    pd.DataFrame(columns).to_csv(path, index=False)


def _closure_dir(tmp_path: Path, name: str = "run") -> Path:
    run_dir = tmp_path / name
    run_dir.mkdir(parents=True)
    return run_dir


def test_closure_passes_when_identity_holds(tmp_path: Path) -> None:
    """QN + QF = QH + QE + QS must pass (QF is a source, not a sink)."""
    run_dir = _closure_dir(tmp_path)
    n = 8
    _write_csv(
        run_dir / "df_output.csv",
        QN=[200.0] * n,
        QF=[20.0] * n,
        QH=[100.0] * n,
        QE=[80.0] * n,
        QS=[40.0] * n,
    )
    res = check_energy_balance_closure(run_dir)
    assert res.passed, res.message
    assert res.details["ratio_mean"] == pytest.approx(0.0)
    assert res.details["n_rows_evaluated"] == n
    assert res.details["terms_missing"] == ["QM", "QMFreeze", "QMRain"]


def test_closure_warns_when_identity_broken(tmp_path: Path) -> None:
    """Sinks summing to QN alone (220 vs 180) is a 20% residual."""
    run_dir = _closure_dir(tmp_path)
    n = 8
    _write_csv(
        run_dir / "df_output.csv",
        QN=[200.0] * n,
        QF=[20.0] * n,
        QH=[80.0] * n,
        QE=[60.0] * n,
        QS=[40.0] * n,
    )
    res = check_energy_balance_closure(run_dir)
    assert not res.passed
    assert res.severity == "warning"
    assert res.details["ratio_mean"] == pytest.approx(0.2)


def test_closure_includes_snow_terms(tmp_path: Path) -> None:
    """QN + QF + QMRain = QH + QE + QS + QM + QMFreeze closes with snow."""
    run_dir = _closure_dir(tmp_path)
    n = 6
    _write_csv(
        run_dir / "df_output.csv",
        QN=[200.0] * n,
        QF=[20.0] * n,
        QMRain=[5.0] * n,
        QH=[100.0] * n,
        QE=[60.0] * n,
        QS=[40.0] * n,
        QM=[15.0] * n,
        QMFreeze=[10.0] * n,
    )
    res = check_energy_balance_closure(run_dir)
    assert res.passed, res.message
    assert res.details["terms_missing"] == []

    # Dropping the snow sinks leaves 25 W m-2 (12.5%) unaccounted for.
    _write_csv(
        run_dir / "df_output.csv",
        QN=[200.0] * n,
        QF=[20.0] * n,
        QMRain=[5.0] * n,
        QH=[100.0] * n,
        QE=[60.0] * n,
        QS=[40.0] * n,
    )
    res = check_energy_balance_closure(run_dir)
    assert not res.passed
    assert res.details["ratio_mean"] == pytest.approx(0.125)
    assert res.details["terms_missing"] == ["QM", "QMFreeze"]


def test_closure_missing_optional_column_does_not_raise(tmp_path: Path) -> None:
    run_dir = _closure_dir(tmp_path)
    n = 4
    _write_csv(
        run_dir / "df_output.csv",
        QN=[200.0] * n,
        QH=[100.0] * n,
        QE=[60.0] * n,
        QS=[40.0] * n,
    )
    res = check_energy_balance_closure(run_dir)
    assert res.passed, res.message
    assert "QF" in res.details["terms_missing"]
    assert "QF" in res.message


def test_closure_missing_required_column_fails(tmp_path: Path) -> None:
    run_dir = _closure_dir(tmp_path)
    n = 4
    _write_csv(
        run_dir / "df_output.csv", QN=[200.0] * n, QH=[100.0] * n, QE=[100.0] * n
    )
    res = check_energy_balance_closure(run_dir)
    assert res.severity == "fail"
    assert res.details["missing"] == ["QS"]


def test_sentinel_values_are_treated_as_missing(tmp_path: Path) -> None:
    run_dir = _closure_dir(tmp_path)
    n = 10
    qh = [100.0] * n
    qh[:4] = [-999.0] * 4
    _write_csv(
        run_dir / "df_output.csv",
        QN=[200.0] * n,
        QF=[20.0] * n,
        QH=qh,
        QE=[80.0] * n,
        QS=[40.0] * n,
    )
    res_nan = check_nan_proportion(run_dir)
    assert not res_nan.passed
    assert res_nan.details["fractions"]["df_output.csv"]["QH"] == pytest.approx(0.4)

    res_eb = check_energy_balance_closure(run_dir)
    assert res_eb.passed, res_eb.message
    assert res_eb.details["n_rows_evaluated"] == 6
    assert res_eb.details["n_rows_skipped_nonfinite"] == 4


def test_all_rows_nonfinite_gives_warning(tmp_path: Path) -> None:
    run_dir = _closure_dir(tmp_path)
    n = 4
    _write_csv(
        run_dir / "df_output.csv",
        QN=[200.0] * n,
        QF=[20.0] * n,
        QH=[np.nan] * n,
        QE=[80.0] * n,
        QS=[40.0] * n,
    )
    res = check_energy_balance_closure(run_dir)
    assert res.severity == "warning"
    assert res.details["n_rows_evaluated"] == 0


def test_every_csv_partition_is_inspected(tmp_path: Path) -> None:
    """A second CSV whose fluxes are all NaN must not hide behind the first."""
    run_dir = _closure_dir(tmp_path)
    n = 8
    _write_csv(
        run_dir / "df_output_a.csv",
        QN=[200.0] * n,
        QF=[20.0] * n,
        QH=[100.0] * n,
        QE=[80.0] * n,
        QS=[40.0] * n,
    )
    _write_csv(
        run_dir / "df_output_z.csv",
        QN=[200.0] * n,
        QF=[20.0] * n,
        QH=[np.nan] * n,
        QE=[np.nan] * n,
        QS=[40.0] * n,
    )
    res_nan = check_nan_proportion(run_dir)
    assert not res_nan.passed
    assert res_nan.details["n_partitions"] == 2
    assert res_nan.details["fractions"]["df_output_z.csv"]["QH"] == pytest.approx(1.0)
    assert res_nan.details["fractions"]["df_output_a.csv"]["QH"] == pytest.approx(0.0)
    assert "df_output_z.csv:QH" in res_nan.message

    res_eb = check_energy_balance_closure(run_dir)
    assert res_eb.details["n_partitions"] == 2
    assert res_eb.details["n_rows"] == 2 * n
    assert res_eb.details["n_rows_skipped_nonfinite"] == n


def test_every_grid_in_parquet_is_a_partition(tmp_path: Path) -> None:
    run_dir = _closure_dir(tmp_path)
    n = 6
    idx = pd.MultiIndex.from_product(
        [[1, 2], pd.date_range("2012-01-01", periods=n, freq="h")],
        names=["grid", "datetime"],
    )
    qn = np.full(2 * n, 200.0)
    qh = np.full(2 * n, 100.0)
    qh[n:] = np.nan  # grid 2 has no QH
    df = pd.DataFrame(
        np.column_stack([qn, qh, 0.4 * qn, 0.2 * qn, 0.1 * qn]),
        index=idx,
        columns=_suews_columns(["QN", "QH", "QE", "QS", "QF"]),
    )
    df.to_parquet(run_dir / "SUEWS_output.parquet")

    res = check_nan_proportion(run_dir)
    assert not res.passed
    assert res.details["n_partitions"] == 2
    assert res.details["fractions"]["SUEWS_output.parquet[grid=1]"][
        "QH"
    ] == pytest.approx(0.0)
    assert res.details["fractions"]["SUEWS_output.parquet[grid=2]"][
        "QH"
    ] == pytest.approx(1.0)


def test_multi_group_parquet_uses_core_suews_group(tmp_path: Path) -> None:
    """A variable repeated in another group (ESTM QS) must not break the check."""
    run_dir = _closure_dir(tmp_path)
    n = 6
    qn = np.full(n, 200.0)
    columns = pd.MultiIndex.from_tuples(
        [
            ("SUEWS", "QN"),
            ("SUEWS", "QH"),
            ("SUEWS", "QE"),
            ("SUEWS", "QS"),
            ("SUEWS", "QF"),
            ("ESTM", "QS"),
        ],
        names=["group", "var"],
    )
    df = pd.DataFrame(
        np.column_stack([qn, 0.5 * qn, 0.4 * qn, 0.2 * qn, 0.1 * qn, 5.0 * qn]),
        columns=columns,
    )
    df.to_parquet(run_dir / "SUEWS_output.parquet")

    res = check_energy_balance_closure(run_dir)
    assert res.passed, res.message
    assert res.details["ratio_mean"] == pytest.approx(0.0)


def test_duplicate_output_formats_are_not_double_counted(tmp_path: Path) -> None:
    """The same run saved as parquet and CSV is read once, from parquet."""
    run_dir = _closure_dir(tmp_path)
    n = 8
    qn = np.full(n, 200.0)
    df = pd.DataFrame(
        np.column_stack([qn, 0.5 * qn, 0.4 * qn, 0.2 * qn, 0.1 * qn]),
        columns=_suews_columns(["QN", "QH", "QE", "QS", "QF"]),
    )
    df.to_parquet(run_dir / "SUEWS_output.parquet")
    _write_csv(
        run_dir / "df_output.csv",
        QN=qn,
        QH=[np.nan] * n,
        QE=0.4 * qn,
        QS=0.2 * qn,
        QF=0.1 * qn,
    )
    assert check_output_files_present(run_dir).details["files"]

    res_nan = check_nan_proportion(run_dir)
    assert res_nan.passed, res_nan.message
    assert list(res_nan.details["fractions"]) == ["SUEWS_output.parquet"]

    res_eb = check_energy_balance_closure(run_dir)
    assert res_eb.passed, res_eb.message
    assert res_eb.details["n_partitions"] == 1
    assert res_eb.details["n_rows"] == n


def test_legacy_text_output_partitions(tmp_path: Path) -> None:
    """Legacy per-grid text files are all read; -999 is missing."""
    run_dir = _closure_dir(tmp_path)
    header = "Year DOY Hour Min Dectime QN QF QS QH QE"
    rows_ok = "\n".join(
        f"2012 1 {h} 0 1.0 200.0 20.0 40.0 100.0 80.0" for h in range(4)
    )
    rows_bad = "\n".join(
        f"2012 1 {h} 0 1.0 200.0 20.0 40.0 -999.0 -999.0" for h in range(4)
    )
    (run_dir / "Site1_2012_SUEWS_60.txt").write_text(
        f"{header}\n{rows_ok}\n", encoding="utf-8"
    )
    (run_dir / "Site2_2012_SUEWS_60.txt").write_text(
        f"{header}\n{rows_bad}\n", encoding="utf-8"
    )

    res_nan = check_nan_proportion(run_dir)
    assert not res_nan.passed
    assert res_nan.details["n_partitions"] == 2
    assert res_nan.details["fractions"]["Site2_2012_SUEWS_60.txt"][
        "QH"
    ] == pytest.approx(1.0)

    res_eb = check_energy_balance_closure(run_dir)
    assert res_eb.passed, res_eb.message
    assert res_eb.details["n_rows_evaluated"] == 4
    assert res_eb.details["n_rows_skipped_nonfinite"] == 4


def test_check_run_aggregator_returns_all_checks(tmp_path: Path) -> None:
    run_dir = _make_run_dir(tmp_path)
    list_results = check_run(run_dir)
    assert len(list_results) == 4
    list_names = {res.name for res in list_results}
    assert list_names == {
        "provenance_present",
        "output_files_present",
        "nan_proportion",
        "energy_balance_closure",
    }


def test_check_run_reads_real_parquet_output_name_and_columns(tmp_path: Path) -> None:
    run_dir = tmp_path / "parquet-run"
    run_dir.mkdir(parents=True)

    qn = np.full(12, 200.0)
    df = pd.DataFrame(
        np.column_stack([qn, 0.5 * qn, 0.4 * qn, 0.2 * qn, 0.1 * qn]),
        columns=_suews_columns(["QN", "QH", "QE", "QS", "QF"]),
    )
    df.to_parquet(run_dir / "SUEWS_output.parquet")
    (run_dir / "provenance.json").write_text("{}", encoding="utf-8")

    list_results = check_run(run_dir)
    by_name = {res.name: res for res in list_results}

    assert by_name["output_files_present"].passed
    assert by_name["nan_proportion"].passed
    assert by_name["energy_balance_closure"].passed
