"""Unit tests for ``supy.diagnostics`` check helpers."""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

pytestmark = pytest.mark.api


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
        Multiply QH/QE/QS/QF sum by ``(1 + closure_offset)`` so the energy
        balance residual is non-trivial when nonzero.
    n : int
        Number of synthetic timesteps.
    """
    run_dir = tmp_path / "run"
    run_dir.mkdir(parents=True, exist_ok=True)

    rng = np.random.default_rng(seed=1)
    qn = rng.normal(loc=200.0, scale=10.0, size=n)
    qh = 0.4 * qn
    qe = 0.3 * qn
    qs = 0.2 * qn
    qf = 0.1 * qn

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
    from supy.diagnostics import check_provenance_present

    run_dir = _make_run_dir(tmp_path, with_provenance=True)
    res = check_provenance_present(run_dir)
    assert res.passed
    assert res.severity == "pass"
    assert res.name == "provenance_present"


def test_check_provenance_present_warning(tmp_path: Path) -> None:
    from supy.diagnostics import check_provenance_present

    run_dir = _make_run_dir(tmp_path, with_provenance=False)
    res = check_provenance_present(run_dir)
    assert not res.passed
    assert res.severity == "warning"


def test_check_output_files_present_pass(tmp_path: Path) -> None:
    from supy.diagnostics import check_output_files_present

    run_dir = _make_run_dir(tmp_path, with_output=True)
    res = check_output_files_present(run_dir)
    assert res.passed
    assert res.severity == "pass"
    assert res.details["files"]


def test_check_output_files_present_fail(tmp_path: Path) -> None:
    from supy.diagnostics import check_output_files_present

    empty = tmp_path / "empty"
    empty.mkdir(parents=True)
    res = check_output_files_present(empty)
    assert not res.passed
    assert res.severity == "fail"


def test_check_nan_proportion_pass(tmp_path: Path) -> None:
    from supy.diagnostics import check_nan_proportion

    run_dir = _make_run_dir(tmp_path, nan_fraction=0.0)
    res = check_nan_proportion(run_dir)
    assert res.passed
    assert res.severity == "pass"


def test_check_nan_proportion_warning(tmp_path: Path) -> None:
    from supy.diagnostics import check_nan_proportion

    run_dir = _make_run_dir(tmp_path, nan_fraction=0.10)
    res = check_nan_proportion(run_dir)
    assert not res.passed
    assert res.severity == "warning"


def test_check_energy_balance_closure_pass(tmp_path: Path) -> None:
    from supy.diagnostics import check_energy_balance_closure

    run_dir = _make_run_dir(tmp_path, closure_offset=0.0)
    res = check_energy_balance_closure(run_dir)
    assert res.passed
    assert res.severity == "pass"


def test_check_energy_balance_closure_warning(tmp_path: Path) -> None:
    from supy.diagnostics import check_energy_balance_closure

    # 80% offset on QH inflates the residual well beyond the 10% gate.
    run_dir = _make_run_dir(tmp_path, closure_offset=0.8)
    res = check_energy_balance_closure(run_dir)
    assert not res.passed
    assert res.severity == "warning"


def test_check_run_aggregator_returns_all_checks(tmp_path: Path) -> None:
    from supy.diagnostics import check_run

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
