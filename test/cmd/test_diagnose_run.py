"""Tests for ``suews diagnose``."""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest
from click.testing import CliRunner

pytestmark = pytest.mark.api


def _build_minimal_run_dir(tmp_path: Path) -> Path:
    """Create a small synthetic run dir that satisfies the Phase-1 checks."""
    run_dir = tmp_path / "fake_run"
    run_dir.mkdir(parents=True)

    n = 96  # one synthetic day at 15-min steps
    rng = np.random.default_rng(seed=2026)
    qn = rng.normal(loc=200.0, scale=50.0, size=n)
    # Construct fluxes that close the energy balance to within ~5%.
    qh = 0.4 * qn + rng.normal(scale=2.0, size=n)
    qe = 0.3 * qn + rng.normal(scale=2.0, size=n)
    qs = 0.2 * qn + rng.normal(scale=2.0, size=n)
    qf = 0.1 * qn + rng.normal(scale=2.0, size=n)
    df = pd.DataFrame({"QN": qn, "QH": qh, "QE": qe, "QS": qs, "QF": qf})
    df.to_csv(run_dir / "df_output.csv", index=False)

    (run_dir / "provenance.json").write_text(
        json.dumps({"command": "suews run --format json"}, indent=2),
        encoding="utf-8",
    )
    return run_dir


def test_diagnose_returns_non_empty_check_list(tmp_path: Path) -> None:
    """A synthetic run dir must yield a non-empty checks payload."""
    from supy.cmd.diagnose_run import diagnose_run_cmd

    run_dir = _build_minimal_run_dir(tmp_path)

    runner = CliRunner()
    result = runner.invoke(
        diagnose_run_cmd, [str(run_dir), "--format", "json"]
    )
    assert result.exit_code == 0, result.output
    envelope = json.loads(result.stdout)

    data = envelope["data"]
    assert data["run_dir"] == str(run_dir)
    assert isinstance(data["checks"], list)
    assert len(data["checks"]) >= 4
    list_names = {check["name"] for check in data["checks"]}
    for required in (
        "provenance_present",
        "output_files_present",
        "nan_proportion",
        "energy_balance_closure",
    ):
        assert required in list_names

    summary = data["summary"]
    assert summary["n_pass"] + summary["n_warn"] + summary["n_fail"] == len(data["checks"])


def test_diagnose_missing_output_returns_error(tmp_path: Path) -> None:
    """A run dir with no output files must surface a fail-severity check."""
    from supy.cmd.diagnose_run import diagnose_run_cmd

    empty_dir = tmp_path / "empty"
    empty_dir.mkdir(parents=True)

    runner = CliRunner()
    result = runner.invoke(
        diagnose_run_cmd, [str(empty_dir), "--format", "json"]
    )
    # Hard fail (no output) maps to envelope error per the CLI contract.
    assert result.exit_code == 0  # diagnose itself does not exit non-zero
    envelope = json.loads(result.stdout)
    assert envelope["status"] == "error"
    assert envelope["errors"]


def test_diagnose_text_mode(tmp_path: Path) -> None:
    """Text mode prints a human-readable summary."""
    from supy.cmd.diagnose_run import diagnose_run_cmd

    run_dir = _build_minimal_run_dir(tmp_path)

    runner = CliRunner()
    result = runner.invoke(diagnose_run_cmd, [str(run_dir)])
    assert result.exit_code == 0, result.output
    assert "Checks:" in result.output
