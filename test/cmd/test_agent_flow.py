"""End-to-end agent-flow demos for the gh#1361 wave-3 commands.

Each test simulates one branch of a typical MCP / AI-agent decision tree
that consumes the unified ``suews`` CLI envelope. The point is to
demonstrate (and lock in) that ``status`` + ``data.recommendations`` +
``data.per_variable`` are sufficient inputs for an agent to chain
``diagnose`` -> ``summarise`` / ``compare`` without ever loading a
DataFrame on its own.

Tests invoke the **dispatcher** ``supy.cmd.suews_cli.cli`` rather than
the per-command Click functions directly, so wiring at the
``suews <subcommand>`` level is exercised here too — the same surface
the future MCP server will call through.
"""

from __future__ import annotations

import json
from pathlib import Path

from click.testing import CliRunner
import numpy as np
import pandas as pd
import pytest

from supy.cmd.suews_cli import cli

pytestmark = pytest.mark.api


def _make_run_dir(
    base: Path,
    name: str,
    *,
    seed: int = 1,
    nan_fraction: float = 0.0,
    n: int = 144,
    bias_offset: float = 0.0,
) -> Path:
    """Synthetic run directory with provenance + parquet output.

    Parameters
    ----------
    base : Path
        Parent tmp_path.
    name : str
        Subdirectory name.
    seed : int
        RNG seed for reproducible per-variable ordering.
    nan_fraction : float
        Fraction of QH samples to set to NaN.
    n : int
        Number of synthetic timesteps (5-min cadence).
    bias_offset : float
        Additive offset applied to QH (used by the comparison demo to
        guarantee a deterministic worst-bias variable).
    """
    run_dir = base / name
    run_dir.mkdir(parents=True, exist_ok=True)

    rng = np.random.default_rng(seed)
    idx = pd.date_range("2024-06-01", periods=n, freq="5min", name="datetime")
    qn = rng.normal(loc=200.0, scale=50.0, size=n)
    qh = 0.4 * qn + rng.normal(scale=2.0, size=n) + bias_offset
    qe = 0.3 * qn + rng.normal(scale=2.0, size=n)
    qs = 0.2 * qn + rng.normal(scale=2.0, size=n)
    qf = 0.1 * qn + rng.normal(scale=2.0, size=n)

    if nan_fraction > 0.0:
        n_nan = int(n * nan_fraction)
        qh[:n_nan] = np.nan

    df = pd.DataFrame({"QN": qn, "QH": qh, "QE": qe, "QS": qs, "QF": qf}, index=idx)
    df.to_parquet(run_dir / "df_output.parquet")
    (run_dir / "provenance.json").write_text(
        json.dumps({"command": "suews run --format json"}), encoding="utf-8"
    )
    return run_dir


def _invoke(args: list[str]) -> dict:
    """Invoke ``suews`` dispatcher and return the parsed envelope."""
    runner = CliRunner()
    result = runner.invoke(cli, args)
    assert result.stdout, f"expected envelope on stdout, got: {result.output!r}"
    return json.loads(result.stdout)


def test_demo_1_happy_path_chains_diagnose_to_summarise(tmp_path: Path) -> None:
    """Agent step 1: diagnose passes -> step 2: summarise the same run."""
    run_dir = _make_run_dir(tmp_path, "happy")

    # Step 1 — triage.
    diag = _invoke(["diagnose", str(run_dir), "--format", "json"])
    assert diag["status"] in {"success", "warning"}
    assert diag["data"]["summary"]["n_fail"] == 0

    # Step 2 — drill into descriptive stats. An agent would only reach
    # this branch if step 1 did not return status="error".
    summary = _invoke(["summarise", str(run_dir), "--format", "json"])
    assert summary["status"] in {"success", "warning"}
    list_names = {var["name"] for var in summary["data"]["variables"]}
    assert {"QH", "QE", "QN"} <= list_names
    for var in summary["data"]["variables"]:
        if var["name"] in {"QH", "QE", "QN"}:
            assert var["mean"] is not None and not np.isnan(var["mean"])


def test_demo_2_warning_branch_exposes_structured_recommendations(
    tmp_path: Path,
) -> None:
    """Warning-severity check must surface a structured recommendation.

    The agent contract: read ``data.recommendations`` to decide the next
    step instead of NL-parsing the human-readable message.
    """
    run_dir = _make_run_dir(tmp_path, "noisy", nan_fraction=0.10)

    diag = _invoke(["diagnose", str(run_dir), "--format", "json"])
    assert diag["status"] == "warning"

    list_names = {check["name"] for check in diag["data"]["checks"]}
    assert "nan_proportion" in list_names

    # The recommendation channel is what the agent reads next.
    list_recs = diag["data"]["recommendations"]
    assert any("forcing-file" in rec for rec in list_recs), (
        "nan_proportion warning must surface the forcing-gap recommendation; "
        f"got {list_recs!r}"
    )


def test_demo_3_error_short_circuits_agent(tmp_path: Path) -> None:
    """An empty run dir yields status=error from BOTH diagnose and summarise.

    Encodes the agent's "if status=error, stop" rule: the contract is
    consistent across post-run commands so the agent never falls through
    to a downstream command when an upstream one already failed.
    """
    empty_dir = tmp_path / "empty"
    empty_dir.mkdir(parents=True)

    diag = _invoke(["diagnose", str(empty_dir), "--format", "json"])
    assert diag["status"] == "error"
    assert diag["errors"]

    summary = _invoke(["summarise", str(empty_dir), "--format", "json"])
    assert summary["status"] == "error"
    assert summary["errors"]


def test_demo_4_compare_drives_drill_down_into_summarise(tmp_path: Path) -> None:
    """Agent picks the variable with worst bias from compare, drills into it.

    The bias is engineered (``bias_offset=20`` on QH for run B only) so
    the worst-biased variable is deterministically QH; the test then
    asserts the agent's drill-down returns exactly QH from summarise.
    """
    run_a = _make_run_dir(tmp_path, "runA", seed=1)
    # Identical seed for runB so QE/QN match exactly between the two runs;
    # the bias_offset on QH guarantees QH is the worst variable.
    run_b = _make_run_dir(tmp_path, "runB", seed=1, bias_offset=20.0)

    cmp = _invoke([
        "compare",
        str(run_a),
        str(run_b),
        "--format",
        "json",
    ])
    assert cmp["status"] in {"success", "warning"}

    per_var = cmp["data"]["per_variable"]
    assert {"QH", "QE", "QN"} <= set(per_var.keys())

    # Agent picks the worst-biased variable (deterministic given fixed seeds).
    list_ranked = sorted(
        per_var.items(),
        key=lambda kv: (-abs(kv[1]["bias"]), kv[0]),
    )
    name_worst = list_ranked[0][0]
    assert name_worst == "QH", (
        "expected QH to be the worst-biased variable given the engineered "
        "bias_offset, got ordering %r" % [k for k, _ in list_ranked]
    )

    # Agent drills into the worst variable on the scenario run.
    summary = _invoke([
        "summarise",
        str(run_b),
        "--variables",
        name_worst,
        "--format",
        "json",
    ])
    assert summary["status"] in {"success", "warning"}
    list_names = [var["name"] for var in summary["data"]["variables"]]
    assert list_names == [name_worst]
