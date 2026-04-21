"""GH-1292 PR3: smoke test for the moisture-aware LAI parameter-sweep tool.

This test guards the sweep CLI's public contract without paying the full
cost of running SUEWS for many parameter values: it runs a single-value
sweep (equivalent to one extra SUEWS invocation past the baseline), and
asserts that the expected JSON artefact is produced with the documented
schema. The test is tagged slow because even one extra sample run takes
roughly a minute.
"""

from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

import pytest

pytestmark = pytest.mark.api

REPO_ROOT = Path(__file__).resolve().parents[1]
SWEEP_SCRIPT = REPO_ROOT / "scripts" / "verify" / "moisture_phenology_sweep.py"


@pytest.mark.slow
def test_sweep_cli_emits_expected_json() -> None:
    out_path = REPO_ROOT / ".context" / "gh1292" / "London-test" / "sweep_w_opt.json"
    if out_path.exists():
        out_path.unlink()

    result = subprocess.run(
        [
            sys.executable,
            str(SWEEP_SCRIPT),
            "--param",
            "w_opt",
            "--values",
            "0.40",
            "--site",
            "London-test",
            "--dry-start",
        ],
        cwd=str(REPO_ROOT),
        check=False,
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, f"sweep CLI failed:\n{result.stderr}"
    assert out_path.exists(), f"expected {out_path} to be written"

    payload = json.loads(out_path.read_text(encoding="utf-8"))
    assert payload["parameter"] == "w_opt"
    assert isinstance(payload["baseline_mean_lai"], (int, float))
    assert payload["points"] and payload["points"][0]["value"] == 0.40
    required_keys = {"value", "mean_lai", "rmse_vs_baseline", "amplitude", "green_up_doy"}
    assert required_keys.issubset(payload["points"][0].keys())
