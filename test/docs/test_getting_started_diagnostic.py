"""The sample diagnostic shares its observation and rejects missing boundaries."""

from pathlib import Path
import sys
from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest
import yaml

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from scripts.suews import getting_started_diagnostic as diagnostic  # noqa: E402
from scripts.suews.guide_journeys.manifest import load  # noqa: E402

pytestmark = pytest.mark.api
MANIFEST = ROOT / "test/docs/journeys/getting_started.yml"


def test_missing_sample_run(monkeypatch, tmp_path):
    """An otherwise bound manifest fails before starting a reader process."""
    monkeypatch.setattr(
        sys,
        "argv",
        [
            "diagnostic",
            "--revision",
            "unused",
            "--venv",
            str(tmp_path),
            "--output",
            str(tmp_path / "result.json"),
        ],
    )
    monkeypatch.setattr(diagnostic, "read_page", lambda *args: "unused")
    monkeypatch.setattr(diagnostic, "Document", lambda *args: None)
    monkeypatch.setattr(diagnostic, "check", lambda *args: [])
    monkeypatch.setattr(diagnostic, "resolve_steps", lambda *args: [{"id": "other"}])
    with pytest.raises(ValueError, match="requires a 'sample-run' step"):
        diagnostic.main()
    assert not (tmp_path / "result.json").exists()


def test_manifest_observes_diagnostic_residual():
    """The manifest's resolved expression returns the known maximum residual."""
    manifest = load(MANIFEST)
    promise = next(
        promise
        for step in manifest["journeys"][1]["steps"]
        for promise in step["promises"]
        if promise["kind"] == "physical"
    )
    index = pd.MultiIndex.from_product(
        [[1], pd.date_range("2012-01-01", periods=2, freq="5min")],
        names=["grid", "datetime"],
    )
    values = {
        "Rain": [8.0, 1.0],
        "Irr": [1.0, 0.0],
        "Evap": [2.0, 1.0],
        "RO": [3.0, 2.0],
        "TotCh": [1.0, 2.0],
    }
    sim = SimpleNamespace(
        get_variable=lambda name, group: pd.DataFrame(values[name], index=index)
    )
    namespace = {"np": np, "pd": pd, "sim": sim}
    assert eval(promise["expression"], namespace) == pytest.approx(4.0, rel=0, abs=0)
    assert eval(diagnostic.WATER_OBSERVATION, namespace)["residual_mm"][
        "max_abs"
    ] == pytest.approx(4.0, rel=0, abs=0)
    assert promise["tolerance"] is None


@pytest.mark.parametrize("ambiguous", [False, True])
def test_invalid_expression_reference(tmp_path, ambiguous):
    """Unknown references and simultaneous inline expressions fail closed."""
    manifest = yaml.safe_load(MANIFEST.read_text(encoding="utf-8"))
    promise = next(
        promise
        for step in manifest["journeys"][1]["steps"]
        for promise in step["promises"]
        if promise["kind"] == "physical"
    )
    if ambiguous:
        promise["expression"] = "0"
    else:
        promise["expression_ref"] = "unknown"
    path = tmp_path / "manifest.yml"
    path.write_text(yaml.safe_dump(manifest), encoding="utf-8")
    with pytest.raises(ValueError, match="Unknown or ambiguous"):
        load(path)
