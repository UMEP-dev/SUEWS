"""The assembler must turn an index.json into report + tidy CSV."""

import csv
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from assemble_drift import assemble_drift  # noqa: E402

INDEX = {
    "site": "TEST",
    "versions": {
        "2025.1.1": {
            "fingerprint": "aaa",
            "full": {"QH": {"MAE": 10.0, "MBE": 1.0, "n": 100}},
        },
        "2025.2.2": {
            "fingerprint": "bbb",
            "full": {"QH": {"MAE": 12.5, "MBE": 0.4, "n": 100}},
        },
    },
}


def test_assemble_writes_report_and_csv(tmp_path):
    (tmp_path / "index.json").write_text(json.dumps(INDEX), encoding="utf-8")
    report = assemble_drift(results_dir=tmp_path, baseline_tag="2025.1.1")

    report_path = tmp_path / "drift_report.json"
    csv_path = tmp_path / "drift_table.csv"
    assert report_path.exists() and csv_path.exists()

    saved = json.loads(report_path.read_text(encoding="utf-8"))
    assert saved["site"] == "TEST"
    assert saved["baseline_tag"] == "2025.1.1"
    assert saved["records"], "expected at least one drift record"

    rows = list(csv.DictReader(csv_path.open(encoding="utf-8")))
    header = ["from_tag", "to_tag", "framing", "flux", "interval", "bucket", "metric", "value"]
    assert list(rows[0].keys()) == header
    metrics = {r["metric"] for r in rows}
    assert metrics == {"delta_mae", "delta_mbe"}


def test_assemble_is_idempotent(tmp_path):
    (tmp_path / "index.json").write_text(json.dumps(INDEX), encoding="utf-8")
    assemble_drift(results_dir=tmp_path, baseline_tag="2025.1.1")
    first = (tmp_path / "drift_table.csv").read_bytes()
    assemble_drift(results_dir=tmp_path, baseline_tag="2025.1.1")
    second = (tmp_path / "drift_table.csv").read_bytes()
    assert first == second
