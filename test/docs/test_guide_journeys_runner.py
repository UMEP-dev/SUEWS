"""Failures must stop later consumers and retain a complete JSON record."""

import json
import os
from pathlib import Path
import sys

import pytest

pytest.importorskip(
    "docutils",
    reason="The runner's source parser needs the optional docs dependency docutils",
)

# Script packages are not installed in the wheel; support the pytest console entry point.
_REPO_ROOT = Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from scripts.suews.guide_journeys.record import (  # noqa: E402 - source script imports follow path setup
    new_record,
    write,
)
from scripts.suews.guide_journeys.runner import run_steps  # noqa: E402

pytestmark = pytest.mark.api


def test_failed_creation_stops_consumer_and_records_lines(tmp_path):
    """Even a real successful process cannot certify a file it did not create."""
    specs = [
        {
            "id": "producer",
            "kind": "documented",
            "source_lines": [261, 261],
            "command": f"{sys.executable} -I --version",
            "consumes": [],
            "promises": [{"kind": "creates", "path": "missing.yml"}],
        },
        {
            "id": "consumer",
            "kind": "documented",
            "source_lines": [262, 262],
            "command": f"{sys.executable} -I --version",
            "consumes": ["missing.yml"],
            "promises": [],
        },
    ]
    steps, consumer = run_steps(specs, tmp_path, dict(os.environ), {})
    assert steps[0]["exit_status"] == 0
    assert steps[0]["status"] == "failed"
    assert steps[1]["status"] == "unrun"
    assert steps[1]["exit_status"] is None
    assert consumer == "consumer"
    record = new_record(
        {"guide": "guide.rst", "envelope": {"reviewed_revision": "reviewed"}},
        "current",
        {"id": "missing-output", "choices": ["clean"]},
        "Guide text",
    )
    record.update(status="failed", steps=steps, first_unrun_consumer=consumer)
    output = tmp_path / "failure.json"
    write(output, record)
    saved = json.loads(output.read_text(encoding="utf-8"))
    assert saved["steps"][1]["source_lines"] == [262, 262]
    assert saved["guide_text"] == "Guide text"
    assert {
        "input_sha256s",
        "wheel_sha256",
        "supy_version",
        "environment_preflight",
        "package_preflight",
        "wheelhouse",
        "installed_help",
        "revision_run",
        "revision_reviewed",
        "choice_ids",
    } <= saved.keys()
    assert {
        "before",
        "after",
        "stdout",
        "stderr",
        "warnings",
        "exit_status",
        "promises",
    } <= saved["steps"][0].keys()


def test_counterexample_requires_planted_bytes_across_migration(tmp_path):
    """A skip must preserve the pinned stale file, not merely leave any file."""
    from scripts.suews.guide_journeys.oracle import snapshot
    from scripts.suews.guide_journeys.replays import assert_counterexample

    output = tmp_path / "stale.yml"
    output.write_text("planted\n", encoding="utf-8")
    before = snapshot(tmp_path)
    expected = {"step": "migrate", "promise": "creates", "consumer": "validate"}
    record = {
        "planted_file": {"path": output.name, "sha256": before[output.name]["sha256"]},
        "steps": [
            {
                "id": "migrate",
                "status": "failed",
                "exit_status": 0,
                "before": before,
                "after": before,
                "promises": [{"passed": False, "expected": {"kind": "creates"}}],
            },
            {"id": "validate", "status": "unrun"},
        ],
        "path_outcomes": [],
        "first_unrun_consumer": "validate",
    }
    assert assert_counterexample(record, expected)
    output.write_text("replacement\n", encoding="utf-8")
    record["steps"][0]["after"] = snapshot(tmp_path)
    assert not assert_counterexample(record, expected)


@pytest.mark.parametrize("failure", ["step", "wheel", "history"])
def test_setup_errors_preserve_failure_record(tmp_path, failure):
    """Binding, wheel and historical setup errors retain run and reviewed revisions."""
    import argparse
    from scripts.suews.guide_journeys.extractor import Document
    from scripts.suews.guide_journeys.manifest import load
    from scripts.suews.guide_journeys.runner import run_journey

    manifest = load(_REPO_ROOT / "test/docs/journeys/transition_guide.yml")
    text = (_REPO_ROOT / manifest["guide"]).read_text(encoding="utf-8")
    document = Document(text, manifest["title"])
    journey = manifest["journeys"][0]
    if failure == "step":
        journey["steps"][0]["id"] = "no-binding"
    if failure == "history":
        journey = next(j for j in manifest["journeys"] if j.get("historical"))
        journey["steps"][0]["command_index"] = 999
    args = argparse.Namespace(
        revision="new-run", wheel=tmp_path / "missing.whl", output=tmp_path
    )
    record = run_journey(_REPO_ROOT, manifest, document, journey, args)
    saved = json.loads(
        (tmp_path / (journey["id"] + ".json")).read_text(encoding="utf-8")
    )
    assert saved == record
    assert saved["status"] == "failed"
    assert not saved["accepted"]
    assert saved["revision_run"] == "new-run"
    assert saved["revision_reviewed"] == (
        manifest["historical"]["revision"]
        if failure == "history"
        else manifest["envelope"]["reviewed_revision"]
    )
    assert saved["failure"].startswith(
        {"step": "KeyError", "wheel": "FileNotFoundError", "history": "IndexError"}[
            failure
        ]
    )


def test_malformed_manifest_writes_setup_record(tmp_path):
    """CLI setup errors produce JSON evidence instead of a bare traceback."""
    import subprocess

    manifest = tmp_path / "broken.yml"
    manifest.write_text("format: [\n", encoding="utf-8")
    output = tmp_path / "records"
    process = subprocess.run(
        [
            sys.executable,
            "-m",
            "scripts.suews.guide_journeys.runner",
            "--manifest",
            str(manifest),
            "--revision",
            "new-run",
            "--wheel",
            str(tmp_path / "missing.whl"),
            "--wheelhouse",
            str(tmp_path),
            "--output",
            str(output),
        ],
        cwd=_REPO_ROOT,
        capture_output=True,
        text=True,
        check=False,
    )
    assert process.returncode == 1
    assert "Traceback" not in process.stderr
    record = json.loads((output / "setup.json").read_text(encoding="utf-8"))
    assert record["status"] == "failed"
    assert record["revision_run"] == "new-run"
