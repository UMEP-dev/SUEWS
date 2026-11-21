"""Failures must stop later consumers and retain a complete JSON record."""

import json
import os
import sys

import pytest

pytest.importorskip(
    "docutils",
    reason="The runner's source parser needs the optional docs dependency docutils",
)

from scripts.suews.guide_journeys.record import new_record, write
from scripts.suews.guide_journeys.runner import run_steps

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
