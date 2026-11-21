"""Python reader statements share one process, warnings and failure boundaries."""

import os
from pathlib import Path
import sys

import pytest

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from scripts.suews.guide_journeys.extractor import Document  # noqa: E402
from scripts.suews.guide_journeys.runner import run_steps  # noqa: E402

pytestmark = pytest.mark.api


def specs_from_fixture():
    """Use the actual page statements, without a venv or package installation."""
    text = (ROOT / "test/fixtures/guide_journeys/python_page.rst").read_text(
        encoding="utf-8"
    )
    document = Document(text, "Small reader journey")
    return [
        {
            "id": f"s{i}",
            "kind": "documented",
            "span_type": "python",
            "command": s.text,
            "source_lines": [s.first, s.last],
            "source_path": "python_page.rst",
            "promises": [],
            "consumes": [],
        }
        for i, s in enumerate(document.statements(document.anchors["fixture/python"]))
    ]


def test_python_namespace_and_warning_capture(tmp_path):
    """The final statement reads earlier state and only its emitter has a warning."""
    records, _ = run_steps(specs_from_fixture(), tmp_path, dict(os.environ), {})
    assert all(r["status"] == "passed" for r in records)
    assert records[-1]["stdout"] == "6\n"
    assert [bool(r["warnings"]) for r in records] == [False, False, False, True, False]
    assert records[3]["warnings"][0]["lineno"] == records[3]["source_lines"][0]
    assert len({r["pid"] for r in records}) == 1


def test_failed_python_promise_stops_next_statement(tmp_path):
    """A successful statement with a failed promise still stops the consumer."""
    specs = specs_from_fixture()
    specs[2]["promises"] = [
        {"kind": "diagnostic", "stream": "stdout", "pattern": "not printed"}
    ]
    records, _ = run_steps(specs, tmp_path, dict(os.environ), {})
    assert records[2]["exit_status"] == 0
    assert records[2]["status"] == "failed"
    assert all(r["status"] == "unrun" for r in records[3:])


def test_python_exception_preserves_source_location(tmp_path):
    """The traceback names the source page and its original line."""
    specs = specs_from_fixture()
    specs[0].update(command="raise ValueError('reader failure')", source_lines=[37, 37])
    records, _ = run_steps(specs, tmp_path, dict(os.environ), {})
    assert records[0]["exit_status"] == 1
    assert 'File "python_page.rst", line 37' in records[0]["stderr"]
    assert records[1]["status"] == "unrun"
