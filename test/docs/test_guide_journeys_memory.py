"""State promises observe the reader namespace and stop subsequent consumers."""

import os
from pathlib import Path
import sys

import pytest

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from scripts.suews.guide_journeys.runner import run_steps  # noqa: E402

pytestmark = pytest.mark.api


def run_promised(tmp_path, promise):
    """Execute tiny source statements without installing anything."""
    specs = [
        {
            "id": "producer",
            "kind": "documented",
            "span_type": "python",
            "command": "total = 6",
            "source_lines": [12, 12],
            "promises": [promise],
        },
        {
            "id": "consumer",
            "kind": "documented",
            "span_type": "python",
            "command": "print(total)",
            "source_lines": [13, 13],
            "promises": [],
        },
    ]
    return run_steps(specs, tmp_path, dict(os.environ), {})[0]


@pytest.mark.parametrize("expected", [6, 7])
def test_memory_expected_outcome_and_stop(tmp_path, expected):
    """A false memory promise prevents the next documented statement."""
    records = run_promised(
        tmp_path, {"kind": "memory", "expression": "total", "expected": expected}
    )
    assert records[0]["promises"][0]["observed"]["value"] == 6
    assert records[0]["promises"][0]["passed"] == (expected == 6)
    assert records[1]["status"] == ("passed" if expected == 6 else "unrun")


@pytest.mark.parametrize("expression", ["missing_name", "1 / 0", "total = 7"])
def test_memory_errors_are_failed_observations(tmp_path, expression):
    """Missing state, expression errors and replacement statements cannot pass."""
    records = run_promised(
        tmp_path, {"kind": "memory", "expression": expression, "expected": 6}
    )
    assert not records[0]["promises"][0]["passed"]
    assert records[1]["status"] == "unrun"


@pytest.mark.parametrize("residual,passed", [(0.125, True), (0.5, False)])
def test_physical_uses_manifest_tolerance(tmp_path, residual, passed):
    """The number below is a TEST PLACEHOLDER, not a physical acceptance limit."""
    placeholder_tolerance = 0.25
    records = run_promised(
        tmp_path,
        {
            "kind": "physical",
            "check": "absolute_residual",
            "expression": f"total - 6 + {residual}",
            "tolerance": placeholder_tolerance,
        },
    )
    assert records[0]["promises"][0]["passed"] is passed
    assert records[1]["status"] == ("passed" if passed else "unrun")


@pytest.mark.parametrize(
    "changes",
    [
        {},
        {"tolerance": -1},
        {"tolerance": float("nan")},
        {"tolerance": float("inf")},
        {"tolerance": True},
        {"tolerance": 0, "check": "unknown"},
    ],
)
def test_physical_has_no_default_or_invalid_tolerance(tmp_path, changes):
    """Unknown checks and absent or invalid tolerances fail closed."""
    promise = {
        "kind": "physical",
        "check": "absolute_residual",
        "expression": "total",
    } | changes
    records = run_promised(tmp_path, promise)
    assert not records[0]["promises"][0]["passed"]
    assert records[1]["status"] == "unrun"
