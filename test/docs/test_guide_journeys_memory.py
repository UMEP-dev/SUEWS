"""State promises observe the reader namespace and stop subsequent consumers."""

import argparse
import os
from pathlib import Path
import sys

import pytest

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from scripts.suews.guide_journeys import runner  # noqa: E402
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


def list_journey(promise):
    """Model a reader whose last statement depends on untouched earlier state."""
    return [
        {
            "id": f"s{i}",
            "kind": "documented",
            "span_type": "python",
            "command": command,
            "source_lines": [i + 1, i + 1],
            "promises": [promise] if i == 1 else [],
        }
        for i, command in enumerate(["xs = [0]", "xs.append(2)", "assert xs == [0, 2]"])
    ]


@pytest.mark.parametrize(
    "promise",
    [
        {"kind": "memory", "expression": "xs.append(1) or xs", "expected": [0, 2, 1]},
        {
            "kind": "physical",
            "check": "absolute_residual",
            "expression": "xs.pop() - 2",
            "tolerance": 0,
        },
    ],
)
def test_observation_cannot_change_later_documented_state(tmp_path, promise):
    """A probe's mutation must be invisible to every later documented step."""
    records, consumer = run_steps(list_journey(promise), tmp_path, dict(os.environ), {})
    assert records[1]["promises"][0]["passed"]
    assert records[2]["status"] == "passed"
    assert consumer is None
    assert records[1]["promises"][0]["observed"]["pid"] != records[1]["pid"]


@pytest.mark.parametrize("kind", ["memory", "physical"])
@pytest.mark.parametrize("expected", [6, 7])
def test_journey_outcomes_observe_live_reader(tmp_path, monkeypatch, kind, expected):
    """Outcomes inspect the final live namespace and retain true/false verdicts."""
    promise = (
        {"kind": "memory", "expression": "total", "expected": expected}
        if kind == "memory"
        else {
            "kind": "physical",
            "check": "absolute_residual",
            "expression": f"total - {expected}",
            "tolerance": 0,
        }
    )
    specs = list_journey(promise)[:1]
    specs[0]["command"] = "total = 6"
    monkeypatch.setattr(runner, "bind_journey", lambda *args: specs)
    monkeypatch.setattr(runner, "resolve_install", lambda *args: {"command": "unused"})
    work = tmp_path / "fixture" / "work"
    work.mkdir(parents=True)
    monkeypatch.setattr(
        runner,
        "prepare",
        lambda *args: (
            dict(os.environ),
            {"package_preflight": {"supy_version": "test"}},
        ),
    )
    wheel = tmp_path / "unused.whl"
    wheel.write_bytes(b"fixture")
    args = argparse.Namespace(
        output=tmp_path,
        wheel=wheel,
        wheelhouse=tmp_path,
        pip_version="unused",
        revision="unused",
    )
    record = {"input_sha256s": {}}
    runner.execute_journey(
        ROOT,
        {"installation": {}},
        None,
        {"id": "fixture", "inputs": {}, "outcomes": [promise]},
        args,
        record,
    )
    observation = record["path_outcomes"][0]
    assert observation["passed"] is (expected == 6)
    assert observation["observed"]["pid"] == record["steps"][-1]["pid"]
    assert record["accepted"] is (expected == 6)


def test_last_documented_step_observes_main_reader(tmp_path):
    """A terminal observation may mutate the live namespace after source execution."""
    promise = {
        "kind": "memory",
        "expression": "xs.append(1) or xs",
        "expected": [0, 2, 1],
    }
    specs = list_journey(promise)
    specs[1]["promises"] = []
    specs[-1]["promises"] = [promise]
    records, _ = run_steps(specs, tmp_path, dict(os.environ), {})
    assert all(record["status"] == "passed" for record in records)
    assert records[-1]["promises"][0]["observed"]["pid"] == records[-1]["pid"]
