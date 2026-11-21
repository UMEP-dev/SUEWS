"""Choice coverage requires an executed step and its observed input condition."""

from pathlib import Path
import sys

import pytest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from scripts.suews.guide_journeys.inventory import coverage  # noqa: E402

pytestmark = pytest.mark.api


def test_later_choice_is_not_credited_by_earlier_failure():
    """Starting a journey says nothing about an unrun command's option value."""
    manifest = {
        "journeys": [{"id": "path"}],
        "promise_edits": [],
        "combinations": [],
        "choices": [
            {
                "id": "json",
                "journeys": ["path"],
                "witness": {
                    "steps": ["validate"],
                    "condition": {
                        "kind": "option",
                        "names": ["--format"],
                        "value": "json",
                    },
                },
            }
        ],
    }
    step = {
        "id": "validate",
        "kind": "documented",
        "status": "unrun",
        "exit_status": None,
        "argv": ["suews", "validate", "--format", "json"],
        "promises": [],
    }
    record = {
        "journey_id": "path",
        "seconds": 0,
        "steps": [
            {
                "id": "migrate",
                "kind": "documented",
                "status": "failed",
                "exit_status": 1,
            },
            step,
        ],
    }
    assert coverage(manifest, [record], [], {})["choices"][0]["status"] == "missed"
    step.update(status="failed", exit_status=1, promises=[{"passed": False}])
    result = coverage(manifest, [record], [], {})["choices"][0]
    assert result["status"] == "exercised"
    assert result["witnesses"][0]["step_id"] == "validate"
    assert result["witnesses"][0]["observed_condition"]["value"] == "json"
    assert result["witnesses"][0]["promises_satisfied"] is False
    step["argv"][-1] = "text"
    assert coverage(manifest, [record], [], {})["choices"][0]["status"] == "missed"
    step.update(status="failed", exit_status=None)
    step["argv"][-1] = "json"
    assert coverage(manifest, [record], [], {})["choices"][0]["status"] == "missed"
