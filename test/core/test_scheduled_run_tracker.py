"""Guard the wiring of scheduled-run-tracker.yml.

The tracker reports a scheduled workflow only if that workflow's ``name:`` is
in its ``workflow_run.workflows`` list, so a new or renamed scheduled
workflow would otherwise go silent without any test failing.
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

pytestmark = [pytest.mark.tooling, pytest.mark.contract("C-TOOLING")]

REPO_ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS = REPO_ROOT / ".github/workflows"
TRACKER = WORKFLOWS / "scheduled-run-tracker.yml"


def _load(path: Path) -> dict:
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def _triggers(wf: dict) -> dict:
    # PyYAML reads the bare key `on` as boolean True.
    triggers = wf.get(True, wf.get("on", {}))
    return triggers if isinstance(triggers, dict) else {}


@pytest.mark.smoke
@pytest.mark.core
def test_tracker_watches_every_scheduled_workflow() -> None:
    scheduled = {
        _load(path)["name"]
        for path in sorted(WORKFLOWS.glob("*.yml"))
        if "schedule" in _triggers(_load(path))
    }
    watched = set(_triggers(_load(TRACKER))["workflow_run"]["workflows"])
    assert scheduled, "no scheduled workflows found"
    assert watched == scheduled


@pytest.mark.smoke
@pytest.mark.core
def test_tracker_reports_scheduled_runs_only_with_minimal_permissions() -> None:
    wf = _load(TRACKER)
    assert _triggers(wf)["workflow_run"]["types"] == ["completed"]
    assert wf["permissions"] == {}
    job = wf["jobs"]["track"]
    assert job["if"] == "github.event.workflow_run.event == 'schedule'"
    assert job["permissions"] == {"issues": "write", "actions": "read", "contents": "read"}
    checkout = job["steps"][0]
    assert checkout["uses"].startswith("actions/checkout@")
    assert "ref" not in checkout["with"]
    assert checkout["with"]["persist-credentials"] is False


@pytest.mark.smoke
@pytest.mark.core
def test_in_run_report_no_longer_writes_issues() -> None:
    caller = _load(WORKFLOWS / "build-publish_to_pypi.yml")
    assert caller["jobs"]["report_scheduled_run"]["permissions"] == {}


@pytest.mark.smoke
@pytest.mark.core
def test_watched_workflows_do_not_open_issues_themselves() -> None:
    """One reporting path per workflow: a watched workflow writing its own
    issue would report a red run twice and leave one issue unclosed."""
    for path in sorted(WORKFLOWS.glob("*.yml")):
        wf = _load(path)
        if "schedule" not in _triggers(wf) or path.name == "build-publish_to_pypi.yml":
            continue
        assert "issues: write" not in path.read_text(encoding="utf-8"), path.name
