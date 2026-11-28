"""Tests for the ``assess_readiness`` MCP tool.

A freshly scaffolded config is the bundled KCL/London sample, so every
site-defining value is an assumed default. The tool must say so honestly;
once a value is customised, it must move out of the assumed list.
"""

from __future__ import annotations

import shutil
from importlib.resources import as_file
from importlib.resources.abc import Traversable
from pathlib import Path

import pytest
import yaml

pytestmark = [pytest.mark.native, pytest.mark.e2e, pytest.mark.contract("C-API")]

_requires_suews_cli = pytest.mark.skipif(
    shutil.which("suews") is None,
    reason="`suews` CLI not on PATH; assess_readiness shells to `suews inspect`. Run `make dev`.",
)


def _sample_dir() -> Traversable:
    """The bundled ``sample_data`` directory as a packaged resource."""
    from supy._env import trv_supy_module

    return trv_supy_module / "sample_data"


def _sample() -> Traversable:
    """The bundled sample config as a packaged resource (read-only)."""
    return _sample_dir() / "sample_config.yml"


@_requires_suews_cli
def test_fresh_sample_is_all_assumed(tmp_path: Path) -> None:
    """The bundled sample, unedited, flags location / land_cover / forcing as assumed."""
    from suews_mcp.tools import assess_readiness

    # Run the bundled example as a project inside the test's authorised root.
    with as_file(_sample_dir()) as sample_dir:
        project = shutil.copytree(sample_dir, tmp_path / "sample")
    env = assess_readiness(str(project / "sample_config.yml"), project_root=str(project))
    assert env["status"] == "success", env.get("errors")
    data = env["data"]
    assert data["ready"] is False
    fields = {a["field"] for a in data["assumed_defaults"]}
    assert {"location", "land_cover", "forcing"} <= fields, fields
    assert data["checklist_for_a_meaningful_run"], "checklist must not be empty"
    # Every assumed entry carries a risk and a fix the user can act on.
    for a in data["assumed_defaults"]:
        assert a["risk"] and a["fix"]


@_requires_suews_cli
def test_customised_location_leaves_assumed_list(tmp_path: Path) -> None:
    """Changing the location moves it from assumed to looks_customised."""
    from suews_mcp.tools import assess_readiness

    cfg = yaml.safe_load(_sample().read_text(encoding="utf-8"))
    cfg["sites"][0]["properties"]["lat"]["value"] = 33.45  # Phoenix-ish
    cfg["sites"][0]["properties"]["lng"]["value"] = -112.07
    out = tmp_path / "phoenix.yml"
    out.write_text(yaml.safe_dump(cfg), encoding="utf-8")

    env = assess_readiness(str(out), project_root=str(tmp_path))
    assert env["status"] == "success", env.get("errors")
    data = env["data"]
    assumed = {a["field"] for a in data["assumed_defaults"]}
    assert "location" not in assumed, "edited location should not be flagged assumed"
    assert "location" in data["looks_customised"]
