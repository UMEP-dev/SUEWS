"""Tests for the ``assess_readiness`` MCP tool.

A freshly scaffolded config is the bundled KCL/London sample, so every
site-defining value is an assumed default. The tool must say so honestly;
once a value is customised, it must move out of the assumed list.
"""

from __future__ import annotations

import shutil
from pathlib import Path

import pytest
import yaml

pytestmark = pytest.mark.api

_requires_suews_cli = pytest.mark.skipif(
    shutil.which("suews") is None,
    reason="`suews` CLI not on PATH; assess_readiness shells to `suews inspect`. Run `make dev`.",
)


def _sample() -> Path:
    import supy

    return Path(supy.__file__).resolve().parent / "sample_data" / "sample_config.yml"


@_requires_suews_cli
def test_fresh_sample_is_all_assumed() -> None:
    """The bundled sample, unedited, flags location / land_cover / forcing as assumed."""
    from suews_mcp.tools import assess_readiness

    env = assess_readiness(str(_sample()), project_root=str(_sample().parent))
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
