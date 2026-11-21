"""Inventory domains compared against the installed public Click commands."""

import json
from pathlib import Path
import sys

import pytest

from supy.data_model.core.type import SurfaceType

# Script packages are not installed in the wheel; support the pytest console entry point.
_REPO_ROOT = Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from scripts.suews.guide_journeys.inventory import (  # noqa: E402 - source script imports follow path setup
    compare,
    compare_values,
    installed_commands,
)

pytestmark = pytest.mark.api


def test_public_inventory_hides_compatibility_options():
    """Hidden backup switches cannot inflate reader-choice coverage."""
    commands = installed_commands()
    assert json.loads(json.dumps(commands)) == commands
    assert "--backup" not in commands["migrate"]["options"]
    assert commands["migrate"]["options"]["--target-version"]["values"] is None
    assert "C" in commands["validate"]["options"]["--pipeline"]["values"]
    assert "--dry-run" in commands["migrate"]["help"]


def test_inventory_domains_detect_both_directions():
    """Unknown options and obsolete enums are stale; unrestricted values are not."""
    commands = installed_commands()
    declared = {name: {"documented": {}, "excluded": {}} for name in commands}
    declared["migrate"]["documented"] = {
        "--target-version": ["2026.6.dev12"],
        "--invented": [],
    }
    declared["validate"]["documented"] = {"--pipeline": ["C", "NOT-A-PIPELINE"]}
    result = compare(commands, declared)
    assert result["migrate"]["option_names"]["stale"] == ["--invented"]
    assert "--output-dir" in result["migrate"]["option_names"]["missed"]
    assert result["validate"]["enumerated_values"]["--pipeline"]["stale"] == [
        "NOT-A-PIPELINE"
    ]
    assert "ABC" in result["validate"]["enumerated_values"]["--pipeline"]["missed"]
    assert result["migrate"]["unrestricted"]["--target-version"]["documented"] == [
        "2026.6.dev12"
    ]


def test_schema_enumeration_comparison():
    """Surface choices use schema enum members, with stale and missed values separate."""

    result = compare_values(
        [surface.value for surface in SurfaceType], ["paved", "bldgs", "invented"]
    )
    assert result["stale"] == ["invented"]
    assert "grass" in result["missed"]
