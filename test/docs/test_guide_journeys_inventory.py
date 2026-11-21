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

from click.testing import CliRunner  # noqa: E402 - source imports follow path setup
import yaml  # noqa: E402 - source imports follow path setup

from scripts.suews.guide_journeys.inventory import (  # noqa: E402 - source script imports follow path setup
    compare,
    compare_values,
    installed_commands,
)
from scripts.suews.guide_journeys.summary import (  # noqa: E402 - source imports follow path setup
    failed_summary,
    option_witnesses,
    schema_versions,
    summarise,
    unrestricted_coverage,
)
from supy.cmd.schema_cli import cli  # noqa: E402 - source imports follow path setup

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


def test_real_manifest_matches_installed_commands():
    """The reviewed option inventory stays aligned without importing docutils."""

    manifest = yaml.safe_load(
        (_REPO_ROOT / "test/docs/journeys/transition_guide.yml").read_text(
            encoding="utf-8"
        )
    )
    result = compare(installed_commands(), manifest["inventory"])
    for domains in result.values():
        assert not domains["option_names"]["stale"]
        assert not domains["option_names"]["missed"]
        assert all(not v["stale"] for v in domains["enumerated_values"].values())


def test_unrestricted_values_need_execution_witnesses():
    """Aliases and equals forms count; unrun and unrelated commands do not."""

    records = [
        {
            "journey_id": "example",
            "steps": [
                {
                    "id": "long",
                    "status": "passed",
                    "argv": ["suews", "convert", "--input", "site.yml"],
                },
                {
                    "id": "short",
                    "status": "passed",
                    "argv": ["suews", "convert", "-i=other.yml"],
                },
                {
                    "id": "unrun",
                    "status": "unrun",
                    "argv": ["suews", "convert", "-i", "missing.yml"],
                },
                {
                    "id": "wrong",
                    "status": "passed",
                    "argv": ["suews", "validate", "-i", "missing.yml"],
                },
            ],
        }
    ]
    witnesses = option_witnesses(records, ["suews", "convert"], ["--input", "-i"])
    assert witnesses == {"site.yml": ["example:long"], "other.yml": ["example:short"]}
    commands = installed_commands()
    declared = {
        name: {
            "documented": {},
            "excluded": dict.fromkeys(c["options"], "outside test"),
        }
        for name, c in commands.items()
    }
    declared["convert"]["documented"] = {"--input": ["site.yml", "missing.yml"]}
    result = compare(commands, declared)
    unrestricted_coverage(result, records, commands)
    values = result["convert"]["unrestricted"]["--input"]
    assert values["exercised"] == {"site.yml": ["example:long"]}
    assert values["missed"] == ["missing.yml"]
    summary = {"inventory": result, "journeys": [], "behaviour_audit": {"passed": True}}
    assert failed_summary(summary)
    records[0]["steps"][2]["status"] = "passed"
    unrestricted_coverage(result, records, commands)
    assert not failed_summary(summary)


def test_schema_info_version_list():
    """The public CLI's list supplies versions, not unrestricted help literals."""

    result = CliRunner().invoke(cli, ["info"])
    assert result.exit_code == 0
    assert {"2026.6.dev11", "2026.6.dev12"} <= set(schema_versions(result.output))
    assert schema_versions(
        "Current version: 2026.6.dev12\n  v2026.6.dev11: old\n  \u2192 v2026.6.dev12: current\n"
    ) == ["2026.6.dev11", "2026.6.dev12"]


def test_omitted_audit_is_reported_and_fails_summary():
    """A subset without the designated audit journey cannot silently exit green."""

    manifest = yaml.safe_load(
        (_REPO_ROOT / "test/docs/journeys/transition_guide.yml").read_text(
            encoding="utf-8"
        )
    )
    summary = summarise(manifest, [], [], {"documented_steps": 6, "bound_steps": 6})
    assert summary["behaviour_audit"]["status"] == "unrun"
    assert (
        manifest["behaviour_audit"]["journey"] in summary["behaviour_audit"]["reason"]
    )
    assert failed_summary(summary)
