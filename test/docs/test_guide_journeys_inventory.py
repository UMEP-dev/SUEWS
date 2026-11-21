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

from scripts.suews.guide_journeys.behaviour import removed_paths  # noqa: E402
from scripts.suews.guide_journeys.inventory import (  # noqa: E402 - source script imports follow path setup
    compare,
    compare_values,
    coverage,
    historical_journeys,
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
                    "kind": "documented",
                    "status": "passed",
                    "argv": ["suews", "convert", "--input", "site.yml"],
                },
                {
                    "id": "short",
                    "kind": "documented",
                    "status": "passed",
                    "argv": ["suews", "convert", "-i=other.yml"],
                },
                {
                    "id": "unrun",
                    "kind": "documented",
                    "status": "unrun",
                    "argv": ["suews", "convert", "-i", "missing.yml"],
                },
                {
                    "id": "wrong",
                    "kind": "documented",
                    "status": "passed",
                    "argv": ["suews", "validate", "-i", "missing.yml"],
                },
                {
                    "id": "probe",
                    "kind": "probe",
                    "status": "passed",
                    "argv": ["suews", "convert", "-i", "missing.yml"],
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
    unrestricted_coverage(result, records, commands, set())
    values = result["convert"]["unrestricted"]["--input"]
    assert values["exercised"] == {"site.yml": ["example:long"]}
    assert values["missed"] == ["missing.yml"]
    summary = {"inventory": result, "journeys": [], "behaviour_audit": {"passed": True}}
    assert failed_summary(summary)
    records[0]["steps"][2]["status"] = "passed"
    unrestricted_coverage(result, records, commands, set())
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


def test_observed_deletions_are_not_the_planted_list():
    """Report actual removals, including an unexpected one, and retain kept zeros."""

    before = {
        "sites": [
            {
                "waterdist": {
                    "planted_zero": 0,
                    "retained_zero": 0,
                    "unexpected": {"ref": "keep"},
                }
            }
        ]
    }
    after = {"sites": [{"waterdist": {"retained_zero": 0}}]}
    assert removed_paths(before, after) == [
        "sites/0/waterdist/planted_zero",
        "sites/0/waterdist/unexpected",
    ]


def test_historical_runs_cannot_credit_corrected_steps_or_choices():
    """Shared step IDs do not make the old guide evidence for the corrected guide."""
    manifest = yaml.safe_load(
        (_REPO_ROOT / "test/docs/journeys/transition_guide.yml").read_text(
            encoding="utf-8"
        )
    )
    records = [
        {
            "journey_id": "earlier-stale",
            "seconds": 1,
            "steps": [
                {
                    "id": "migrate",
                    "kind": "documented",
                    "status": "failed",
                    "exit_status": 0,
                    "before": {
                        "site.yml": {
                            "sha256": manifest["fixtures"]["untagged-dev11"]["sha256"]
                        },
                        "site.migrated.yml": {
                            "sha256": manifest["fixtures"]["stale-dev12"]["sha256"]
                        },
                    },
                },
                {"id": "validate-migrated", "kind": "documented", "status": "unrun"},
            ],
        }
    ]
    result = coverage(manifest, records, [], {"documented_steps": 6, "bound_steps": 6})
    assert result["executed_steps"] == []
    assert all(c["exercised_by"] == [] for c in result["choices"])
    historical = {c["id"]: c for c in result["historical_choices"]}
    assert historical["earlier-stale"]["exercised_by"] == ["earlier-stale"]


def test_historical_and_probe_runs_cannot_witness_or_credit_corrected_coverage():
    """Neither an earlier-guide replay nor a probe is evidence for the corrected guide."""
    manifest = yaml.safe_load(
        (_REPO_ROOT / "test/docs/journeys/transition_guide.yml").read_text(
            encoding="utf-8"
        )
    )
    manifest["combinations"].append({
        "id": "corrected-example",
        "choices": [],
        "journeys": ["earlier-stale"],
    })
    records = [
        {
            "journey_id": "earlier-stale",
            "seconds": 1,
            "steps": [
                {
                    "id": "migrate",
                    "kind": "documented",
                    "status": "failed",
                    "exit_status": 0,
                    "before": {
                        "site.yml": {
                            "sha256": manifest["fixtures"]["untagged-dev11"]["sha256"]
                        },
                        "site.migrated.yml": {
                            "sha256": manifest["fixtures"]["stale-dev12"]["sha256"]
                        },
                    },
                }
            ],
        }
    ]
    result = coverage(manifest, records, [], {"documented_steps": 6, "bound_steps": 6})
    untested = {c["id"] for c in result["combinations"]["untested"]}
    assert "corrected-example" in untested
    assert result["combinations"]["exercised"] == []
    assert [c["id"] for c in result["historical_combinations"]["exercised"]] == [
        "untagged-stale-output"
    ]

    commands = installed_commands()
    declared = {
        name: {
            "documented": {},
            "excluded": dict.fromkeys(c["options"], "outside test"),
        }
        for name, c in commands.items()
    }
    declared["migrate"]["documented"] = {"--target-version": ["2026.6.dev12"]}
    argv = [
        "suews",
        "schema",
        "migrate",
        "site.yml",
        "--target-version",
        "2026.6.dev12",
    ]
    probe = {
        "journey_id": "tagged-dev11",
        "steps": [{"id": "probe", "kind": "probe", "status": "passed", "argv": argv}],
    }
    historical = {
        "journey_id": "earlier-stale",
        "steps": [
            {"id": "migrate", "kind": "documented", "status": "passed", "argv": argv}
        ],
    }
    inventory = compare(commands, declared)
    unrestricted_coverage(
        inventory, [probe, historical], commands, historical_journeys(manifest)
    )
    values = inventory["migrate"]["unrestricted"]["--target-version"]
    assert values["exercised"] == {}
    assert values["missed"] == ["2026.6.dev12"]
