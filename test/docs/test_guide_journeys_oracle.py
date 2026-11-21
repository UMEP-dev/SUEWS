"""Creation and provenance must reject apparently successful stale outputs."""

from pathlib import Path
import sys

import pytest

# Script packages are not installed in the wheel; support the pytest console entry point.
_REPO_ROOT = Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

import yaml  # noqa: E402 - source imports follow path setup

from scripts.suews.guide_journeys.oracle import (  # noqa: E402 - source script imports follow path setup
    evaluate,
    require_provenance,
    snapshot,
)

pytestmark = pytest.mark.api


def test_stale_file_never_gets_creation_credit(tmp_path):
    """A successful process and existing valid YAML do not prove creation."""
    output = tmp_path / "site.migrated.yml"
    output.write_text("schema_version: 2026.6.dev12\n", encoding="utf-8")
    before = snapshot(tmp_path)
    result = evaluate(
        {"kind": "creates", "path": output.name},
        before,
        snapshot(tmp_path),
        tmp_path,
        {"exit_status": 0},
    )
    assert not result["passed"]
    output.write_text("schema_version: 2026.6.dev12\nname: changed\n", encoding="utf-8")
    assert not evaluate(
        {"kind": "creates", "path": output.name},
        before,
        snapshot(tmp_path),
        tmp_path,
        {"exit_status": 0},
    )["passed"]


def test_provenance_refuses_unpinned_and_modified_input(tmp_path):
    """A consumed path must have the exact checksum pinned in this run."""
    source = tmp_path / "site.yml"
    source.write_text("name: pinned\n", encoding="utf-8")
    before = snapshot(tmp_path)
    with pytest.raises(ValueError, match=r"Uncertified.*site.yml"):
        require_provenance(["site.yml"], {}, before)
    require_provenance(["site.yml"], before, before)
    source.write_text("name: changed\n", encoding="utf-8")
    with pytest.raises(ValueError, match="Uncertified"):
        require_provenance(["site.yml"], before, snapshot(tmp_path))


def test_creation_checks_retained_representation(tmp_path):
    """The delta oracle catches unrelated edits and scalar-wrapper changes."""
    (tmp_path / "site.yml").write_text(
        "schema_version: old\nx: {value: 0.0, ref: keep}\ny: 0\n", encoding="utf-8"
    )
    before = snapshot(tmp_path)
    promise = {
        "kind": "creates",
        "path": "out.yml",
        "source": "site.yml",
        "delta": {"remove": ["y"], "set": {"schema_version": "new"}},
    }
    output = tmp_path / "out.yml"
    output.write_text(
        "schema_version: new\nx: {value: 0.0, ref: keep}\n", encoding="utf-8"
    )
    assert evaluate(promise, before, snapshot(tmp_path), tmp_path, {})["passed"]
    output.write_text("schema_version: new\nx: 0.0\n", encoding="utf-8")
    assert not evaluate(promise, before, snapshot(tmp_path), tmp_path, {})["passed"]


def test_malformed_created_yaml_is_a_recordable_failure(tmp_path):
    """A newly written but unreadable output fails its content promise."""
    (tmp_path / "site.yml").write_text("schema_version: old\n", encoding="utf-8")
    before = snapshot(tmp_path)
    (tmp_path / "out.yml").write_text("broken: [\n", encoding="utf-8")
    result = evaluate(
        {
            "kind": "creates",
            "path": "out.yml",
            "source": "site.yml",
            "delta": {"remove": [], "set": {"schema_version": "new"}},
        },
        before,
        snapshot(tmp_path),
        tmp_path,
        {},
    )
    assert not result["passed"]
    assert "content_error" in result["observed"]


def test_nonzero_recovery_diagnostic_requires_migration_failure(tmp_path):
    """The real diagnostic wording and field path matter, including wrapped cells."""

    manifest = yaml.safe_load(
        (_REPO_ROOT / "test/docs/journeys/transition_guide.yml").read_text(
            encoding="utf-8"
        )
    )
    journey = next(j for j in manifest["journeys"] if j["id"] == "recovery-nonzero")
    promise = next(
        p for p in journey["steps"][0]["promises"] if p["kind"] == "diagnostic"
    )
    diagnostic = "Cannot migrate configuration to schema 2026.6.dev12: sites[0] (KCL, gridiv=1) properties.land_cover.paved.waterdist.to_soilstore: Disallowed water distribution field to_soilstore for paved (got 0.1)"
    for text in (
        diagnostic,
        diagnostic.replace(
            "properties.", "\n\u2502          \u2502           \u2502 properties."
        ),
    ):
        assert evaluate(promise, {}, {}, tmp_path, {"stdout": text})["passed"]
    assert not evaluate(
        promise, {}, {}, tmp_path, {"stdout": "sites[0] paved to_soilstore"}
    )["passed"]
