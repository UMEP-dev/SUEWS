"""A non-migration page does not inherit migration fixtures or CLI inspections."""

from pathlib import Path
import sys

import pytest

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from scripts.suews.guide_journeys.extractor import Document  # noqa: E402
from scripts.suews.guide_journeys.manifest import load  # noqa: E402
from scripts.suews.guide_journeys.runner import setup_optional  # noqa: E402
from scripts.suews.guide_journeys.summary import summarise  # noqa: E402

pytestmark = pytest.mark.api


def test_fixture_needs_no_migration_setup(tmp_path):
    """No environment or installed CLI is needed when setup was not requested."""
    text = (ROOT / "test/fixtures/guide_journeys/python_page.rst").read_text(
        encoding="utf-8"
    )
    document = Document(text, "Small reader journey")
    record = {"input_sha256s": {}, "installed_help": {}}
    assert setup_optional({}, {"id": "fixture"}, tmp_path, {}, document, record) == []
    assert record == {"input_sha256s": {}, "installed_help": {}}
    manifest = {
        "steps": [],
        "journeys": [],
        "choices": [],
        "combinations": [],
        "promise_edits": [],
    }
    summary = summarise(manifest, [], [], {"documented_steps": 0, "bound_steps": 0})
    assert summary["behaviour_audit"]["status"] == "not_requested"
    assert summary["inventory"]["status"] == "not_requested"


def test_migration_manifest_retains_explicit_setup():
    """The existing pilot continues to request all migration-specific observations."""
    manifest = load(ROOT / "test/docs/journeys/transition_guide.yml")
    assert manifest["forcing_resource"]["target"]
    assert manifest["behaviour_audit"]["journey"]
    assert set(manifest["cli_inspection"]["commands"]) == {
        "suews schema migrate",
        "suews validate",
        "suews convert",
    }
