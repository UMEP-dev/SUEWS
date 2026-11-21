"""Probes cannot prepare a documented reader path."""

from pathlib import Path
import sys

import pytest
import yaml

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from scripts.suews.guide_journeys.extractor import Document  # noqa: E402
from scripts.suews.guide_journeys.manifest import load  # noqa: E402
from scripts.suews.guide_journeys.runner import resolve_steps  # noqa: E402

pytestmark = pytest.mark.api


@pytest.mark.parametrize("command", ["touch site.migrated.yml", "pip install other"])
def test_probe_cannot_precede_documented_step(tmp_path, command):
    """Reject both filesystem and installed-environment preparation by a probe."""
    manifest = yaml.safe_load(
        (ROOT / "test/docs/journeys/transition_guide.yml").read_text(encoding="utf-8")
    )
    manifest["journeys"][0]["steps"].insert(
        0, {"id": "prepare", "probe": command, "promises": []}
    )
    path = tmp_path / "manifest.yml"
    path.write_text(yaml.safe_dump(manifest), encoding="utf-8")
    with pytest.raises(ValueError, match="last step"):
        load(path)


def test_probe_cannot_reuse_documented_id(tmp_path):
    """Probe identities never count as documented instructions."""
    manifest = load(ROOT / "test/docs/journeys/transition_guide.yml")
    journey = manifest["journeys"][0]
    journey["steps"].append({"id": "migrate", "probe": "true", "promises": []})
    document = Document(
        (ROOT / manifest["guide"]).read_text(encoding="utf-8"), manifest["title"]
    )
    with pytest.raises(ValueError, match="collid"):
        resolve_steps(document, manifest, journey)
