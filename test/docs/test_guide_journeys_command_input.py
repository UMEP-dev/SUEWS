"""A documented command cannot be supplied by a manifest."""

from pathlib import Path
import sys

import pytest
import yaml

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from scripts.suews.guide_journeys.manifest import load  # noqa: E402

pytestmark = pytest.mark.api


def test_supplied_command_is_rejected(tmp_path):
    """Reject replacement text even when extraction would overwrite it."""
    data = yaml.safe_load((ROOT / "test/docs/journeys/transition_guide.yml").read_text(encoding="utf-8"))
    data["journeys"][0]["steps"][0]["command"] = "echo repaired"
    path = tmp_path / "manifest.yml"
    path.write_text(yaml.safe_dump(data), encoding="utf-8")
    with pytest.raises(ValueError, match="command text"):
        load(path)
