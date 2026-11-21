"""Reader paths follow source order and declare skipped branches."""

from pathlib import Path
import sys

import pytest

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from scripts.suews.guide_journeys.extractor import Document  # noqa: E402
from scripts.suews.guide_journeys.manifest import load  # noqa: E402
from scripts.suews.guide_journeys.runner import resolve_steps  # noqa: E402

pytestmark = pytest.mark.api


def test_documented_steps_cannot_run_backwards():
    """A manifest cannot validate before producing the documented output."""
    data = load(ROOT / "test/docs/journeys/transition_guide.yml")
    journey = data["journeys"][0]
    journey["steps"].reverse()
    document = Document(
        (ROOT / data["guide"]).read_text(encoding="utf-8"), data["title"]
    )
    with pytest.raises(ValueError, match="source order"):
        resolve_steps(document, data, journey)


def test_skip_requires_named_condition():
    """Omitting a documented producer needs an explicit prose branch binding."""
    data = load(ROOT / "test/docs/journeys/transition_guide.yml")
    journey = data["journeys"][0]
    journey["steps"] = journey["steps"][1:]
    journey.pop("skips", None)
    document = Document(
        (ROOT / data["guide"]).read_text(encoding="utf-8"), data["title"]
    )
    with pytest.raises(ValueError, match="condition"):
        resolve_steps(document, data, journey)
