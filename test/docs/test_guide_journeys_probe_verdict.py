"""Required acceptance probes and optional diagnostics have different authority."""

from pathlib import Path
import sys

import pytest

pytest.importorskip("docutils", reason="Source binding requires optional docutils")
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from scripts.suews.guide_journeys import runner  # noqa: E402

pytestmark = pytest.mark.api


@pytest.mark.parametrize("kind, expected", [("required", False), ("diagnostic", True)])
def test_probe_verdict_authority(kind, expected):
    """An optional failed diagnostic cannot overturn a passed reader path."""
    steps = [
        {"kind": "documented", "status": "passed"},
        {"kind": "probe", "probe_kind": kind, "status": "failed"},
    ]
    assert runner.journey_passed(steps, []) is expected
