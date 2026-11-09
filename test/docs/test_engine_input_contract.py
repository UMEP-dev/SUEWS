"""Keep engine guidance explicit about incomplete enforcement of resolved input."""

from pathlib import Path

import pytest

pytestmark = pytest.mark.api
ROOT = Path(__file__).resolve().parents[2]


@pytest.mark.parametrize(
    "path", ["docs/source/api/command-line.rst", "src/suews_bridge/README.md"]
)
def test_engine_contract_distinguishes_caller_duty_from_enforcement(path):
    text = " ".join((ROOT / path).read_text(encoding="utf-8").split()).lower()
    assert "caller contract" in text
    assert "defaults belong in the data model" in text
    assert "enforcement" in text and "still being completed" in text
    assert "legacy bridge fallbacks" in text
    assert "#1991/#2002" in text
    assert "the engine itself takes no defaults" not in text
    assert "supplies no defaults of its own" not in text
