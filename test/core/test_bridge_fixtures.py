"""Keep portable buffer-only Rust test inputs equal to data-model exports."""

import importlib.util
from pathlib import Path

import pytest

pytestmark = [pytest.mark.api, pytest.mark.contract("C-EMIT", "C-TOOLING")]

SCRIPT = Path(__file__).resolve().parents[2] / "scripts/suews/gen_bridge_fixtures.py"
SPEC = importlib.util.spec_from_file_location("gen_bridge_fixtures", SCRIPT)
assert SPEC is not None and SPEC.loader is not None
generator = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(generator)


@pytest.mark.parametrize("name", generator.FIXTURES)
def test_resolved_bridge_fixture_is_current(name):
    path = generator.OUTPUT_DIR / f"{name}_resolved.yml"
    expected = generator.resolved_fixture(name)
    assert path.is_file() and path.read_bytes() == expected, (
        f"{path.name} is missing or stale; with the project venv active, run "
        "python scripts/suews/gen_bridge_fixtures.py"
    )
