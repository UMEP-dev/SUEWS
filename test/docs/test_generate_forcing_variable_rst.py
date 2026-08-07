"""Tests for the registry-derived forcing documentation fragment."""

from pathlib import Path
import runpy

import pytest

pytestmark = pytest.mark.api

PROJECT_ROOT = Path(__file__).resolve().parents[2]


def test_committed_forcing_reference_matches_registry() -> None:
    """Keep the checked-in reference fragment synchronised with the registry."""
    module = runpy.run_path(str(PROJECT_ROOT / "docs/generate_forcing_variable_rst.py"))
    generated = module["render_forcing_reference"]()
    committed = (
        PROJECT_ROOT / "docs/source/inputs/generated/forcing-variable-reference.rst"
    ).read_text(encoding="utf-8")

    assert committed == generated
