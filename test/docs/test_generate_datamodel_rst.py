"""Tests for generated config-reference documentation helpers."""

from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest

from supy.data_model.core.physics_families import resolve_scalar_name


pytestmark = pytest.mark.api

PROJECT_ROOT = Path(__file__).resolve().parents[2]


def _load_generator_module():
    module_path = PROJECT_ROOT / "docs" / "generate_datamodel_rst.py"
    spec = importlib.util.spec_from_file_location(
        "generate_datamodel_rst_for_test", module_path
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_modelphysics_selector_guide_uses_rubric_not_section() -> None:
    module = _load_generator_module()

    lines = module.RSTGenerator._format_modelphysics_selector_guide()

    assert lines[0] == ".. rubric:: Public selector forms"
    assert "---------------------" not in lines[:3]


def test_modelphysics_selector_guide_lists_registered_edge_tokens() -> None:
    module = _load_generator_module()

    guide = "\n".join(module.RSTGenerator._format_modelphysics_selector_guide())

    assert "``not_used2``" in guide
    assert "``rst`` / ``T19``" in guide
    assert "nested ``narp``; nested ``spartacus``" in guide
    assert resolve_scalar_name("stability", "not_used2") == 1
    assert resolve_scalar_name("roughness_sublayer", "rst") == 1
