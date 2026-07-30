"""Tests for the generated forcing-variable reference."""

from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest

pytestmark = pytest.mark.api

PROJECT_ROOT = Path(__file__).resolve().parents[2]


def _load_generator():
    module_path = PROJECT_ROOT / "docs" / "generate_forcing_variable_rst.py"
    spec = importlib.util.spec_from_file_location(
        "generate_forcing_variable_rst_for_test",
        module_path,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_generated_reference_uses_runtime_registry(tmp_path: Path) -> None:
    module = _load_generator()

    output_path = module.generate_rst(tmp_path)
    content = output_path.read_text(encoding="utf-8")

    assert "Total variables" not in content
    assert ".. _forcing-variable-wuh:" in content
    assert ":Unit: ``mm``" in content
    assert "``water_use`` in {1}" in content
    assert "wuh_water" in content
    assert "m3" not in content
