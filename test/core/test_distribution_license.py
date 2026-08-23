"""Regression checks for the licence signals shipped by the supy package."""

from __future__ import annotations

from importlib.metadata import distribution
import json
from pathlib import Path
import tomllib

import pytest

pytestmark = [pytest.mark.physics, pytest.mark.smoke]

PROJECT_ROOT = Path(__file__).resolve().parents[2]
EXPECTED_EXPRESSION = "MPL-2.0 AND Apache-2.0"
EXPECTED_LICENSE_FILES = {
    "LICENSE",
    "src/suews/ext_lib/spartacus-surface/LICENSE",
    "src/suews/ext_lib/spartacus-surface/NOTICE",
}


def test_source_licence_signals_are_consistent() -> None:
    with (PROJECT_ROOT / "pyproject.toml").open("rb") as stream:
        pyproject = tomllib.load(stream)
    with (PROJECT_ROOT / "src/suews_bridge/Cargo.toml").open("rb") as stream:
        cargo = tomllib.load(stream)
    with (PROJECT_ROOT / ".zenodo.json").open(encoding="utf-8") as stream:
        zenodo = json.load(stream)

    assert pyproject["project"]["license"] == EXPECTED_EXPRESSION
    assert set(pyproject["project"]["license-files"]) == EXPECTED_LICENSE_FILES
    assert "meson-python>=0.18" in pyproject["build-system"]["requires"]
    assert cargo["package"]["license"] == "MPL-2.0"
    assert zenodo["license"] == "MPL-2.0"
    assert "license: MPL-2.0" in (PROJECT_ROOT / "CITATION.cff").read_text(
        encoding="utf-8"
    )


def test_installed_wheel_exposes_pep639_licence_metadata() -> None:
    installed = distribution("supy")
    metadata = installed.metadata

    assert metadata["Metadata-Version"] == "2.4"
    assert metadata["License-Expression"] == EXPECTED_EXPRESSION
    assert metadata["License"] is None
    assert set(metadata.get_all("License-File", [])) == EXPECTED_LICENSE_FILES

    installed_paths = {str(path) for path in installed.files or []}
    for relative in EXPECTED_LICENSE_FILES:
        assert any(
            path.endswith(f".dist-info/licenses/{relative}") for path in installed_paths
        )
