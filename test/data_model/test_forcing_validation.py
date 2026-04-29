"""Tests for forcing-related Pydantic models and validators (gh#1372)."""

import pytest

from supy.data_model.core.model import ForcingControl, ModelControl


def test_forcing_control_accepts_single_path():
    """ForcingControl accepts a bare string path under .file."""
    control = ForcingControl(file="forcing.txt")
    file_value = control.file.value if hasattr(control.file, "value") else control.file
    assert file_value == "forcing.txt"


def test_forcing_control_accepts_list_of_paths():
    """ForcingControl accepts a list of paths under .file."""
    control = ForcingControl(file=["a.txt", "b.txt"])
    file_value = control.file.value if hasattr(control.file, "value") else control.file
    assert file_value == ["a.txt", "b.txt"]


def test_model_control_holds_forcing_subobject():
    """ModelControl exposes a .forcing sub-object, not a flat forcing_file."""
    control = ModelControl(forcing={"file": "forcing.txt"})
    assert isinstance(control.forcing, ForcingControl)
    assert not hasattr(control, "forcing_file")


def test_current_schema_version_bumped_for_forcing_restructure():
    """gh#1372 forcing.file restructure must move the schema label."""
    from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION, SCHEMA_VERSIONS

    assert CURRENT_SCHEMA_VERSION == "2026.5.dev7"
    assert "2026.5.dev7" in SCHEMA_VERSIONS
    desc = SCHEMA_VERSIONS["2026.5.dev7"]
    assert "forcing" in desc.lower()
    assert "1372" in desc
