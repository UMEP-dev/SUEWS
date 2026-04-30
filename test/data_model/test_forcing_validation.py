"""Tests for forcing-related Pydantic models and validators (gh#1372)."""

import pytest

from supy.data_model.core.model import ForcingControl, ModelControl

pytestmark = pytest.mark.api


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
    """ModelControl exposes .forcing and keeps legacy forcing_file as an alias."""
    control = ModelControl(forcing={"file": "forcing.txt"})
    assert isinstance(control.forcing, ForcingControl)
    assert control.forcing_file == control.forcing.file


def test_model_control_accepts_legacy_forcing_file():
    """Legacy forcing_file input is moved under forcing.file."""
    control = ModelControl(forcing_file={"value": "legacy.txt"})
    assert control.forcing.file.value == "legacy.txt"
    assert control.forcing_file.value == "legacy.txt"


def test_current_schema_version_bumped_for_forcing_restructure():
    """gh#1372 forcing.file restructure must move the schema label."""
    from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION, SCHEMA_VERSIONS

    assert CURRENT_SCHEMA_VERSION == "2026.5.dev7"
    assert "2026.5.dev7" in SCHEMA_VERSIONS
    desc = SCHEMA_VERSIONS["2026.5.dev7"]
    assert "forcing" in desc.lower()
    assert "1372" in desc


def test_validate_forcing_columns_against_physics_raises_for_missing_ldown():
    """T7: pure helper raises when net_radiation==11 and ldown is missing."""
    from types import SimpleNamespace

    from supy.data_model.core.forcing_validation import (
        validate_forcing_columns_against_physics,
    )

    physics = SimpleNamespace(net_radiation=SimpleNamespace(value=11))
    columns = {"iy", "id", "it", "imin", "Tair", "RH", "U", "pres", "kdown", "rain"}
    with pytest.raises(ValueError, match=r"\bldown\b.*\bnet_radiation=11\b"):
        validate_forcing_columns_against_physics(columns, physics)


def test_validate_forcing_columns_against_physics_accepts_when_present():
    """Helper is silent when required column is present."""
    from types import SimpleNamespace

    from supy.data_model.core.forcing_validation import (
        validate_forcing_columns_against_physics,
    )

    physics = SimpleNamespace(net_radiation=SimpleNamespace(value=11))
    columns = {
        "iy", "id", "it", "imin", "Tair", "RH", "U", "pres", "kdown", "rain", "ldown",
    }
    # No exception expected.
    validate_forcing_columns_against_physics(columns, physics)


def test_validate_forcing_columns_against_physics_method_1_requires_ldown_not_fcld():
    """Net radiation method 1 uses observed Ldown, not cloud fraction."""
    from types import SimpleNamespace

    from supy.data_model.core.forcing_validation import (
        validate_forcing_columns_against_physics,
    )

    physics = SimpleNamespace(net_radiation=1)
    columns = {
        "iy", "id", "it", "imin", "Tair", "RH", "U", "pres", "kdown", "rain", "ldown",
    }
    validate_forcing_columns_against_physics(columns, physics)


def test_validate_forcing_columns_against_physics_rejects_all_sentinel_data():
    """Filled optional columns still fail when the selected physics needs real data."""
    import pandas as pd
    from types import SimpleNamespace

    from supy.data_model.core.forcing_validation import (
        validate_forcing_columns_against_physics,
    )

    physics = SimpleNamespace(net_radiation=1)
    df = pd.DataFrame(
        {
            "iy": [2020],
            "id": [1],
            "it": [0],
            "imin": [0],
            "Tair": [5.0],
            "RH": [80.0],
            "U": [2.0],
            "pres": [1013.0],
            "kdown": [0.0],
            "rain": [0.0],
            "ldown": [-999.0],
        }
    )
    with pytest.raises(ValueError, match=r"ldown.*valid data.*net_radiation=1"):
        validate_forcing_columns_against_physics(df, physics)


def test_python_rust_whitelist_parity():
    """gh#1372 cross-language guard: Python and Rust must agree on the
    per-landcover whitelist, surface short codes, baseline-required set,
    and the -999 sentinel. Drift would silently produce divergent
    forcing handling in the two readers.
    """
    import re
    from pathlib import Path

    from supy._load import (
        BASELINE_FORCING_COLUMNS,
        FORCING_OPTIONAL_FILL,
        LAI_LANDCOVER_SUFFIXES,
        LANDCOVER_SUFFIXES,
        PER_LANDCOVER_FORCING_VARS,
        WUH_LANDCOVER_SUFFIXES,
    )

    rust_src = Path(__file__).resolve().parents[2] / "src" / "suews_bridge" / "src" / "forcing_io.rs"
    text = rust_src.read_text()

    def _list(name: str) -> set[str]:
        match = re.search(rf"const {name}: &\[&str\] = &\[(.*?)\];", text, re.DOTALL)
        if match is None:
            raise AssertionError(f"const {name} not found in forcing_io.rs")
        return set(re.findall(r'"([^"]+)"', match.group(1)))

    assert _list("PER_LANDCOVER_FORCING_VARS") == set(PER_LANDCOVER_FORCING_VARS)
    assert _list("LANDCOVER_SUFFIXES") == set(LANDCOVER_SUFFIXES)
    assert _list("LAI_LANDCOVER_SUFFIXES") == set(LAI_LANDCOVER_SUFFIXES)
    assert _list("WUH_LANDCOVER_SUFFIXES") == set(WUH_LANDCOVER_SUFFIXES)
    assert _list("BASELINE_FORCING_COLUMNS") == {c.lower() for c in BASELINE_FORCING_COLUMNS}

    fill_match = re.search(r"const FORCING_OPTIONAL_FILL: f64 = ([-\d.]+);", text)
    assert fill_match is not None, "FORCING_OPTIONAL_FILL not found in forcing_io.rs"
    assert float(fill_match.group(1)) == FORCING_OPTIONAL_FILL


def test_validate_forcing_columns_against_physics_handles_plain_int_physics():
    """Helper accepts both RefValue-wrapped and bare int physics values."""
    from types import SimpleNamespace

    from supy.data_model.core.forcing_validation import (
        validate_forcing_columns_against_physics,
    )

    physics = SimpleNamespace(net_radiation=11)  # bare int, not RefValue
    columns = {"iy", "id", "it", "imin", "Tair", "RH", "U", "pres", "kdown", "rain"}
    with pytest.raises(ValueError, match=r"\bldown\b"):
        validate_forcing_columns_against_physics(columns, physics)
