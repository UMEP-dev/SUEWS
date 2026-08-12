"""Guards for the conditionally-required field registry.

The registry is consumed by two unrelated places: the configuration validator,
which emits its message strings, and the documentation generator, which
describes its conditions to readers. Neither notices if the other drifts, so
the coupling is pinned here.
"""

from __future__ import annotations

import pytest

from supy.data_model.core.site import (
    Conductance,
    DectrProperties,
    EvetrProperties,
    LAIParams,
)
from supy.data_model.core.surface import BldgsProperties
from supy.data_model.validation.required_fields import (
    BUILDING_REQUIRED,
    BUILDING_REQUIRED_PROVIDED_FAI,
    CONDUCTANCE_REQUIRED,
    DECIDUOUS_REQUIRED,
    DECIDUOUS_REQUIRED_PROVIDED_FAI,
    DOC_REQUIRED_WHEN,
    EVERGREEN_REQUIRED,
    EVERGREEN_REQUIRED_PROVIDED_FAI,
    LAI_CALCULATED_ONLY_REQUIRED,
    LAI_REQUIRED,
    required_when,
)

pytestmark = pytest.mark.api

MODELS = {
    "Conductance": Conductance,
    "LAIParams": LAIParams,
    "BldgsProperties": BldgsProperties,
    "EvetrProperties": EvetrProperties,
    "DectrProperties": DectrProperties,
}

ALL_TABLES = (
    LAI_REQUIRED,
    LAI_CALCULATED_ONLY_REQUIRED,
    CONDUCTANCE_REQUIRED,
    BUILDING_REQUIRED,
    BUILDING_REQUIRED_PROVIDED_FAI,
    EVERGREEN_REQUIRED,
    EVERGREEN_REQUIRED_PROVIDED_FAI,
    DECIDUOUS_REQUIRED,
    DECIDUOUS_REQUIRED_PROVIDED_FAI,
)


def test_documented_fields_exist_on_their_models() -> None:
    """Every documented condition names a field that really exists."""
    # ARRANGE / ACT
    missing = [
        f"{model_name}.{field}"
        for model_name, fields in DOC_REQUIRED_WHEN.items()
        for field in fields
        if field not in MODELS[model_name].model_fields
    ]

    # ASSERT
    assert not missing


def test_every_required_field_has_a_documented_condition() -> None:
    """A field the validator can demand must be explainable to the reader."""
    # ARRANGE
    documented = {field for fields in DOC_REQUIRED_WHEN.values() for field in fields}

    # ACT
    undocumented = {
        field for table in ALL_TABLES for field in table
    } - documented

    # ASSERT
    assert not undocumented


def test_message_strings_are_frozen() -> None:
    """phase_c classifies issues by matching message substrings.

    Rewording a message changes how an issue is reported without failing any
    type check, so the exact text is pinned. If you intend to change one,
    update this test deliberately and re-check the matchers in
    ``validation/pipeline/phase_c.py``.
    """
    # ARRANGE / ACT / ASSERT
    assert LAI_REQUIRED["lai_max"] == (
        "Maximum LAI is required for active vegetation",
        "Add maximum leaf area index for full leaf-on conditions",
    )
    assert LAI_CALCULATED_ONLY_REQUIRED["base_temperature_senescence"] == (
        "Senescence base temperature is required for active vegetation",
        "Add the base temperature for senescence degree day accumulation",
    )
    assert BUILDING_REQUIRED_PROVIDED_FAI["faibldg"] == (
        "Building frontal area index is required when buildings are active",
        "Add frontal area index for wind and roughness calculations",
    )
    assert sum(len(table) for table in ALL_TABLES) == 22


def test_required_when_returns_condition_or_empty() -> None:
    """The generator lookup answers for known fields and declines otherwise."""
    # ARRANGE / ACT / ASSERT
    assert "modelled rather than observed" in required_when(
        "LAIParams", "base_temperature_senescence"
    )
    assert required_when("BldgsProperties", "bldgh") == "buildings are present"
    # store_cap is in no validator, so nothing may be claimed about it.
    assert not required_when("StorageDrainParams", "store_cap")
    assert not required_when("LAIParams", "not_a_field")
