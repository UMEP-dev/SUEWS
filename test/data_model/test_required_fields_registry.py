"""Guards for the conditionally-required field registry.

The registry is consumed by two unrelated places: the configuration validator,
which emits its message strings, and the documentation generator, which
describes its conditions to readers. Neither notices if the other drifts, so
the coupling is pinned here.
"""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.human_activity import AnthropogenicEmissions
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
    "AnthropogenicEmissions": AnthropogenicEmissions,
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

EXPECTED_TABLES = {
    "LAI_REQUIRED": {
        "lai_max": (
            "Maximum LAI is required for active vegetation",
            "Add maximum leaf area index for full leaf-on conditions",
        ),
    },
    "LAI_CALCULATED_ONLY_REQUIRED": {
        "base_temperature": (
            "Base temperature is required for active vegetation",
            "Add the base temperature for growing degree day accumulation",
        ),
        "base_temperature_senescence": (
            "Senescence base temperature is required for active vegetation",
            "Add the base temperature for senescence degree day accumulation",
        ),
        "gdd_full": (
            "Growing degree days for full LAI are required for active vegetation",
            "Add the growing degree day threshold for full leaf-on conditions",
        ),
        "sdd_full": (
            "Senescence degree days are required for active vegetation",
            "Add the senescence degree day threshold for leaf-off conditions",
        ),
    },
    "CONDUCTANCE_REQUIRED": {
        "g_max": (
            "Maximum surface conductance is required for active vegetation",
            "Add g_max for evapotranspiration calculations",
        ),
        "g_k": (
            "Solar radiation response parameter is required for active vegetation",
            "Add g_k for evapotranspiration calculations",
        ),
        "g_q_base": (
            "Vapour pressure deficit base parameter is required for active vegetation",
            "Add g_q_base for evapotranspiration calculations",
        ),
        "g_q_shape": (
            "Vapour pressure deficit shape parameter is required for active vegetation",
            "Add g_q_shape for evapotranspiration calculations",
        ),
        "g_t": (
            "Temperature response parameter is required for active vegetation",
            "Add g_t for evapotranspiration calculations",
        ),
        "g_sm": (
            "Soil moisture response parameter is required for active vegetation",
            "Add g_sm for evapotranspiration calculations",
        ),
        "kmax": (
            "Maximum shortwave radiation parameter is required for active vegetation",
            "Add kmax for evapotranspiration calculations",
        ),
        "s1": (
            "Lower soil moisture threshold is required for active vegetation",
            "Add s1 for evapotranspiration calculations",
        ),
        "s2": (
            "Soil moisture dependence parameter is required for active vegetation",
            "Add s2 for evapotranspiration calculations",
        ),
        "tl": (
            "Lower temperature threshold is required for active vegetation",
            "Add tl for evapotranspiration calculations",
        ),
        "th": (
            "Upper temperature threshold is required for active vegetation",
            "Add th for evapotranspiration calculations",
        ),
    },
    "BUILDING_REQUIRED": {
        "bldgh": (
            "Building height is required when buildings are active",
            "Add building height in meters",
        ),
    },
    "BUILDING_REQUIRED_PROVIDED_FAI": {
        "faibldg": (
            "Building frontal area index is required when buildings are active",
            "Add frontal area index for wind and roughness calculations",
        ),
    },
    "EVERGREEN_REQUIRED": {
        "height_evergreen_tree": (
            "Evergreen tree height is required when evergreen vegetation is active",
            "Add evergreen tree height in meters",
        ),
    },
    "EVERGREEN_REQUIRED_PROVIDED_FAI": {
        "fai_evergreen_tree": (
            "Evergreen tree frontal area index is required when evergreen vegetation is active",
            "Add evergreen tree frontal area index",
        ),
    },
    "DECIDUOUS_REQUIRED": {
        "height_deciduous_tree": (
            "Deciduous tree height is required when deciduous vegetation is active",
            "Add deciduous tree height in meters",
        ),
    },
    "DECIDUOUS_REQUIRED_PROVIDED_FAI": {
        "fai_deciduous_tree": (
            "Deciduous tree frontal area index is required when deciduous vegetation is active",
            "Add deciduous tree frontal area index",
        ),
    },
}


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
    undocumented = {field for table in ALL_TABLES for field in table} - documented

    # ASSERT
    assert not undocumented


def test_message_strings_are_frozen() -> None:
    """Pin every user-facing annotated-YAML message and fix string."""
    # ARRANGE
    actual_tables = {
        "LAI_REQUIRED": LAI_REQUIRED,
        "LAI_CALCULATED_ONLY_REQUIRED": LAI_CALCULATED_ONLY_REQUIRED,
        "CONDUCTANCE_REQUIRED": CONDUCTANCE_REQUIRED,
        "BUILDING_REQUIRED": BUILDING_REQUIRED,
        "BUILDING_REQUIRED_PROVIDED_FAI": BUILDING_REQUIRED_PROVIDED_FAI,
        "EVERGREEN_REQUIRED": EVERGREEN_REQUIRED,
        "EVERGREEN_REQUIRED_PROVIDED_FAI": EVERGREEN_REQUIRED_PROVIDED_FAI,
        "DECIDUOUS_REQUIRED": DECIDUOUS_REQUIRED,
        "DECIDUOUS_REQUIRED_PROVIDED_FAI": DECIDUOUS_REQUIRED_PROVIDED_FAI,
    }

    # ACT / ASSERT
    assert actual_tables == EXPECTED_TABLES


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


def _load_config(path_yaml: Path, clock: str, **dls: float) -> SUEWSConfig:
    """Load a configuration through the public YAML path."""
    config = {
        "sites": [{"properties": {"anthropogenic_emissions": dls}}],
        "model": {"control": {"output": {"timestamp_reference": clock}}},
    }
    path_yaml.write_text(yaml.safe_dump(config), encoding="utf-8")
    return SUEWSConfig.from_yaml(str(path_yaml))


@pytest.mark.parametrize(
    ("missing", "given"), [("startdls", "enddls"), ("enddls", "startdls")]
)
def test_documented_dls_condition_matches_validator(
    missing: str, given: str, tmp_path: Path
) -> None:
    """The documented DLS conditions are the ones the validator enforces."""
    # ARRANGE
    path_yaml = tmp_path / "config.yml"
    window = {"startdls": 86.0, "enddls": 303.0}
    condition = required_when("AnthropogenicEmissions", missing)

    # ACT / ASSERT - the daylight output clock needs the whole window ...
    assert "timestamp_reference: daylight" in condition
    with pytest.raises(ValueError, match="timestamp_reference='daylight' requires"):
        _load_config(path_yaml, "daylight", **{given: window[given]})
    _load_config(path_yaml, "daylight", **window)

    # ... and so does giving the other end, whatever the clock ...
    assert f"``{given}`` is given" in condition
    with pytest.raises(ValueError, match="must both be set or both be None"):
        _load_config(path_yaml, "follow", **{given: window[given]})

    # ... but otherwise the window may be omitted.
    _load_config(path_yaml, "follow")
    _load_config(path_yaml, "utc")
