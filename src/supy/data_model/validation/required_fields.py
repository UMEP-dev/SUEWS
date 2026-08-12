"""Registry of conditionally required configuration fields.

A number of parameters are declared ``Optional[...] = None`` on their Pydantic
model so that a partial configuration still loads and the validation layer can
report what is missing. Whether such a parameter must actually be supplied
depends on the configuration: which surface types have a non-zero fraction, and
which physics options are selected.

That knowledge used to live only as local dictionary literals inside
``SUEWSConfig._iter_critical_null_site_param_issues``, where nothing but the
validator itself could reach it. It is hoisted here so the documentation
generator can describe the same conditions to readers instead of reporting that
these parameters have no default and leaving the reader to guess.

Two distinct things live in this module:

- The ``*_REQUIRED`` tables, which map a field name to the
  ``(message, fix)`` pair the validator emits. These are consumed by
  ``SUEWSConfig._iter_critical_null_site_param_issues``.
- ``DOC_REQUIRED_WHEN``, which maps a model class name to a reader-facing
  description of when each field is required. This is consumed by the
  documentation generator.

.. warning::

   The ``message`` strings are load-bearing. ``validation/pipeline/phase_c.py``
   classifies issues by matching substrings of the emitted text, so rewording a
   message can silently change how an issue is reported without failing any
   type check. A freeze test guards the exact strings; if you need to change
   one, update that test deliberately and check the phase C matchers.
"""

from __future__ import annotations

# (message, fix) keyed by field name.
RequirementTable = dict[str, tuple[str, str]]

LAI_REQUIRED: RequirementTable = {
    "lai_max": (
        "Maximum LAI is required for active vegetation",
        "Add maximum leaf area index for full leaf-on conditions",
    ),
}

# Required only when leaf area index is modelled rather than observed.
LAI_CALCULATED_ONLY_REQUIRED: RequirementTable = {
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
}

CONDUCTANCE_REQUIRED: RequirementTable = {
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
}

BUILDING_REQUIRED: RequirementTable = {
    "bldgh": (
        "Building height is required when buildings are active",
        "Add building height in meters",
    ),
}

# Added to BUILDING_REQUIRED only when frontal area index comes from site input.
BUILDING_REQUIRED_PROVIDED_FAI: RequirementTable = {
    "faibldg": (
        "Building frontal area index is required when buildings are active",
        "Add frontal area index for wind and roughness calculations",
    ),
}

EVERGREEN_REQUIRED: RequirementTable = {
    "height_evergreen_tree": (
        "Evergreen tree height is required when evergreen vegetation is active",
        "Add evergreen tree height in meters",
    ),
}

EVERGREEN_REQUIRED_PROVIDED_FAI: RequirementTable = {
    "fai_evergreen_tree": (
        "Evergreen tree frontal area index is required when evergreen vegetation is active",
        "Add evergreen tree frontal area index",
    ),
}

DECIDUOUS_REQUIRED: RequirementTable = {
    "height_deciduous_tree": (
        "Deciduous tree height is required when deciduous vegetation is active",
        "Add deciduous tree height in meters",
    ),
}

DECIDUOUS_REQUIRED_PROVIDED_FAI: RequirementTable = {
    "fai_deciduous_tree": (
        "Deciduous tree frontal area index is required when deciduous vegetation is active",
        "Add deciduous tree frontal area index",
    ),
}


_VEGETATION_PRESENT = "a vegetated surface using these parameters is present"
_LAI_MODELLED = "leaf area index is modelled rather than observed"
_PROVIDED_FAI = "frontal area index is taken from site parameters"

# Reader-facing conditions, keyed by the model class that declares the field.
# Consumed by the documentation generator; see docs/generate_datamodel_rst.py.
DOC_REQUIRED_WHEN: dict[str, dict[str, str]] = {
    "Conductance": dict.fromkeys(CONDUCTANCE_REQUIRED, _VEGETATION_PRESENT),
    "LAIParams": {
        **dict.fromkeys(LAI_REQUIRED, _VEGETATION_PRESENT),
        **dict.fromkeys(
            LAI_CALCULATED_ONLY_REQUIRED,
            f"{_VEGETATION_PRESENT} and {_LAI_MODELLED}",
        ),
    },
    "BldgsProperties": {
        "bldgh": "buildings are present",
        "faibldg": f"buildings are present and {_PROVIDED_FAI}",
    },
    "EvetrProperties": {
        "height_evergreen_tree": "evergreen trees are present",
        "fai_evergreen_tree": f"evergreen trees are present and {_PROVIDED_FAI}",
    },
    "DectrProperties": {
        "height_deciduous_tree": "deciduous trees are present",
        "fai_deciduous_tree": f"deciduous trees are present and {_PROVIDED_FAI}",
    },
}


def required_when(model_name: str, field_name: str) -> str:
    """Return the condition under which a field must be supplied.

    Parameters
    ----------
    model_name : str
        Name of the Pydantic model class declaring the field.
    field_name : str
        Name of the field.

    Returns
    -------
    str
        A reader-facing condition, or an empty string when the field has no
        recorded requirement condition.
    """
    return DOC_REQUIRED_WHEN.get(model_name, {}).get(field_name, "")
