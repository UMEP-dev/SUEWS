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

This module contains:

- The ``*_REQUIRED`` tables, which map a field name to the
  ``(message, fix)`` pair emitted in annotated-YAML feedback. These are
  consumed by ``SUEWSConfig._iter_critical_null_site_param_issues``.
- ``RSL_VARIABLE_BLDGS_REQUIRED`` and the ``DYOHM_BUILDING_*`` constants,
  which ``SUEWSConfig._validate_rsl`` and ``SUEWSConfig._validate_storage``
  consume for their physics-dependent checks.
- ``WATER_DISTRIBUTION_REQUIRED``, the destinations each surface must route
  water to, consumed by ``WaterDistribution.validate_distribution``. A failure
  there is reported as a warning while a configuration loads, but the missing
  fractions then reach the model as nulls, so the reference documents them as
  required.
- ``DOC_REQUIRED_WHEN``, which maps a model class name to a reader-facing
  description of when each field is required. This is consumed by the
  documentation generator. Most entries are derived from the constants above;
  the ``AnthropogenicEmissions`` entry mirrors the DLS checks in
  ``SUEWSConfig.validate_model_output_timestamp_reference`` and
  ``SUEWSConfig.validate_dls_parameters`` instead, and the ``IrrigationParams``
  entry mirrors the irrigation-window check in the Phase B validation pipeline
  (``phase_b_rules.other_rules``).
- ``STEBBS_SHADING_REQUIRED_MODES`` and ``stebbs_shading_requirements``,
  which share shading requirements across parameter models, full
  configurations and raw YAML diagnostics.

Only requirements that a configuration can fail are recorded. A parameter that
carries a default never has to be supplied, so a validator that merely rejects
an explicit null on it is not a requirement on the reader; the shading
parameters are listed because ``StebbsProperties`` demands they be given
explicitly even though they carry defaults. Every other parameter is described
as optional when it carries a default, and otherwise as having no default and
needing a value.

.. warning::

   The ``message`` and ``fix`` strings are user-facing annotated-YAML output.
   Rewording either changes validation feedback without failing any type check.
   A freeze test guards every pair; update it deliberately when changing this
   public feedback contract.
"""

from __future__ import annotations

# (message, fix) keyed by field name.
RequirementTable = dict[str, tuple[str, str]]

# Fixed shading inputs that must be explicitly supplied in each active mode.
STEBBS_SHADING_REQUIRED_MODES: dict[str, tuple[int, ...]] = {
    "reduction_factor_shading": (1, 2),
    "temperature_threshold_shading": (2,),
    "radiation_threshold_shading": (2,),
}


def stebbs_shading_requirements(mode: int | None) -> dict[str, str]:
    """Return required shading fields and their missing-value messages.

    Parameters
    ----------
    mode : int or None
        Validated shading mode; disabled or absent modes require no fields.

    Returns
    -------
    dict of str to str
        Required field names mapped to messages shared by model and YAML checks.
    """
    return {
        field: f"{field} must be provided when internal_shading is "
        + " or ".join(str(value) for value in modes)
        for field, modes in STEBBS_SHADING_REQUIRED_MODES.items()
        if mode in modes
    }


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


# Physics-dependent requirements checked by SUEWSConfig._validate_rsl: when
# model.physics.roughness_sublayer is explicitly 2 (VARIABLE), a site with
# buildings must give these BldgsProperties fields.
RSL_VARIABLE_BLDGS_REQUIRED: tuple[str, ...] = ("faibldg",)

# Physics-dependent requirements checked by SUEWSConfig._validate_storage: the
# building DyOHM storage-heat methods need the building thermal layers.
DYOHM_BUILDING_METHODS: tuple[int, ...] = (6, 8)
DYOHM_BUILDING_LAYERS_REQUIRED: tuple[str, ...] = ("dz", "k", "rho_cp")

# Water-distribution destinations each surface must give when its waterdist
# block is supplied; an omitted block takes surface-specific defaults. Keyed by
# surface type value; the water surface has no distribution.
WATER_DISTRIBUTION_REQUIRED: dict[str, tuple[str, ...]] = {
    "paved": (
        "to_bldgs", "to_dectr", "to_evetr", "to_grass", "to_bsoil", "to_water",
        "to_runoff",
    ),
    "bldgs": (
        "to_paved", "to_dectr", "to_evetr", "to_grass", "to_bsoil", "to_water",
        "to_runoff",
    ),
    "dectr": (
        "to_paved", "to_bldgs", "to_evetr", "to_grass", "to_bsoil", "to_water",
        "to_soilstore",
    ),
    "evetr": (
        "to_paved", "to_bldgs", "to_dectr", "to_grass", "to_bsoil", "to_water",
        "to_soilstore",
    ),
    "grass": (
        "to_paved", "to_bldgs", "to_dectr", "to_evetr", "to_bsoil", "to_water",
        "to_soilstore",
    ),
    "bsoil": (
        "to_paved", "to_bldgs", "to_dectr", "to_evetr", "to_grass", "to_water",
        "to_soilstore",
    ),
}


def _option(path: str, codes: tuple[int, ...], names: tuple[str, ...]) -> str:
    """Name a physics option and the codes that trigger a requirement."""
    choices = " or ".join(
        f"``{code}`` ({name})" for code, name in zip(codes, names)
    )
    return f"``{path}`` is {choices}"


_VEGETATION_PRESENT = (
    "a vegetated surface using these parameters is present "
    "(``sfr`` above zero for ``grass``, ``dectr`` or ``evetr``)"
)
_LAI_MODELLED = _option("model.physics.laimethod", (1,), ("MODELLED",))
_PROVIDED_FAI = _option("model.physics.frontal_area_index", (0,), ("OBSERVED",))
# _validate_rsl runs only when the option is set explicitly, and VARIABLE is
# also the default, so the condition says "set explicitly".
_RSL_VARIABLE = (
    "``model.physics.roughness_sublayer`` is set explicitly to ``2`` (VARIABLE)"
)
_DYOHM_BUILDING = _option(
    "model.physics.storage_heat", DYOHM_BUILDING_METHODS, ("DyOHM", "DyOHM_BUILDING")
)


def _present(surface: str, label: str) -> str:
    """Describe a land cover whose surface fraction is above zero."""
    return f"{label} are present (``land_cover.{surface}.sfr`` above zero)"


_BUILDINGS_PRESENT = _present("bldgs", "buildings")
_EVERGREEN_PRESENT = _present("evetr", "evergreen trees")
_DECIDUOUS_PRESENT = _present("dectr", "deciduous trees")
# Mirrors two SUEWSConfig validators: validate_model_output_timestamp_reference
# rejects a daylight output clock unless both ends of the DLS window are
# supplied, and validate_dls_parameters rejects a window with only one end.
_DAYLIGHT_OUTPUT = (
    "saved output uses the daylight-saving clock "
    "(``model.control.output.timestamp_reference: daylight``)"
)


def _dls_condition(other_end: str) -> str:
    """Describe when one end of the DLS window must be supplied."""
    return f"{_DAYLIGHT_OUTPUT}, or when ``{other_end}`` is given"


def _irrigation_condition(other_end: str) -> str:
    """Describe when one end of the irrigation window must be supplied."""
    return (
        f"``{other_end}`` is set to a day of year (not ``0`` or null): the "
        "irrigation window "
        "needs both ends, or neither"
    )


def _shading_condition(modes: tuple[int, ...]) -> str:
    """Describe the internal shading modes that need a field given explicitly."""
    choices = " or ".join(f"``{mode}``" for mode in modes)
    return (
        f"``internal_shading`` is {choices}; the default does not count, so give "
        "a value explicitly"
    )


def _water_distribution_conditions() -> dict[str, str]:
    """Describe, per destination, the surfaces whose waterdist must give it."""
    destinations = sorted(
        {field for fields in WATER_DISTRIBUTION_REQUIRED.values() for field in fields}
    )
    conditions = {}
    for field in destinations:
        surfaces = [
            f"``{surface}``"
            for surface, fields in WATER_DISTRIBUTION_REQUIRED.items()
            if field in fields
        ]
        listed = " or ".join(
            filter(None, (", ".join(surfaces[:-1]), surfaces[-1]))
        )
        conditions[field] = (
            f"``waterdist`` is given for a {listed} surface; an omitted "
            "``waterdist`` block takes surface-specific defaults"
        )
    return conditions


def _fai_condition(present: str, rsl_applies: bool = False) -> str:
    """Describe when a frontal area index must be supplied."""
    if rsl_applies:
        return f"{present} and either {_PROVIDED_FAI} or {_RSL_VARIABLE}"
    return f"{present} and {_PROVIDED_FAI}"


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
        "bldgh": _BUILDINGS_PRESENT,
        "faibldg": _fai_condition(_BUILDINGS_PRESENT, rsl_applies=True),
    },
    "EvetrProperties": {
        "height_evergreen_tree": _EVERGREEN_PRESENT,
        "fai_evergreen_tree": _fai_condition(_EVERGREEN_PRESENT),
    },
    "DectrProperties": {
        "height_deciduous_tree": _DECIDUOUS_PRESENT,
        "fai_deciduous_tree": _fai_condition(_DECIDUOUS_PRESENT),
    },
    "ThermalLayers": dict.fromkeys(
        DYOHM_BUILDING_LAYERS_REQUIRED,
        "used as the building layers (``land_cover.bldgs.thermal_layers``) "
        f"and {_DYOHM_BUILDING}",
    ),
    "StebbsProperties": {
        field: _shading_condition(modes)
        for field, modes in STEBBS_SHADING_REQUIRED_MODES.items()
    },
    "WaterDistribution": _water_distribution_conditions(),
    "IrrigationParams": {
        "ie_start": _irrigation_condition("ie_end"),
        "ie_end": _irrigation_condition("ie_start"),
    },
    "AnthropogenicEmissions": {
        "startdls": _dls_condition("enddls"),
        "enddls": _dls_condition("startdls"),
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
