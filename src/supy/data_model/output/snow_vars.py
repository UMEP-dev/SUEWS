"""Snow output variables.

These variables provide snow-specific outputs for each surface type.
Most variables are replicated across 7 surface types:
- Paved
- Bldgs (Buildings)
- EveTr (Evergreen Trees)
- DecTr (Deciduous Trees)
- Grass
- BSoil (Bare Soil)
- Water
"""

from .variables import (
    OutputVariable,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)


# Surface types for snow variables
SURFACE_TYPES = ["Paved", "Bldgs", "EveTr", "DecTr", "Grass", "BSoil", "Water"]
SURFACE_NAMES = {
    "Paved": "paved surface",
    "Bldgs": "building surface",
    "EveTr": "evergreen tree surface",
    "DecTr": "deciduous tree surface",
    "Grass": "grass surface",
    "BSoil": "bare soil surface",
    "Water": "water surface",
}


def make_surface_vars(base_name, unit, description_template, aggregation):
    """Helper to create variables for all surface types.

    Args:
        base_name: Base variable name (e.g., "SWE")
        unit: Physical unit
        description_template: Description with {} placeholder for surface type
        aggregation: Aggregation method

    Returns:
        List of OutputVariable instances for all surface types
    """
    return [
        OutputVariable(
            name=f"{base_name}_{surf}",
            unit=unit,
            description=description_template.format(SURFACE_NAMES[surf]),
            aggregation=aggregation,
            group=OutputGroup.SNOW,
            level=OutputLevel.DEFAULT,
        )
        for surf in SURFACE_TYPES
    ]


# Snow water equivalent by surface
SWE_VARS = make_surface_vars(
    "SWE", "mm", "Snow water equivalent for {}", AggregationMethod.AVERAGE
)

# Meltwater by surface
MW_VARS = make_surface_vars("Mw", "mm", "Snow melted during the timestep on {}", AggregationMethod.SUM)

# Snow-related heat exchange by surface
QM_VARS = make_surface_vars(
    "Qm",
    "W m-2",
    "Heat used to melt snow on {} (negative when meltwater refreezes)",
    AggregationMethod.AVERAGE,
)

# Advective heat by surface
QA_VARS = make_surface_vars(
    "Qa", "W m-2", "Heat carried into the snowpack by rain on {}", AggregationMethod.AVERAGE
)

# Heat related to freezing by surface
QMFR_VARS = make_surface_vars(
    "QmFr",
    "W m-2",
    "Heat released when surface water freezes on {}",
    AggregationMethod.AVERAGE,
)

# Fraction of snow by surface (excluding Water)
FR_VARS = make_surface_vars(
    "fr", "-", "Fraction of {} covered by snow", AggregationMethod.AVERAGE
)[:-1]  # No fr_Water

# Rain on snow by surface
RAINSN_VARS = make_surface_vars(
    "RainSn", "mm", "Rain falling on snow on {}", AggregationMethod.SUM
)

# Net radiation for snow surface
QN_SNOW_VARS = [
    OutputVariable(
        name=f"Qn_{surf}Snow",
        unit="W m-2",
        description=f"Net all-wave radiation for snow on {SURFACE_NAMES[surf]}",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.SNOW,
        level=OutputLevel.DEFAULT,
    )
    for surf in SURFACE_TYPES
]

# Reflected shortwave radiation for snow surface
KUP_SNOW_VARS = [
    OutputVariable(
        name=f"kup_{surf}Snow",
        unit="W m-2",
        description=f"Reflected shortwave radiation for snow on {SURFACE_NAMES[surf]}",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.SNOW,
        level=OutputLevel.DEFAULT,
    )
    for surf in SURFACE_TYPES
]

# Refrozen meltwater by surface
FRMELT_VARS = make_surface_vars(
    "frMelt",
    "mm",
    "Refrozen meltwater for {}",
    AggregationMethod.AVERAGE,
)

# Meltwater store by surface
MWSTORE_VARS = make_surface_vars(
    "MwStore", "mm", "Liquid water held in the snowpack on {}", AggregationMethod.AVERAGE
)

# Snow density by surface
DENSSNOW_VARS = make_surface_vars(
    "DensSnow", "kg m-3", "Snow density for {}", AggregationMethod.AVERAGE
)

# Snow depth by surface
SD_VARS = make_surface_vars("Sd", "m", "Snow depth for {}", AggregationMethod.AVERAGE)

# Snow surface temperature by surface
TSNOW_VARS = make_surface_vars(
    "Tsnow",
    "K",
    "Snow surface temperature for {} (0 where there is no snow)",
    AggregationMethod.AVERAGE,
)

# Single snow albedo variable
SNOW_ALB_VAR = [
    OutputVariable(
        name="SnowAlb",
        unit="-",
        description=(
            "Albedo of the snowpack, one value shared by all snow-covered "
            "surfaces: it decreases as the snow ages (linearly below 0 degC, "
            "towards the minimum snow albedo at or above 0 degC), returns to "
            "the maximum after enough fresh snowfall, and is 0 when no "
            "surface has snow"
        ),
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.SNOW,
        level=OutputLevel.DEFAULT,
    )
]


# Combine all snow variables
SNOW_VARIABLES = (
    SWE_VARS
    + MW_VARS
    + QM_VARS
    + QA_VARS
    + QMFR_VARS
    + FR_VARS
    + RAINSN_VARS
    + QN_SNOW_VARS
    + KUP_SNOW_VARS
    + FRMELT_VARS
    + MWSTORE_VARS
    + DENSSNOW_VARS
    + SD_VARS
    + TSNOW_VARS
    + SNOW_ALB_VAR
)
