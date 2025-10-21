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


def make_surface_vars(base_name, unit, description_template, aggregation, format_spec="f106"):
    """Helper to create variables for all surface types.

    Args:
        base_name: Base variable name (e.g., "SWE")
        unit: Physical unit
        description_template: Description with {} placeholder for surface type
        aggregation: Aggregation method
        format_spec: Fortran format specifier

    Returns:
        List of OutputVariable instances for all surface types
    """
    surface_names = {
        "Paved": "paved surface",
        "Bldgs": "building surface",
        "EveTr": "evergreen tree surface",
        "DecTr": "deciduous tree surface",
        "Grass": "grass surface",
        "BSoil": "bare soil surface",
        "Water": "water surface",
    }

    return [
        OutputVariable(
            name=f"{base_name}_{surf}",
            unit=unit,
            description=description_template.format(surface_names[surf]),
            aggregation=aggregation,
            group=OutputGroup.SNOW,
            level=OutputLevel.DEFAULT,
            format=format_spec,
        )
        for surf in SURFACE_TYPES
    ]


# Snow water equivalent by surface
SWE_VARS = make_surface_vars(
    "SWE", "mm", "Snow water equivalent for {}",
    AggregationMethod.AVERAGE, "f106"
)

# Meltwater by surface
MW_VARS = make_surface_vars(
    "Mw", "mm", "Meltwater for {}",
    AggregationMethod.SUM, "f106"
)

# Snow-related heat exchange by surface
QM_VARS = make_surface_vars(
    "Qm", "W m-2", "Snow-related heat exchange for {}",
    AggregationMethod.AVERAGE, "f106"
)

# Advective heat by surface
QA_VARS = make_surface_vars(
    "Qa", "W m-2", "Advective heat for {}",
    AggregationMethod.AVERAGE, "f106"
)

# Heat related to freezing by surface
QMFR_VARS = make_surface_vars(
    "QmFr", "W m-2", "Heat related to freezing for {}",
    AggregationMethod.AVERAGE, "f146"
)

# Fraction of snow by surface (excluding Water)
FR_VARS = make_surface_vars(
    "fr", "1", "Fraction of snow for {}",
    AggregationMethod.AVERAGE, "f106"
)[:-1]  # No fr_Water

# Rain on snow by surface
RAINSN_VARS = make_surface_vars(
    "RainSn", "mm", "Rain on snow for {}",
    AggregationMethod.SUM, "f146"
)

# Net radiation for snow surface
QN_SNOW_VARS = make_surface_vars(
    "Qn", "W m-2", "Net all-wave radiation for snow {}",
    AggregationMethod.AVERAGE, "f146"
)
# Rename to match Fortran naming (e.g., Qn_PavedSnow)
for var in QN_SNOW_VARS:
    var.name = var.name.replace("Qn_", "Qn_") + "Snow"
    var.name = var.name.replace("_", "_", 1)  # Keep first underscore
    # Fix the renaming - should be Qn_PavedSnow not Qn_PaveddSnow
QN_SNOW_VARS = [
    OutputVariable(
        name=f"Qn_{surf}Snow",
        unit="W m-2",
        description=f"Net all-wave radiation for snow {surf.lower()} surface" if surf != "Bldgs"
                    else "Net all-wave radiation for snow building surface",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.SNOW,
        level=OutputLevel.DEFAULT,
        format="f146",
    )
    for surf in SURFACE_TYPES
]

# Reflected shortwave radiation for snow surface
KUP_SNOW_VARS = [
    OutputVariable(
        name=f"kup_{surf}Snow",
        unit="W m-2",
        description=f"Reflected shortwave radiation for snow {surf.lower()} surface" if surf != "Bldgs"
                    else "Reflected shortwave radiation for snow building surface",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.SNOW,
        level=OutputLevel.DEFAULT,
        format="f146",
    )
    for surf in SURFACE_TYPES
]

# Amount of freezing melt water by surface
FRMELT_VARS = make_surface_vars(
    "frMelt", "mm", "Amount of freezing melt water for {}",
    AggregationMethod.AVERAGE, "f146"
)

# Meltwater store by surface
MWSTORE_VARS = make_surface_vars(
    "MwStore", "mm", "Meltwater store for {}",
    AggregationMethod.AVERAGE, "f146"
)

# Snow density by surface
DENSSNOW_VARS = make_surface_vars(
    "DensSnow", "kg m-3", "Snow density for {}",
    AggregationMethod.AVERAGE, "f146"
)

# Snow depth by surface
SD_VARS = make_surface_vars(
    "Sd", "mm", "Snow depth for {}",
    AggregationMethod.AVERAGE, "f106"
)

# Snow surface temperature by surface
TSNOW_VARS = make_surface_vars(
    "Tsnow", "degC", "Snow surface temperature for {}",
    AggregationMethod.AVERAGE, "f146"
)

# Single snow albedo variable
SNOW_ALB_VAR = [
    OutputVariable(
        name="SnowAlb",
        unit="-",
        description="Surface albedo for snow/ice",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.SNOW,
        level=OutputLevel.DEFAULT,
        format="f146",
    )
]


# Combine all snow variables
SNOW_VARIABLES = (
    SWE_VARS +
    MW_VARS +
    QM_VARS +
    QA_VARS +
    QMFR_VARS +
    FR_VARS +
    RAINSN_VARS +
    QN_SNOW_VARS +
    KUP_SNOW_VARS +
    FRMELT_VARS +
    MWSTORE_VARS +
    DENSSNOW_VARS +
    SD_VARS +
    TSNOW_VARS +
    SNOW_ALB_VAR
)
