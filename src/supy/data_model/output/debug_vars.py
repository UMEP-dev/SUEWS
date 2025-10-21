"""Debug output variables.

These variables provide detailed diagnostic information broken down by surface type.
Useful for debugging and detailed model analysis.
"""

from .variables import (
    OutputVariable,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)


# Surface types for debug variables
SURFACE_TYPES = ["Paved", "Bldgs", "EveTr", "DecTr", "Grass", "BSoil", "Water"]


def make_debug_surface_vars(base_name, unit, description_template, aggregation=AggregationMethod.AVERAGE, format_spec="f104"):
    """Helper to create debug variables for all surface types.

    Args:
        base_name: Base variable name (e.g., "Ts", "QN")
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
            group=OutputGroup.DEBUG,
            level=OutputLevel.DEFAULT,
            format=format_spec,
        )
        for surf in SURFACE_TYPES
    ]


# Test flag
FLAG_VAR = [
    OutputVariable(
        name="flag_test",
        unit="-",
        description="Flag for testing",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.DEBUG,
        level=OutputLevel.DEFAULT,
        format="f104",
    )
]

# Surface temperature by surface type
TS_VARS = make_debug_surface_vars(
    "Ts", "degC", "Surface temperature for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Net radiation by surface type
QN_VARS = make_debug_surface_vars(
    "QN", "W m-2", "Net all-wave radiation for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Storage heat flux by surface type
QS_VARS = make_debug_surface_vars(
    "QS", "W m-2", "Storage heat flux for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Latent heat flux from PM by surface type
QE0_VARS = make_debug_surface_vars(
    "QE0", "W m-2", "Latent heat flux from PM for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Latent heat flux by surface type
QE_VARS = make_debug_surface_vars(
    "QE", "W m-2", "Latent heat flux for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Sensible heat flux by surface type
QH_VARS = make_debug_surface_vars(
    "QH", "W m-2", "Sensible heat flux for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Water use by surface type
WU_VARS = make_debug_surface_vars(
    "wu", "mm", "Water use for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Evapotranspiration from PM by surface type
EV0_VARS = make_debug_surface_vars(
    "ev0", "mm", "Evapotranspiration from PM for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Evapotranspiration by surface type
EV_VARS = make_debug_surface_vars(
    "ev", "mm", "Evapotranspiration for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Drainage by surface type
DRAIN_VARS = make_debug_surface_vars(
    "drain", "mm", "Drainage for {}",
    AggregationMethod.AVERAGE, "f104"
)

# Surface wetness (previous timestep) by surface type
ST_PREV_VARS = make_debug_surface_vars(
    "st", "mm", "Surface wetness (previous timestep) for {}",
    AggregationMethod.AVERAGE, "f104"
)
# Rename to add _prev suffix
for var in ST_PREV_VARS:
    var.name = var.name.replace("_", "_", 1) + "_prev"
    # Fix naming: should be st_Paved_prev not st_Paved_prev (already correct)
    # Actually need to insert _prev before the last part
    parts = var.name.split("_")
    var.name = f"{parts[0]}_{parts[1]}_prev"

# Surface wetness (next timestep) by surface type
ST_NEXT_VARS = make_debug_surface_vars(
    "st", "mm", "Surface wetness (next timestep) for {}",
    AggregationMethod.AVERAGE, "f104"
)
# Rename to add _next suffix
for var in ST_NEXT_VARS:
    parts = var.name.split("_")
    var.name = f"{parts[0]}_{parts[1]}_next"


# Combine all debug variables
DEBUG_VARIABLES = (
    FLAG_VAR +
    TS_VARS +
    QN_VARS +
    QS_VARS +
    QE0_VARS +
    QE_VARS +
    QH_VARS +
    WU_VARS +
    EV0_VARS +
    EV_VARS +
    DRAIN_VARS +
    ST_PREV_VARS +
    ST_NEXT_VARS
)
