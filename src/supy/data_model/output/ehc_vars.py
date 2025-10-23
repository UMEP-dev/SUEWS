"""EHC (Element Heat Capacity) output variables.

These variables provide detailed thermal and hydrological states
for roof and wall elements with 15 vertical layers each.
"""

from .variables import (
    OutputVariable,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)


def make_layer_vars_roof(base_name, unit, description_template):
    """Helper to create roof variables for 15 layers.

    Args:
        base_name: Base variable name (e.g., "Tsfc_roof")
        unit: Physical unit
        description_template: Description with {} placeholder for layer number

    Returns:
        List of OutputVariable instances for layers 1-15
    """
    return [
        OutputVariable(
            name=f"{base_name}{i}",
            unit=unit,
            description=description_template.format(i),
            aggregation=AggregationMethod.AVERAGE,
            group=OutputGroup.EHC,
            level=OutputLevel.DEFAULT,
            format="f104",
        )
        for i in range(1, 16)
    ]


def make_layer_vars_wall(base_name, unit, description_template):
    """Helper to create wall variables for 15 layers.

    Args:
        base_name: Base variable name (e.g., "Tsfc_wall")
        unit: Physical unit
        description_template: Description with {} placeholder for layer number

    Returns:
        List of OutputVariable instances for layers 1-15
    """
    return [
        OutputVariable(
            name=f"{base_name}{i}",
            unit=unit,
            description=description_template.format(i),
            aggregation=AggregationMethod.AVERAGE,
            group=OutputGroup.EHC,
            level=OutputLevel.DEFAULT,
            format="f104",
        )
        for i in range(1, 16)
    ]


# Surface-level variables (scalars)
SURFACE_VARS = [
    OutputVariable(
        name="Tsfc_surf",
        unit="degK",
        description="Surface temperature (aggregated)",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.EHC,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="QS_surf",
        unit="W m-2",
        description="Storage heat flux at surface (aggregated)",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.EHC,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
]

# Roof variables (15 layers each)
TSFC_ROOF_VARS = make_layer_vars_roof("Tsfc_roof", "degK", "Roof surface temperature layer {}")
QN_ROOF_VARS = make_layer_vars_roof("Qn_roof", "W m-2", "Roof net radiation layer {}")
QS_ROOF_VARS = make_layer_vars_roof("QS_roof", "W m-2", "Roof storage heat flux layer {}")
QE_ROOF_VARS = make_layer_vars_roof("QE_roof", "W m-2", "Roof latent heat flux layer {}")
QH_ROOF_VARS = make_layer_vars_roof("QH_roof", "W m-2", "Roof sensible heat flux layer {}")
STATE_ROOF_VARS = make_layer_vars_roof("state_roof", "mm", "Roof water state layer {}")
SOILSTORE_ROOF_VARS = make_layer_vars_roof("soilstore_roof", "mm", "Roof soil water storage layer {}")

# Wall variables (15 layers each)
TSFC_WALL_VARS = make_layer_vars_wall("Tsfc_wall", "degK", "Wall surface temperature layer {}")
QN_WALL_VARS = make_layer_vars_wall("Qn_wall", "W m-2", "Wall net radiation layer {}")
QS_WALL_VARS = make_layer_vars_wall("QS_wall", "W m-2", "Wall storage heat flux layer {}")
QE_WALL_VARS = make_layer_vars_wall("QE_wall", "W m-2", "Wall latent heat flux layer {}")
QH_WALL_VARS = make_layer_vars_wall("QH_wall", "W m-2", "Wall sensible heat flux layer {}")
STATE_WALL_VARS = make_layer_vars_wall("state_wall", "mm", "Wall water state layer {}")
SOILSTORE_WALL_VARS = make_layer_vars_wall("soilstore_wall", "mm", "Wall soil water storage layer {}")


# Combine all EHC variables
EHC_VARIABLES = (
    SURFACE_VARS +
    TSFC_ROOF_VARS +
    QN_ROOF_VARS +
    QS_ROOF_VARS +
    QE_ROOF_VARS +
    QH_ROOF_VARS +
    STATE_ROOF_VARS +
    SOILSTORE_ROOF_VARS +
    TSFC_WALL_VARS +
    QN_WALL_VARS +
    QS_WALL_VARS +
    QE_WALL_VARS +
    QH_WALL_VARS +
    STATE_WALL_VARS +
    SOILSTORE_WALL_VARS
)
