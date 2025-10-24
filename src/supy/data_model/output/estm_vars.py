"""ESTM (Element Surface Temperature Model) output variables.

These variables provide temperature and storage heat flux information
from the Element Surface Temperature Model.
"""

from .variables import (
    OutputVariable,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)


def make_layer_vars(base_name, description_template):
    """Helper to create temperature variables for 5 layers.

    Args:
        base_name: Base variable name (e.g., "TWALL")
        description_template: Description with {} placeholder for layer number

    Returns:
        List of OutputVariable instances for layers 1-5
    """
    return [
        OutputVariable(
            name=f"{base_name}{i}",
            unit="degK",
            description=description_template.format(i),
            aggregation=AggregationMethod.AVERAGE,
            group=OutputGroup.ESTM,
            level=OutputLevel.DEFAULT,
            format="f104",
        )
        for i in range(1, 6)
    ]


# Storage heat fluxes
STORAGE_VARS = [
    OutputVariable(
        name="QS",
        unit="W m-2",
        description="Total storage heat flux",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.ESTM,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="QSAir",
        unit="W m-2",
        description="Storage air",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.ESTM,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="QSWall",
        unit="W m-2",
        description="Storage wall",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.ESTM,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="QSRoof",
        unit="W m-2",
        description="Storage roof",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.ESTM,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="QSGround",
        unit="W m-2",
        description="Storage ground",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.ESTM,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="QSIBld",
        unit="W m-2",
        description="Storage internal building",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.ESTM,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
]

# Wall temperatures (5 layers)
TWALL_VARS = make_layer_vars("TWALL", "Temperature in wall layer {}")

# Roof temperatures (5 layers)
TROOF_VARS = make_layer_vars("TROOF", "Temperature in roof layer {}")

# Ground temperatures (5 layers)
TGROUND_VARS = make_layer_vars("TGROUND", "Temperature in ground layer {}")

# Internal building temperatures (5 layers)
TIBLD_VARS = make_layer_vars("TiBLD", "Temperature in internal building layer {}")

# Indoor air temperature
INDOOR_TEMP_VAR = [
    OutputVariable(
        name="TaBLD",
        unit="degK",
        description="Indoor air temperature",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.ESTM,
        level=OutputLevel.DEFAULT,
        format="f104",
    )
]


# Combine all ESTM variables
ESTM_VARIABLES = (
    STORAGE_VARS + TWALL_VARS + TROOF_VARS + TGROUND_VARS + TIBLD_VARS + INDOOR_TEMP_VAR
)
