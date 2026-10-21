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


# Layer positions for a 5-layer element. Layers are node temperatures, not
# surface temperatures: walls and roofs run from the outdoor to the indoor
# face, the ground from the surface downwards, and internal building elements
# face indoor air on both sides.
_ENVELOPE_POSITIONS = {
    1: "outermost, next to the outdoor surface",
    2: "outer-middle",
    3: "middle",
    4: "inner-middle",
    5: "innermost, next to the indoor surface",
}
_GROUND_POSITIONS = {
    1: "uppermost, next to the ground surface",
    2: "upper-middle",
    3: "middle",
    4: "lower-middle",
    5: "deepest, next to the lower soil boundary",
}
_INTERNAL_POSITIONS = {
    1: "surface layer, facing indoor air",
    2: "between surface and centre",
    3: "centre",
    4: "between surface and centre",
    5: "surface layer, facing indoor air",
}


def make_layer_vars(base_name, description_template, layer_positions):
    """Helper to create temperature variables for 5 layers.

    Args:
        base_name: Base variable name (e.g., "TWALL")
        description_template: Description with {} placeholder for layer number
        layer_positions: Mapping of layer number to its position label

    Returns:
        List of OutputVariable instances for layers 1-5
    """
    return [
        OutputVariable(
            name=f"{base_name}{i}",
            unit="degK",
            description=description_template.format(f"{i} ({layer_positions[i]})"),
            aggregation=AggregationMethod.AVERAGE,
            group=OutputGroup.ESTM,
            level=OutputLevel.DEFAULT,
        )
        for i in range(1, 6)
    ]


# Storage heat fluxes
STORAGE_VARS = [
    OutputVariable(
        name="QS",
        unit="W m-2",
        description=(
            "Storage heat flux from ESTM: sum of QSWall, QSRoof, QSGround and "
            "QSIBld, excluding QSAir (positive into storage)"
        ),
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.ESTM,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="QSAir",
        unit="W m-2",
        description=(
            "Heat storage rate in outdoor air below twice the mean building "
            "height, from the change in air temperature (not included in QS)"
        ),
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.ESTM,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="QSWall",
        unit="W m-2",
        description="Heat storage rate in wall elements",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.ESTM,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="QSRoof",
        unit="W m-2",
        description="Heat storage rate in roof elements",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.ESTM,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="QSGround",
        unit="W m-2",
        description="Heat storage rate in ground elements",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.ESTM,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="QSIBld",
        unit="W m-2",
        description="Heat storage rate in internal building mass",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.ESTM,
        level=OutputLevel.DEFAULT,
    ),
]

# Wall temperatures (5 layers)
TWALL_VARS = make_layer_vars(
    "TWALL", "Temperature in wall layer {}", _ENVELOPE_POSITIONS
)

# Roof temperatures (5 layers)
TROOF_VARS = make_layer_vars(
    "TROOF", "Temperature in roof layer {}", _ENVELOPE_POSITIONS
)

# Ground temperatures (5 layers)
TGROUND_VARS = make_layer_vars(
    "TGROUND", "Temperature in ground layer {}", _GROUND_POSITIONS
)

# Internal building temperatures (5 layers)
TIBLD_VARS = make_layer_vars(
    "TiBLD", "Temperature in internal building element layer {}", _INTERNAL_POSITIONS
)

# Indoor air temperature
INDOOR_TEMP_VAR = [
    OutputVariable(
        name="TaBLD",
        unit="degK",
        description="Indoor air temperature",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.ESTM,
        level=OutputLevel.DEFAULT,
    )
]


# Combine all ESTM variables
ESTM_VARIABLES = (
    STORAGE_VARS + TWALL_VARS + TROOF_VARS + TGROUND_VARS + TIBLD_VARS + INDOOR_TEMP_VAR
)
