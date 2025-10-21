"""RSL (Roughness Sublayer) output variables.

These variables provide vertical profiles within the roughness sublayer,
including heights, wind speed, temperature, and specific humidity at 30 levels.
"""

from .variables import (
    OutputVariable,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)


def make_profile_vars(base_name, unit, description_template, format_spec="f104"):
    """Helper to create profile variables for 30 vertical levels.

    Args:
        base_name: Base variable name (e.g., "z", "U", "T", "q")
        unit: Physical unit
        description_template: Description with {} placeholder for level
        format_spec: Fortran format specifier

    Returns:
        List of OutputVariable instances for levels 1-30
    """
    # Height descriptions
    height_desc = {
        1: "0.1Zh", 2: "0.2Zh", 3: "0.3Zh", 4: "0.4Zh", 5: "0.5Zh",
        6: "0.6Zh", 7: "0.7Zh", 8: "0.8Zh", 9: "0.9Zh", 10: "Zh",
        11: "1.1Zh", 12: "1.2Zh", 13: "1.3Zh", 14: "1.4Zh", 15: "1.5Zh",
        16: "1.6Zh", 17: "1.7Zh", 18: "1.8Zh", 19: "1.9Zh", 20: "2.0Zh",
        21: "2.1Zh", 22: "2.2Zh", 23: "2.3Zh", 24: "2.4Zh", 25: "2.5Zh",
        26: "2.6Zh", 27: "2.7Zh", 28: "2.8Zh", 29: "2.9Zh", 30: "3.0Zh",
    }

    # Special format for levels 14 and 21
    format_map = {14: "f146", 21: "f146"}

    return [
        OutputVariable(
            name=f"{base_name}_{i}",
            unit=unit,
            description=description_template.format(height_desc[i]),
            aggregation=AggregationMethod.AVERAGE,
            group=OutputGroup.RSL,
            level=OutputLevel.DEFAULT,
            format=format_map.get(i, format_spec),
        )
        for i in range(1, 31)
    ]


# Height profiles (30 levels)
Z_VARS = make_profile_vars("z", "m", "{}", "f104")

# Wind speed profiles (30 levels)
U_VARS = make_profile_vars("U", "m s-1", "U at {}", "f104")

# Temperature profiles (30 levels)
T_VARS = make_profile_vars("T", "degC", "T at {}", "f104")

# Specific humidity profiles (30 levels)
Q_VARS = make_profile_vars("q", "g kg-1", "q at {}", "f104")

# RSL diagnostic variables
RSL_DIAG_VARS = [
    OutputVariable(
        name="L_MOD_RSL",
        unit="m",
        description="Obukhov length",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="zH_RSL",
        unit="m",
        description="Canyon depth",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="Lc",
        unit="m",
        description="Canopy drag length scale",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="beta",
        unit="m",
        description="Beta coefficient from Harman 2012",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="zd_RSL",
        unit="m",
        description="Displacement height",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="z0_RSL",
        unit="m",
        description="Roughness length",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="elm",
        unit="m",
        description="Mixing length",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="Scc",
        unit="-",
        description="Schmidt number for temperature and humidity",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="f",
        unit="g kg-1",
        description="H&F07 and H&F08 constants",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="UStar_RSL",
        unit="m s-1",
        description="Friction velocity used in RSL",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="UStar_heat",
        unit="m s-1",
        description="Friction velocity implied by RA_h",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="TStar_RSL",
        unit="K",
        description="Friction temperature used in RSL",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="FAI",
        unit="-",
        description="Frontal area index",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="PAI",
        unit="-",
        description="Plan area index",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
    OutputVariable(
        name="flag_RSL",
        unit="-",
        description="Flag for RSL",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
]


# Combine all RSL variables
RSL_VARIABLES = (
    Z_VARS +
    U_VARS +
    T_VARS +
    Q_VARS +
    RSL_DIAG_VARS
)
