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


def make_profile_vars(base_name, unit, description_template):
    """Helper to create profile variables for 30 vertical levels.

    Heights follow the Fortran `setup_RSL_heights` logic:
    - Levels 1–20 are within the canopy (dense near surface up to canyon top)
    - Levels 21–30 span above-canopy to the measurement height (zMeas)
    """
    return [
        OutputVariable(
            name=f"{base_name}_{i}",
            unit=unit,
            description=description_template.format(
                f"level {i} (within canopy, 1=ground to 20=canopy top)"
                if i <= 20
                else f"level {i} (above canopy, 21=canopy top to 30=measurement height)"
            ),
            aggregation=AggregationMethod.AVERAGE,
            group=OutputGroup.RSL,
            level=OutputLevel.DEFAULT,
        )
        for i in range(1, 31)
    ]


# Height profiles (30 levels)
Z_VARS = make_profile_vars("z", "m", "Height at {}")

# Wind speed profiles (30 levels)
U_VARS = make_profile_vars("U", "m s-1", "Wind speed at {}")

# Temperature profiles (30 levels)
T_VARS = make_profile_vars("T", "degC", "Air temperature at {}")

# Specific humidity profiles (30 levels)
Q_VARS = make_profile_vars("q", "g kg-1", "Specific humidity at {}")

# RSL diagnostic variables
RSL_DIAG_VARS = [
    OutputVariable(
        name="L_MOD_RSL",
        unit="m",
        description="Obukhov length",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="zH_RSL",
        unit="m",
        description="Canyon depth",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Lc",
        unit="m",
        description="Canopy drag length scale",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="beta",
        unit="m",
        description="Beta coefficient from Harman 2012",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="zd_RSL",
        unit="m",
        description="Displacement height",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="z0_RSL",
        unit="m",
        description="Roughness length",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="elm",
        unit="m",
        description="Mixing length",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Scc",
        unit="-",
        description="Schmidt number for temperature and humidity",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="f",
        unit="-",
        description="RSL correction functions from Harman & Finnigan (2007, 2008)",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="UStar_RSL",
        unit="m s-1",
        description="Friction velocity used in RSL",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="UStar_heat",
        unit="m s-1",
        description="Friction velocity implied by RA_h",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="TStar_RSL",
        unit="K",
        description="Friction temperature used in RSL",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="FAI",
        unit="-",
        description="Frontal area index",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="PAI",
        unit="-",
        description="Plan area index",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="flag_RSL",
        unit="-",
        description="RSL calculation status flag (0=success, >0=iteration warning)",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
]


# Combine all RSL variables (Fortran outputs exactly 135 cols)
RSL_VARIABLES = Z_VARS + U_VARS + T_VARS + Q_VARS + RSL_DIAG_VARS
