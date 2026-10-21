"""RSL (Roughness Sublayer) output variables.

These variables provide diagnostic vertical profiles of height, wind speed,
temperature and specific humidity at 30 levels between the surface and the
forcing height, plus the diagnostics of the profile calculation. The profiles
follow roughness-sublayer theory (RST) or Monin-Obukhov similarity theory
(MOST) according to ``flag_RSL``.
"""

from .variables import (
    OutputVariable,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)


def make_profile_vars(base_name, unit, description_template):
    """Helper to create profile variables for 30 vertical levels.

    The level layout depends on the profile scheme (Fortran
    `setup_RSL_heights` and `setup_MOST_heights`):
    - RST: level 1 at 1% of the effective canopy height (capped just below
      2 m), levels 2-9 closing in on half the canopy height (level 10),
      levels 11-20 closing in on the canopy top (level 20), levels 21-30
      from just above the canopy top to the forcing height;
    - MOST: 30 levels spaced logarithmically from just above zd + z0 to the
      forcing height, with 2 m and 10 m included when in range.
    """
    return [
        OutputVariable(
            name=f"{base_name}_{i}",
            unit=unit,
            description=description_template.format(i),
            aggregation=AggregationMethod.AVERAGE,
            group=OutputGroup.RSL,
            level=OutputLevel.DEFAULT,
        )
        for i in range(1, 31)
    ]


# Height profiles (30 levels)
Z_VARS = make_profile_vars(
    "z", "m", "Height above ground of profile level {0} (layout depends on flag_RSL)"
)

# Wind speed profiles (30 levels)
U_VARS = make_profile_vars(
    "U", "m s-1", "Wind speed at profile level {0} (height given by z_{0})"
)

# Temperature profiles (30 levels)
T_VARS = make_profile_vars(
    "T", "degC", "Air temperature at profile level {0} (height given by z_{0})"
)

# Specific humidity profiles (30 levels)
Q_VARS = make_profile_vars(
    "q", "g kg-1", "Specific humidity at profile level {0} (height given by z_{0})"
)

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
        unit="-",
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
        description=(
            "Roughness-sublayer parameter f of Harman and Finnigan (2008), "
            "derived from the Schmidt number Scc, setting the in-canopy decay "
            "of temperature and humidity; missing when MOST is used"
        ),
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
        description=(
            "Profile scheme used at the timestep: 1 when roughness-sublayer "
            "theory (RST) was used, 0 when Monin-Obukhov similarity theory "
            "(MOST) was used; averaged on resampling, so the fraction of "
            "timesteps that used RST"
        ),
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
]


# Combine all RSL variables (Fortran outputs exactly 135 cols)
RSL_VARIABLES = Z_VARS + U_VARS + T_VARS + Q_VARS + RSL_DIAG_VARS
