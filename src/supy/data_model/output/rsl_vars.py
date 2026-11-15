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
    "z", "m", "Height above ground of profile level {0}; the level heights differ between the RST and MOST schemes (see flag_RSL)"
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
        description="Obukhov length used in the RSL profile calculation",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="zH_RSL",
        unit="m",
        description="Mean canyon height used in the RSL profile calculation",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Lc",
        unit="m",
        description="Canopy drag length scale. Missing when MOST is used; after resampling, the mean over the timesteps that used RST.",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="beta",
        unit="1",
        description="Roughness-sublayer attenuation coefficient beta of Harman (2012). Missing when MOST is used; after resampling, the mean over the timesteps that used RST.",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="zd_RSL",
        unit="m",
        description="Zero-plane displacement height from the RSL scheme (the SUEWS value when MOST is used)",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="z0_RSL",
        unit="m",
        description="Roughness length for momentum from the RSL scheme (the SUEWS value when MOST is used)",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="elm",
        unit="m",
        description="Mixing length at the canopy top, used for the in-canopy profiles. Missing when MOST is used; after resampling, the mean over the timesteps that used RST.",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Scc",
        unit="1",
        description="Schmidt number for temperature and humidity. Missing when MOST is used; after resampling, the mean over the timesteps that used RST.",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="f",
        unit="1",
        description=(
            "Roughness-sublayer parameter f of Harman and Finnigan (2008), "
            "derived from the Schmidt number Scc, which sets the in-canopy decay "
            "of temperature and humidity. Missing when MOST is used; after resampling, the mean over the timesteps that used RST."
        ),
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="UStar_RSL",
        unit="m s-1",
        description="Friction velocity used in the RSL profile calculation",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="UStar_heat",
        unit="m s-1",
        description="Friction velocity used for the temperature and humidity profiles: implied by the aerodynamic resistance for heat when MOST is used, and UStar_RSL with a floor of 0.15 m s-1 when RST is used",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="TStar_RSL",
        units_metadata="temperature: difference",
        unit="K",
        description="Temperature scale (friction temperature) used in the RSL profile calculation, from QH and UStar_heat",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="FAI",
        unit="1",
        description="Frontal area index",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="PAI",
        unit="1",
        description="Plan area index",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="flag_RSL",
        unit="1",
        description=(
            "Profile scheme used at the timestep: 1 when roughness-sublayer "
            "theory (RST) was used, 0 when Monin-Obukhov similarity theory "
            "(MOST) was used. After resampling, the value is the fraction of "
            "timesteps that used RST."
        ),
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.RSL,
        level=OutputLevel.DEFAULT,
    ),
]


# Combine all RSL variables (Fortran outputs exactly 135 cols)
RSL_VARIABLES = Z_VARS + U_VARS + T_VARS + Q_VARS + RSL_DIAG_VARS
