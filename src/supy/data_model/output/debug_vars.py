"""Debug output variables matching Fortran output exactly.

These variables are output in debug mode for diagnostic purposes.
Names match Fortran dataOutLineDebug assignment exactly (131 variables).
"""

from .variables import (
    OutputVariable,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)

# Variable definitions matching Fortran debug output exactly
# Format: (name, unit, aggregation_code, description)
# aggregation_code: A=AVERAGE, S=SUM
DEBUG_VARIABLE_DEFS = [
    # Test flag
    ("flag_test", "-", "A", "Debug test flag"),
    # Surface temperature by surface type (7)
    ("Ts_Paved", "degC", "A", "Surface temperature - paved"),
    ("Ts_Bldgs", "degC", "A", "Surface temperature - buildings"),
    ("Ts_EveTr", "degC", "A", "Surface temperature - evergreen trees"),
    ("Ts_DecTr", "degC", "A", "Surface temperature - deciduous trees"),
    ("Ts_Grass", "degC", "A", "Surface temperature - grass"),
    ("Ts_BSoil", "degC", "A", "Surface temperature - bare soil"),
    ("Ts_Water", "degC", "A", "Surface temperature - water"),
    # Net radiation by surface type (7)
    ("QN_Paved", "W m-2", "A", "Net radiation - paved"),
    ("QN_Bldgs", "W m-2", "A", "Net radiation - buildings"),
    ("QN_EveTr", "W m-2", "A", "Net radiation - evergreen trees"),
    ("QN_DecTr", "W m-2", "A", "Net radiation - deciduous trees"),
    ("QN_Grass", "W m-2", "A", "Net radiation - grass"),
    ("QN_BSoil", "W m-2", "A", "Net radiation - bare soil"),
    ("QN_Water", "W m-2", "A", "Net radiation - water"),
    # Storage heat flux by surface type (7)
    ("QS_Paved", "W m-2", "A", "Storage heat flux - paved"),
    ("QS_Bldgs", "W m-2", "A", "Storage heat flux - buildings"),
    ("QS_EveTr", "W m-2", "A", "Storage heat flux - evergreen trees"),
    ("QS_DecTr", "W m-2", "A", "Storage heat flux - deciduous trees"),
    ("QS_Grass", "W m-2", "A", "Storage heat flux - grass"),
    ("QS_BSoil", "W m-2", "A", "Storage heat flux - bare soil"),
    ("QS_Water", "W m-2", "A", "Storage heat flux - water"),
    # Initial latent heat flux by surface type (7)
    ("QE0_Paved", "W m-2", "A", "Initial latent heat flux - paved"),
    ("QE0_Bldgs", "W m-2", "A", "Initial latent heat flux - buildings"),
    ("QE0_EveTr", "W m-2", "A", "Initial latent heat flux - evergreen trees"),
    ("QE0_DecTr", "W m-2", "A", "Initial latent heat flux - deciduous trees"),
    ("QE0_Grass", "W m-2", "A", "Initial latent heat flux - grass"),
    ("QE0_BSoil", "W m-2", "A", "Initial latent heat flux - bare soil"),
    ("QE0_Water", "W m-2", "A", "Initial latent heat flux - water"),
    # Latent heat flux by surface type (7)
    ("QE_Paved", "W m-2", "A", "Latent heat flux - paved"),
    ("QE_Bldgs", "W m-2", "A", "Latent heat flux - buildings"),
    ("QE_EveTr", "W m-2", "A", "Latent heat flux - evergreen trees"),
    ("QE_DecTr", "W m-2", "A", "Latent heat flux - deciduous trees"),
    ("QE_Grass", "W m-2", "A", "Latent heat flux - grass"),
    ("QE_BSoil", "W m-2", "A", "Latent heat flux - bare soil"),
    ("QE_Water", "W m-2", "A", "Latent heat flux - water"),
    # Sensible heat flux by surface type (7)
    ("QH_Paved", "W m-2", "A", "Sensible heat flux - paved"),
    ("QH_Bldgs", "W m-2", "A", "Sensible heat flux - buildings"),
    ("QH_EveTr", "W m-2", "A", "Sensible heat flux - evergreen trees"),
    ("QH_DecTr", "W m-2", "A", "Sensible heat flux - deciduous trees"),
    ("QH_Grass", "W m-2", "A", "Sensible heat flux - grass"),
    ("QH_BSoil", "W m-2", "A", "Sensible heat flux - bare soil"),
    ("QH_Water", "W m-2", "A", "Sensible heat flux - water"),
    # Water use by surface type (7)
    ("wu_Paved", "mm", "A", "Water use - paved"),
    ("wu_Bldgs", "mm", "A", "Water use - buildings"),
    ("wu_EveTr", "mm", "A", "Water use - evergreen trees"),
    ("wu_DecTr", "mm", "A", "Water use - deciduous trees"),
    ("wu_Grass", "mm", "A", "Water use - grass"),
    ("wu_BSoil", "mm", "A", "Water use - bare soil"),
    ("wu_Water", "mm", "A", "Water use - water"),
    # Initial evaporation by surface type (7)
    ("ev0_Paved", "mm", "A", "Initial evaporation - paved"),
    ("ev0_Bldgs", "mm", "A", "Initial evaporation - buildings"),
    ("ev0_EveTr", "mm", "A", "Initial evaporation - evergreen trees"),
    ("ev0_DecTr", "mm", "A", "Initial evaporation - deciduous trees"),
    ("ev0_Grass", "mm", "A", "Initial evaporation - grass"),
    ("ev0_BSoil", "mm", "A", "Initial evaporation - bare soil"),
    ("ev0_Water", "mm", "A", "Initial evaporation - water"),
    # Evaporation by surface type (7)
    ("ev_Paved", "mm", "A", "Evaporation - paved"),
    ("ev_Bldgs", "mm", "A", "Evaporation - buildings"),
    ("ev_EveTr", "mm", "A", "Evaporation - evergreen trees"),
    ("ev_DecTr", "mm", "A", "Evaporation - deciduous trees"),
    ("ev_Grass", "mm", "A", "Evaporation - grass"),
    ("ev_BSoil", "mm", "A", "Evaporation - bare soil"),
    ("ev_Water", "mm", "A", "Evaporation - water"),
    # Drainage by surface type (7)
    ("drain_Paved", "mm", "A", "Drainage - paved"),
    ("drain_Bldgs", "mm", "A", "Drainage - buildings"),
    ("drain_EveTr", "mm", "A", "Drainage - evergreen trees"),
    ("drain_DecTr", "mm", "A", "Drainage - deciduous trees"),
    ("drain_Grass", "mm", "A", "Drainage - grass"),
    ("drain_BSoil", "mm", "A", "Drainage - bare soil"),
    ("drain_Water", "mm", "A", "Drainage - water"),
    # Previous surface state by surface type (7)
    ("st_Paved_prev", "mm", "A", "Previous surface state - paved"),
    ("st_Bldgs_prev", "mm", "A", "Previous surface state - buildings"),
    ("st_EveTr_prev", "mm", "A", "Previous surface state - evergreen trees"),
    ("st_DecTr_prev", "mm", "A", "Previous surface state - deciduous trees"),
    ("st_Grass_prev", "mm", "A", "Previous surface state - grass"),
    ("st_BSoil_prev", "mm", "A", "Previous surface state - bare soil"),
    ("st_Water_prev", "mm", "A", "Previous surface state - water"),
    # Next surface state by surface type (7)
    ("st_Paved_next", "mm", "A", "Next surface state - paved"),
    ("st_Bldgs_next", "mm", "A", "Next surface state - buildings"),
    ("st_EveTr_next", "mm", "A", "Next surface state - evergreen trees"),
    ("st_DecTr_next", "mm", "A", "Next surface state - deciduous trees"),
    ("st_Grass_next", "mm", "A", "Next surface state - grass"),
    ("st_BSoil_next", "mm", "A", "Next surface state - bare soil"),
    ("st_Water_next", "mm", "A", "Next surface state - water"),
    # Previous soil state by surface type (7)
    ("ss_Paved_prev", "mm", "A", "Previous soil state - paved"),
    ("ss_Bldgs_prev", "mm", "A", "Previous soil state - buildings"),
    ("ss_EveTr_prev", "mm", "A", "Previous soil state - evergreen trees"),
    ("ss_DecTr_prev", "mm", "A", "Previous soil state - deciduous trees"),
    ("ss_Grass_prev", "mm", "A", "Previous soil state - grass"),
    ("ss_BSoil_prev", "mm", "A", "Previous soil state - bare soil"),
    ("ss_Water_prev", "mm", "A", "Previous soil state - water"),
    # Next soil state by surface type (7)
    ("ss_Paved_next", "mm", "A", "Next soil state - paved"),
    ("ss_Bldgs_next", "mm", "A", "Next soil state - buildings"),
    ("ss_EveTr_next", "mm", "A", "Next soil state - evergreen trees"),
    ("ss_DecTr_next", "mm", "A", "Next soil state - deciduous trees"),
    ("ss_Grass_next", "mm", "A", "Next soil state - grass"),
    ("ss_BSoil_next", "mm", "A", "Next soil state - bare soil"),
    ("ss_Water_next", "mm", "A", "Next soil state - water"),
    # Resistance terms (4)
    ("RS", "s m-1", "A", "Surface resistance"),
    ("RA", "s m-1", "A", "Aerodynamic resistance"),
    ("RB", "s m-1", "A", "Boundary layer resistance"),
    ("RAsnow", "s m-1", "A", "Aerodynamic resistance for snow"),
    # Surface resistance by surface type (7)
    ("RSS_Paved", "s m-1", "A", "Surface resistance - paved"),
    ("RSS_Bldgs", "s m-1", "A", "Surface resistance - buildings"),
    ("RSS_EveTr", "s m-1", "A", "Surface resistance - evergreen trees"),
    ("RSS_DecTr", "s m-1", "A", "Surface resistance - deciduous trees"),
    ("RSS_Grass", "s m-1", "A", "Surface resistance - grass"),
    ("RSS_BSoil", "s m-1", "A", "Surface resistance - bare soil"),
    ("RSS_Water", "s m-1", "A", "Surface resistance - water"),
    # Additional debug variables (9)
    ("vsmd", "mm", "A", "Volumetric soil moisture deficit"),
    ("dsp", "mm", "A", "Depression storage ponding"),
    ("G_sm", "-", "A", "Soil moisture conductance factor"),
    ("x_exp", "-", "A", "Exponential factor"),
    ("g_kdown", "-", "A", "Conductance factor - downward radiation"),
    ("g_dq", "-", "A", "Conductance factor - humidity deficit"),
    ("g_ta", "-", "A", "Conductance factor - air temperature"),
    ("g_smd", "-", "A", "Conductance factor - soil moisture deficit"),
    ("g_lai", "-", "A", "Conductance factor - LAI"),
    # Atmospheric variables (6)
    ("vpd_hPa", "hPa", "A", "Vapour pressure deficit"),
    ("lv_J_kg", "J kg-1", "A", "Latent heat of vaporisation"),
    ("avdens", "kg m-3", "A", "Air density"),
    ("avcp", "J kg-1 K-1", "A", "Specific heat of air"),
    ("s_hPa", "hPa K-1", "A", "Slope of saturation vapour pressure"),
    ("psyc_hPa", "hPa K-1", "A", "Psychrometric constant"),
    # Iteration counter (uses SUM aggregation)
    ("iter", "-", "S", "Iteration counter"),
    # FAI variables (5)
    ("FAIBldg_use", "-", "A", "Frontal area index - buildings used"),
    ("FAIEveTree_use", "-", "A", "Frontal area index - evergreen trees used"),
    ("FAIDecTree_use", "-", "A", "Frontal area index - deciduous trees used"),
    ("FAI", "-", "A", "Total frontal area index"),
    ("dqndt", "W m-2", "A", "Rate of change of net radiation"),
]


def _get_agg_method(code: str) -> AggregationMethod:
    """Convert aggregation code to enum."""
    return {
        "A": AggregationMethod.AVERAGE,
        "S": AggregationMethod.SUM,
        "L": AggregationMethod.LAST,
        "T": AggregationMethod.TIME,
    }[code]


DEBUG_VARIABLES = [
    OutputVariable(
        name=name,
        unit=unit,
        description=description,
        aggregation=_get_agg_method(aggm),
        group=OutputGroup.DEBUG,
        level=OutputLevel.DEFAULT,
    )
    for name, unit, aggm, description in DEBUG_VARIABLE_DEFS
]
