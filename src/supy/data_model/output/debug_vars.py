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
    ("Ts_Paved", "degC", "A", "Surface temperature for paved surface"),
    ("Ts_Bldgs", "degC", "A", "Surface temperature for building surface"),
    ("Ts_EveTr", "degC", "A", "Surface temperature for evergreen tree surface"),
    ("Ts_DecTr", "degC", "A", "Surface temperature for deciduous tree surface"),
    ("Ts_Grass", "degC", "A", "Surface temperature for grass surface"),
    ("Ts_BSoil", "degC", "A", "Surface temperature for bare soil surface"),
    ("Ts_Water", "degC", "A", "Surface temperature for water surface"),
    # Net radiation by surface type (7)
    ("QN_Paved", "W m-2", "A", "Net all-wave radiation for paved surface"),
    ("QN_Bldgs", "W m-2", "A", "Net all-wave radiation for building surface"),
    ("QN_EveTr", "W m-2", "A", "Net all-wave radiation for evergreen tree surface"),
    ("QN_DecTr", "W m-2", "A", "Net all-wave radiation for deciduous tree surface"),
    ("QN_Grass", "W m-2", "A", "Net all-wave radiation for grass surface"),
    ("QN_BSoil", "W m-2", "A", "Net all-wave radiation for bare soil surface"),
    ("QN_Water", "W m-2", "A", "Net all-wave radiation for water surface"),
    # Storage heat flux by surface type (7)
    ("QS_Paved", "W m-2", "A", "Storage heat flux for paved surface"),
    ("QS_Bldgs", "W m-2", "A", "Storage heat flux for building surface"),
    ("QS_EveTr", "W m-2", "A", "Storage heat flux for evergreen tree surface"),
    ("QS_DecTr", "W m-2", "A", "Storage heat flux for deciduous tree surface"),
    ("QS_Grass", "W m-2", "A", "Storage heat flux for grass surface"),
    ("QS_BSoil", "W m-2", "A", "Storage heat flux for bare soil surface"),
    ("QS_Water", "W m-2", "A", "Storage heat flux for water surface"),
    # Initial latent heat flux by surface type (7)
    ("QE0_Paved", "W m-2", "A", "Initial latent heat flux for paved surface"),
    ("QE0_Bldgs", "W m-2", "A", "Initial latent heat flux for building surface"),
    ("QE0_EveTr", "W m-2", "A", "Initial latent heat flux for evergreen tree surface"),
    ("QE0_DecTr", "W m-2", "A", "Initial latent heat flux for deciduous tree surface"),
    ("QE0_Grass", "W m-2", "A", "Initial latent heat flux for grass surface"),
    ("QE0_BSoil", "W m-2", "A", "Initial latent heat flux for bare soil surface"),
    ("QE0_Water", "W m-2", "A", "Initial latent heat flux for water surface"),
    # Latent heat flux by surface type (7)
    ("QE_Paved", "W m-2", "A", "Latent heat flux for paved surface"),
    ("QE_Bldgs", "W m-2", "A", "Latent heat flux for building surface"),
    ("QE_EveTr", "W m-2", "A", "Latent heat flux for evergreen tree surface"),
    ("QE_DecTr", "W m-2", "A", "Latent heat flux for deciduous tree surface"),
    ("QE_Grass", "W m-2", "A", "Latent heat flux for grass surface"),
    ("QE_BSoil", "W m-2", "A", "Latent heat flux for bare soil surface"),
    ("QE_Water", "W m-2", "A", "Latent heat flux for water surface"),
    # Sensible heat flux by surface type (7)
    ("QH_Paved", "W m-2", "A", "Sensible heat flux for paved surface"),
    ("QH_Bldgs", "W m-2", "A", "Sensible heat flux for building surface"),
    ("QH_EveTr", "W m-2", "A", "Sensible heat flux for evergreen tree surface"),
    ("QH_DecTr", "W m-2", "A", "Sensible heat flux for deciduous tree surface"),
    ("QH_Grass", "W m-2", "A", "Sensible heat flux for grass surface"),
    ("QH_BSoil", "W m-2", "A", "Sensible heat flux for bare soil surface"),
    ("QH_Water", "W m-2", "A", "Sensible heat flux for water surface"),
    # Water use by surface type (7)
    ("wu_Paved", "mm", "A", "Water use for paved surface"),
    ("wu_Bldgs", "mm", "A", "Water use for building surface"),
    ("wu_EveTr", "mm", "A", "Water use for evergreen tree surface"),
    ("wu_DecTr", "mm", "A", "Water use for deciduous tree surface"),
    ("wu_Grass", "mm", "A", "Water use for grass surface"),
    ("wu_BSoil", "mm", "A", "Water use for bare soil surface"),
    ("wu_Water", "mm", "A", "Water use for water surface"),
    # Initial evaporation by surface type (7)
    ("ev0_Paved", "mm", "A", "Initial evaporation for paved surface"),
    ("ev0_Bldgs", "mm", "A", "Initial evaporation for building surface"),
    ("ev0_EveTr", "mm", "A", "Initial evaporation for evergreen tree surface"),
    ("ev0_DecTr", "mm", "A", "Initial evaporation for deciduous tree surface"),
    ("ev0_Grass", "mm", "A", "Initial evaporation for grass surface"),
    ("ev0_BSoil", "mm", "A", "Initial evaporation for bare soil surface"),
    ("ev0_Water", "mm", "A", "Initial evaporation for water surface"),
    # Evaporation by surface type (7)
    ("ev_Paved", "mm", "A", "Evaporation for paved surface"),
    ("ev_Bldgs", "mm", "A", "Evaporation for building surface"),
    ("ev_EveTr", "mm", "A", "Evaporation for evergreen tree surface"),
    ("ev_DecTr", "mm", "A", "Evaporation for deciduous tree surface"),
    ("ev_Grass", "mm", "A", "Evaporation for grass surface"),
    ("ev_BSoil", "mm", "A", "Evaporation for bare soil surface"),
    ("ev_Water", "mm", "A", "Evaporation for water surface"),
    # Drainage by surface type (7)
    ("drain_Paved", "mm", "A", "Drainage for paved surface"),
    ("drain_Bldgs", "mm", "A", "Drainage for building surface"),
    ("drain_EveTr", "mm", "A", "Drainage for evergreen tree surface"),
    ("drain_DecTr", "mm", "A", "Drainage for deciduous tree surface"),
    ("drain_Grass", "mm", "A", "Drainage for grass surface"),
    ("drain_BSoil", "mm", "A", "Drainage for bare soil surface"),
    ("drain_Water", "mm", "A", "Drainage for water surface"),
    # Previous surface state by surface type (7)
    ("st_Paved_prev", "mm", "A", "Surface wetness state for paved surface at the start of the timestep"),
    ("st_Bldgs_prev", "mm", "A", "Surface wetness state for building surface at the start of the timestep"),
    ("st_EveTr_prev", "mm", "A", "Surface wetness state for evergreen tree surface at the start of the timestep"),
    ("st_DecTr_prev", "mm", "A", "Surface wetness state for deciduous tree surface at the start of the timestep"),
    ("st_Grass_prev", "mm", "A", "Surface wetness state for grass surface at the start of the timestep"),
    ("st_BSoil_prev", "mm", "A", "Surface wetness state for bare soil surface at the start of the timestep"),
    ("st_Water_prev", "mm", "A", "Surface wetness state for water surface at the start of the timestep"),
    # Next surface state by surface type (7)
    ("st_Paved_next", "mm", "A", "Surface wetness state for paved surface at the end of the timestep"),
    ("st_Bldgs_next", "mm", "A", "Surface wetness state for building surface at the end of the timestep"),
    ("st_EveTr_next", "mm", "A", "Surface wetness state for evergreen tree surface at the end of the timestep"),
    ("st_DecTr_next", "mm", "A", "Surface wetness state for deciduous tree surface at the end of the timestep"),
    ("st_Grass_next", "mm", "A", "Surface wetness state for grass surface at the end of the timestep"),
    ("st_BSoil_next", "mm", "A", "Surface wetness state for bare soil surface at the end of the timestep"),
    ("st_Water_next", "mm", "A", "Surface wetness state for water surface at the end of the timestep"),
    # Previous soil state by surface type (7)
    ("ss_Paved_prev", "mm", "A", "Soil water store for paved surface at the start of the timestep"),
    ("ss_Bldgs_prev", "mm", "A", "Soil water store for building surface at the start of the timestep"),
    ("ss_EveTr_prev", "mm", "A", "Soil water store for evergreen tree surface at the start of the timestep"),
    ("ss_DecTr_prev", "mm", "A", "Soil water store for deciduous tree surface at the start of the timestep"),
    ("ss_Grass_prev", "mm", "A", "Soil water store for grass surface at the start of the timestep"),
    ("ss_BSoil_prev", "mm", "A", "Soil water store for bare soil surface at the start of the timestep"),
    ("ss_Water_prev", "mm", "A", "Soil water store for water surface at the start of the timestep"),
    # Next soil state by surface type (7)
    ("ss_Paved_next", "mm", "A", "Soil water store for paved surface at the end of the timestep"),
    ("ss_Bldgs_next", "mm", "A", "Soil water store for building surface at the end of the timestep"),
    ("ss_EveTr_next", "mm", "A", "Soil water store for evergreen tree surface at the end of the timestep"),
    ("ss_DecTr_next", "mm", "A", "Soil water store for deciduous tree surface at the end of the timestep"),
    ("ss_Grass_next", "mm", "A", "Soil water store for grass surface at the end of the timestep"),
    ("ss_BSoil_next", "mm", "A", "Soil water store for bare soil surface at the end of the timestep"),
    ("ss_Water_next", "mm", "A", "Soil water store for water surface at the end of the timestep"),
    # Resistance terms (4)
    ("RS", "s m-1", "A", "Surface resistance"),
    ("RA", "s m-1", "A", "Aerodynamic resistance"),
    ("RB", "s m-1", "A", "Boundary-layer resistance"),
    ("RAsnow", "s m-1", "A", "Aerodynamic resistance for snow"),
    # Surface resistance by surface type (7)
    ("RSS_Paved", "s m-1", "A", "Surface resistance for paved surface"),
    ("RSS_Bldgs", "s m-1", "A", "Surface resistance for building surface"),
    ("RSS_EveTr", "s m-1", "A", "Surface resistance for evergreen tree surface"),
    ("RSS_DecTr", "s m-1", "A", "Surface resistance for deciduous tree surface"),
    ("RSS_Grass", "s m-1", "A", "Surface resistance for grass surface"),
    ("RSS_BSoil", "s m-1", "A", "Surface resistance for bare soil surface"),
    ("RSS_Water", "s m-1", "A", "Surface resistance for water surface"),
    # Additional debug variables (9)
    ("vsmd", "mm", "A", "Volumetric soil moisture deficit"),
    ("dsp", "mm", "A", "Depression storage ponding"),
    ("G_sm", "-", "A", "Soil moisture conductance factor"),
    ("x_exp", "-", "A", "Exponential factor"),
    ("g_kdown", "-", "A", "Conductance factor for incoming shortwave radiation"),
    ("g_dq", "-", "A", "Conductance factor for humidity deficit"),
    ("g_ta", "-", "A", "Conductance factor for air temperature"),
    ("g_smd", "-", "A", "Conductance factor for soil moisture deficit"),
    ("g_lai", "-", "A", "Conductance factor for leaf area index"),
    # Atmospheric variables (6)
    ("vpd_hPa", "hPa", "A", "Vapour pressure deficit"),
    ("lv_J_kg", "J kg-1", "A", "Latent heat of vaporisation"),
    ("avdens", "kg m-3", "A", "Air density"),
    ("avcp", "J kg-1 K-1", "A", "Specific heat capacity of air"),
    ("s_hPa", "hPa K-1", "A", "Slope of the saturation vapour pressure curve"),
    ("psyc_hPa", "hPa K-1", "A", "Psychrometric constant"),
    # Iteration counter (uses SUM aggregation)
    ("iter", "-", "S", "Iteration counter"),
    # FAI variables (5)
    ("FAIBldg_use", "-", "A", "Frontal area index of buildings, as used by the model"),
    ("FAIEveTree_use", "-", "A", "Frontal area index of evergreen trees, as used by the model"),
    ("FAIDecTree_use", "-", "A", "Frontal area index of deciduous trees, as used by the model"),
    ("FAI", "-", "A", "Total frontal area index"),
    ("dqndt", "W m-2", "A", "Rate of change of net all-wave radiation"),
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
