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
# Format: (name, unit, aggregation_code, description, interval_water_flux)
# aggregation_code: A=AVERAGE, S=SUM
DEBUG_VARIABLE_DEFS = [
    # Test flag
    ("flag_test", "1", "A", "Debug flag: 1 when state snapshots between modules are saved", False),
    # Surface temperature by surface type (7)
    ("Ts_Paved", "degC", "A", "Surface temperature for paved surface", False),
    ("Ts_Bldgs", "degC", "A", "Surface temperature for building surface", False),
    ("Ts_EveTr", "degC", "A", "Surface temperature for evergreen tree surface", False),
    ("Ts_DecTr", "degC", "A", "Surface temperature for deciduous tree surface", False),
    ("Ts_Grass", "degC", "A", "Surface temperature for grass surface", False),
    ("Ts_BSoil", "degC", "A", "Surface temperature for bare soil surface", False),
    ("Ts_Water", "degC", "A", "Surface temperature for water surface", False),
    # Net radiation by surface type (7)
    ("QN_Paved", "W m-2", "A", "Net all-wave radiation for paved surface", False),
    ("QN_Bldgs", "W m-2", "A", "Net all-wave radiation for building surface", False),
    ("QN_EveTr", "W m-2", "A", "Net all-wave radiation for evergreen tree surface", False),
    ("QN_DecTr", "W m-2", "A", "Net all-wave radiation for deciduous tree surface", False),
    ("QN_Grass", "W m-2", "A", "Net all-wave radiation for grass surface", False),
    ("QN_BSoil", "W m-2", "A", "Net all-wave radiation for bare soil surface", False),
    ("QN_Water", "W m-2", "A", "Net all-wave radiation for water surface", False),
    # Storage heat flux by surface type (7)
    ("QS_Paved", "W m-2", "A", "Storage heat flux for paved surface", False),
    ("QS_Bldgs", "W m-2", "A", "Storage heat flux for building surface", False),
    ("QS_EveTr", "W m-2", "A", "Storage heat flux for evergreen tree surface", False),
    ("QS_DecTr", "W m-2", "A", "Storage heat flux for deciduous tree surface", False),
    ("QS_Grass", "W m-2", "A", "Storage heat flux for grass surface", False),
    ("QS_BSoil", "W m-2", "A", "Storage heat flux for bare soil surface", False),
    ("QS_Water", "W m-2", "A", "Storage heat flux for water surface", False),
    # Initial latent heat flux by surface type (7)
    ("QE0_Paved", "W m-2", "A", "Latent heat flux for paved surface before the water-availability limit", False),
    ("QE0_Bldgs", "W m-2", "A", "Latent heat flux for building surface before the water-availability limit", False),
    ("QE0_EveTr", "W m-2", "A", "Latent heat flux for evergreen tree surface before the water-availability limit", False),
    ("QE0_DecTr", "W m-2", "A", "Latent heat flux for deciduous tree surface before the water-availability limit", False),
    ("QE0_Grass", "W m-2", "A", "Latent heat flux for grass surface before the water-availability limit", False),
    ("QE0_BSoil", "W m-2", "A", "Latent heat flux for bare soil surface before the water-availability limit", False),
    ("QE0_Water", "W m-2", "A", "Latent heat flux for water surface before the water-availability limit", False),
    # Latent heat flux by surface type (7)
    ("QE_Paved", "W m-2", "A", "Latent heat flux for paved surface", False),
    ("QE_Bldgs", "W m-2", "A", "Latent heat flux for building surface", False),
    ("QE_EveTr", "W m-2", "A", "Latent heat flux for evergreen tree surface", False),
    ("QE_DecTr", "W m-2", "A", "Latent heat flux for deciduous tree surface", False),
    ("QE_Grass", "W m-2", "A", "Latent heat flux for grass surface", False),
    ("QE_BSoil", "W m-2", "A", "Latent heat flux for bare soil surface", False),
    ("QE_Water", "W m-2", "A", "Latent heat flux for water surface", False),
    # Sensible heat flux by surface type (7)
    ("QH_Paved", "W m-2", "A", "Sensible heat flux for paved surface", False),
    ("QH_Bldgs", "W m-2", "A", "Sensible heat flux for building surface", False),
    ("QH_EveTr", "W m-2", "A", "Sensible heat flux for evergreen tree surface", False),
    ("QH_DecTr", "W m-2", "A", "Sensible heat flux for deciduous tree surface", False),
    ("QH_Grass", "W m-2", "A", "Sensible heat flux for grass surface", False),
    ("QH_BSoil", "W m-2", "A", "Sensible heat flux for bare soil surface", False),
    ("QH_Water", "W m-2", "A", "Sensible heat flux for water surface", False),
    # Water use by surface type (7)
    ("wu_Paved", "kg m-2 s-1", "S", "Water use for paved surface", True),
    ("wu_Bldgs", "kg m-2 s-1", "S", "Water use for building surface", True),
    ("wu_EveTr", "kg m-2 s-1", "S", "Water use for evergreen tree surface", True),
    ("wu_DecTr", "kg m-2 s-1", "S", "Water use for deciduous tree surface", True),
    ("wu_Grass", "kg m-2 s-1", "S", "Water use for grass surface", True),
    ("wu_BSoil", "kg m-2 s-1", "S", "Water use for bare soil surface", True),
    ("wu_Water", "kg m-2 s-1", "S", "Water use for water surface", True),
    # Initial evaporation by surface type (7)
    ("ev0_Paved", "kg m-2 s-1", "S", "Evaporation for paved surface before the water-availability limit", True),
    ("ev0_Bldgs", "kg m-2 s-1", "S", "Evaporation for building surface before the water-availability limit", True),
    ("ev0_EveTr", "kg m-2 s-1", "S", "Evaporation for evergreen tree surface before the water-availability limit", True),
    ("ev0_DecTr", "kg m-2 s-1", "S", "Evaporation for deciduous tree surface before the water-availability limit", True),
    ("ev0_Grass", "kg m-2 s-1", "S", "Evaporation for grass surface before the water-availability limit", True),
    ("ev0_BSoil", "kg m-2 s-1", "S", "Evaporation for bare soil surface before the water-availability limit", True),
    ("ev0_Water", "kg m-2 s-1", "S", "Evaporation for water surface before the water-availability limit", True),
    # Evaporation by surface type (7)
    ("ev_Paved", "kg m-2 s-1", "S", "Evaporation for paved surface", True),
    ("ev_Bldgs", "kg m-2 s-1", "S", "Evaporation for building surface", True),
    ("ev_EveTr", "kg m-2 s-1", "S", "Evaporation for evergreen tree surface", True),
    ("ev_DecTr", "kg m-2 s-1", "S", "Evaporation for deciduous tree surface", True),
    ("ev_Grass", "kg m-2 s-1", "S", "Evaporation for grass surface", True),
    ("ev_BSoil", "kg m-2 s-1", "S", "Evaporation for bare soil surface", True),
    ("ev_Water", "kg m-2 s-1", "S", "Evaporation for water surface", True),
    # Drainage by surface type (7)
    ("drain_Paved", "kg m-2 s-1", "S", "Drainage for paved surface", True),
    ("drain_Bldgs", "kg m-2 s-1", "S", "Drainage for building surface", True),
    ("drain_EveTr", "kg m-2 s-1", "S", "Drainage for evergreen tree surface", True),
    ("drain_DecTr", "kg m-2 s-1", "S", "Drainage for deciduous tree surface", True),
    ("drain_Grass", "kg m-2 s-1", "S", "Drainage for grass surface", True),
    ("drain_BSoil", "kg m-2 s-1", "S", "Drainage for bare soil surface", True),
    ("drain_Water", "kg m-2 s-1", "S", "Drainage for water surface", True),
    # Previous surface state by surface type (7)
    ("st_Paved_prev", "kg m-2", "A", "Surface wetness state for paved surface at the start of the timestep", False),
    ("st_Bldgs_prev", "kg m-2", "A", "Surface wetness state for building surface at the start of the timestep", False),
    ("st_EveTr_prev", "kg m-2", "A", "Surface wetness state for evergreen tree surface at the start of the timestep", False),
    ("st_DecTr_prev", "kg m-2", "A", "Surface wetness state for deciduous tree surface at the start of the timestep", False),
    ("st_Grass_prev", "kg m-2", "A", "Surface wetness state for grass surface at the start of the timestep", False),
    ("st_BSoil_prev", "kg m-2", "A", "Surface wetness state for bare soil surface at the start of the timestep", False),
    ("st_Water_prev", "kg m-2", "A", "Surface wetness state for water surface at the start of the timestep", False),
    # Next surface state by surface type (7)
    ("st_Paved_next", "kg m-2", "A", "Surface wetness state for paved surface at the end of the timestep", False),
    ("st_Bldgs_next", "kg m-2", "A", "Surface wetness state for building surface at the end of the timestep", False),
    ("st_EveTr_next", "kg m-2", "A", "Surface wetness state for evergreen tree surface at the end of the timestep", False),
    ("st_DecTr_next", "kg m-2", "A", "Surface wetness state for deciduous tree surface at the end of the timestep", False),
    ("st_Grass_next", "kg m-2", "A", "Surface wetness state for grass surface at the end of the timestep", False),
    ("st_BSoil_next", "kg m-2", "A", "Surface wetness state for bare soil surface at the end of the timestep", False),
    ("st_Water_next", "kg m-2", "A", "Surface wetness state for water surface at the end of the timestep", False),
    # Previous soil state by surface type (7)
    ("ss_Paved_prev", "kg m-2", "A", "Soil water store for paved surface at the start of the timestep", False),
    ("ss_Bldgs_prev", "kg m-2", "A", "Soil water store for building surface at the start of the timestep", False),
    ("ss_EveTr_prev", "kg m-2", "A", "Soil water store for evergreen tree surface at the start of the timestep", False),
    ("ss_DecTr_prev", "kg m-2", "A", "Soil water store for deciduous tree surface at the start of the timestep", False),
    ("ss_Grass_prev", "kg m-2", "A", "Soil water store for grass surface at the start of the timestep", False),
    ("ss_BSoil_prev", "kg m-2", "A", "Soil water store for bare soil surface at the start of the timestep", False),
    ("ss_Water_prev", "kg m-2", "A", "Soil water store for water surface at the start of the timestep", False),
    # Next soil state by surface type (7)
    ("ss_Paved_next", "kg m-2", "A", "Soil water store for paved surface at the end of the timestep", False),
    ("ss_Bldgs_next", "kg m-2", "A", "Soil water store for building surface at the end of the timestep", False),
    ("ss_EveTr_next", "kg m-2", "A", "Soil water store for evergreen tree surface at the end of the timestep", False),
    ("ss_DecTr_next", "kg m-2", "A", "Soil water store for deciduous tree surface at the end of the timestep", False),
    ("ss_Grass_next", "kg m-2", "A", "Soil water store for grass surface at the end of the timestep", False),
    ("ss_BSoil_next", "kg m-2", "A", "Soil water store for bare soil surface at the end of the timestep", False),
    ("ss_Water_next", "kg m-2", "A", "Soil water store for water surface at the end of the timestep", False),
    # Resistance terms (4)
    ("RS", "s m-1", "A", "Surface resistance", False),
    ("RA", "s m-1", "A", "Aerodynamic resistance", False),
    ("RB", "s m-1", "A", "Leaf boundary-layer resistance", False),
    ("RAsnow", "s m-1", "A", "Aerodynamic resistance for snow", False),
    # Surface resistance by surface type (7)
    ("RSS_Paved", "s m-1", "A", "Surface resistance for paved surface", False),
    ("RSS_Bldgs", "s m-1", "A", "Surface resistance for building surface", False),
    ("RSS_EveTr", "s m-1", "A", "Surface resistance for evergreen tree surface", False),
    ("RSS_DecTr", "s m-1", "A", "Surface resistance for deciduous tree surface", False),
    ("RSS_Grass", "s m-1", "A", "Surface resistance for grass surface", False),
    ("RSS_BSoil", "s m-1", "A", "Surface resistance for bare soil surface", False),
    ("RSS_Water", "s m-1", "A", "Surface resistance for water surface", False),
    # Additional debug variables (9)
    ("vsmd", "kg m-2", "A", "Soil moisture deficit of the vegetated surfaces", False),
    ("dsp", "kg m-2", "A", "Soil moisture deficit threshold in the soil-moisture conductance function", False),
    ("G_sm", "1", "A", "Conductance parameter for soil moisture (G_sm, from the site configuration)", False),
    ("x_exp", "1", "A", "Argument of the exponential in the soil-moisture conductance function, G_sm*(vsmd - dsp), before any clipping at zero", False),
    ("g_kdown", "1", "A", "Conductance factor for incoming shortwave radiation", False),
    ("g_dq", "1", "A", "Conductance factor for humidity deficit", False),
    ("g_ta", "1", "A", "Conductance factor for air temperature", False),
    ("g_smd", "1", "A", "Conductance factor for soil moisture deficit", False),
    ("g_lai", "1", "A", "Conductance factor for leaf area index", False),
    # Atmospheric variables (6)
    ("vpd_hPa", "hPa", "A", "Vapour pressure deficit", False),
    ("lv_J_kg", "J kg-1", "A", "Latent heat of vaporisation", False),
    ("avdens", "kg m-3", "A", "Air density", False),
    ("avcp", "J kg-1 K-1", "A", "Specific heat capacity of air", False),
    ("s_hPa", "hPa K-1", "A", "Slope of the saturation vapour pressure curve", False),
    ("psyc_hPa", "hPa K-1", "A", "Psychrometric constant", False),
    # Iteration counter (uses SUM aggregation)
    ("iter", "1", "S", "Iteration counter of the surface temperature and QH convergence loop", False),
    # FAI variables (5)
    ("FAIBldg_use", "1", "A", "Frontal area index of buildings used by the model, after the frontal area index option is applied", False),
    ("FAIEveTree_use", "1", "A", "Frontal area index of evergreen trees used by the model, after the frontal area index option is applied", False),
    ("FAIDecTree_use", "1", "A", "Frontal area index of deciduous trees used by the model, after the frontal area index option is applied", False),
    ("FAI", "1", "A", "Total frontal area index of buildings and trees", False),
    ("dqndt", "W m-2", "A", "Rate of change of net all-wave radiation", False),
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
        interval_water_flux=interval_water_flux,
        description=description,
        aggregation=_get_agg_method(aggm),
        **({"units_metadata": "temperature: difference"}
           if name in {"avcp", "s_hPa", "psyc_hPa"} else {}),
        group=OutputGroup.DEBUG,
        level=OutputLevel.DEFAULT,
    )
    for name, unit, aggm, description, interval_water_flux in DEBUG_VARIABLE_DEFS
]
