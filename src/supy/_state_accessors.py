"""Unified state accessor interface.

Re-exports all accessor functions from the modular Fortran accessor modules
to provide a single namespace for Python code.

This module consolidates the 12 state-specific accessor modules:
- module_accessor_heat
- module_accessor_hydro
- module_accessor_snow
- module_accessor_flag
- module_accessor_solar
- module_accessor_roughness
- module_accessor_nhood
- module_accessor_ohm
- module_accessor_atm
- module_accessor_anthro
- module_accessor_phen
- module_accessor_stebbs

Raises
------
ImportError
    If the Fortran extension module is not compiled or accessor modules
    are missing. This typically means the package needs to be rebuilt.
"""

# Import all accessor functions from the modular modules
# Each import is wrapped to provide meaningful error messages
_ACCESSOR_MODULES = [
    "heat", "hydro", "snow", "flag", "solar", "roughness",
    "nhood", "ohm", "atm", "anthro", "phen", "stebbs", "site"
]

try:
    from .supy_driver import module_accessor_heat as _heat
    from .supy_driver import module_accessor_hydro as _hydro
    from .supy_driver import module_accessor_snow as _snow
    from .supy_driver import module_accessor_flag as _flag
    from .supy_driver import module_accessor_solar as _solar
    from .supy_driver import module_accessor_roughness as _roughness
    from .supy_driver import module_accessor_nhood as _nhood
    from .supy_driver import module_accessor_ohm as _ohm
    from .supy_driver import module_accessor_atm as _atm
    from .supy_driver import module_accessor_anthro as _anthro
    from .supy_driver import module_accessor_phen as _phen
    from .supy_driver import module_accessor_stebbs as _stebbs
    from .supy_driver import module_accessor_site as _site
except ImportError as e:
    raise ImportError(
        f"Failed to import DTS accessor modules: {e}. "
        "The Fortran extension may not be compiled. "
        "Try rebuilding with: pip install -e . or make dev"
    ) from e

# Heat state accessors
get_heat_state_dims = _heat.get_heat_state_dims
get_heat_state_temp = _heat.get_heat_state_temp
set_heat_state_temp = _heat.set_heat_state_temp
get_heat_state_tsfc = _heat.get_heat_state_tsfc
set_heat_state_tsfc = _heat.set_heat_state_tsfc
get_heat_state_flux_roof = _heat.get_heat_state_flux_roof
get_heat_state_flux_wall = _heat.get_heat_state_flux_wall
get_heat_state_scalars = _heat.get_heat_state_scalars
set_heat_state_scalars = _heat.set_heat_state_scalars
get_heat_state_surf_flux = _heat.get_heat_state_surf_flux
set_heat_state_surf_flux = _heat.set_heat_state_surf_flux

# Hydro state accessors
get_hydro_state_dims = _hydro.get_hydro_state_dims
get_hydro_state_soilstore = _hydro.get_hydro_state_soilstore
set_hydro_state_soilstore = _hydro.set_hydro_state_soilstore
get_hydro_state_wetness = _hydro.get_hydro_state_wetness
set_hydro_state_wetness = _hydro.set_hydro_state_wetness
get_hydro_state_evap = _hydro.get_hydro_state_evap
get_hydro_state_scalars = _hydro.get_hydro_state_scalars

# Snow state accessors
get_snow_state_dims = _snow.get_snow_state_dims
get_snow_state_arrays = _snow.get_snow_state_arrays
set_snow_state_arrays = _snow.set_snow_state_arrays
get_snow_state_scalars = _snow.get_snow_state_scalars
set_snow_state_scalars = _snow.set_snow_state_scalars

# Flag state accessors
get_flag_state = _flag.get_flag_state
set_flag_state = _flag.set_flag_state

# Solar state accessors
get_solar_state = _solar.get_solar_state
set_solar_state = _solar.set_solar_state

# Roughness state accessors
get_roughness_state = _roughness.get_roughness_state
set_roughness_state = _roughness.set_roughness_state

# Nhood state accessors
get_nhood_state = _nhood.get_nhood_state
set_nhood_state = _nhood.set_nhood_state

# OHM state accessors
get_ohm_state_radiation = _ohm.get_ohm_state_radiation
set_ohm_state_radiation = _ohm.set_ohm_state_radiation
get_ohm_state_coef_grid = _ohm.get_ohm_state_coef_grid
set_ohm_state_coef_grid = _ohm.set_ohm_state_coef_grid
get_ohm_state_averages = _ohm.get_ohm_state_averages
set_ohm_state_averages = _ohm.set_ohm_state_averages
get_ohm_state_coef_surf = _ohm.get_ohm_state_coef_surf
set_ohm_state_coef_surf = _ohm.set_ohm_state_coef_surf

# Atm state accessors
get_atm_state_thermo = _atm.get_atm_state_thermo
set_atm_state_thermo = _atm.set_atm_state_thermo
get_atm_state_vapour = _atm.get_atm_state_vapour
set_atm_state_vapour = _atm.set_atm_state_vapour
get_atm_state_turb = _atm.get_atm_state_turb
set_atm_state_turb = _atm.set_atm_state_turb
get_atm_state_diag = _atm.get_atm_state_diag
set_atm_state_diag = _atm.set_atm_state_diag
get_atm_state_rss_surf = _atm.get_atm_state_rss_surf
set_atm_state_rss_surf = _atm.set_atm_state_rss_surf

# Anthro state accessors
get_anthro_state_hdd = _anthro.get_anthro_state_hdd
set_anthro_state_hdd = _anthro.set_anthro_state_hdd
get_anthro_state_co2 = _anthro.get_anthro_state_co2
set_anthro_state_co2 = _anthro.set_anthro_state_co2

# Phen state accessors
get_phen_state_alb = _phen.get_phen_state_alb
set_phen_state_alb = _phen.set_phen_state_alb
get_phen_state_lai = _phen.get_phen_state_lai
set_phen_state_lai = _phen.set_phen_state_lai
get_phen_state_scalars = _phen.get_phen_state_scalars
set_phen_state_scalars = _phen.set_phen_state_scalars
get_phen_state_conductance = _phen.get_phen_state_conductance
set_phen_state_conductance = _phen.set_phen_state_conductance
get_phen_state_drain = _phen.get_phen_state_drain
set_phen_state_drain = _phen.set_phen_state_drain

# STEBBS state accessors
get_stebbs_state_krad = _stebbs.get_stebbs_state_krad
set_stebbs_state_krad = _stebbs.set_stebbs_state_krad
get_stebbs_state_lrad = _stebbs.get_stebbs_state_lrad
set_stebbs_state_lrad = _stebbs.set_stebbs_state_lrad
get_stebbs_state_rsl = _stebbs.get_stebbs_state_rsl
set_stebbs_state_rsl = _stebbs.set_stebbs_state_rsl
get_stebbs_state_temps_envelope = _stebbs.get_stebbs_state_temps_envelope
set_stebbs_state_temps_envelope = _stebbs.set_stebbs_state_temps_envelope
get_stebbs_state_temps_water = _stebbs.get_stebbs_state_temps_water
set_stebbs_state_temps_water = _stebbs.set_stebbs_state_temps_water
get_stebbs_state_qs = _stebbs.get_stebbs_state_qs
set_stebbs_state_qs = _stebbs.set_stebbs_state_qs
get_stebbs_building_temps = _stebbs.get_stebbs_building_temps
set_stebbs_building_temps = _stebbs.set_stebbs_building_temps
allocate_stebbs_buildings = _stebbs.allocate_stebbs_buildings
get_stebbs_buildings_info = _stebbs.get_stebbs_buildings_info

# Site accessor functions (for nested site parameters)
get_site_conductance = _site.get_site_conductance
set_site_conductance = _site.set_site_conductance

# Site surface property accessors
get_site_soil_params = _site.get_site_soil_params
set_site_soil_params = _site.set_site_soil_params
get_site_water_limits = _site.get_site_water_limits
set_site_water_limits = _site.set_site_water_limits
get_site_sfr = _site.get_site_sfr
set_site_sfr = _site.set_site_sfr
get_site_emis = _site.get_site_emis
set_site_emis = _site.set_site_emis
