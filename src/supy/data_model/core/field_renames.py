"""Central registry of deprecated field name mappings.

Each dict maps old (fused) field names to new (snake_case) names.
Used by ``@model_validator(mode='before')`` on affected Pydantic models,
the Phase A validation pipeline, and raw-dict compatibility helpers.
"""

from __future__ import annotations

import warnings
from collections.abc import Mapping
from typing import Any, Dict


# -- ModelPhysics (model.py) -------------------------------------------------
#
# MODELPHYSICS_RENAMES maps legacy fused spellings DIRECTLY to the current
# final names (Category 1 fused -> Category 2+3 bare where applicable).
# This consolidation keeps ALL_FIELD_RENAMES a one-to-one map between
# legacy-fused keys and final values — critical for the Rust bridge's
# reverse lookup (Fortran state is still keyed by the fused spelling).
#
# MODELPHYSICS_SUFFIX_RENAMES maps the Category 1 intermediate (shipped
# in Schema 2026.5 as `net_radiation_method` etc.) to the final bare
# names. Consumed only by the Pydantic backward-compat shim on
# ``ModelPhysics`` so users who hand-wrote their YAMLs to the 2026.5
# snake_case-with-suffix shape still load with a DeprecationWarning.

MODELPHYSICS_RENAMES: Dict[str, str] = {
    # Fused -> Category 2+3 final (shadows both Cat 1 and Cat 2+3 for
    # users arriving from any pre-2026.5.dev2 schema in a single step).
    "netradiationmethod": "net_radiation",
    "emissionsmethod": "emissions",
    "storageheatmethod": "storage_heat",
    "roughlenmommethod": "roughness_length_momentum",
    "roughlenheatmethod": "roughness_length_heat",
    "stabilitymethod": "stability",
    "smdmethod": "soil_moisture_deficit",
    "waterusemethod": "water_use",
    "rslmethod": "roughness_sublayer",
    "faimethod": "frontal_area_index",
    "rsllevel": "roughness_sublayer_level",
    "gsmodel": "surface_conductance",
    "stebbsmethod": "stebbs",
    "rcmethod": "outer_cap_fraction",
    "setpointmethod": "setpoint",  # fused identifier missed by Category 1
    # Flags/enum choices not touched by Category 2+3 keep their Cat 1 target.
    "ohmincqf": "ohm_inc_qf",
    "snowuse": "snow_use",
}

# Category 1 intermediate (snake_case with redundant suffix) -> final bare.
# Applied in ``ModelPhysics._rename_physics_fields`` after
# ``MODELPHYSICS_RENAMES`` so YAMLs authored against Schema 2026.5 (15
# days of release window) keep loading with a deprecation warning.
# NOT spread into ``ALL_FIELD_RENAMES`` — doing so would introduce a
# second alias for the same final name and break the one-to-one
# reverse-mapping the Rust bridge depends on. Users running the Rust
# CLI (``suews run``) on a Schema 2026.5 YAML should migrate via
# ``suews-schema migrate --target-version 2026.5.dev2`` first.

MODELPHYSICS_SUFFIX_RENAMES: Dict[str, str] = {
    # Suffix drop (8) -- enum type (e.g. NetRadiationMethod) carries "method"
    "net_radiation_method": "net_radiation",
    "emissions_method": "emissions",
    "storage_heat_method": "storage_heat",
    "roughness_length_momentum_method": "roughness_length_momentum",
    "roughness_length_heat_method": "roughness_length_heat",
    "stability_method": "stability",
    "water_use_method": "water_use",
    "stebbs_method": "stebbs",
    # Expand opaque domain abbreviations (4)
    "smd_method": "soil_moisture_deficit",
    "rsl_method": "roughness_sublayer",
    "rsl_level": "roughness_sublayer_level",
    "fai_method": "frontal_area_index",
    # Expand + semantic rename (2)
    "rc_method": "outer_cap_fraction",
    "gs_model": "surface_conductance",
}

# -- SurfaceProperties (surface.py) ------------------------------------------

SURFACEPROPERTIES_RENAMES: Dict[str, str] = {
    "soildepth": "soil_depth",
    "soilstorecap": "soil_store_capacity",
    "statelimit": "state_limit",
    "wetthresh": "wet_threshold",
    "sathydraulicconduct": "saturated_hydraulic_conductivity",
    "soildensity": "soil_density",
    "storedrainprm": "storage_drain_params",
    "snowpacklimit": "snowpack_limit",
    "irrfrac": "irrigation_fraction",
    "ohm_threshsw": "ohm_threshold_summer_winter",
    "ohm_threshwd": "ohm_threshold_wet_dry",
}

# -- LAIParams (site.py) -----------------------------------------------------

LAIPARAMS_RENAMES: Dict[str, str] = {
    "baset": "base_temperature",
    "gddfull": "gdd_full",
    "basete": "base_temperature_senescence",
    "sddfull": "sdd_full",
    "laimin": "lai_min",
    "laimax": "lai_max",
    "laipower": "lai_power",
    "laitype": "lai_type",
}

# -- VegetatedSurfaceProperties (site.py) ------------------------------------

VEGETATEDSURFACEPROPERTIES_RENAMES: Dict[str, str] = {
    "maxconductance": "max_conductance",
    "beta_bioco2": "beta_bio_co2",
    "alpha_bioco2": "alpha_bio_co2",
    "theta_bioco2": "theta_bio_co2",
}

# -- EvetrProperties (site.py) -----------------------------------------------

EVETRPROPERTIES_RENAMES: Dict[str, str] = {
    "faievetree": "fai_evergreen_tree",
    "evetreeh": "height_evergreen_tree",
}

# -- DectrProperties (site.py) -----------------------------------------------

DECTRPROPERTIES_RENAMES: Dict[str, str] = {
    "faidectree": "fai_deciduous_tree",
    "dectreeh": "height_deciduous_tree",
    "pormin_dec": "porosity_min_deciduous",
    "pormax_dec": "porosity_max_deciduous",
    "capmax_dec": "capacity_max_deciduous",
    "capmin_dec": "capacity_min_deciduous",
}

# -- ArchetypeProperties (site.py) -------------------------------------------
#
# gh#1334 retires the STEBBS PascalCase exception: the full ArchetypeProperties
# surface converts to snake_case. Keys in this dict are the legacy spellings
# users encounter in older YAMLs; values are the gh#1334 final snake_case
# names. Two legacy families are collapsed into a one-to-one map here:
#
#   * The fused `Wallext` / `Roofext` cluster from pre-gh#1327 (2025.10/11/12,
#     2026.1, 2026.4 releases). These skip the gh#1327 intermediate and go
#     straight to snake_case.
#   * The PascalCase names shipped through Schema 2026.5.dev2 (the form every
#     user has today). For fields that never had a fused predecessor the
#     PascalCase IS the sole legacy spelling.
#
# Users on the Cat 5 (#1329) intermediate PascalCase (`WallExternalThickness`,
# etc.) are caught by ARCHETYPEPROPERTIES_PASCAL_RENAMES below — those cannot
# live in this dict without breaking the one-to-one invariant the Rust bridge
# depends on (every final name must have exactly one reverse-lookup key).

ARCHETYPEPROPERTIES_RENAMES: Dict[str, str] = {
    # Pre-gh#1327 fused -> gh#1334 snake_case (skipping the gh#1329 intermediate)
    "WallextThickness": "wall_external_thickness",
    "WallextEffectiveConductivity": "wall_external_effective_conductivity",
    "WallextDensity": "wall_external_density",
    "WallextCp": "wall_external_specific_heat_capacity",
    "RoofextThickness": "roof_external_thickness",
    "RoofextEffectiveConductivity": "roof_external_effective_conductivity",
    "RoofextDensity": "roof_external_density",
    "RoofextCp": "roof_external_specific_heat_capacity",
    # Building metadata / geometry
    "BuildingType": "building_type",
    "BuildingName": "building_name",
    "BuildingCount": "building_count",
    "Occupants": "occupants",
    "stebbs_Height": "building_height",
    "FootprintArea": "footprint_area",
    "WallExternalArea": "wall_external_area",
    "RatioInternalVolume": "internal_volume_ratio",
    "InternalMassArea": "internal_mass_area",
    "WWR": "window_to_wall_ratio",
    # Wall (non-ext)
    "WallThickness": "wall_thickness",
    "WallEffectiveConductivity": "wall_effective_conductivity",
    "WallDensity": "wall_density",
    "WallCp": "wall_specific_heat_capacity",
    "WallOuterCapFrac": "wall_outer_heat_capacity_fraction",
    "WallExternalEmissivity": "wall_external_emissivity",
    "WallInternalEmissivity": "wall_internal_emissivity",
    "WallTransmissivity": "wall_transmissivity",
    "WallAbsorbtivity": "wall_absorptivity",  # spelling fix
    "WallReflectivity": "wall_reflectivity",
    # Roof (non-ext)
    "RoofThickness": "roof_thickness",
    "RoofEffectiveConductivity": "roof_effective_conductivity",
    "RoofDensity": "roof_density",
    "RoofCp": "roof_specific_heat_capacity",
    "RoofOuterCapFrac": "roof_outer_heat_capacity_fraction",
    "RoofExternalEmissivity": "roof_external_emissivity",
    "RoofInternalEmissivity": "roof_internal_emissivity",
    "RoofTransmissivity": "roof_transmissivity",
    "RoofAbsorbtivity": "roof_absorptivity",  # spelling fix
    "RoofReflectivity": "roof_reflectivity",
    # Ground floor (align FloorThickness with GroundFloor* siblings)
    "FloorThickness": "ground_floor_thickness",
    "GroundFloorEffectiveConductivity": "ground_floor_effective_conductivity",
    "GroundFloorDensity": "ground_floor_density",
    "GroundFloorCp": "ground_floor_specific_heat_capacity",
    # Window
    "WindowThickness": "window_thickness",
    "WindowEffectiveConductivity": "window_effective_conductivity",
    "WindowDensity": "window_density",
    "WindowCp": "window_specific_heat_capacity",
    "WindowExternalEmissivity": "window_external_emissivity",
    "WindowInternalEmissivity": "window_internal_emissivity",
    "WindowTransmissivity": "window_transmissivity",
    "WindowAbsorbtivity": "window_absorptivity",  # spelling fix
    "WindowReflectivity": "window_reflectivity",
    # Internal mass
    "InternalMassDensity": "internal_mass_density",
    "InternalMassCp": "internal_mass_specific_heat_capacity",
    "InternalMassEmissivity": "internal_mass_emissivity",
    # HVAC / hot water. `water_tank_water_volume` unified into
    # `hot_water_tank_volume` — same fluid, tank is just the storage
    # component of the hot-water subsystem (see StebbsProperties below).
    "MaxHeatingPower": "max_heating_power",
    "WaterTankWaterVolume": "hot_water_tank_volume",
    "MaximumHotWaterHeatingPower": "maximum_hot_water_heating_power",
    "HeatingSetpointTemperature": "heating_setpoint_temperature",
    "CoolingSetpointTemperature": "cooling_setpoint_temperature",
    "HeatingSetpointTemperatureProfile": "heating_setpoint_temperature_profile",
    "CoolingSetpointTemperatureProfile": "cooling_setpoint_temperature_profile",
    "MetabolismProfile": "metabolism_profile",
}

# gh#1329 intermediate PascalCase (`WallExternalThickness`, etc.) -> gh#1334
# final snake_case. NOT spread into ALL_FIELD_RENAMES (keeping the one-to-one
# invariant); the Pydantic shim on ArchetypeProperties runs this after the
# main dict so YAMLs authored at the Schema 2026.5.dev1 / dev2 shape still
# load with a DeprecationWarning.

ARCHETYPEPROPERTIES_PASCAL_RENAMES: Dict[str, str] = {
    "WallExternalThickness": "wall_external_thickness",
    "WallExternalEffectiveConductivity": "wall_external_effective_conductivity",
    "WallExternalDensity": "wall_external_density",
    "WallExternalCp": "wall_external_specific_heat_capacity",
    "RoofExternalThickness": "roof_external_thickness",
    "RoofExternalEffectiveConductivity": "roof_external_effective_conductivity",
    "RoofExternalDensity": "roof_external_density",
    "RoofExternalCp": "roof_external_specific_heat_capacity",
}

# Schema 2026.5.dev3 `water_tank_water_volume` -> unified `hot_water_tank_volume`
# (drop the double-`water` redundancy and fold under the `hot_water_tank_*`
# component prefix). NOT spread into ALL_FIELD_RENAMES — keeping the
# one-to-one invariant; the ArchetypeProperties Pydantic shim runs this
# after the main dict so dev3 YAMLs still load with a DeprecationWarning.

ARCHETYPEPROPERTIES_DEV3_RENAMES: Dict[str, str] = {
    "water_tank_water_volume": "hot_water_tank_volume",
}

# -- StebbsProperties (site.py) ----------------------------------------------
#
# gh#1334: PascalCase -> snake_case for the full StebbsProperties surface.
# No fused predecessor — every field's PascalCase is the sole legacy form,
# so each entry maps directly into ALL_FIELD_RENAMES without an intermediate
# dict (contrast with ArchetypeProperties).

STEBBSPROPERTIES_RENAMES: Dict[str, str] = {
    # Convection coefficients
    "WallInternalConvectionCoefficient": "wall_internal_convection_coefficient",
    "RoofInternalConvectionCoefficient": "roof_internal_convection_coefficient",
    "InternalMassConvectionCoefficient": "internal_mass_convection_coefficient",
    "FloorInternalConvectionCoefficient": "floor_internal_convection_coefficient",
    "WindowInternalConvectionCoefficient": "window_internal_convection_coefficient",
    "WallExternalConvectionCoefficient": "wall_external_convection_coefficient",
    "RoofExternalConvectionCoefficient": "roof_external_convection_coefficient",
    "WindowExternalConvectionCoefficient": "window_external_convection_coefficient",
    # Ground & environment. `diffmax` stays as a compound token (cross-layer
    # naming parity with the Rust bridge — expanding it further would cross
    # into Tier B, #1324); the `month` -> `monthly` expansion is kept.
    "GroundDepth": "ground_depth",
    "ExternalGroundConductivity": "external_ground_conductivity",
    "MonthMeanAirTemperature_diffmax": "month_mean_air_temperature_diffmax",
    "AnnualMeanAirTemperature": "annual_mean_air_temperature",
    # Behaviour & controls. `cop` kept as a lowercase acronym (matches the
    # Rust bridge struct `cooling_system_cop`; an expansion to
    # `coefficient_of_performance` would require a Rust-side rename in
    # Tier B, out of scope for gh#1334).
    "MetabolismThreshold": "metabolism_threshold",
    "LatentSensibleRatio": "latent_sensible_ratio",
    "DaylightControl": "daylight_control",
    "LightingIlluminanceThreshold": "lighting_illuminance_threshold",
    "HeatingSystemEfficiency": "heating_system_efficiency",
    "MaxCoolingPower": "max_cooling_power",
    "CoolingSystemCOP": "cooling_system_cop",
    # Ventilation & initial state
    "VentilationRate": "ventilation_rate",
    "InitialOutdoorTemperature": "initial_outdoor_temperature",
    "InitialIndoorTemperature": "initial_indoor_temperature",
    # Hot-water system. All `DHW*` and `WaterTank*` legacy fields
    # unify under `hot_water_*`: the tank (storage) keeps its
    # `hot_water_tank_*` component prefix, the point-of-use vessel gets
    # `hot_water_vessel_*`, and the bulk fluid properties (density, Cp,
    # volume in use, surface area in use) drop the vessel/tank qualifier
    # since the water is the same fluid in both places. DHW is just an
    # opaque acronym for "domestic hot water" which this prefix already
    # conveys without the abbreviation.
    "WaterTankWallThickness": "hot_water_tank_wall_thickness",
    "MainsWaterTemperature": "mains_water_temperature",
    "WaterTankSurfaceArea": "hot_water_tank_surface_area",
    "HotWaterHeatingSetpointTemperature": "hot_water_heating_setpoint_temperature",
    "HotWaterTankWallEmissivity": "hot_water_tank_wall_emissivity",
    "DHWVesselWallThickness": "hot_water_vessel_wall_thickness",
    "DHWWaterVolume": "hot_water_volume",
    "DHWSurfaceArea": "hot_water_surface_area",
    "HotWaterFlowRate": "hot_water_flow_rate",
    "HotWaterFlowProfile": "hot_water_flow_profile",
    "DHWSpecificHeatCapacity": "hot_water_specific_heat_capacity",
    "HotWaterTankSpecificHeatCapacity": "hot_water_tank_specific_heat_capacity",
    "DHWVesselSpecificHeatCapacity": "hot_water_vessel_specific_heat_capacity",
    "DHWDensity": "hot_water_density",
    "HotWaterTankWallDensity": "hot_water_tank_wall_density",
    "DHWVesselDensity": "hot_water_vessel_density",
    "HotWaterTankBuildingWallViewFactor": "hot_water_tank_building_wall_view_factor",
    "HotWaterTankInternalMassViewFactor": "hot_water_tank_internal_mass_view_factor",
    "HotWaterTankWallConductivity": "hot_water_tank_wall_conductivity",
    "HotWaterTankInternalWallConvectionCoefficient": "hot_water_tank_internal_wall_convection_coefficient",
    "HotWaterTankExternalWallConvectionCoefficient": "hot_water_tank_external_wall_convection_coefficient",
    "DHWVesselWallConductivity": "hot_water_vessel_wall_conductivity",
    "DHWVesselInternalWallConvectionCoefficient": "hot_water_vessel_internal_wall_convection_coefficient",
    "DHWVesselExternalWallConvectionCoefficient": "hot_water_vessel_external_wall_convection_coefficient",
    "DHWVesselWallEmissivity": "hot_water_vessel_wall_emissivity",
    "HotWaterHeatingEfficiency": "hot_water_heating_efficiency",
    # Profiles
    "ApplianceProfile": "appliance_profile",
    "LightingPowerDensity": "lighting_power_density",
}

# -- SnowParams (site.py) ----------------------------------------------------
#
# gh#1334 updates:
#   * Six existing entries (preciplimit/preciplimitalb/snowlim*/tempmeltfact/
#     radmeltfact) retarget to the new snake_case names. `preciplimit` becomes
#     `temperature_rain_snow_threshold` (semantic fix — the value is a
#     temperature, unit degC, despite the name).
#   * Five new entries for fields that had no fused predecessor
#     (tau_a/f/r, snowprof_24hr, narp_emis_snow). Their "legacy" key is the
#     Schema 2026.5.dev2 snake_case name, which also happens to be the
#     Fortran-bridge column name so the reverse-lookup into the DataFrame
#     layer still resolves.

SNOWPARAMS_RENAMES: Dict[str, str] = {
    "crwmax": "water_holding_capacity_max",
    "crwmin": "water_holding_capacity_min",
    "preciplimit": "temperature_rain_snow_threshold",
    "preciplimitalb": "precipitation_threshold_albedo_reset",
    "snowalbmax": "snow_albedo_max",
    "snowalbmin": "snow_albedo_min",
    "snowdensmin": "snow_density_min",
    "snowdensmax": "snow_density_max",
    "snowlimbldg": "snow_depth_limit_building",
    "snowlimpaved": "snow_depth_limit_paved",
    "tempmeltfact": "temperature_melt_factor",
    "radmeltfact": "radiation_melt_factor",
    # New #1334 renames for fields without a fused predecessor.
    "tau_a": "tau_cold_snow",
    "tau_f": "tau_melting_snow",
    "tau_r": "tau_refreezing_snow",
    "snowprof_24hr": "snow_profile_24hr",
    "narp_emis_snow": "narp_emissivity_snow",
}

# Schema 2026.5.dev2 snake_case intermediate -> gh#1334 final snake_case.
# Users who hand-wrote YAMLs against the intermediate shape (the six fields
# that already had a fused predecessor in SNOWPARAMS_RENAMES) need this
# additional alias. NOT spread into ALL_FIELD_RENAMES for the same
# one-to-one reason as ARCHETYPEPROPERTIES_PASCAL_RENAMES.

SNOWPARAMS_INTERMEDIATE_RENAMES: Dict[str, str] = {
    "precip_limit": "temperature_rain_snow_threshold",
    "precip_limit_albedo": "precipitation_threshold_albedo_reset",
    "snow_limit_building": "snow_depth_limit_building",
    "snow_limit_paved": "snow_depth_limit_paved",
    "temp_melt_factor": "temperature_melt_factor",
    "rad_melt_factor": "radiation_melt_factor",
}

# Schema 2026.5.dev3 `dhw_*` / `water_tank_*` intermediate -> unified
# `hot_water_*`. Drops the opaque `dhw` acronym (= domestic hot water,
# which the `hot_water_` prefix already conveys) and aligns the tank
# storage component with the `hot_water_tank_*` sibling fields that
# were already canonical at dev3. NOT spread into ALL_FIELD_RENAMES —
# the StebbsProperties Pydantic shim runs this after the main dict so
# dev3 YAMLs still load with a DeprecationWarning.

STEBBSPROPERTIES_DEV3_RENAMES: Dict[str, str] = {
    "water_tank_wall_thickness": "hot_water_tank_wall_thickness",
    "water_tank_surface_area": "hot_water_tank_surface_area",
    "dhw_vessel_wall_thickness": "hot_water_vessel_wall_thickness",
    "dhw_water_volume": "hot_water_volume",
    "dhw_surface_area": "hot_water_surface_area",
    "dhw_specific_heat_capacity": "hot_water_specific_heat_capacity",
    "dhw_vessel_specific_heat_capacity": "hot_water_vessel_specific_heat_capacity",
    "dhw_density": "hot_water_density",
    "dhw_vessel_density": "hot_water_vessel_density",
    "dhw_vessel_wall_conductivity": "hot_water_vessel_wall_conductivity",
    "dhw_vessel_internal_wall_convection_coefficient": "hot_water_vessel_internal_wall_convection_coefficient",
    "dhw_vessel_external_wall_convection_coefficient": "hot_water_vessel_external_wall_convection_coefficient",
    "dhw_vessel_wall_emissivity": "hot_water_vessel_wall_emissivity",
}

# -- EHC (suews_type_ehc.f95) -------------------------------------------------
#
# Fortran-only internal-state members added to the registry for gh#1326
# (Tier D). These are roof/wall-qualified variants of SURFACEPROPERTIES's
# ``soilstorecap`` and keep the same ``soil_store_capacity`` naming.

EHC_RENAMES: Dict[str, str] = {
    "soil_storecap_roof": "soil_store_capacity_roof",
    "soil_storecap_wall": "soil_store_capacity_wall",
}

# -- SNOW_STATE (suews_type_snow.f95) -----------------------------------------
#
# Fortran-only internal-state members of SNOW_STATE. Added for gh#1326
# (Tier D). Distinct from ``SNOWPARAMS_RENAMES`` which covers SNOW_PRM
# (the user-facing YAML side).

# -- IRRIGATION_PRM (suews_type_waterdist.f95) --------------------------------

WATERDIST_RENAMES: Dict[str, str] = {
    "faut": "f_aut",
}

# -- anthroHEAT_PRM + anthroEMIS_PRM (suews_type_anthro.f95) ------------------

ANTHRO_RENAMES: Dict[str, str] = {
    # anthroHEAT_PRM
    "popdensnighttime": "pop_density_nighttime",
    "popdensdaytime_working": "pop_density_daytime_working",
    "popdensdaytime_holiday": "pop_density_daytime_holiday",
    # anthroEMIS_PRM
    "startdls": "start_dls",
    "enddls": "end_dls",
    "anthroheat": "anthro_heat",
    "EF_umolCO2perJ": "ef_umol_co2_per_j",
    "EnEF_v_Jkm": "en_ef_v_jkm",
    "FrFossilFuel_Heat": "fr_fossil_fuel_heat",
    "FrFossilFuel_NonHeat": "fr_fossil_fuel_non_heat",
    "FcEF_v_kgkm": "fc_ef_v_kgkm",
    "HumActivity_24hr_working": "hum_activity_24hr_working",
    "HumActivity_24hr_holiday": "hum_activity_24hr_holiday",
    "MaxFCMetab": "max_fc_metab",
    "MaxQFMetab": "max_qf_metab",
    "MinFCMetab": "min_fc_metab",
    "MinQFMetab": "min_qf_metab",
    "TrafficRate_working": "traffic_rate_working",
    "TrafficRate_holiday": "traffic_rate_holiday",
    "TrafficUnits": "traffic_units",
    "TraffProf_24hr_working": "traff_prof_24hr_working",
    "TraffProf_24hr_holiday": "traff_prof_24hr_holiday",
}

# -- PHENOLOGY_STATE (suews_type_vegetation.f95) ------------------------------
#
# Fortran-only internal-state members. LAI_PRM members live under
# ``LAIPARAMS_RENAMES`` already; ``StoreDrainPrm`` reuses the
# ``storage_drain_params`` target from ``SURFACEPROPERTIES_RENAMES`` so no
# registry entry is added here (adding one would break the one-to-one
# fused->final invariant the Rust bridge reverse lookup depends on).

# -- surface types (suews_type_surface.f95) -----------------------------------
#
# ``gsmodel`` is NOT added here: MODELPHYSICS_RENAMES already owns
# ``gsmodel -> surface_conductance`` for the Python YAML side. The
# Fortran CONDUCTANCE_PRM.gsmodel member has different semantics
# (holds the model choice integer) and is renamed to ``gs_model``
# only in Fortran source — registering a second entry with the same
# key would break the one-to-one invariant.

SURFACE_RENAMES: Dict[str, str] = {
    # LUMPS_PRM
    "raincover": "rain_cover",
    "rainmaxres": "rain_max_res",
    "drainrt": "drain_rate",
    # OHM_PRM
    "chanohm": "ch_anohm",
    "cpanohm": "cp_anohm",
    "kkanohm": "kk_anohm",
    # CONDUCTANCE_PRM
    "kmax": "k_max",
    # ROUGHNESS_STATE
    "FAIBldg_use": "fai_bldg_use",
    "FAIEveTree_use": "fai_evetree_use",
    "FAIDecTree_use": "fai_dectree_use",
}

# -- atm_state + solar_State (suews_type_atmosphere.f95) ----------------------

ATMOSPHERE_RENAMES: Dict[str, str] = {
    "fcld": "f_cloud",
    "avcp": "av_cp",
    "avdens": "av_density",
    "zL": "z_l",
    "UStar": "u_star",
    "TStar": "t_star",
    "ZENITH_deg": "zenith_deg",
    "psycIce_hPa": "psyc_ice_h_pa",
    "sIce_hpa": "s_ice_hpa",
    "lvS_J_kg": "lv_s_j_kg",
}

PHENOLOGYSTATE_RENAMES: Dict[str, str] = {
    "VegPhenLumps": "veg_phen_lumps",
    "TempVeg": "temp_veg",
    "gfunc": "g_func",
}

SNOWSTATE_RENAMES: Dict[str, str] = {
    "snowfallCum": "snowfall_cum",
    "snowalb": "snow_albedo",
    "mwstore": "melt_water_store",
    "QmFreez": "qm_freeze",
    "QmRain": "qm_rain",
    "z0vSnow": "z0v_snow",
    "RAsnow": "ra_snow",
    "SnowRemoval": "snow_removal",
    "icefrac": "ice_frac",
    "snowdens": "snow_density",
    "snowfrac": "snow_fraction",
    "snowpack": "snow_pack",
    "snowwater": "snow_water",
    "deltaQi": "delta_qi",
}

# -- Combined -----------------------------------------------------------------
#
# ``ALL_FIELD_RENAMES`` is a one-to-one map from every legacy fused key to
# its current final name. ``MODELPHYSICS_SUFFIX_RENAMES``,
# ``ARCHETYPEPROPERTIES_PASCAL_RENAMES``, and
# ``SNOWPARAMS_INTERMEDIATE_RENAMES`` are deliberately NOT spread here — they
# carry schema-intermediate aliases that would introduce a second alias per
# final name, which the Rust bridge's reverse lookup (``ALL_FIELD_RENAMES``
# inverted) cannot represent. The Pydantic shim on each affected class runs
# both the main dict and its intermediate dict in sequence so users on any
# prior dev-cycle shape still load with a DeprecationWarning.

ALL_FIELD_RENAMES: Dict[str, str] = {
    **MODELPHYSICS_RENAMES,
    **SURFACEPROPERTIES_RENAMES,
    **LAIPARAMS_RENAMES,
    **VEGETATEDSURFACEPROPERTIES_RENAMES,
    **EVETRPROPERTIES_RENAMES,
    **DECTRPROPERTIES_RENAMES,
    **ARCHETYPEPROPERTIES_RENAMES,
    **STEBBSPROPERTIES_RENAMES,
    **SNOWPARAMS_RENAMES,
    **EHC_RENAMES,
    **SNOWSTATE_RENAMES,
    **WATERDIST_RENAMES,
    **PHENOLOGYSTATE_RENAMES,
    **ANTHRO_RENAMES,
    **ATMOSPHERE_RENAMES,
    **SURFACE_RENAMES,
}

# Raw-YAML structural checks (Phase A / precheck) need a wider view than the
# one-to-one public registry above: they must accept both the final public
# names and the short-lived Schema 2026.5 intermediate ModelPhysics aliases
# plus the gh#1334 Archetype/Snow intermediate aliases.
# Keeping this separate preserves the bridge-safe one-to-one contract of
# ``ALL_FIELD_RENAMES`` while letting raw-dict callers normalise older YAMLs
# before they compare against the current sample schema.
RAW_YAML_FIELD_RENAMES: Dict[str, str] = {
    **MODELPHYSICS_SUFFIX_RENAMES,
    **ARCHETYPEPROPERTIES_PASCAL_RENAMES,
    **ARCHETYPEPROPERTIES_DEV3_RENAMES,
    **SNOWPARAMS_INTERMEDIATE_RENAMES,
    **STEBBSPROPERTIES_DEV3_RENAMES,
    **ALL_FIELD_RENAMES,
}

# Reverse mapping: new_name -> old_name (for serialisation to Fortran bridge).
# The Fortran bridge still indexes state by fused spellings, so `_REVERSE_*`
# maps each final public name back to its original fused form.
_REVERSE_RENAMES: Dict[str, str] = {v: k for k, v in ALL_FIELD_RENAMES.items()}

# For raw-YAML preflight checks that bypass Pydantic. Both the fused form
# (`netradiationmethod`) and the Category 1 intermediate form
# (`net_radiation_method`) must resolve to the final bare name, so the
# combined mapping unions MODELPHYSICS_RENAMES (fused -> final) with
# MODELPHYSICS_SUFFIX_RENAMES (intermediate -> final). The Cat 1
# intermediate keys are dict-disjoint from the fused keys, so the
# ``|`` union is safe and order-independent.
_MODELPHYSICS_ALL_RENAMES: Dict[str, str] = {
    # Order matters for the reverse map below: spreading SUFFIX first and
    # RENAMES last means inverting the dict prefers the fused legacy
    # (``netradiationmethod``) over the Cat 1 intermediate
    # (``net_radiation_method``) when both claim the same final target.
    # The Fortran bridge's state is keyed by the fused form, so the
    # reverse map must point at the fused legacy.
    **MODELPHYSICS_SUFFIX_RENAMES,
    **MODELPHYSICS_RENAMES,
}
_REVERSE_MODELPHYSICS_RENAMES: Dict[str, str] = {
    v: k for k, v in _MODELPHYSICS_ALL_RENAMES.items()
}
_MISSING = object()


def apply_field_renames(
    values: dict, renames: Dict[str, str], class_name: str
) -> dict:
    """Replace deprecated field names in *values* with their new equivalents.

    Called from ``@model_validator(mode='before')`` on each affected model.

    Parameters
    ----------
    values : dict
        Raw input dict (YAML or kwargs).
    renames : dict
        ``{old_name: new_name}`` mapping for this model class.
    class_name : str
        Model class name, used in warning messages.

    Returns
    -------
    dict
        Updated dict with old keys replaced by new keys.

    Raises
    ------
    ValueError
        If both old and new names are present for the same field.
    """
    for old_name, new_name in renames.items():
        if old_name in values:
            if new_name in values:
                raise ValueError(
                    f"{class_name}: both '{old_name}' (deprecated) and "
                    f"'{new_name}' are present. Use only '{new_name}'."
                )
            values[new_name] = values.pop(old_name)
            warnings.warn(
                f"{class_name}: field '{old_name}' is deprecated, "
                f"use '{new_name}' instead.",
                DeprecationWarning,
                stacklevel=4,
            )
    return values


def read_physics_key(physics: dict, new_name: str, default: Any = None):
    """Read a physics key from raw YAML, accepting either the new name or its legacy alias.

    Public-mode gates and other preflight checks inspect the raw user YAML
    before Phase A has renamed keys. The Pydantic backward-compat shim
    accepts both spellings, so these gates must as well, or users on either
    spelling can silently bypass them.

    Unwraps RefValue-style ``{"value": X}`` wrappers. Returns ``default``
    when neither spelling is present.
    """
    entry = read_renamed_key(
        physics,
        new_name,
        renames=_MODELPHYSICS_ALL_RENAMES,
        reverse_renames=_REVERSE_MODELPHYSICS_RENAMES,
        default=default,
    )
    if isinstance(entry, dict) and "value" in entry:
        return entry["value"]
    return entry


def read_renamed_key(
    data: dict,
    name: str,
    *,
    renames: Dict[str, str] = ALL_FIELD_RENAMES,
    reverse_renames: Dict[str, str] | None = None,
    default: Any = None,
):
    """Read a renamed key from a raw dict, accepting both spellings.

    Parameters
    ----------
    data : dict
        Raw mapping to inspect.
    name : str
        Preferred key name. Can be either the new name or the legacy name.
    renames : dict, optional
        ``{old_name: new_name}`` rename mapping.
    reverse_renames : dict, optional
        Precomputed ``{new_name: old_name}`` mapping. If omitted, it is built
        from ``renames``.
    default : Any, optional
        Value returned when neither spelling is present.
    """
    if not isinstance(data, Mapping):
        return default

    reverse = reverse_renames or (
        _REVERSE_RENAMES if renames is ALL_FIELD_RENAMES else {v: k for k, v in renames.items()}
    )

    entry = data.get(name, _MISSING)
    if entry is not _MISSING:
        return entry

    legacy_name = reverse.get(name)
    if legacy_name is not None and legacy_name in data:
        return data[legacy_name]

    renamed_name = renames.get(name)
    if renamed_name is not None and renamed_name in data:
        return data[renamed_name]

    # A single ``name`` may be the target of more than one legacy alias
    # (e.g. ``net_radiation`` is reached from both fused ``netradiationmethod``
    # and Cat 1 intermediate ``net_radiation_method``). The one-to-one
    # reverse map only captures one of those; walk the full rename dict
    # to catch any other legacy alias still sitting in ``data``.
    for old_key, new_key in renames.items():
        if new_key == name and old_key in data:
            return data[old_key]

    return default


def has_renamed_key(
    data: dict,
    name: str,
    *,
    renames: Dict[str, str] = ALL_FIELD_RENAMES,
    reverse_renames: Dict[str, str] | None = None,
) -> bool:
    """Return True if either the preferred or legacy spelling is present."""
    return read_renamed_key(
        data,
        name,
        renames=renames,
        reverse_renames=reverse_renames,
        default=_MISSING,
    ) is not _MISSING


def rename_keys_recursive(
    data: Any,
    renames: Dict[str, str] = ALL_FIELD_RENAMES,
    *,
    reverse_renames: Dict[str, str] | None = None,
    path: str = "",
) -> Any:
    """Recursively rewrite legacy keys to their preferred names.

    Raises
    ------
    ValueError
        If a dict contains both the legacy and preferred spellings for the
        same logical field.
    """
    reverse = reverse_renames or (
        _REVERSE_RENAMES if renames is ALL_FIELD_RENAMES else {v: k for k, v in renames.items()}
    )

    if isinstance(data, dict):
        result = {}
        source_keys: Dict[str, str] = {}
        for key, value in data.items():
            out_key = renames.get(key, key)
            if out_key in source_keys and source_keys[out_key] != key:
                prev_key = source_keys[out_key]
                location = path or "<root>"
                legacy_key = None
                if renames.get(prev_key) == out_key and key == out_key:
                    legacy_key = prev_key
                elif renames.get(key) == out_key and prev_key == out_key:
                    legacy_key = key

                if legacy_key is not None:
                    raise ValueError(
                        f"Both '{legacy_key}' (deprecated) and '{out_key}' are present at {location}. "
                        f"Use only '{out_key}'."
                    )

                raise ValueError(
                    f"Conflicting keys '{prev_key}' and '{key}' both resolve to '{out_key}' at {location}."
                )

            child_path = f"{path}.{out_key}" if path else out_key
            result[out_key] = rename_keys_recursive(
                value,
                renames,
                reverse_renames=reverse,
                path=child_path,
            )
            source_keys[out_key] = key
        return result

    if isinstance(data, list):
        return [
            rename_keys_recursive(
                item,
                renames,
                reverse_renames=reverse,
                path=f"{path}[{idx}]",
            )
            for idx, item in enumerate(data)
        ]

    return data


def reverse_field_renames(data: dict) -> dict:
    """Recursively replace new field names with old ones for serialisation.

    Used before passing ``model_dump()`` output to the Rust/Fortran bridge,
    which expects the legacy (fused) field names.
    """
    result = {}
    for key, value in data.items():
        out_key = _REVERSE_RENAMES.get(key, key)
        if isinstance(value, dict):
            result[out_key] = reverse_field_renames(value)
        elif isinstance(value, list):
            result[out_key] = [
                reverse_field_renames(item) if isinstance(item, dict) else item
                for item in value
            ]
        else:
            result[out_key] = value
    return result
