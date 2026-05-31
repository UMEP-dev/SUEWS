"""Central registry of deprecated field name mappings.

Each dict maps old (fused) field names to new (snake_case) names.
Used by ``@model_validator(mode='before')`` on affected Pydantic models,
the Phase A validation pipeline, and raw-dict compatibility helpers.
"""

from __future__ import annotations

import warnings
from collections.abc import Mapping
from typing import Any, Dict

from .physics_families import coerce_nested_to_flat


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
    # Schema 2026.5.dev12 (Reading Column D): the former `rcmethod` field is
    # renamed `outer_cap_fraction` -> `capacitance`. The fused legacy chains
    # straight to the new dev12 final so ALL_FIELD_RENAMES stays one-to-one
    # and the bridge reverse map gives `capacitance -> rcmethod`.
    "rcmethod": "capacitance",
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
    # Expand + semantic rename (2). `rc_method` chains straight to the dev12
    # final `capacitance` (was `outer_cap_fraction` pre-dev12).
    "rc_method": "capacitance",
    "gs_model": "surface_conductance",
}

# Schema 2026.5.dev11 -> 2026.5.dev12: the `model.physics` field formerly
# spelled `outer_cap_fraction` (itself the former fused `rcmethod`) is renamed
# `capacitance` to align with the Reading STEBBS team's "Column D" naming
# (D. Hertwig / S. Rognone, 2026-05). This is a PURE KEY RENAME — the field
# stays a `RCMethod` enum with the same values and behaviour; the larger
# relocation under STEBBS and the capacitance-vs-fraction semantics are
# deferred to a future PR. The dev11 spelling `outer_cap_fraction` is a value
# of MODELPHYSICS_RENAMES / MODELPHYSICS_SUFFIX_RENAMES (now retargeted to
# `capacitance`), so this table only needs to catch users who hand-wrote
# `outer_cap_fraction` directly. Applied by the ModelPhysics Pydantic shim
# AFTER the two tables above so dev11 YAMLs still load with a
# DeprecationWarning. NOT spread into ALL_FIELD_RENAMES (it would introduce a
# second alias for the same final name, breaking the one-to-one reverse map
# the Rust bridge depends on); it is composed into RAW_YAML_FIELD_RENAMES for
# the Phase A precheck path.
MODELPHYSICS_DEV12_RENAMES: Dict[str, str] = {
    "outer_cap_fraction": "capacitance",
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
    # Pre-gh#1327 fused -> gh#1390 dev7 final names (chained through the
    # gh#1334 snake_case and gh#1390 Rule-2 reorder layers).
    "WallextThickness": "thickness_wall_outer",
    "WallextEffectiveConductivity": "conductivity_wall_outer",
    "WallextDensity": "density_wall_outer",
    "WallextCp": "specific_heat_capacity_wall_outer",
    "RoofextThickness": "thickness_roof_outer",
    "RoofextEffectiveConductivity": "conductivity_roof_outer",
    "RoofextDensity": "density_roof_outer",
    "RoofextCp": "specific_heat_capacity_roof_outer",
    # Building metadata / geometry. Tier-1 completion (gh#1392) chains the
    # archetype-scoped, geometry, and ratio fields one hop further to their
    # final names: `archetype_*` namespace prefix, `area_*` quantity-first,
    # `ratio_*` category prefix. The snake-intermediate spellings (building_name,
    # footprint_area, ...) remain accepted via ARCHETYPEPROPERTIES_DEV7_RENAMES.
    "BuildingName": "archetype_name",
    "BuildingCount": "archetype_building_count",
    "Occupants": "occupants",
    "stebbs_Height": "archetype_height",
    "FootprintArea": "area_footprint",
    "WallExternalArea": "area_wall_external",
    "RatioInternalVolume": "ratio_internal_mass_volume",
    "InternalMassArea": "area_internal_mass",
    "WWR": "ratio_window_to_wall",
    # Wall (non-ext)
    "WallThickness": "thickness_wall",
    "WallEffectiveConductivity": "conductivity_wall",
    "WallDensity": "density_wall",
    "WallCp": "specific_heat_capacity_wall",
    "WallOuterCapFrac": "fraction_heat_capacity_wall_external",
    "WallExternalEmissivity": "emissivity_wall_external",
    "WallInternalEmissivity": "emissivity_wall_internal",
    "WallTransmissivity": "transmissivity_wall_external",
    "WallAbsorbtivity": "absorptivity_wall_external",  # spelling fix
    "WallReflectivity": "reflectivity_wall_external",
    # Roof (non-ext)
    "RoofThickness": "thickness_roof",
    "RoofEffectiveConductivity": "conductivity_roof",
    "RoofDensity": "density_roof",
    "RoofCp": "specific_heat_capacity_roof",
    "RoofOuterCapFrac": "fraction_heat_capacity_roof_external",
    "RoofExternalEmissivity": "emissivity_roof_external",
    "RoofInternalEmissivity": "emissivity_roof_internal",
    "RoofTransmissivity": "transmissivity_roof_external",
    "RoofAbsorbtivity": "absorptivity_roof_external",  # spelling fix
    "RoofReflectivity": "reflectivity_roof_external",
    # Ground floor (align FloorThickness with GroundFloor* siblings)
    "FloorThickness": "thickness_ground_floor",
    "GroundFloorEffectiveConductivity": "conductivity_ground_floor",
    "GroundFloorDensity": "density_ground_floor",
    "GroundFloorCp": "specific_heat_capacity_ground_floor",
    # Window
    "WindowThickness": "thickness_window",
    "WindowEffectiveConductivity": "conductivity_window",
    "WindowDensity": "density_window",
    "WindowCp": "specific_heat_capacity_window",
    "WindowExternalEmissivity": "emissivity_window_external",
    "WindowInternalEmissivity": "emissivity_window_internal",
    "WindowTransmissivity": "transmissivity_window_external",
    "WindowAbsorbtivity": "absorptivity_window_external",  # spelling fix
    "WindowReflectivity": "reflectivity_window_external",
    # Internal mass
    "InternalMassDensity": "density_internal_mass",
    "InternalMassCp": "specific_heat_capacity_internal_mass",
    "InternalMassEmissivity": "emissivity_internal_mass",
    # HVAC / hot water. Tier-1 completion (gh#1392) chains to final names:
    # power_<system>_<class>_max, temperature_air_*_setpoint,
    # volume_hot_water_tank, and profile_* for the setpoint/metabolism profiles.
    # `water_tank_water_volume` was already unified into the dev-stage
    # `hot_water_tank_volume`; Tier-1 reorders it quantity-first to
    # `volume_hot_water_tank`, keeping the `hot_water_tank_*` component prefix
    # for consistency with its sibling fields (Denise Hertwig col D, 2026-05).
    # snake-intermediate spellings remain accepted via
    # ARCHETYPEPROPERTIES_DEV7_RENAMES.
    "MaxHeatingPower": "power_air_heating_max",
    "WaterTankWaterVolume": "volume_hot_water_tank",
    "MaximumHotWaterHeatingPower": "power_water_heating_max",
    "HeatingSetpointTemperature": "temperature_air_heating_setpoint",
    "CoolingSetpointTemperature": "temperature_air_cooling_setpoint",
    "HeatingSetpointTemperatureProfile": "profile_temperature_air_heating_setpoint",
    "CoolingSetpointTemperatureProfile": "profile_temperature_air_cooling_setpoint",
    "MetabolismProfile": "profile_metabolism",
}

# gh#1329 intermediate PascalCase (`WallExternalThickness`, etc.) -> gh#1334
# final snake_case. NOT spread into ALL_FIELD_RENAMES (keeping the one-to-one
# invariant); the Pydantic shim on ArchetypeProperties runs this after the
# main dict so YAMLs authored at the Schema 2026.5.dev1 / dev2 shape still
# load with a DeprecationWarning.

ARCHETYPEPROPERTIES_PASCAL_RENAMES: Dict[str, str] = {
    "WallExternalThickness": "thickness_wall_outer",
    "WallExternalEffectiveConductivity": "conductivity_wall_outer",
    "WallExternalDensity": "density_wall_outer",
    "WallExternalCp": "specific_heat_capacity_wall_outer",
    "RoofExternalThickness": "thickness_roof_outer",
    "RoofExternalEffectiveConductivity": "conductivity_roof_outer",
    "RoofExternalDensity": "density_roof_outer",
    "RoofExternalCp": "specific_heat_capacity_roof_outer",
}

# Schema 2026.5.dev3 `water_tank_water_volume` -> unified `hot_water_tank_volume`
# (drop the double-`water` redundancy and fold under the `hot_water_tank_*`
# component prefix). NOT spread into ALL_FIELD_RENAMES — keeping the
# one-to-one invariant; the ArchetypeProperties Pydantic shim runs this
# after the main dict so dev3 YAMLs still load with a DeprecationWarning.

ARCHETYPEPROPERTIES_DEV3_RENAMES: Dict[str, str] = {
    "water_tank_water_volume": "hot_water_tank_volume",
}

# Rule 2 ArchetypeProperties rename table. This landed on master as a
# dev6 -> dev7 bump before gh#1372 reserved dev7/dev8 for forcing/output
# control restructures; in the merged lineage it is applied at dev8 -> dev9.
#
# Apply Rule 2 of the SUEWS
# naming convention (`.claude/rules/naming-convention.md`) — physical
# quantity → component → sub-class — to ArchetypeProperties bulk-material
# and surface optical fields. Three orthogonal moves embedded in the
# rename:
#
# * Reorder so the physical quantity (`thickness`, `density`,
#   `conductivity`, `specific_heat_capacity`, `emissivity`,
#   `transmissivity`, `absorptivity`, `reflectivity`) leads.
# * Rename the layer-to-insulation qualifier from `external` to `outer`
#   (xlsx col 3 + convention "Specific tokens": outer/inner = bulk-
#   material layer; external/internal stays for the radiative surface).
# * Drop the `effective_` qualifier on the conductivity rows — the
#   convention requires it only when partner fields (density, specific
#   heat capacity) also use it, which they do not.
#
# Walls also get the `fraction_*` non-physical category prefix moved to
# the front (`wall_outer_heat_capacity_fraction` ->
# `fraction_heat_capacity_wall_external`).
#
# Applied directly by the ArchetypeProperties Pydantic shim so users on
# the dev6 spelling still load with a DeprecationWarning. The dev7 values
# also flow into the canonical maps above (their literal values were
# updated in lockstep with this delta) so ALL_FIELD_RENAMES, the Rust
# YAML preprocessor mirror, and the bridge DataFrame column lookup all
# point at the current dev7 final names.

ARCHETYPEPROPERTIES_DEV6_RENAMES: Dict[str, str] = {
    # -- Walls --
    # Whole-wall bulk material
    "wall_thickness": "thickness_wall",
    "wall_effective_conductivity": "conductivity_wall",
    "wall_density": "density_wall",
    "wall_specific_heat_capacity": "specific_heat_capacity_wall",
    # Outer-layer (external-to-insulation) bulk material
    "wall_external_thickness": "thickness_wall_outer",
    "wall_external_effective_conductivity": "conductivity_wall_outer",
    "wall_external_density": "density_wall_outer",
    "wall_external_specific_heat_capacity": "specific_heat_capacity_wall_outer",
    # Heat-capacity distribution (non-physical, fraction_* leads)
    "wall_outer_heat_capacity_fraction": "fraction_heat_capacity_wall_external",
    # Wall radiative surface (external/internal stays — surface property)
    "wall_external_emissivity": "emissivity_wall_external",
    "wall_internal_emissivity": "emissivity_wall_internal",
    "wall_transmissivity": "transmissivity_wall_external",
    "wall_absorptivity": "absorptivity_wall_external",
    "wall_reflectivity": "reflectivity_wall_external",
    # -- Roofs (mirror of wall pattern) --
    "roof_thickness": "thickness_roof",
    "roof_effective_conductivity": "conductivity_roof",
    "roof_density": "density_roof",
    "roof_specific_heat_capacity": "specific_heat_capacity_roof",
    "roof_external_thickness": "thickness_roof_outer",
    "roof_external_effective_conductivity": "conductivity_roof_outer",
    "roof_external_density": "density_roof_outer",
    "roof_external_specific_heat_capacity": "specific_heat_capacity_roof_outer",
    "roof_outer_heat_capacity_fraction": "fraction_heat_capacity_roof_external",
    "roof_external_emissivity": "emissivity_roof_external",
    "roof_internal_emissivity": "emissivity_roof_internal",
    "roof_transmissivity": "transmissivity_roof_external",
    "roof_absorptivity": "absorptivity_roof_external",
    "roof_reflectivity": "reflectivity_roof_external",
    # -- Windows (no internal-mass bulk; same surface optical set) --
    "window_thickness": "thickness_window",
    "window_effective_conductivity": "conductivity_window",
    "window_density": "density_window",
    "window_specific_heat_capacity": "specific_heat_capacity_window",
    "window_external_emissivity": "emissivity_window_external",
    "window_internal_emissivity": "emissivity_window_internal",
    "window_transmissivity": "transmissivity_window_external",
    "window_absorptivity": "absorptivity_window_external",
    "window_reflectivity": "reflectivity_window_external",
    # -- Ground floor (single bulk layer; no surface optical set) --
    "ground_floor_thickness": "thickness_ground_floor",
    "ground_floor_effective_conductivity": "conductivity_ground_floor",
    "ground_floor_density": "density_ground_floor",
    "ground_floor_specific_heat_capacity": "specific_heat_capacity_ground_floor",
    # -- Internal mass --
    "internal_mass_density": "density_internal_mass",
    "internal_mass_specific_heat_capacity": "specific_heat_capacity_internal_mass",
    "internal_mass_emissivity": "emissivity_internal_mass",
}


# Tier 1 completion (gh#1392) on ArchetypeProperties. Maps the
# snake-intermediate spellings (the names users authored against the dev-stage
# layout) to the final Tier-1 names. The canonical ARCHETYPEPROPERTIES_RENAMES
# values above were chained one hop further to these same finals, so a user on
# the PascalCase legacy form resolves in a single pass while a user on the
# snake-intermediate form resolves through this table. Three orthogonal moves:
#
# * Archetype namespace prefix (Rule 2 exception) — fields describing
#   the archetype as a whole take the `archetype_*` prefix:
#   `building_name` -> `archetype_name`,
#   `building_count` -> `archetype_building_count`,
#   `building_height` -> `archetype_height`.
# * Geometry Rule 2 reorder — physical quantity leads:
#   `footprint_area` -> `area_footprint`,
#   `wall_external_area` -> `area_wall_external`, etc. The `ratio_*`
#   non-physical category prefix leads for fraction-style fields:
#   `internal_volume_ratio` -> `ratio_internal_mass_volume`,
#   `window_to_wall_ratio` -> `ratio_window_to_wall`.
# * HVAC + setpoint air_/water_ qualifier (per the convention's
#   "Specific tokens" section) — every heating/cooling power and
#   setpoint disambiguates between air-system and DHW-system control:
#   `heating_setpoint_temperature` -> `temperature_air_heating_setpoint`,
#   `max_heating_power` -> `power_air_heating_max`,
#   `maximum_hot_water_heating_power` -> `power_water_heating_max`, etc.
#   Setpoint profiles take the `profile_*` non-physical category prefix:
#   `metabolism_profile` -> `profile_metabolism`.

ARCHETYPEPROPERTIES_DEV7_RENAMES: Dict[str, str] = {
    # Archetype namespace
    "building_name": "archetype_name",
    "building_count": "archetype_building_count",
    "building_height": "archetype_height",
    # Geometry — quantity leads
    "footprint_area": "area_footprint",
    "wall_external_area": "area_wall_external",
    "internal_mass_area": "area_internal_mass",
    # Geometry — non-physical, ratio_* leads
    "internal_volume_ratio": "ratio_internal_mass_volume",
    "window_to_wall_ratio": "ratio_window_to_wall",
    # HVAC max powers — power_<system>_<sub_class>_max
    "max_heating_power": "power_air_heating_max",
    "maximum_hot_water_heating_power": "power_water_heating_max",
    # DHW tank volume — quantity leads; keep `hot_water_tank_*` component prefix
    "hot_water_tank_volume": "volume_hot_water_tank",
    # Setpoints — temperature leads + air_/water_ qualifier
    "heating_setpoint_temperature": "temperature_air_heating_setpoint",
    "cooling_setpoint_temperature": "temperature_air_cooling_setpoint",
    # Setpoint profiles — profile_* category prefix leads
    "heating_setpoint_temperature_profile": "profile_temperature_air_heating_setpoint",
    "cooling_setpoint_temperature_profile": "profile_temperature_air_cooling_setpoint",
    "metabolism_profile": "profile_metabolism",
    # Heat-capacity distribution moved from the outer-layer (`outer`) framing to
    # the external radiative node (`external`) per the Reading STEBBS team
    # (D. Hertwig col D, 2026-05): the gh#1390 final spelling
    # `fraction_wall/roof_heat_capacity_outer` migrates one hop further here.
    "fraction_wall_heat_capacity_outer": "fraction_heat_capacity_wall_external",
    "fraction_roof_heat_capacity_outer": "fraction_heat_capacity_roof_external",
}


# Schema 2026.5.dev11 -> 2026.5.dev12: align six ArchetypeProperties HVAC
# power / setpoint fields with the Reading STEBBS team's "Column D" naming
# (D. Hertwig / S. Rognone, 2026-05). The powers take the qualifier-first
# `max_power_heating_system_<air|water>` shape and the setpoints (and their
# profiles) take `setpoint_temperature_<heating|cooling>_air`. The pre-dev12
# names are dev7-level values of ARCHETYPEPROPERTIES_RENAMES, so the
# dev12->ancestry chain resolves through that map (see
# ARCHETYPEPROPERTIES_DEV7_TO_PASCAL below). Pure snake->snake reorders applied
# AFTER ARCHETYPEPROPERTIES_DEV7_RENAMES so users on the dev7 spelling still
# load with a DeprecationWarning.
ARCHETYPEPROPERTIES_DEV12_RENAMES: Dict[str, str] = {
    "power_air_heating_max": "max_power_heating_system_air",
    "power_water_heating_max": "max_power_heating_system_water",
    "temperature_air_heating_setpoint": "setpoint_temperature_heating_air",
    "temperature_air_cooling_setpoint": "setpoint_temperature_cooling_air",
    "profile_temperature_air_heating_setpoint": "profile_setpoint_temperature_heating_air",
    "profile_temperature_air_cooling_setpoint": "profile_setpoint_temperature_cooling_air",
}


# Reverse map: final name -> PascalCase legacy column name. Used by
# ArchetypeProperties._ARCHETYPE_LEGACY_COL_NAMES so the Fortran/Rust bridge
# (keyed on the fused lowercased PascalCase, e.g. `wallextthickness`) still
# resolves from the current Pydantic field name. The canonical
# ARCHETYPEPROPERTIES_RENAMES values are the dev7 final names (Rule-2 reorder
# from PR#1395 plus the Tier-1 completion in gh#1392), so the base layer is
# just its inverse. The dev12 Column D alignment then chains each new name back
# to the same PascalCase ancestry its pre-dev12 name resolves to (e.g.
# `max_power_heating_system_air` -> `MaxHeatingPower`), so the bridge column key
# is unchanged after the rename.
def _build_archetype_dev_to_pascal() -> Dict[str, str]:
    base_reverse: Dict[str, str] = {
        new: old for old, new in ARCHETYPEPROPERTIES_RENAMES.items()
    }
    chain: Dict[str, str] = dict(base_reverse)
    for old_dev11, new_dev12 in ARCHETYPEPROPERTIES_DEV12_RENAMES.items():
        chain[new_dev12] = base_reverse.get(old_dev11, old_dev11)
    return chain


ARCHETYPEPROPERTIES_DEV7_TO_PASCAL: Dict[str, str] = _build_archetype_dev_to_pascal()

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

# Schema 2026.5.dev8 -> 2026.5.dev9: apply Rule 2 of the SUEWS naming
# convention to StebbsProperties. Three orthogonal moves:
#
# * Rule 2 reorder — physical quantity leads:
#   `wall_internal_convection_coefficient` ->
#   `convection_coefficient_wall_internal`,
#   `external_ground_conductivity` -> `thermal_conductivity_ground`,
#   `hot_water_tank_wall_thickness` ->
#   `thickness_hot_water_tank_wall`, etc.
# * `floor` -> `ground_floor` per the convention's Specific tokens
#   ("ground_floor" is two words):
#   `floor_internal_convection_coefficient` ->
#   `convection_coefficient_ground_floor_internal`.
# * HVAC + setpoint air_/water_ qualifier (mirrors the dev7 -> dev8
#   ArchetypeProperties pass): `heating_system_efficiency` ->
#   `efficiency_heating_system_air`, `max_cooling_power` ->
#   `power_air_cooling_max`, `cooling_system_cop` ->
#   `efficiency_cooling_system_air`,
#   `hot_water_heating_setpoint_temperature` ->
#   `temperature_water_heating_setpoint`,
#   `hot_water_heating_efficiency` -> `efficiency_heating_system_water`.
#
# Non-physical category prefixes lead for non-quantity fields:
# `metabolism_threshold` -> `threshold_metabolism`,
# `latent_sensible_ratio` -> `ratio_latent_sensible`,
# `daylight_control` -> `control_daylight`,
# `lighting_illuminance_threshold` -> `threshold_lighting_illuminance`,
# `appliance_profile` -> `profile_appliance`,
# `hot_water_flow_profile` -> `profile_hot_water_flow`.
#
# Initial / climatology temperatures take the `temperature_*` quantity
# prefix and trailing `initial` / `annual_mean` sub-class:
# `initial_outdoor_temperature` -> `temperature_air_outdoor_initial`,
# `annual_mean_air_temperature` -> `temperature_air_annual_mean`.
#
# These four straggler fields were left as compound nouns at dev9; they
# are reordered quantity-first at dev12 (gh#1392 follow-up) per the
# Reading STEBBS team review ("Column D", D. Hertwig / S. Rognone,
# 2026-05): `ground_depth` -> `depth_ground`,
# `ventilation_rate` -> `rate_ventilation`,
# `lighting_power_density` -> `power_density_lighting`,
# `month_mean_air_temperature_diffmax` ->
# `temperature_air_month_mean_diffmax`. See STEBBSPROPERTIES_DEV12_RENAMES
# below.

STEBBSPROPERTIES_DEV8_RENAMES: Dict[str, str] = {
    # Convection coefficients (Rule 2 reorder + floor -> ground_floor)
    "wall_internal_convection_coefficient": "convection_coefficient_wall_internal",
    "roof_internal_convection_coefficient": "convection_coefficient_roof_internal",
    "internal_mass_convection_coefficient": "convection_coefficient_internal_mass",
    "floor_internal_convection_coefficient": "convection_coefficient_ground_floor_internal",
    "window_internal_convection_coefficient": "convection_coefficient_window_internal",
    "wall_external_convection_coefficient": "convection_coefficient_wall_external",
    "roof_external_convection_coefficient": "convection_coefficient_roof_external",
    "window_external_convection_coefficient": "convection_coefficient_window_external",
    # Ground (external = beyond building footprint per convention)
    "external_ground_conductivity": "thermal_conductivity_ground",
    # Metabolism + occupant control (non-physical category prefixes)
    "metabolism_threshold": "threshold_metabolism",
    "latent_sensible_ratio": "ratio_latent_sensible",
    # Daylight + lighting controls (non-physical category prefixes)
    "daylight_control": "control_daylight",
    "lighting_illuminance_threshold": "threshold_lighting_illuminance",
    # Heating / cooling system (Rule 2 + air_/water_ qualifier)
    "heating_system_efficiency": "efficiency_heating_system_air",
    "max_cooling_power": "power_air_cooling_max",
    "cooling_system_cop": "efficiency_cooling_system_air",
    # Initial / climatology temperatures
    "initial_outdoor_temperature": "temperature_air_outdoor_initial",
    "initial_indoor_temperature": "temperature_air_indoor_initial",
    "annual_mean_air_temperature": "temperature_air_annual_mean",
    # Hot water tank — bulk + walls (Rule 2 reorder)
    "hot_water_tank_wall_thickness": "thickness_hot_water_tank_wall",
    "mains_water_temperature": "temperature_water_mains",
    "hot_water_tank_surface_area": "area_hot_water_tank_surface",
    "hot_water_heating_setpoint_temperature": "temperature_water_heating_setpoint",
    "hot_water_tank_wall_emissivity": "emissivity_hot_water_tank_wall",
    "hot_water_tank_wall_conductivity": "conductivity_hot_water_tank_wall",
    "hot_water_tank_wall_density": "density_hot_water_tank_wall",
    "hot_water_tank_specific_heat_capacity": "specific_heat_capacity_hot_water_tank_wall",
    "hot_water_tank_internal_wall_convection_coefficient":
        "convection_coefficient_hot_water_tank_wall_internal",
    "hot_water_tank_external_wall_convection_coefficient":
        "convection_coefficient_hot_water_tank_wall_external",
    # Hot water vessel — in-use water containers (Rule 2 reorder)
    "hot_water_vessel_wall_thickness": "thickness_hot_water_vessel_wall",
    "hot_water_vessel_wall_conductivity": "conductivity_hot_water_vessel_wall",
    "hot_water_vessel_density": "density_hot_water_vessel_wall",
    "hot_water_vessel_specific_heat_capacity": "specific_heat_capacity_hot_water_vessel_wall",
    "hot_water_vessel_internal_wall_convection_coefficient":
        "convection_coefficient_hot_water_vessel_wall_internal",
    "hot_water_vessel_external_wall_convection_coefficient":
        "convection_coefficient_hot_water_vessel_wall_external",
    "hot_water_vessel_wall_emissivity": "emissivity_hot_water_vessel_wall",
    # Hot water (the water itself, in use) — Rule 2 reorder
    "hot_water_volume": "volume_hot_water",
    "hot_water_surface_area": "area_hot_water_surface",
    "hot_water_flow_rate": "rate_hot_water_flow",
    "hot_water_density": "density_hot_water",
    "hot_water_specific_heat_capacity": "specific_heat_capacity_hot_water",
    "hot_water_heating_efficiency": "efficiency_heating_system_water",
    # Profiles (non-physical: profile_* category prefix leads)
    "hot_water_flow_profile": "profile_hot_water_flow",
    "appliance_profile": "profile_appliance",
}


# Schema 2026.5.dev11 -> 2026.5.dev12: align STEBBS and Archetype field names
# with the Reading STEBBS team's "Column D" naming (D. Hertwig / S. Rognone,
# 2026-05).
#
# The first four entries are the original stragglers that dev9 deliberately
# kept as compound nouns; their old names are dev8-level values of
# STEBBSPROPERTIES_RENAMES so they chain straight back to PascalCase.
#
# The remaining ten entries extend the Column D alignment to fields that dev9
# already reordered once: powers and setpoints take the qualifier-first
# `max_power_*` / `setpoint_temperature_*` shape, the mains-water / hot-water
# surface-area / flow-rate / flow-profile fields take the Tier-3 word order,
# `control_daylight` reverts to the Reading idiom `daylight_control`, and the
# DHW vessel convection coefficients move to `hot_water_tank_vessel`. All ten
# old names are dev9-level values of STEBBSPROPERTIES_DEV8_RENAMES, so the
# dev12->ancestry chain must resolve THROUGH the dev9->PascalCase map (see
# _build_stebbs_dev_to_pascal) rather than the dev8 base reverse map.
#
# All are pure snake->snake reorders applied AFTER STEBBSPROPERTIES_DEV8_RENAMES
# so users on the dev9 spelling still load with a DeprecationWarning.
STEBBSPROPERTIES_DEV12_RENAMES: Dict[str, str] = {
    # Original stragglers (gh#1392 follow-up)
    "ground_depth": "depth_ground",
    "ventilation_rate": "rate_ventilation",
    "lighting_power_density": "power_density_lighting",
    "month_mean_air_temperature_diffmax": "temperature_air_month_mean_diffmax",
    # Column D alignment — qualifier-first powers
    "power_air_cooling_max": "max_power_cooling_system_air",
    # Column D alignment — qualifier-first water setpoint
    "temperature_water_heating_setpoint": "setpoint_temperature_heating_water",
    # Column D alignment — Tier-3 word order
    "temperature_water_mains": "temperature_mains_water",
    "area_hot_water_tank_surface": "surface_area_hot_water_tank",
    "area_hot_water_surface": "surface_area_hot_water",
    "rate_hot_water_flow": "rate_flow_hot_water",
    "profile_hot_water_flow": "profile_flow_hot_water",
    # Reading idiom — daylight control flag
    "control_daylight": "daylight_control",
    # DHW vessel convection coefficients -> hot_water_tank_vessel
    "convection_coefficient_hot_water_vessel_wall_internal":
        "convection_coefficient_hot_water_tank_vessel_internal",
    "convection_coefficient_hot_water_vessel_wall_external":
        "convection_coefficient_hot_water_tank_vessel_external",
}


# DEV8 entries that DEV12 reverts must NOT fire in the Pydantic
# `_rename_stebbs_fields` chain. Currently this is only
# `daylight_control -> control_daylight` (DEV8), reverted by
# `control_daylight -> daylight_control` (DEV12). Applying both in sequence
# renames a current canonical `daylight_control` to `control_daylight` and back,
# emitting a spurious DeprecationWarning for a valid config (and failing where
# `DeprecationWarning` is promoted to an error). The FULL DEV8 map is still used
# for the bridge legacy-col chain (`_build_stebbs_dev_to_pascal`) and the
# raw-YAML composed map (which prunes the 2-cycle itself), so only the Pydantic
# application needs the pruned form.
STEBBSPROPERTIES_DEV8_RENAMES_PYDANTIC: Dict[str, str] = {
    old: new
    for old, new in STEBBSPROPERTIES_DEV8_RENAMES.items()
    if STEBBSPROPERTIES_DEV12_RENAMES.get(new) != old
}

# Chained reverse map: dev9 final name -> PascalCase legacy column name.
# Used by StebbsProperties._STEBBS_LEGACY_COL_NAMES so the Fortran/Rust
# bridge (keyed on the fused lowercased PascalCase, e.g.
# `wallinternalconvectioncoefficient`) still resolves from the dev9
# Pydantic field name. Composes through STEBBSPROPERTIES_DEV8_RENAMES
# (dev9 -> dev8) and the base STEBBSPROPERTIES_RENAMES {dev8:
# PascalCase} reverse map, plus the dev12 straggler reorders chained back
# to the same PascalCase ancestry. STEBBSPROPERTIES_DEV3_RENAMES is NOT chained
# here because its targets are the same dev8 names already covered by
# STEBBSPROPERTIES_RENAMES — passing through the dev3 layer would
# silently rewrite the PascalCase ancestry.
def _build_stebbs_dev_to_pascal() -> Dict[str, str]:
    base_reverse: Dict[str, str] = {
        new: old for old, new in STEBBSPROPERTIES_RENAMES.items()
    }

    chain: Dict[str, str] = {}
    for old_dev8, new_dev9 in STEBBSPROPERTIES_DEV8_RENAMES.items():
        chain[new_dev9] = base_reverse.get(old_dev8, old_dev8)
    # dev12 Column D alignment (gh#1392 follow-up): each new name chains back to
    # the same PascalCase ancestry the old name resolves to. The pre-dev12 names
    # split into two cases:
    #   * the four original stragglers (`ground_depth`, ...) are dev8-level
    #     values of STEBBSPROPERTIES_RENAMES, so `base_reverse` holds the
    #     PascalCase key directly (e.g. `ground_depth` -> `GroundDepth`);
    #   * the ten new fields (`power_air_cooling_max`, ...) are dev9-level
    #     names produced BY the dev8 reorder, so their PascalCase ancestry lives
    #     in `chain` (built above), not in `base_reverse` (e.g.
    #     `power_air_cooling_max` -> `MaxCoolingPower`).
    # Resolve through `chain` first, then the dev8 base reverse, before falling
    # back to the snake name — otherwise the ten new fields would wrongly fall
    # back to the intermediate snake spelling and break the bridge column key.
    for old_dev11, new_dev12 in STEBBSPROPERTIES_DEV12_RENAMES.items():
        chain[new_dev12] = chain.get(
            old_dev11, base_reverse.get(old_dev11, old_dev11)
        )
    return chain


STEBBSPROPERTIES_DEV9_TO_PASCAL: Dict[str, str] = _build_stebbs_dev_to_pascal()

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

# -- STEBBS state (suews_type_stebbs.f95) -------------------------------------
#
# STEBBS_PRM + BUILDING_ARCHETYPE_PRM user-facing fields are covered by
# STEBBSPROPERTIES_RENAMES and ARCHETYPEPROPERTIES_RENAMES (gh#1334).
# This dict holds the Fortran-only internal-state members under
# STEBBS_STATE / NHOOD_STATE that the Tier D sweep also renames.

STEBBSSTATE_RENAMES: Dict[str, str] = {
    "IndoorAirDensity": "indoor_air_density",
    "IndoorAirCp": "indoor_air_cp",
    "Afootprint": "a_footprint",
    "wallExternalArea": "wall_external_area_state",
    "ratioInternalVolume": "ratio_internal_volume_state",
    "windowTransmissivity": "window_transmissivity_state",
    "windowAbsorbtivity": "window_absorbtivity_state",
    "windowReflectivity": "window_reflectivity_state",
    "wallTransmisivity": "wall_transmissivity_state",
    "wallAbsorbtivity": "wall_absorbtivity_state",
    "wallReflectivity": "wall_reflectivity_state",
    "roofTransmisivity": "roof_transmissivity_state",
    "roofAbsorbtivity": "roof_absorbtivity_state",
    "roofReflectivity": "roof_reflectivity_state",
    "occupants": "occupants_state",
    "Awall": "a_wall",
    "Aroof": "a_roof",
    "Vwall": "v_wall",
    "Vroof": "v_roof",
    "Vgroundfloor": "v_ground_floor",
    "Awindow": "a_window",
    "Vwindow": "v_window",
    "Vindoormass": "v_indoor_mass",
    "Aindoormass": "a_indoor_mass",
    "Tindoormass": "t_indoor_mass",
    "Tintwall": "t_int_wall",
    "Tintroof": "t_int_roof",
    "Textwall": "t_ext_wall",
    "Textroof": "t_ext_roof",
    "Tintwindow": "t_int_window",
    "Textwindow": "t_ext_window",
    "Tintgroundfloor": "t_int_ground_floor",
    "Textgroundfloor": "t_ext_ground_floor",
    "Ts": "ts",
    "Cp": "cp",
    "wiTAR": "witar",
    "HTsAverage": "h_ts_average",
    "HWPowerAverage": "hw_power_average",
    "EnergyExchanges": "energy_exchanges",
    "Kdown2d": "kdown_2d",
    "Kup2d": "kup_2d",
    "Kwest": "k_west",
    "Ksouth": "k_south",
    "Knorth": "k_north",
    "Keast": "k_east",
    "Ldown2d": "ldown_2d",
    "Lup2d": "lup_2d",
    "Lwest": "l_west",
    "Lsouth": "l_south",
    "Lnorth": "l_north",
    "Least": "l_east",
    "zarray": "z_array",
    "dataoutLineURSL": "dataout_line_u_rsl",
    "dataoutLineTRSL": "dataout_line_t_rsl",
    "dataoutLineqRSL": "dataout_line_q_rsl",
    "DeepSoilTemperature": "deep_soil_temperature",
    "OutdoorAirStartTemperature": "outdoor_air_start_temperature",
    "IndoorAirStartTemperature": "indoor_air_start_temperature",
    "IndoorMassStartTemperature": "indoor_mass_start_temperature",
    "WallIndoorSurfaceTemperature": "wall_indoor_surface_temperature",
    "WallOutdoorSurfaceTemperature": "wall_outdoor_surface_temperature",
    "RoofIndoorSurfaceTemperature": "roof_indoor_surface_temperature",
    "RoofOutdoorSurfaceTemperature": "roof_outdoor_surface_temperature",
    "WindowIndoorSurfaceTemperature": "window_indoor_surface_temperature",
    "WindowOutdoorSurfaceTemperature": "window_outdoor_surface_temperature",
    "GroundFloorIndoorSurfaceTemperature": "ground_floor_indoor_surface_temperature",
    "GroundFloorOutdoorSurfaceTemperature": "ground_floor_outdoor_surface_temperature",
    "WaterTankTemperature": "water_tank_temperature_state",
    "InternalWallWaterTankTemperature": "internal_wall_water_tank_temperature",
    "ExternalWallWaterTankTemperature": "external_wall_water_tank_temperature",
    "DomesticHotWaterTemperatureInUseInBuilding": "domestic_hot_water_temperature_in_use_in_building",
    "InternalWallDHWVesselTemperature": "internal_wall_dhw_vessel_temperature",
    "ExternalWallDHWVesselTemperature": "external_wall_dhw_vessel_temperature",
    "fnmlLBM": "fnml_lbm",
    "idLBM": "id_lbm",
    "CASE": "case_id",
}

# -- Landcover types (suews_type_landcover.f95) -------------------------------
#
# ``statelimit``, ``wetthresh``, ``maxconductance``, ``capmax_dec``,
# ``capmin_dec``, ``pormin_dec``, ``pormax_dec``, ``faidectree``,
# ``dectreeh``, ``faievetree``, ``evetreeh`` are already handled by
# existing registry entries (SURFACEPROPERTIES_RENAMES,
# VEGETATEDSURFACEPROPERTIES_RENAMES, DECTRPROPERTIES_RENAMES, ...).
# Only the gaps are registered here.

LANDCOVER_RENAMES: Dict[str, str] = {
    "irrfracpaved": "irrigation_fraction_paved",
    "irrfracbldgs": "irrigation_fraction_bldgs",
    "irrfracdectr": "irrigation_fraction_dectr",
    "irrfracevetr": "irrigation_fraction_evetr",
    "irrfracgrass": "irrigation_fraction_grass",
    "irrfracbsoil": "irrigation_fraction_bsoil",
    "irrfracwater": "irrigation_fraction_water",
    "faibldg": "fai_building",
    "bldgh": "height_building",
    "flowchange": "flow_change",
}

# -- HEAT_STATE (suews_type_heat.f95) -----------------------------------------

HEATSTATE_RENAMES: Dict[str, str] = {
    "kclear": "k_clear",
    "kup": "k_up",
    "ldown": "l_down",
    "lup": "l_up",
}

# -- HYDRO_STATE (suews_type_hydro.f95) ---------------------------------------

HYDROSTATE_RENAMES: Dict[str, str] = {
    "soilstore_surf": "soil_store_surf",
    "soilstore_roof": "soil_store_roof",
    "soilstore_wall": "soil_store_wall",
    "runoffSoil": "runoff_soil",
    "runoffAGveg": "runoff_ag_veg",
    "runoffAGimpervious": "runoff_ag_impervious",
    "runoffPipes": "runoff_pipes",
    "runoffwaterbody": "runoff_waterbody",
    "runoffSoil_per_tstep": "runoff_soil_per_tstep",
    "SoilState": "soil_state",
    "SoilMoistCap": "soil_moist_cap",
    "AdditionalWater": "additional_water",
    "addImpervious": "add_impervious",
    "addPipes": "add_pipes",
    "addVeg": "add_veg",
    "addWaterBody": "add_water_body",
    "AddWater": "add_water",
    "NWstate_per_tstep": "nw_state_per_tstep",
}

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
# its current final name on the user-facing YAML / Pydantic surface. It
# flows into ``RAW_YAML_FIELD_RENAMES`` (Phase A preflight) and into the
# Rust preprocessor via ``src/suews_bridge/src/field_renames.rs`` (parity
# enforced by ``scripts/lint/check_rust_yaml_aliases.py``), so every entry
# has to be a real YAML key a user might write.
#
# ``MODELPHYSICS_SUFFIX_RENAMES``, ``ARCHETYPEPROPERTIES_PASCAL_RENAMES``,
# and ``SNOWPARAMS_INTERMEDIATE_RENAMES`` are deliberately NOT spread here
# — they carry schema-intermediate aliases that would introduce a second
# alias per final name, which the Rust bridge's reverse lookup
# (``ALL_FIELD_RENAMES`` inverted) cannot represent. The Pydantic shim on
# each affected class runs both the main dict and its intermediate dict
# in sequence so users on any prior dev-cycle shape still load with a
# DeprecationWarning.
#
# The gh#1326 Tier D sub-dicts (``EHC_RENAMES`` … ``STEBBSSTATE_RENAMES``)
# are deliberately NOT spread here either — they describe Fortran TYPE
# members that the Rust↔Fortran bridge reaches via positional (indexed)
# access, not by name. Adding them to ``ALL_FIELD_RENAMES`` would
# (a) pollute the raw-YAML preflight with identifiers the YAML surface
# never carried, and (b) introduce genuine collisions with user-facing
# final names already reachable from a different sub-dict (e.g.
# ``STEBBSSTATE_RENAMES["occupants"]`` vs.
# ``ARCHETYPEPROPERTIES_RENAMES["Occupants"] -> "occupants"``). They live
# under ``FORTRAN_INTERNAL_RENAMES`` below for registry completeness.

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
}

# -- Fortran-internal rename registry (gh#1326 Tier D) ------------------------
#
# Sibling registry to ``ALL_FIELD_RENAMES`` covering Fortran TYPE-member
# renames that do NOT surface on the user-facing YAML / Pydantic boundary.
# Kept here so the rename history for every cross-layer identifier is
# discoverable from a single source of truth, without leaking
# Fortran-internal identifiers into the YAML preflight or Rust
# preprocessor. Per gh#1326 the Fortran bridge uses positional access
# only — these sub-dicts are documentation, not an input-shape contract.
FORTRAN_INTERNAL_RENAMES: Dict[str, str] = {
    **EHC_RENAMES,
    **SNOWSTATE_RENAMES,
    **WATERDIST_RENAMES,
    **PHENOLOGYSTATE_RENAMES,
    **ANTHRO_RENAMES,
    **ATMOSPHERE_RENAMES,
    **SURFACE_RENAMES,
    **HEATSTATE_RENAMES,
    **HYDROSTATE_RENAMES,
    **LANDCOVER_RENAMES,
    **STEBBSSTATE_RENAMES,
}


def _compose_rename_chains(*tables: Mapping[str, str]) -> Dict[str, str]:
    """Compose raw-YAML rename aliases so every source reaches today's target.

    Some accepted schema spellings are intermediate names from earlier dev
    labels, e.g. ``WallextThickness`` -> ``wall_external_thickness`` ->
    ``thickness_wall_outer``. Pydantic applies those tables sequentially, but
    raw-dict callers use a single recursive pass. Composing the tables here
    keeps those callers from stopping one schema label behind.

    Reversal handling
    -----------------
    A later table may *revert* an earlier rename — e.g.
    ``STEBBSPROPERTIES_DEV8_RENAMES`` mapped ``daylight_control`` ->
    ``control_daylight`` and ``STEBBSPROPERTIES_DEV12_RENAMES`` maps it back,
    ``control_daylight`` -> ``daylight_control`` (Reading STEBBS team Column D
    review, 2026-05). Naively combining the tables yields the 2-cycle
    ``daylight_control -> control_daylight -> daylight_control``. The latest
    table wins: the reverted edge from the earlier table is dropped, so both
    spellings resolve to the dev12 final (``daylight_control``).
    """
    combined: Dict[str, str] = {}
    pair_origin: Dict[str, int] = {}  # old_name -> index of the table that set it
    for idx, table in enumerate(tables):
        for old, new in table.items():
            combined[old] = new
            pair_origin[old] = idx

    # Prune direct 2-cycles (A->B and B->A) by honouring the later table.
    for a in list(combined):
        b = combined.get(a)
        if b is not None and combined.get(b) == a and a != b:
            # `a->b` and `b->a` form a reversal. Keep whichever edge came
            # from the later table; drop the earlier one so the chain walk
            # terminates at the later table's target.
            if pair_origin.get(a, -1) < pair_origin.get(b, -1):
                # `b->a` is newer; `a` is the canonical final. Drop `a->b`.
                del combined[a]
            else:
                del combined[b]

    resolved: Dict[str, str] = {}
    for old_name, first_target in combined.items():
        seen = {old_name}
        target = first_target
        while target in combined:
            if target in seen:
                chain = " -> ".join([*seen, target])
                raise ValueError(f"Cyclic field rename chain detected: {chain}")
            seen.add(target)
            target = combined[target]
        resolved[old_name] = target
    return resolved


# Raw-YAML structural checks (Phase A / precheck) need a wider view than the
# one-to-one public registry above: they must accept both the final public
# names and the short-lived Schema 2026.5 intermediate ModelPhysics aliases
# plus the gh#1334 Archetype/Snow intermediate aliases.
# Keeping this separate preserves the bridge-safe one-to-one contract of
# ``ALL_FIELD_RENAMES`` while letting raw-dict callers normalise older YAMLs
# before they compare against the current sample schema.
RAW_YAML_FIELD_RENAMES: Dict[str, str] = _compose_rename_chains(
    MODELPHYSICS_SUFFIX_RENAMES,
    MODELPHYSICS_DEV12_RENAMES,
    ARCHETYPEPROPERTIES_PASCAL_RENAMES,
    ARCHETYPEPROPERTIES_DEV3_RENAMES,
    ARCHETYPEPROPERTIES_DEV6_RENAMES,
    ARCHETYPEPROPERTIES_DEV7_RENAMES,
    ARCHETYPEPROPERTIES_DEV12_RENAMES,
    SNOWPARAMS_INTERMEDIATE_RENAMES,
    STEBBSPROPERTIES_DEV3_RENAMES,
    STEBBSPROPERTIES_DEV8_RENAMES,
    STEBBSPROPERTIES_DEV12_RENAMES,
    ALL_FIELD_RENAMES,
)

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
    # The dev11 spelling ``outer_cap_fraction`` -> ``capacitance`` (dev12)
    # is included so raw-YAML prechecks that read by the current name still
    # find a legacy ``outer_cap_fraction`` key. Spread before RENAMES so the
    # reverse map prefers the fused legacy ``rcmethod`` for ``capacitance``.
    **MODELPHYSICS_SUFFIX_RENAMES,
    **MODELPHYSICS_DEV12_RENAMES,
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


# gh#1456: STEBBS flat -> nested fold.
#
# The six STEBBS-scoped switches used to sit flat on `model.physics`. They now
# live under a nested `model.physics.stebbs` object. The legacy master toggle
# (`stebbs`, a tri-state StebbsMethod integer) is split into `enabled` (bool)
# and `parameters` (DEFAULT/PROVIDED). The leaf-name moves for the other five
# switches are recorded here so the fold is auditable in one place.
STEBBS_PHYSICS_LEAF_RENAMES: Dict[str, str] = {
    # legacy flat key (already snake_case after MODELPHYSICS_*_RENAMES) -> nested leaf.
    # After the MODELPHYSICS_* rename pass the former `rcmethod` / `rc_method`
    # land on the dev12 flat name `capacitance` (Reading Column D, gh#1452);
    # the older dev2-era flat spelling `outer_cap_fraction` is accepted too so a
    # hand-written legacy YAML still folds. Both relocate to `stebbs.capacitance`.
    "capacitance": "capacitance",
    "outer_cap_fraction": "capacitance",
    "rcmethod": "capacitance",
    "rc_method": "capacitance",
    "setpoint": "setpoint",
    "setpointmethod": "setpoint",
    "same_albedo_wall": "same_albedo_wall",
    "same_albedo_roof": "same_albedo_roof",
    "same_emissivity_wall": "same_emissivity_wall",
    "same_emissivity_roof": "same_emissivity_roof",
}

# The nested-object keys (so a YAML already in the new shape passes through).
#
# gh#1456: `ref` is deliberately NOT in this discriminating set. A legacy flat
# master toggle carrying provenance is a RefValue scalar -- `{value: 1, ref:
# {...}}` -- whose only keys are `value`/`ref`; including `ref` here would
# misclassify that scalar as the new nested object, silently dropping its
# `value` and defaulting `enabled` to false. A genuine nested block always
# carries `enabled`/`parameters`/a leaf name alongside any optional `ref`, so
# `ref` is never the sole discriminator. See `_is_stebbs_refvalue_scalar`.
_STEBBS_NESTED_KEYS = frozenset(
    {
        "enabled",
        "parameters",
        *STEBBS_PHYSICS_LEAF_RENAMES.keys(),
        *STEBBS_PHYSICS_LEAF_RENAMES.values(),
    }
)

# A RefValue-style scalar wraps only `value` (and optionally `ref`). Such a
# mapping is the LEGACY master toggle, never the nested StebbsPhysics object.
_STEBBS_REFVALUE_KEYS = frozenset({"value", "ref"})


def _is_stebbs_refvalue_scalar(entry: Any) -> bool:
    """Return True for a ``{"value": ...}`` / ``{"value": ..., "ref": ...}`` scalar.

    Such a mapping is a RefValue-wrapped legacy master toggle and must be
    decomposed via ``_decompose_stebbs_master`` rather than treated as the new
    nested ``StebbsPhysics`` object (gh#1456).
    """
    return (
        isinstance(entry, Mapping)
        and "value" in entry
        and set(entry) <= _STEBBS_REFVALUE_KEYS
    )


def _unwrap_scalar(entry: Any) -> Any:
    """Return the inner value of a RefValue-style ``{"value": X}`` mapping."""
    if isinstance(entry, Mapping) and "value" in entry:
        return entry["value"]
    return entry


def _decompose_stebbs_master(entry: Any) -> tuple[Any, Any]:
    """Split a legacy ``stebbs`` master toggle into (enabled, parameters) dicts.

    Decompose the tri-state StebbsMethod integer:
      0 -> (enabled=false, parameters=default)
      1 -> (enabled=true,  parameters=default)
      2 -> (enabled=true,  parameters=provided)

    Returns RefValue-wrapped scalars (``{"value": ...}``) so the downstream
    FlexibleRefValue union resolves them uniformly. Invalid scalar values remain
    invalid, matching the previous flat ``FlexibleRefValue(StebbsMethod)``
    contract instead of silently enabling STEBBS for malformed YAML.
    """
    ref = entry.get("ref") if isinstance(entry, Mapping) else None

    def _wrapped(value: Any, *, carry_ref: bool = False) -> dict:
        wrapped = {"value": value}
        if carry_ref and ref is not None:
            wrapped["ref"] = ref
        return wrapped

    if isinstance(entry, Mapping) and "value" not in entry:
        raise ValueError(
            "Legacy 'stebbs' mappings must use a 'value' key; use "
            "'stebbs.enabled' / 'stebbs.parameters' for the nested form."
        )

    raw = _unwrap_scalar(entry)
    if isinstance(raw, bool):
        code = int(raw)
    elif isinstance(raw, int):
        code = raw
    elif isinstance(raw, float) and raw.is_integer():
        code = int(raw)
    else:
        raise ValueError(
            "Legacy 'stebbs' master toggle expects integer 0, 1, or 2."
        )
    if code == 0:
        return _wrapped(False, carry_ref=True), _wrapped(1)
    if code == 1:
        return _wrapped(True, carry_ref=True), _wrapped(1)
    if code == 2:
        return _wrapped(True, carry_ref=True), _wrapped(2)
    raise ValueError("Legacy 'stebbs' master toggle expects integer 0, 1, or 2.")


def _normalise_stebbs_block_aliases(stebbs_block: dict) -> tuple[dict, list[str]]:
    """Rename legacy keys inside a nested ``stebbs`` block to final leaves."""
    leaf_conflicts = _stebbs_leaf_alias_conflicts(stebbs_block)
    if leaf_conflicts:
        raise ValueError(
            "Multiple nested STEBBS physics switches map to the same nested leaf "
            f"({_format_stebbs_leaf_alias_conflicts(leaf_conflicts)}). Use only "
            "one spelling."
        )

    moved: list[str] = []
    for old_key, nested_leaf in STEBBS_PHYSICS_LEAF_RENAMES.items():
        if old_key == nested_leaf or old_key not in stebbs_block:
            continue
        value = stebbs_block.pop(old_key)
        if nested_leaf not in stebbs_block:
            stebbs_block[nested_leaf] = value
        moved.append(f"stebbs.{old_key}")
    return stebbs_block, moved


def _stebbs_leaf_alias_conflicts(values: Mapping[str, Any]) -> list[tuple[str, list[str]]]:
    """Return flat STEBBS alias groups that would collide after folding."""
    present_by_leaf: dict[str, list[str]] = {}
    for old_key, nested_leaf in STEBBS_PHYSICS_LEAF_RENAMES.items():
        if old_key in values:
            present_by_leaf.setdefault(nested_leaf, []).append(old_key)
    return [
        (leaf, sorted(keys))
        for leaf, keys in sorted(present_by_leaf.items())
        if len(keys) > 1
    ]


def _format_stebbs_leaf_alias_conflicts(conflicts: list[tuple[str, list[str]]]) -> str:
    """Format colliding flat STEBBS aliases for validation errors."""
    return "; ".join(
        f"{', '.join(keys)} -> stebbs.{leaf}" for leaf, keys in conflicts
    )


def _stebbs_flat_leaf_siblings(values: Mapping[str, Any]) -> list[str]:
    """Return flat STEBBS leaf keys present beside a nested ``stebbs`` block."""
    return sorted({key for key in STEBBS_PHYSICS_LEAF_RENAMES if key in values})


def fold_stebbs_physics(values: dict, class_name: str, *, warn: bool = True) -> dict:
    """Fold legacy flat STEBBS switches under a nested ``stebbs`` object.

    Called from ``ModelPhysics._rename_physics_fields`` after the key renames.

    Behaviour:
      * If ``values["stebbs"]`` is already the nested object (its keys include
        any of ``enabled``/``parameters``/``capacitance``/``setpoint``/
        ``same_*``), it is left untouched; stray flat siblings are rejected as
        ambiguous.
      * Otherwise ``stebbs`` is treated as the legacy master toggle and
        decomposed into ``stebbs.enabled`` + ``stebbs.parameters``.
      * The five sibling switches (``outer_cap_fraction`` -> ``capacitance``,
        ``setpoint``, ``same_*``) are moved under ``stebbs`` at their nested
        leaf names.

    A single ``DeprecationWarning`` summarises the fold when any legacy flat key
    was present.
    """
    if not isinstance(values, dict):
        return values

    existing = values.get("stebbs")

    # Already a built StebbsPhysics (or any object exposing the nested
    # members) -- e.g. from_df_state constructs the instance directly and
    # passes it in. Leave it untouched; do not mistake it for a legacy
    # master-toggle scalar. A RefValue wrapping a scalar still has no
    # `enabled` attribute, so this only catches the genuine nested object.
    if existing is not None and not isinstance(existing, (Mapping, int, float, bool, str)):
        if hasattr(existing, "enabled") or hasattr(existing, "parameters"):
            flat_conflicts = _stebbs_flat_leaf_siblings(values)
            if flat_conflicts:
                raise ValueError(
                    f"{class_name}: both nested 'stebbs' and flat STEBBS physics "
                    f"switches ({', '.join(flat_conflicts)}) are present. Use only "
                    "the nested 'stebbs' form."
                )
            return values

    nested_already = (
        isinstance(existing, Mapping)
        and not _is_stebbs_refvalue_scalar(existing)
        and any(k in existing for k in _STEBBS_NESTED_KEYS)
    )

    moved: list[str] = []

    if nested_already:
        stebbs_block, nested_moves = _normalise_stebbs_block_aliases(dict(existing))
        moved.extend(nested_moves)
        flat_conflicts = _stebbs_flat_leaf_siblings(values)
        if flat_conflicts:
            raise ValueError(
                f"{class_name}: both nested 'stebbs' and flat STEBBS physics "
                f"switches ({', '.join(flat_conflicts)}) are present. Use only "
                "the nested 'stebbs' form."
            )
    else:
        stebbs_block = {}
        if "stebbs" in values:
            enabled, parameters = _decompose_stebbs_master(values.pop("stebbs"))
            stebbs_block["enabled"] = enabled
            stebbs_block["parameters"] = parameters
            moved.append("stebbs")

    leaf_conflicts = _stebbs_leaf_alias_conflicts(values)
    if leaf_conflicts:
        raise ValueError(
            f"{class_name}: multiple flat STEBBS physics switches map to the "
            f"same nested leaf ({_format_stebbs_leaf_alias_conflicts(leaf_conflicts)}). "
            "Use only one spelling."
        )

    for flat_key, nested_leaf in STEBBS_PHYSICS_LEAF_RENAMES.items():
        if flat_key in values:
            if nested_leaf in stebbs_block:
                # Nested value wins; drop the stale flat one but record the move.
                values.pop(flat_key)
            else:
                stebbs_block[nested_leaf] = values.pop(flat_key)
            moved.append(flat_key)

    if not nested_already and not stebbs_block and not moved:
        # No STEBBS keys at all -> leave values untouched (defaults apply).
        return values

    if stebbs_block:
        values["stebbs"] = stebbs_block

    if moved and warn:
        warnings.warn(
            f"{class_name}: flat STEBBS physics switches "
            f"({', '.join(sorted(set(moved)))}) are deprecated; they now live "
            f"under the nested 'stebbs' object (e.g. "
            f"'stebbs.enabled', 'stebbs.capacitance'). See gh#1456.",
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

    Unwraps both flat RefValue-style ``{"value": X}`` wrappers and the
    family-tagged nested physics form introduced in gh#972. Returns
    ``default`` when neither spelling is present.
    """
    entry = read_renamed_key(
        physics,
        new_name,
        renames=_MODELPHYSICS_ALL_RENAMES,
        reverse_renames=_REVERSE_MODELPHYSICS_RENAMES,
        default=default,
    )
    entry = coerce_nested_to_flat(new_name, entry)
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


def normalise_yaml_renames(data: Any) -> Any:
    """Rewrite legacy field spellings in a loaded YAML mapping to current names.

    ``SUEWSConfig.from_yaml`` normalises old->new before validation, so the
    normal load path already sees current names. The standalone Phase B/C
    validation path (``suews validate`` in BC / C / AC modes) inspects the raw
    user YAML without going through ``from_yaml``, so a still-compatible config
    written with a legacy spelling would otherwise be looked up by the current
    name and silently missed (gh#1457). Normalising once at each phase's load
    chokepoint lets every rule match either spelling.

    If both a legacy and its current spelling are present (ambiguous), the data
    is returned unchanged so the downstream validation path reports the
    conflict, rather than this normalisation step raising. This guarantees the
    helper can only resolve rename-blindness, never introduce a new failure.
    """
    if not isinstance(data, dict):
        return data
    try:
        normalised = rename_keys_recursive(data, RAW_YAML_FIELD_RENAMES)
    except ValueError:
        return data

    model = normalised.get("model")
    if isinstance(model, dict):
        physics = model.get("physics")
        if isinstance(physics, dict):
            model["physics"] = fold_stebbs_physics(
                physics,
                "ModelPhysics",
                warn=False,
            )

    return normalised


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
