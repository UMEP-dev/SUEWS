"""
SUEWS YAML Configuration Schema Version Management.

This module defines the configuration schema versioning for SUEWS YAML files.
Schema versions track structural changes to the configuration format, NOT
the SUEWS model version.

Schema Version Policy:
- Major version (1.0 -> 2.0): Breaking changes requiring migration
- Minor version (1.0 -> 1.1): Backward compatible additions
- Schema versions are independent of SUEWS release versions
"""

from functools import lru_cache
from typing import Optional
import warnings

# Current supported schema version (aligned with SUEWS CalVer: YYYY.MM).
#
# During a release cycle we carry a PEP 440 `.devN` suffix and only
# drop it in the release PR itself; see `.claude/rules/python/
# schema-versioning.md` (Dev-label convention). Every structural PR
# between releases bumps the dev counter instead of consuming a new
# CalVer label.
CURRENT_SCHEMA_VERSION = "2026.5.dev7"

# Schema version history and descriptions.
#
# Retrospectively populated (gh#1304) after auditing structural changes to
# `src/supy/data_model/` between each formal supy release. Prior to that
# audit, `CURRENT_SCHEMA_VERSION` remained at "2025.12" through several
# breaking changes; the lineage below anchors each YAML shape to the release
# that shipped it, so the YAML-upgrade dispatcher can reason about real
# schema identities rather than synthetic labels.
SCHEMA_VERSIONS: dict[str, str] = {
    "2025.12": (
        "Initial formal YAML schema for the 2025.10.15 / 2025.11.20 releases "
        "(pre-#879 STEBBS layout: Wallx1 / Roofx1, DHWVesselEmissivity, "
        "standalone appliance/occupant fields, explicit initial-state and "
        "runtime-state slots)."
    ),
    "2026.1": (
        "2026.1.28 release shape: #879 STEBBS clean-up landed "
        "(Wallx1/Roofx1 -> WallOuterCapFrac/RoofOuterCapFrac, "
        "IndoorAirStartTemperature/OutdoorAirStartTemperature -> "
        "InitialIndoorTemperature/InitialOutdoorTemperature, drop of "
        "DHWVesselEmissivity and the runtime-state temperature/view-factor "
        "fields), STEBBS hourly profiles added for setpoints/appliance/"
        "occupants/hot water (#1038), DeepSoilTemperature and volume bounds "
        "present."
    ),
    "2026.4": (
        "2026.4.3 release shape: DeepSoilTemperature renamed to "
        "AnnualMeanAirTemperature (#1240), MinimumVolumeOfDHWinUse / "
        "MaximumVolumeOfDHWinUse removed (#1242), STEBBS "
        "HeatingSetpointTemperature / CoolingSetpointTemperature split into "
        "scalar + *Profile siblings gated on model.physics.setpointmethod "
        "(#1261), daylight-control and lighting/metabolism fields added."
    ),
    "2026.5": (
        "Category 1 of #1256: 59 fused compound field names in "
        "ModelPhysics, SurfaceProperties, LAIParams, "
        "VegetatedSurfaceProperties, EvetrProperties, DectrProperties, "
        "and SnowParams renamed to snake_case (e.g. netradiationmethod "
        "-> net_radiation_method, soildepth -> soil_depth, baset -> "
        "base_temperature, crwmax -> water_holding_capacity_max). Full "
        "mapping lives in src/supy/data_model/core/field_renames.py."
    ),
    "2026.5.dev1": (
        "Category 5 of #1256 (gh#1327): eight STEBBS "
        "ArchetypeProperties fields with the fused `ext` fragment "
        "renamed to the spelt-out `External` form, bringing them into "
        "line with sibling `WallExternalEmissivity` / "
        "`RoofExternalEmissivity` — "
        "Wallext{Thickness, EffectiveConductivity, Density, Cp} -> "
        "WallExternal{Thickness, EffectiveConductivity, Density, Cp}; "
        "Roofext{Thickness, EffectiveConductivity, Density, Cp} -> "
        "RoofExternal{Thickness, EffectiveConductivity, Density, Cp}. "
        "STEBBS PascalCase itself is kept; Fortran-side identifiers are "
        "unchanged (reverse rename maps back for the bridge). Rename "
        "table lives in src/supy/data_model/core/field_renames.py; the "
        "(2026.5 -> 2026.5.dev1) migration is registered in "
        "src/supy/util/converter/yaml_upgrade.py::_HANDLERS. First dev "
        "label in the 2026.5 release cycle (collapse to plain `2026.5` "
        "in the release PR per `.claude/rules/python/"
        "schema-versioning.md`)."
    ),
    "2026.5.dev2": (
        "Categories 2 and 3 of #1256 (gh#1321): 15 ModelPhysics fields "
        "stripped of the redundant `_method` / `_model` suffix and/or "
        "expanded out of opaque domain abbreviations — "
        "net_radiation_method -> net_radiation, emissions_method -> "
        "emissions, storage_heat_method -> storage_heat, "
        "roughness_length_{momentum,heat}_method -> "
        "roughness_length_{momentum,heat}, stability_method -> "
        "stability, water_use_method -> water_use, stebbs_method -> "
        "stebbs, setpointmethod -> setpoint (fused leftover from "
        "Category 1), smd_method -> soil_moisture_deficit, rsl_method "
        "-> roughness_sublayer, rsl_level -> roughness_sublayer_level, "
        "fai_method -> frontal_area_index, rc_method -> "
        "outer_cap_fraction, gs_model -> surface_conductance. "
        "Enum types are unchanged (NetRadiationMethod etc. still "
        "carry the `Method` suffix); DataFrame column names keep the "
        "legacy fused spellings for the Fortran bridge. Rename table "
        "extends MODELPHYSICS_SUFFIX_RENAMES in "
        "src/supy/data_model/core/field_renames.py; the "
        "(2026.5.dev1 -> 2026.5.dev2) migration is registered in "
        "src/supy/util/converter/yaml_upgrade.py::_HANDLERS."
    ),
    "2026.5.dev3": (
        "gh#1334: full user-facing YAML surface to snake_case. "
        "Retires the STEBBS PascalCase exception carried over from "
        "the Fortran-side STEBBS interface so the YAML is a single "
        "convention throughout. 124 renames in three classes: "
        "(a) ArchetypeProperties — 63 fields PascalCase -> snake_case "
        "(BuildingType -> building_type, stebbs_Height -> "
        "building_height, WWR -> window_to_wall_ratio, "
        "WallOuterCapFrac -> wall_outer_heat_capacity_fraction, "
        "WallCp -> wall_specific_heat_capacity, WallAbsorbtivity -> "
        "wall_absorptivity (spelling fix), FloorThickness -> "
        "ground_floor_thickness (alignment), etc.); (b) "
        "StebbsProperties — 50 fields (WallInternalConvectionCoefficient "
        "-> wall_internal_convection_coefficient, CoolingSystemCOP -> "
        "cooling_system_cop, "
        "MonthMeanAirTemperature_diffmax -> "
        "month_mean_air_temperature_diffmax, DHW* family lowercased "
        "to dhw_* so DHWWaterVolume -> dhw_water_volume, "
        "DHWVesselWallEmissivity -> dhw_vessel_wall_emissivity, etc.; "
        "the parallel HotWater* family snake-cases to hot_water_*. "
        "The two prefixes (dhw_*, hot_water_*) and the cop/diffmax "
        "abbreviations are kept to mirror the Rust bridge structs "
        "(src/suews_bridge/src/stebbs_prm.rs); unifying them is Tier "
        "B work tracked under #1324); (c) SnowParams — 11 snake_case clean-ups "
        "(precip_limit -> temperature_rain_snow_threshold (semantic "
        "fix: the value is a temperature, unit degC), tau_a/f/r -> "
        "tau_cold_snow/tau_melting_snow/tau_refreezing_snow, "
        "snow_limit_building/paved -> snow_depth_limit_*, "
        "snowprof_24hr -> snow_profile_24hr, narp_emis_snow -> "
        "narp_emissivity_snow, temp_melt_factor -> "
        "temperature_melt_factor, rad_melt_factor -> "
        "radiation_melt_factor). Rename tables extend "
        "ARCHETYPEPROPERTIES_RENAMES / SNOWPARAMS_RENAMES and add "
        "STEBBSPROPERTIES_RENAMES in "
        "src/supy/data_model/core/field_renames.py; the "
        "(2026.5.dev2 -> 2026.5.dev3) migration is registered in "
        "src/supy/util/converter/yaml_upgrade.py::_HANDLERS. "
        ".claude/rules/00-project-essentials.md and "
        ".claude/rules/python/conventions.md drop the STEBBS "
        "PascalCase exception in the same PR."
    ),
    "2026.5.dev4": (
        "gh#1334 follow-through via PR #1337: the STEBBS hot-water subsystem unifies "
        "under the `hot_water_*` prefix. Drops the opaque `dhw_` acronym "
        "and the redundant `water_tank_*` leftover — 14 renames: "
        "water_tank_wall_thickness -> hot_water_tank_wall_thickness, "
        "water_tank_surface_area -> hot_water_tank_surface_area, "
        "water_tank_water_volume -> hot_water_tank_volume (drop "
        "redundant `water`), dhw_water_volume -> hot_water_volume, "
        "dhw_surface_area -> hot_water_surface_area, "
        "dhw_specific_heat_capacity -> hot_water_specific_heat_capacity, "
        "dhw_density -> hot_water_density, and the seven dhw_vessel_* "
        "fields -> hot_water_vessel_* (wall_thickness, "
        "specific_heat_capacity, density, wall_conductivity, "
        "internal_wall_convection_coefficient, "
        "external_wall_convection_coefficient, wall_emissivity). Tank "
        "vs vessel physical separation is preserved (storage vs "
        "point-of-consumption); only the `dhw` acronym and the "
        "`water_tank_` sibling drop in favour of a single `hot_water_` "
        "prefix with `_tank_` / `_vessel_` component qualifiers. Rename "
        "tables ARCHETYPEPROPERTIES_DEV3_RENAMES and "
        "STEBBSPROPERTIES_DEV3_RENAMES added in "
        "src/supy/data_model/core/field_renames.py; the "
        "(2026.5.dev3 -> 2026.5.dev4) migration is registered in "
        "src/supy/util/converter/yaml_upgrade.py::_HANDLERS. Rust struct "
        "fields and c_api shadow TYPE keep `dhw_*` internally — "
        "cross-layer tracked in #1324."
    ),
    "2026.5.dev5": (
        "gh#972: accept-only widening for three model.physics fields "
        "(net_radiation, storage_heat, emissions). Users may now "
        "supply a family-tagged nested shape "
        "(`net_radiation: {spartacus: {value: 1001}}`) alongside the "
        "existing flat `{value: N}` form. Family tag is validated "
        "against its numeric codes at load time. Canonical internal "
        "representation stays flat; YAML dump and suews-schema "
        "migrate continue to emit the flat form unchanged — every "
        "previously valid YAML round-trips byte-identically. The "
        "(2026.5.dev4 -> 2026.5.dev5) migration is an identity "
        "transform (no rewriting needed); its presence in _HANDLERS "
        "grants compatibility with dev4 under the registry-driven "
        "is_schema_compatible check. Rust CLI acceptance lives in "
        "src/suews_bridge/src/field_renames.rs::collapse_nested_physics; "
        "Python-side helper in src/supy/data_model/core/physics_families.py."
    ),
    "2026.5.dev6": (
        "gh#1333: validator contract change only (no YAML rewrite). "
        "Site-level completeness now raises instead of warning when a "
        "user-declared active surface omits physics-required phenology "
        "(`lai_max`, `base_temperature`, "
        "`base_temperature_senescence`, `gdd_full`, `sdd_full`), "
        "conductance (`g_*`, `s_*`, `kmax`, `tl`, `th`), building "
        "morphology (`bldgh`, `faibldg`), or tree FAI/height "
        "(`height_{evergreen,deciduous}_tree`, "
        "`fai_{evergreen,deciduous}_tree`) inputs. The YAML shape "
        "accepted at 2026.5.dev5 is byte-identical here; the "
        "(2026.5.dev5 -> 2026.5.dev6) migration is therefore an "
        "identity transform whose presence signals the tightened load "
        "contract to users pinning `schema_version`. The check is gated "
        "on `_yaml_path` and explicit user declaration in the raw YAML "
        "so programmatic constructions and default_factory-only sparse "
        "fixtures remain permissive."
    ),
    "2026.5.dev7": (
        "gh#1372: structural restructure of forcing configuration. "
        "model.control.forcing_file (str | list[str]) is moved to "
        "model.control.forcing.file under a new ForcingControl sub-object, "
        "creating a stable home for future forcing fields (gh#1373 "
        "sub-hourly disaggregation; resampling policy). The "
        "(2026.5.dev6 -> 2026.5.dev7) migration is registered in "
        "src/supy/util/converter/yaml_upgrade.py::_HANDLERS via "
        "_apply_forcing_subobject_restructure, which moves the value "
        "(whether a bare string or a RefValue mapping) under the new "
        "key. Forcing-file *content* semantics also tighten in this "
        "label: header column names are now read (previously discarded) "
        "and matched against the canonical forcing column set; baseline-10 "
        "columns (iy, id, it, imin, Tair, RH, U, pres, kdown, rain) are "
        "required; missing optional canonical columns are filled with "
        "-999; whitelisted per-landcover columns are loaded into "
        "SUEWSForcing.extras / ForcingData.extras — lai_<surface> "
        "for the three vegetated surfaces (evetr, dectr, grass) only, "
        "and wuh_<surface> (external water use) for the six land "
        "surfaces only (excludes water). xsmd remains a bulk "
        "site-level column."
    ),
}

@lru_cache(maxsize=1)
def _migration_pair_registry() -> frozenset:
    """Snapshot of registered ``(from, to)`` migration edges.

    Lazy-imported to break the cycle with :mod:`.migration` (which imports
    ``CURRENT_SCHEMA_VERSION`` from this module). Cached because the
    underlying ``_HANDLERS`` table in
    :mod:`supy.util.converter.yaml_upgrade` is populated at import time
    and does not change thereafter.
    """
    from .migration import SchemaMigrator

    return frozenset(SchemaMigrator().migration_handlers.keys())


def is_schema_compatible(
    config_version: str, current_version: str = CURRENT_SCHEMA_VERSION
) -> bool:
    """Check if a configuration schema version is compatible with the current version.

    Compatibility is derived from the migration handler registry — a
    version is compatible iff it equals ``current_version`` or a
    ``(config_version, current_version)`` handler is registered. Adding
    an entry to ``_HANDLERS`` in ``yaml_upgrade.py`` is what grants
    compatibility; there is no separate table (gh#1304).

    Args:
        config_version: Schema version from the configuration.
        current_version: Current supported schema version
            (default: :data:`CURRENT_SCHEMA_VERSION`).

    Returns
    -------
        True if versions are compatible, False otherwise.
    """
    if config_version == current_version:
        return True
    return (config_version, current_version) in _migration_pair_registry()


def get_schema_compatibility_message(config_version: Optional[str]) -> Optional[str]:  # noqa: PLR0911
    """
    Generate an appropriate message about schema compatibility.

    Args:
        config_version: Schema version from configuration (None if not specified)

    Returns
    -------
        Warning message if incompatible, None if compatible
    """
    if config_version is None:
        # No version specified - assume current and don't warn
        return None

    if config_version == CURRENT_SCHEMA_VERSION:
        # Exact match - no message needed
        return None

    if is_schema_compatible(config_version):
        # Compatible but different version
        return f"Configuration uses schema {config_version}, current is {CURRENT_SCHEMA_VERSION} (compatible)"

    # Parse versions for comparison
    try:
        # Parse as major.minor floats for proper comparison
        # e.g., "0.9" -> 0.9, "0.1" -> 0.1, "2.0" -> 2.0
        config_parts = config_version.split(".")
        current_parts = CURRENT_SCHEMA_VERSION.split(".")

        config_value = float(config_parts[0])
        if len(config_parts) > 1:
            config_value += float(config_parts[1]) / 100  # minor as decimal

        current_value = float(current_parts[0])
        if len(current_parts) > 1:
            current_value += float(current_parts[1]) / 100  # minor as decimal

        if config_value < current_value:
            return (
                f"Configuration uses older schema {config_version}, "
                f"current is {CURRENT_SCHEMA_VERSION}. "
                f"Consider updating your configuration."
            )
        elif config_value > current_value:
            return (
                f"Configuration uses newer schema {config_version}, "
                f"this version supports {CURRENT_SCHEMA_VERSION}. "
                f"Please update SUEWS or use an older configuration."
            )
        else:
            # Versions are equal - shouldn't reach here due to earlier check
            return None
    except (ValueError, IndexError):
        # Can't parse versions - generic message
        return (
            f"Configuration schema {config_version} may not be compatible "
            f"with current schema {CURRENT_SCHEMA_VERSION}"
        )


def validate_schema_version(
    config_version: Optional[str], strict: bool = False
) -> None:
    """
    Validate schema version compatibility.

    Args:
        config_version: Schema version from configuration
        strict: If True, raise error on incompatibility; if False, warn

    Raises
    ------
        ValueError: If strict=True and versions are incompatible
    """
    message = get_schema_compatibility_message(config_version)

    if message:
        if strict and not is_schema_compatible(
            config_version or CURRENT_SCHEMA_VERSION
        ):
            raise ValueError(f"Schema version incompatible: {message}")
        else:
            warnings.warn(message, UserWarning, stacklevel=3)
