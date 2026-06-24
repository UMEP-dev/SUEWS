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
# CalVer label. The 2026.5 cycle (dev1..dev14) was collapsed to the
# plain `2026.5` label in the 2026.6.5 release PR.
CURRENT_SCHEMA_VERSION = "2026.6.dev1"

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
        "Released schema for 2026.6.5 (collapsed from the 2026.5.dev1.."
        "dev14 cycle). Union of the #1256 naming convention sweep plus the "
        "STEBBS, forcing/output and physics-selector restructures landed "
        "between the 2026.4.3 and 2026.6.5 releases:\n"
        "  * #1256 Category 1: 59 fused compound field names in ModelPhysics, "
        "SurfaceProperties, LAIParams, the vegetated-surface classes and "
        "SnowParams rewritten to snake_case (netradiationmethod -> "
        "net_radiation_method, soildepth -> soil_depth, baset -> "
        "base_temperature, crwmax -> water_holding_capacity_max).\n"
        "  * #1327 Category 5: eight STEBBS ArchetypeProperties `ext` fields "
        "spelt out to `External`.\n"
        "  * #1321 Categories 2+3: 15 ModelPhysics fields stripped of the "
        "redundant `_method` / `_model` suffix and de-abbreviated "
        "(net_radiation, storage_heat, emissions, stability, water_use, "
        "roughness_sublayer, frontal_area_index, surface_conductance, ...).\n"
        "  * #1334 + follow-through (#1337): the full STEBBS + Snow YAML "
        "surface converted to snake_case (124 renames; retires the STEBBS "
        "PascalCase exception) and the hot-water subsystem unified under a "
        "single hot_water_* prefix (dhw_* dropped).\n"
        "  * #972: accept-only widening for nested physics sub-options "
        "(family-tagged shapes); canonical YAML still dumps flat.\n"
        "  * #1333: site-level completeness validator now raises (was warn) "
        "when an active surface omits physics-required phenology / "
        "conductance / morphology inputs.\n"
        "  * Naming-convention Rule 2 (#1395 and gh#1392 / gh#1394): "
        "ArchetypeProperties and StebbsProperties reordered "
        "quantity-first (thickness_wall_outer, emissivity_wall_external, ...), "
        "the archetype_* namespace prefix introduced, building_type dropped "
        "(logged), and the layer qualifier `external` -> `outer` for "
        "bulk-material rows.\n"
        "  * #1372: model.control restructured -- forcing_file -> "
        "forcing.file and output_file -> output (inner path -> dir, legacy "
        "string form dropped); forcing-file header columns are now read and "
        "per-land-cover lai_<surface> / wuh_<surface> extras accepted "
        "(#1420 adapter follow-up; laimethod=0 observed LAI no longer "
        "clipped to LAImin/LAImax).\n"
        "  * gh#1452 (Column D, Reading STEBBS review): qualifier-first "
        "powers / setpoints (max_power_heating_system_air, "
        "setpoint_temperature_heating_air, ...) and the four compound-noun "
        "stragglers reordered.\n"
        "  * gh#1456: the flat STEBBS-scoped physics switches folded under a "
        "nested model.physics.stebbs object (master toggle -> "
        "enabled + parameters; capacitance / setpoint / same_albedo_* / "
        "same_emissivity_* moved at their leaf names).\n"
        "  * gh#1495: model.physics.frontal_area_index reduced to the "
        "canonical observed/modelled pair; the provided / use_provided / "
        "simple_scheme string aliases retired.\n"
        "Full rename tables live in "
        "src/supy/data_model/core/field_renames.py; the (2026.4 -> 2026.5) "
        "migration in src/supy/util/converter/yaml_upgrade.py::_HANDLERS "
        "applies the whole union in order."
    ),
    "2026.6.dev1": (
        "Development schema for the SPARTACUS direct/diffuse benchmark work: "
        "adds the model.physics.kdown_split_method selector, forest-column "
        "SPARTACUS stream and vegetation-region controls, and the optional "
        "vertical_layers.veg_ext extinction override. Existing 2026.5 YAMLs "
        "remain compatible through a no-op migration because all new inputs "
        "have defaults."
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
