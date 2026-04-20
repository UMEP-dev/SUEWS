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

from typing import Optional
import warnings

# Current supported schema version (aligned with SUEWS CalVer: YYYY.MM)
CURRENT_SCHEMA_VERSION = "2026.4"

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
        "2026.4.3 release shape (current): DeepSoilTemperature renamed to "
        "AnnualMeanAirTemperature (#1240), MinimumVolumeOfDHWinUse / "
        "MaximumVolumeOfDHWinUse removed (#1242), STEBBS "
        "HeatingSetpointTemperature / CoolingSetpointTemperature split into "
        "scalar + *Profile siblings gated on model.physics.setpointmethod "
        "(#1261), daylight-control and lighting/metabolism fields added."
    ),
}

def is_schema_compatible(
    config_version: str, current_version: str = CURRENT_SCHEMA_VERSION
) -> bool:
    """
    Check if a configuration schema version is compatible with the current version.

    Compatibility is derived from the migration handler registry on
    :class:`~supy.data_model.schema.migration.SchemaMigrator`, which is
    seeded from ``_HANDLERS`` in
    :mod:`supy.util.converter.yaml_upgrade`. A version is compatible
    with ``current_version`` iff it is the same version, or a
    ``(config_version, current_version)`` handler is registered that
    actually migrates the YAML forward.

    There is deliberately no hand-maintained compatibility table: the
    handler registry is the single source of truth. Adding an entry to
    ``_HANDLERS`` is what makes an older schema compatible — keeping
    the two in sync used to be a silent failure mode (gh#1304).

    Args:
        config_version: Schema version from the configuration.
        current_version: Current supported schema version
            (default: :data:`CURRENT_SCHEMA_VERSION`).

    Returns
    -------
        True if versions are compatible, False otherwise.
    """
    # Same version is always compatible.
    if config_version == current_version:
        return True

    # Lazy import: `migration` imports from this module for
    # `CURRENT_SCHEMA_VERSION` / `SCHEMA_VERSIONS`, so pulling it in at
    # module load time would cycle. Instantiation is cheap (one dict
    # plus the `register_with_migrator` side-effect) and gives us the
    # handler registry populated from `yaml_upgrade._HANDLERS`.
    from .migration import SchemaMigrator

    migrator = SchemaMigrator()
    return (config_version, current_version) in migrator.migration_handlers


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
