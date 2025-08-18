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

# Current supported schema version
CURRENT_SCHEMA_VERSION = "0.1"

# Schema version history and descriptions
SCHEMA_VERSIONS: dict[str, str] = {
    "0.1": "Initial YAML schema with full Pydantic data model (2025.8)"
    # Future examples:
    # "1.0": "First stable release with complete validation"
    # "1.1": "Added optional field X for feature Y (2025.10)"
    # "2.0": "Breaking change: Renamed field A to B, restructured C (2026.1)"
}

# Compatibility matrix: which schema versions are compatible
COMPATIBLE_VERSIONS = {
    "0.1": ["0.1"],  # 0.1 only compatible with itself (initial release)
    # Future: "1.0": ["0.1", "1.0"],  # 1.0 backward compatible with 0.1
    # Future: "1.1": ["1.0", "1.1"],  # 1.1 backward compatible with 1.0
    # Future: "2.0": ["2.0"],  # Major version breaks compatibility
}


def is_schema_compatible(
    config_version: str, current_version: str = CURRENT_SCHEMA_VERSION
) -> bool:
    """
    Check if a configuration schema version is compatible with the current version.

    Args:
        config_version: Schema version from the configuration
        current_version: Current supported schema version (default: CURRENT_SCHEMA_VERSION)

    Returns
    -------
        True if versions are compatible, False otherwise
    """
    # Same version is always compatible
    if config_version == current_version:
        return True

    # Check compatibility matrix
    if current_version not in COMPATIBLE_VERSIONS:
        return False

    return config_version in COMPATIBLE_VERSIONS.get(current_version, [])


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
