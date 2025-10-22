"""Configuration tools using SUEWSConfig data model."""

from pathlib import Path
from typing import Dict, Any, Optional
from pydantic import ValidationError

from supy.data_model.core.config import SUEWSConfig
from ..utils.helpers import load_yaml_file, save_yaml_file, format_validation_error


async def validate_config(config_path: str) -> Dict[str, Any]:
    """Validate a SUEWS YAML configuration file.

    Args:
        config_path: Path to YAML configuration file

    Returns:
        Dictionary with validation results:
        - valid: bool indicating if config is valid
        - errors: list of validation errors if invalid
        - config: validated config object if valid
    """
    try:
        # Load YAML file
        config_data = load_yaml_file(config_path)

        # Validate using SUEWSConfig
        config = SUEWSConfig.model_validate(config_data)

        return {
            "valid": True,
            "message": f"Configuration '{config.name}' is valid",
            "config": {
                "name": config.name,
                "description": config.description,
                "schema_version": config.schema_version,
                "num_sites": len(config.sites),
            },
        }

    except FileNotFoundError as e:
        return {"valid": False, "error": str(e)}
    except ValidationError as e:
        return {"valid": False, "error": format_validation_error(e)}
    except Exception as e:
        return {"valid": False, "error": f"Unexpected error: {str(e)}"}


async def create_config(
    name: str,
    description: str,
    output_path: str,
    template: Optional[str] = None,
) -> Dict[str, Any]:
    """Create a new SUEWS configuration file.

    Args:
        name: Configuration name
        description: Configuration description
        output_path: Path where config will be saved
        template: Optional template to base config on

    Returns:
        Dictionary with creation results
    """
    try:
        if template:
            # Load template and modify
            config_data = load_yaml_file(template)
            config = SUEWSConfig.model_validate(config_data)
            config.name = name
            config.description = description
        else:
            # Create minimal config with one default site
            # SUEWSConfig requires at least one site
            from supy.data_model.core.site import Site

            minimal_site = Site(
                name="default_site",
                lat=51.5,  # London coordinates as example
                lon=-0.1,
                alt=10.0,
                timezone=0,
            )

            config = SUEWSConfig(
                name=name,
                description=description,
                sites=[minimal_site],
            )

        # Save to file (use mode='json' to ensure enums are strings)
        config_dict = config.model_dump(exclude_none=True, mode='json')
        save_yaml_file(config_dict, output_path)

        return {
            "success": True,
            "message": f"Configuration created at {output_path}",
            "config": {
                "name": config.name,
                "description": config.description,
                "path": str(output_path),
            },
        }

    except Exception as e:
        return {"success": False, "error": str(e)}


async def get_config_info(config_path: str) -> Dict[str, Any]:
    """Get information about a configuration file.

    Args:
        config_path: Path to configuration file

    Returns:
        Dictionary with configuration information
    """
    try:
        config_data = load_yaml_file(config_path)
        config = SUEWSConfig.model_validate(config_data)

        return {
            "name": config.name,
            "description": config.description,
            "schema_version": config.schema_version,
            "num_sites": len(config.sites),
            "site_names": [site.name for site in config.sites],
            "model_config": {
                "timestep": config.model.time.resolution if hasattr(config.model, "time") else None,
            },
        }

    except Exception as e:
        return {"error": str(e)}


def _recursive_update(obj, updates: dict):
    """Apply dictionary updates recursively to a configuration object.

    This function handles nested configuration updates properly, matching
    the implementation in SUEWSSimulation._update_config_from_dict.

    Args:
        obj: Configuration object to update
        updates: Dictionary of updates to apply
    """
    for key, value in updates.items():
        if hasattr(obj, key):
            attr = getattr(obj, key)
            if isinstance(value, dict) and hasattr(attr, "__dict__"):
                # Recursive update for nested objects
                _recursive_update(attr, value)
            else:
                setattr(obj, key, value)


async def update_config(
    config_path: str,
    updates: Dict[str, Any],
) -> Dict[str, Any]:
    """Update an existing configuration file.

    Supports nested updates using dot notation or nested dictionaries:
    - Nested dict: {"model": {"time": {"resolution": 3600}}}
    - Updates are applied recursively to handle any level of nesting

    Args:
        config_path: Path to configuration file
        updates: Dictionary of updates to apply (supports nested structures)

    Returns:
        Dictionary with update results

    Examples:
        >>> # Update top-level field
        >>> update_config("config.yaml", {"name": "New Name"})

        >>> # Update nested field
        >>> update_config("config.yaml", {
        ...     "model": {
        ...         "time": {"resolution": 3600}
        ...     }
        ... })
    """
    try:
        # Load existing config
        config_data = load_yaml_file(config_path)
        config = SUEWSConfig.model_validate(config_data)

        # Apply updates recursively
        _recursive_update(config, updates)

        # Validate updated config
        config = SUEWSConfig.model_validate(config.model_dump())

        # Save back (use mode='json' to ensure enums are strings)
        save_yaml_file(config.model_dump(exclude_none=True, mode='json'), config_path)

        return {
            "success": True,
            "message": f"Configuration updated at {config_path}",
            "updates_applied": updates,
        }

    except Exception as e:
        return {"success": False, "error": str(e)}
