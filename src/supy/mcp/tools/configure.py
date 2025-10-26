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
            # Create minimal config
            config = SUEWSConfig(
                name=name,
                description=description,
                sites=[],  # User will need to add sites
            )

        # Save to file
        config_dict = config.model_dump(exclude_none=True)
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


async def update_config(
    config_path: str,
    updates: Dict[str, Any],
) -> Dict[str, Any]:
    """Update an existing configuration file.

    Args:
        config_path: Path to configuration file
        updates: Dictionary of updates to apply

    Returns:
        Dictionary with update results
    """
    try:
        # Load existing config
        config_data = load_yaml_file(config_path)
        config = SUEWSConfig.model_validate(config_data)

        # Apply updates
        for key, value in updates.items():
            if hasattr(config, key):
                setattr(config, key, value)

        # Validate updated config
        config = SUEWSConfig.model_validate(config.model_dump())

        # Save back
        save_yaml_file(config.model_dump(exclude_none=True), config_path)

        return {
            "success": True,
            "message": f"Configuration updated at {config_path}",
        }

    except Exception as e:
        return {"success": False, "error": str(e)}
