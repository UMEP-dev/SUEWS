"""
Configure Simulation MCP Tool.

Wraps SuPy configuration loading and validation functionality.
"""

from pathlib import Path
from typing import Any, Dict, List, Optional, Union
import yaml

try:
    from ...data_model.core import SUEWSConfig
    from ...suews_sim import SUEWSSimulation
    from ..._supy_module import init_config, load_config_from_df, save_supy
except ImportError:
    # Handle imports for development/testing
    SUEWSConfig = None
    SUEWSSimulation = None
    init_config = None
    load_config_from_df = None
    save_supy = None

from .base import MCPTool


class ConfigureSimulationTool(MCPTool):
    """
    MCP tool for configuring SUEWS simulations.
    
    Wraps SuPy configuration loading, validation, modification, and saving functionality.
    Supports loading from YAML files, creating default configurations,
    applying configuration updates, and saving configurations to file.
    """
    
    def __init__(self):
        super().__init__(
            name="configure_simulation",
            description="Load, configure, validate and save SUEWS simulation parameters"
        )
    
    def get_parameters(self) -> List[Dict[str, Any]]:
        """Get tool parameter definitions."""
        return [
            {
                "name": "config_path",
                "type": "string",
                "description": "Path to YAML configuration file to load (optional - if not provided, creates default config)",
                "required": False
            },
            {
                "name": "config_updates",
                "type": "object",
                "description": "Configuration updates as JSON object (optional - applies updates to loaded or default config)",
                "required": False
            },
            {
                "name": "validate_only",
                "type": "boolean",
                "description": "If true, only validate configuration without creating simulation object (default: false)",
                "required": False
            },
            {
                "name": "site_name",
                "type": "string",
                "description": "Site name for the simulation (optional)",
                "required": False
            },
            {
                "name": "save_path",
                "type": "string",
                "description": "Path to save the configuration as YAML file (optional)",
                "required": False
            },
            {
                "name": "save_format",
                "type": "string",
                "description": "Format for saving: 'yaml' or 'json' (default: 'yaml')",
                "required": False
            }
        ]
    
    async def _execute(self, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """Execute configuration tool."""
        # Check if required modules are available
        if SUEWSConfig is None or SUEWSSimulation is None:
            return self.translator.create_structured_response(
                success=False,
                errors=["SuPy configuration modules not available. This may be a development environment."]
            )
        
        # Extract and validate parameters
        config_path = arguments.get("config_path")
        config_updates = arguments.get("config_updates", {})
        validate_only = self._validate_parameter(arguments, "validate_only", bool, required=False) or False
        site_name = self._validate_parameter(arguments, "site_name", str, required=False)
        save_path = arguments.get("save_path")
        save_format = arguments.get("save_format", "yaml").lower()
        
        try:
            config_obj = None
            config_info = {}
            
            # Load or create configuration
            if config_path:
                # Load from file
                path_obj = self.translator.validate_file_path(config_path)
                config_info["source"] = "file"
                config_info["path"] = str(path_obj)
                
                # Load YAML configuration
                config_obj = SUEWSConfig.from_yaml(str(path_obj))
                config_info["loaded_from_file"] = True
                
            else:
                # Create default configuration
                config_info["source"] = "default"
                if init_config:
                    # Use SuPy's init_config if available
                    df_state_init = init_config()
                    config_obj = load_config_from_df(df_state_init) if load_config_from_df else None
                    config_info["created_default"] = True
                else:
                    # Fallback to basic config creation
                    config_obj = SUEWSConfig()
                    config_info["created_default"] = True
            
            # Apply configuration updates if provided
            if config_updates and isinstance(config_updates, dict):
                config_info["updates_applied"] = True
                config_info["updates"] = config_updates
                
                # Apply nested updates to configuration object
                self._apply_nested_updates(config_obj, config_updates)
                config_info["updates_processed"] = True
            
            # Add site name if provided
            if site_name:
                config_info["site_name"] = site_name
                if hasattr(config_obj, 'site') and hasattr(config_obj.site, 'name'):
                    config_obj.site.name = site_name
                    config_info["site_name_set"] = True
            
            # Validate configuration
            validation_result = self._validate_config(config_obj)
            config_info.update(validation_result)
            
            # Save configuration if requested
            if save_path and validation_result["valid"]:
                save_result = self._save_configuration(config_obj, save_path, save_format)
                config_info["save_result"] = save_result
            
            if validate_only:
                # Return validation results only
                return self.translator.create_structured_response(
                    success=validation_result["valid"],
                    data=config_info,
                    message="Configuration validation completed"
                )
            
            # Create simulation object if validation passed
            if validation_result["valid"]:
                try:
                    simulation = SUEWSSimulation(config_obj)
                    config_info["simulation_created"] = True
                    config_info["simulation_ready"] = True
                    
                    # Extract key configuration details
                    config_info["config_summary"] = self._get_config_summary(config_obj)
                    
                    return self.translator.create_structured_response(
                        success=True,
                        data=config_info,
                        message="Configuration loaded and simulation object created successfully"
                    )
                    
                except Exception as e:
                    return self.translator.create_structured_response(
                        success=False,
                        data=config_info,
                        errors=[f"Failed to create simulation object: {str(e)}"]
                    )
            else:
                return self.translator.create_structured_response(
                    success=False,
                    data=config_info,
                    errors=validation_result.get("errors", ["Configuration validation failed"])
                )
        
        except Exception as e:
            return self.translator.create_structured_response(
                success=False,
                errors=[f"Configuration loading failed: {str(e)}"]
            )
    
    def _apply_nested_updates(self, config_obj, updates: Dict[str, Any]):
        """
        Apply nested configuration updates to config object.
        
        Parameters
        ----------
        config_obj : SUEWSConfig
            Configuration object to update
        updates : dict
            Nested dictionary of updates to apply
        """
        for key, value in updates.items():
            if hasattr(config_obj, key):
                attr = getattr(config_obj, key)
                
                # Handle nested objects
                if isinstance(value, dict) and hasattr(attr, '__dict__'):
                    # Recursively apply updates to nested objects
                    self._apply_nested_updates(attr, value)
                else:
                    # Direct assignment for simple values
                    setattr(config_obj, key, value)
            else:
                # Try to set new attributes (may fail for strict models)
                try:
                    setattr(config_obj, key, value)
                except AttributeError:
                    # Silently ignore attributes that can't be set
                    pass
    
    def _save_configuration(self, config_obj, save_path: str, save_format: str) -> Dict[str, Any]:
        """
        Save configuration to file.
        
        Parameters
        ----------
        config_obj : SUEWSConfig
            Configuration object to save
        save_path : str
            Path to save configuration
        save_format : str
            Format for saving ('yaml' or 'json')
            
        Returns
        -------
        dict
            Save operation results
        """
        save_info = {
            "saved": False,
            "path": None,
            "format": save_format
        }
        
        try:
            # Ensure parent directory exists
            save_path_obj = Path(save_path).expanduser().resolve()
            save_path_obj.parent.mkdir(parents=True, exist_ok=True)
            
            # Convert config to dictionary
            if hasattr(config_obj, 'model_dump'):
                # Pydantic v2
                config_dict = config_obj.model_dump()
            elif hasattr(config_obj, 'dict'):
                # Pydantic v1
                config_dict = config_obj.dict()
            else:
                # Fallback to __dict__
                config_dict = config_obj.__dict__
            
            # Save based on format
            if save_format == "yaml":
                with open(save_path_obj, 'w') as f:
                    yaml.dump(config_dict, f, default_flow_style=False, sort_keys=False)
                save_info["saved"] = True
                save_info["path"] = str(save_path_obj)
                
            elif save_format == "json":
                import json
                with open(save_path_obj, 'w') as f:
                    json.dump(config_dict, f, indent=2)
                save_info["saved"] = True
                save_info["path"] = str(save_path_obj)
                
            else:
                save_info["error"] = f"Unsupported save format: {save_format}"
        
        except Exception as e:
            save_info["error"] = f"Failed to save configuration: {str(e)}"
        
        return save_info
    
    def _get_config_summary(self, config_obj) -> Dict[str, Any]:
        """
        Extract key configuration details for summary.
        
        Parameters
        ----------
        config_obj : SUEWSConfig
            Configuration object
            
        Returns
        -------
        dict
            Configuration summary
        """
        summary = {}
        
        try:
            # Extract model settings
            if hasattr(config_obj, 'model'):
                model = config_obj.model
                summary["model"] = {
                    "name": getattr(model, 'name', 'SUEWS'),
                    "version": getattr(model, 'version', 'unknown')
                }
            
            # Extract site information
            if hasattr(config_obj, 'site'):
                site = config_obj.site
                summary["site"] = {
                    "name": getattr(site, 'name', 'unknown'),
                    "latitude": getattr(site, 'latitude', None),
                    "longitude": getattr(site, 'longitude', None)
                }
            
            # Extract surface fractions if available
            if hasattr(config_obj, 'surface') and hasattr(config_obj.surface, 'fractions'):
                fractions = config_obj.surface.fractions
                summary["surface_fractions"] = {
                    "building": getattr(fractions, 'building', None),
                    "paved": getattr(fractions, 'paved', None),
                    "vegetation": getattr(fractions, 'vegetation', None),
                    "water": getattr(fractions, 'water', None)
                }
            
            # Extract physics options
            if hasattr(config_obj, 'physics'):
                physics = config_obj.physics
                summary["physics"] = {
                    "stability_method": getattr(physics, 'stability_method', None),
                    "roughness_method": getattr(physics, 'roughness_method', None)
                }
        
        except Exception:
            # Return partial summary on any errors
            pass
        
        return summary
    
    def _validate_config(self, config_obj) -> Dict[str, Any]:
        """
        Validate configuration object.
        
        Parameters
        ----------
        config_obj : SUEWSConfig
            Configuration object to validate
            
        Returns
        -------
        dict
            Validation results
        """
        validation_info = {
            "valid": False,
            "errors": [],
            "warnings": [],
            "checks_performed": []
        }
        
        try:
            # Basic structure validation
            if config_obj is None:
                validation_info["errors"].append("Configuration object is None")
                return validation_info
            
            validation_info["checks_performed"].append("structure_check")
            
            # Check if basic attributes exist
            required_attrs = ["model", "site"]
            missing_attrs = []
            
            for attr in required_attrs:
                if not hasattr(config_obj, attr):
                    missing_attrs.append(attr)
            
            if missing_attrs:
                validation_info["errors"].append(f"Missing required attributes: {', '.join(missing_attrs)}")
            else:
                validation_info["checks_performed"].append("attributes_check")
            
            # Additional validation using Pydantic if available
            try:
                if hasattr(config_obj, 'model_validate'):
                    # Try Pydantic v2 validation
                    config_obj.model_validate(config_obj.model_dump())
                    validation_info["checks_performed"].append("pydantic_validation")
                elif hasattr(config_obj, 'dict'):
                    # Check if config can be serialized (basic structure check)
                    _ = config_obj.dict()
                    validation_info["checks_performed"].append("serialization_check")
            except Exception as e:
                validation_info["warnings"].append(f"Advanced validation warning: {str(e)}")
            
            # Physics-based validation checks
            if hasattr(config_obj, 'surface') and hasattr(config_obj.surface, 'fractions'):
                fractions = config_obj.surface.fractions
                # Check that fractions sum to approximately 1
                total = 0
                for frac_type in ['building', 'paved', 'vegetation', 'water', 'soil']:
                    if hasattr(fractions, frac_type):
                        val = getattr(fractions, frac_type)
                        if val is not None:
                            total += val
                
                if abs(total - 1.0) > 0.01:  # Allow 1% tolerance
                    validation_info["warnings"].append(
                        f"Surface fractions sum to {total:.3f}, expected ~1.0"
                    )
                
                validation_info["checks_performed"].append("surface_fraction_check")
            
            # If we got this far without critical errors, consider it valid
            if not validation_info["errors"]:
                validation_info["valid"] = True
            
        except Exception as e:
            validation_info["errors"].append(f"Validation error: {str(e)}")
        
        return validation_info