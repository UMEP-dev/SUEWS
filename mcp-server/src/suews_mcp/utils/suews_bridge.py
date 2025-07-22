"""Bridge to connect MCP server with SUEWS pydantic data models."""

import sys
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple
import yaml
import json

from pydantic import ValidationError

try:
    # Try direct import first (when supy is installed)
    from supy.data_model.core import SUEWSConfig
    from supy.data_model.model import (
        NetRadiationMethod,
        EmissionsMethod,
        StorageHeatMethod,
        RoughnessMethod,
        StabilityMethod,
        FAIMethod,
        StebbsMethod,
        SnowUse,
    )

    # Try to import newer features if available
    try:
        from supy.data_model.model import RSLMethod, RSLLevel, GSModel
    except ImportError:
        # Create placeholder classes for compatibility
        class RSLMethod:
            NONE = 0
            VARIABLE = 1
        
        class RSLLevel:
            Z = 0
            ZD = 1
        
        class GSModel:
            J11 = 0
            W16 = 1

    # Try to import validation controller (optional)
    try:
        from supy.data_model import ValidationController
    except ImportError:
        ValidationController = None
except ImportError as e:
    raise ImportError(
        f"Failed to import SUEWS data models. Ensure SuPy is properly installed: {e}"
    )


class SUEWSBridge:
    """Bridge between MCP server and SUEWS data models."""

    def __init__(self):
        """Initialize the bridge."""
        self.physics_methods = {
            "NetRadiationMethod": NetRadiationMethod,
            "EmissionsMethod": EmissionsMethod,
            "StorageHeatMethod": StorageHeatMethod,
            "RoughnessMethod": RoughnessMethod,
            "StabilityMethod": StabilityMethod,
            "RSLMethod": RSLMethod,
            "FAIMethod": FAIMethod,
            "RSLLevel": RSLLevel,
            "GSModel": GSModel,
            "StebbsMethod": StebbsMethod,
            "SnowUse": SnowUse,
        }

    def validate_config(
        self, config_dict: Dict[str, Any], use_conditional: bool = True
    ) -> Dict[str, Any]:
        """
        Validate a SUEWS configuration using pydantic models.

        Args:
            config_dict: Configuration dictionary
            use_conditional: Whether to use conditional validation

        Returns:
            Dictionary with validation results
        """
        try:
            if use_conditional:
                # Use conditional validation
                config = SUEWSConfig.model_validate(
                    config_dict, context={"conditional_validation": True}
                )
            else:
                # Full validation
                config = SUEWSConfig(**config_dict)

            return {"valid": True, "config": config, "message": "Configuration is valid"}
        except ValidationError as e:
            return {
                "valid": False,
                "errors": self._parse_validation_errors(e),
                "raw_errors": e.errors(),
            }
        except Exception as e:
            return {
                "valid": False,
                "errors": [{"message": str(e), "type": "unknown"}],
                "raw_errors": [],
            }

    def _parse_validation_errors(self, error: ValidationError) -> List[Dict[str, Any]]:
        """Parse pydantic validation errors into a more readable format."""
        parsed_errors = []

        for err in error.errors():
            location = " -> ".join(str(loc) for loc in err["loc"])
            parsed_errors.append(
                {
                    "location": location,
                    "message": err["msg"],
                    "type": err["type"],
                    "input": err.get("input", "N/A"),
                }
            )

        return parsed_errors

    def get_required_parameters(self, physics_methods: Dict[str, Any]) -> Dict[str, List[str]]:
        """
        Get required parameters based on selected physics methods.

        Args:
            physics_methods: Dictionary of selected methods

        Returns:
            Dictionary mapping method to required parameters
        """
        # If ValidationController not available, return basic requirements
        if ValidationController is None:
            return self._get_basic_requirements(physics_methods)

        controller = ValidationController(physics_methods)

        # Get validation requirements for each method
        requirements = {}

        if controller.radiation_spartacus_enabled:
            requirements["SPARTACUS"] = [
                "height_mean",
                "height_stdev",
                "frontal_area_index",
                "plan_area_fraction",
                "wall_specular_fraction",
            ]

        if controller.storage_estm_enabled:
            requirements["ESTM"] = [
                "estm_code",
                "Internal_thickness_*",
                "Internal_k_*",
                "Internal_volumetric_heat_capacity_*",
                "thickness_*_*",
                "k_*_*",
                "volumetric_heat_capacity_*_*",
            ]

        if controller.diagmethod_rst_enabled:
            requirements["RST"] = [
                "height_mean",
                "height_stdev",
                "z",
                "displacement_height",
                "frontal_area_index",
            ]

        return requirements

    def _get_basic_requirements(self, physics_methods: Dict[str, Any]) -> Dict[str, List[str]]:
        """Get basic requirements when ValidationController not available."""
        requirements = {}

        # Basic checks based on method names
        if physics_methods.get("NetRadiationMethod") == "SPARTACUS":
            requirements["SPARTACUS"] = [
                "height_mean",
                "height_stdev",
                "frontal_area_index",
                "plan_area_fraction",
                "wall_specular_fraction",
            ]

        if physics_methods.get("StorageHeatMethod") == "ESTM":
            requirements["ESTM"] = ["estm_code", "thermal layer parameters"]

        if physics_methods.get("RSLMethod") == "RST":
            requirements["RST"] = [
                "height_mean",
                "height_stdev",
                "z",
                "displacement_height",
                "frontal_area_index",
            ]

        return requirements

    def check_method_compatibility(self, methods: Dict[str, str]) -> Dict[str, Any]:
        """
        Check compatibility between physics methods.

        Args:
            methods: Selected physics methods

        Returns:
            Compatibility analysis
        """
        issues = []
        warnings = []

        # Check RSL method compatibility
        if methods.get("RSLMethod") == "Variable" and methods.get("StabilityMethod") != "SG2000":
            issues.append(
                {
                    "methods": ["RSLMethod", "StabilityMethod"],
                    "issue": "Variable RSL method requires SG2000 stability method",
                    "suggestion": "Change StabilityMethod to SG2000",
                }
            )

        # Check storage heat method compatibility
        if (
            methods.get("StorageHeatMethod") == "OHM"
            and methods.get("EmissionsMethod") == "PreprocessedWrfspecial"
        ):
            warnings.append(
                {
                    "methods": ["StorageHeatMethod", "EmissionsMethod"],
                    "warning": "OHM storage method may not accurately capture heat storage with WRF emissions",
                    "suggestion": "Consider using ESTM for better accuracy",
                }
            )

        return {"compatible": len(issues) == 0, "issues": issues, "warnings": warnings}

    def load_config_from_yaml(self, yaml_path: str) -> Dict[str, Any]:
        """Load configuration from YAML file."""
        path = Path(yaml_path)
        if not path.exists():
            raise FileNotFoundError(f"Configuration file not found: {yaml_path}")

        with open(path, "r") as f:
            return yaml.safe_load(f)

    def get_parameter_info(self, parameter_name: str) -> Dict[str, Any]:
        """
        Get information about a specific parameter.

        Args:
            parameter_name: Name of the parameter

        Returns:
            Parameter information including type, description, units
        """
        # This would connect to a parameter database
        # For now, return basic info based on common parameters
        param_info = {
            "albedo_deciduous_summer": {
                "type": "float",
                "units": "dimensionless",
                "range": [0.0, 1.0],
                "typical": [0.15, 0.20],
                "description": "Summer albedo of deciduous vegetation",
                "physics": "Controls solar radiation reflection from deciduous trees in leaf",
            },
            "height_mean": {
                "type": "float",
                "units": "m",
                "range": [0.0, 200.0],
                "typical": {
                    "suburban": [5.0, 15.0],
                    "city_centre": [15.0, 50.0],
                    "industrial": [8.0, 20.0],
                },
                "description": "Mean building height",
                "physics": "Affects aerodynamic roughness, displacement height, and wind profile",
            },
            "population_density_day": {
                "type": "float",
                "units": "people/ha",
                "range": [0.0, 10000.0],
                "typical": {
                    "residential": [50.0, 200.0],
                    "commercial": [100.0, 500.0],
                    "city_centre": [200.0, 1000.0],
                },
                "description": "Daytime population density",
                "physics": "Used to calculate anthropogenic heat emissions from human metabolism",
            },
        }

        return param_info.get(
            parameter_name,
            {
                "description": f"Parameter '{parameter_name}' - documentation pending",
                "type": "unknown",
            },
        )
