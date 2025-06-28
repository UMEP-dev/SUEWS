"""Validation utilities for SUEWS data models."""

import warnings
from typing import List, Dict


def warn_missing_params(
    missing_params: List[str], category: str, impact: str, stacklevel: int = 3
) -> None:
    """
    Issue standardized warning for missing parameters.

    Args:
        missing_params: List of parameter descriptions (e.g., "param_name (description)")
        category: Category of parameters (e.g., "CO2 emission", "building")
        impact: What will be affected (e.g., "model accuracy", "evapotranspiration calculations")
        stacklevel: Stack level for warning origin (default 3 for model_validator context)
    """
    if missing_params:
        warnings.warn(
            f"Missing critical {category} parameters which may affect {impact}:\n"
            f"  - " + "\n  - ".join(missing_params) + "\n"
            f"Consider providing values for these parameters in your configuration.",
            UserWarning,
            stacklevel=stacklevel,
        )


def check_missing_params(
    params_dict: Dict[str, str], instance: object, category: str, impact: str
) -> List[str]:
    """
    Check for missing parameters and return list of missing ones.

    Args:
        params_dict: Dictionary of parameter_name: description
        instance: Object instance to check parameters on
        category: Category of parameters
        impact: What will be affected

    Returns:
        List of missing parameter descriptions
    """
    missing_params = []

    for param_name, description in params_dict.items():
        value = getattr(instance, param_name)
        if value is None:
            missing_params.append(f"{param_name} ({description})")

    return missing_params
