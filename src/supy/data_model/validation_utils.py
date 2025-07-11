"""Validation utilities for SUEWS data models."""

import warnings
import functools
import inspect
from typing import List, Dict, TypeVar, Callable, TYPE_CHECKING

if TYPE_CHECKING:
    from .type import RefValue

T = TypeVar('T')


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
        
        # Check if value is None or if it's a RefValue with None value
        is_missing = False
        if value is None:
            is_missing = True
        elif hasattr(value, 'value') and hasattr(value, 'ref'):
            # This is likely a RefValue object
            if value.value is None:
                is_missing = True
        
        if is_missing:
            missing_params.append(f"{param_name} ({description})")

    return missing_params


def suppress_internal_validation_warnings(func: Callable[[T], T]) -> Callable[[T], T]:
    """Decorator to suppress warnings during Pydantic's internal validation phases.
    
    This decorator prevents spurious warnings that occur when Pydantic creates
    temporary objects during validation where fields are temporarily None.
    
    Usage:
        @model_validator(mode="after")
        @suppress_internal_validation_warnings
        def check_missing_params(self) -> "MyModel":
            # Validation logic here
            pass
    """
    @functools.wraps(func)
    def wrapper(self: T) -> T:
        # Get the current call stack
        frame = inspect.currentframe()
        if not frame:
            return func(self)
            
        # Check if we're being called during default factory execution
        # This is the key indicator of spurious warnings
        depth = 0
        current_frame = frame
        
        while current_frame and depth < 20:
            if current_frame.f_code:
                filename = current_frame.f_code.co_filename
                function = current_frame.f_code.co_name
                
                # Check if we're in a default factory lambda
                if function == '<lambda>' and 'core.py' in filename:
                    # This is likely the default_factory=lambda: [Site()] case
                    return self
                    
                # Check for model construction during class definition
                if function == 'SUEWSConfig' and '<module>' in str(current_frame.f_back.f_code.co_name if current_frame.f_back else ''):
                    return self
                    
            current_frame = current_frame.f_back
            depth += 1
        
        # Run the actual validator
        return func(self)
    
    return wrapper


def validate_only_when_complete(*required_fields: str) -> Callable:
    """Decorator to run validation only when specified fields are present.
    
    This prevents warnings during object construction when fields are temporarily None.
    
    Args:
        *required_fields: Field names that must be present for validation to run
        
    Usage:
        @model_validator(mode="after")
        @validate_only_when_complete('sfr', 'bldgh', 'faibldg')
        def check_missing_building_params(self) -> "BldgsProperties":
            # This will only run when all three fields exist
            pass
    """
    def decorator(func: Callable[[T], T]) -> Callable[[T], T]:
        @functools.wraps(func)
        def wrapper(self: T) -> T:
            # Check if all required fields exist
            if not all(hasattr(self, field) for field in required_fields):
                return self
                
            return func(self)
        
        return wrapper
    return decorator
