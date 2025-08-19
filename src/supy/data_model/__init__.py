"""
SUEWS Data Model

This module provides Pydantic-based data models for the SUEWS urban climate model.

All physical parameters use RefValue wrappers with explicit units:
- Physical quantities have appropriate SI-based units (m, kg, W, degC, etc.)
- Dimensionless ratios are marked with unit="dimensionless"
- Configuration parameters (methods, flags, paths) appropriately have no units

See README.md for detailed unit conventions and usage examples.
"""

# Import everything from core module
from .core import (
    # Configuration
    SUEWSConfig,
    init_config_from_yaml,
    # Core models
    Site,
    SiteProperties,
    Model,
    ModelPhysics,
    ModelControl,
    # Surface
    SurfaceProperties,
    PavedProperties,
    BldgsProperties,
    EvetrProperties,
    DectrProperties,
    GrassProperties,
    BsoilProperties,
    WaterProperties,
    VerticalLayers,
    ThermalLayers,
    # State
    InitialStates,
    # Parameters
    AnthropogenicEmissions,
    AnthropogenicHeat,
    CO2Params,
    IrrigationParams,
    WaterDistribution,
    StorageDrainParams,
    OHM_Coefficient_season_wetness,
    # Profiles
    DayProfile,
    WeeklyProfile,
    HourlyProfile,
    # Types
    RefValue,
    Reference,
    SurfaceType,
    TimezoneOffset,
)

# Import from new validation module
try:
    from .validation import (
        ValidationController,
        ValidationResult,
        validate_suews_config_conditional,
        run_precheck,
    )
except ImportError:
    # Fallback if validation module not available
    ValidationController = None
    ValidationResult = None
    validate_suews_config_conditional = None
    run_precheck = None

# Export everything
__all__ = [
    # Configuration
    "SUEWSConfig",
    "init_config_from_yaml",
    # Core models
    "Site",
    "SiteProperties",
    "Model",
    "ModelPhysics",
    "ModelControl",
    # Surface
    "SurfaceProperties",
    "PavedProperties",
    "BldgsProperties",
    "EvetrProperties",
    "DectrProperties",
    "GrassProperties",
    "BsoilProperties",
    "WaterProperties",
    "VerticalLayers",
    "ThermalLayers",
    # State
    "InitialStates",
    # Parameters
    "AnthropogenicEmissions",
    "AnthropogenicHeat",
    "CO2Params",
    "IrrigationParams",
    "WaterDistribution",
    "StorageDrainParams",
    "OHM_Coefficient_season_wetness",
    # Profiles
    "DayProfile",
    "WeeklyProfile",
    "HourlyProfile",
    # Types
    "RefValue",
    "Reference",
    "SurfaceType",
    "TimezoneOffset",
    # Validation
    "ValidationController",
    "ValidationResult",
    "validate_suews_config_conditional",
    "run_precheck",
]
