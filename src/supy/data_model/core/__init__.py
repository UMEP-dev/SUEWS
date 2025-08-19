"""
SUEWS Core Data Model Definitions

This module contains all core data structures, models, and types for SUEWS.
"""

# Configuration
from .config import SUEWSConfig, init_config_from_yaml

# Core models
from .site import (
    Site,
    SiteProperties,
    EvetrProperties,
    DectrProperties,
    GrassProperties,
)
from .model import Model, ModelPhysics, ModelControl
from .surface import (
    SurfaceProperties,
    PavedProperties,
    BldgsProperties,
    BsoilProperties,
    WaterProperties,
    VerticalLayers,
    ThermalLayers,
)
from .state import InitialStates

# Parameters
from .human_activity import (
    AnthropogenicEmissions,
    AnthropogenicHeat,
    CO2Params,
    IrrigationParams,
)
from .hydro import WaterDistribution, StorageDrainParams
from .ohm import OHM_Coefficient_season_wetness
from .profile import DayProfile, WeeklyProfile, HourlyProfile

# Types
from .type import RefValue, Reference, SurfaceType
from .timezone_enum import TimezoneOffset

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
]
