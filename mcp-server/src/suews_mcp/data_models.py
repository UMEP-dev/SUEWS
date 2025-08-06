"""Minimal SUEWS data models for MCP server when SuPy is not available."""

from enum import Enum
from typing import Any, Dict


class EmissionsMethod(Enum):
    """Method for calculating anthropogenic heat flux (QF)."""

    NO_EMISSIONS = 0
    L11 = 1
    J11 = 2
    J19 = 4


class NetRadiationMethod(Enum):
    """Method for calculating net all-wave radiation (Q*)."""

    OBSERVED = 0
    LDOWN_OBSERVED = 1
    LDOWN_CLOUD = 2
    LDOWN_AIR = 3


class StorageHeatMethod(Enum):
    """Method for calculating storage heat flux (ΔQS)."""

    OBSERVED = 0
    OHM_WITHOUT_QF = 1
    OHM_WITH_QF = 2
    OHM_WITH_ESTM = 3


class OhmIncQF(Enum):
    """Whether to include QF in OHM calculations."""

    EXCLUDE_QF = 0
    INCLUDE_QF = 1


class StabilityMethod(Enum):
    """Atmospheric stability calculation method."""

    NEUTRAL = 0
    BUSINGER_DYER = 1
    SG2000 = 2


class RoughnessMethod(Enum):
    """Roughness length calculation method."""

    FIXED = 0
    VARIABLE = 1


class FAIMethod(Enum):
    """Frontal area index method."""

    FIXED = 0
    VARIABLE = 1


class StebbsMethod(Enum):
    """Stebbs method for radiation."""

    FIXED = 0
    VARIABLE = 1


class SnowUse(Enum):
    """Whether to use snow module."""

    NO = 0
    YES = 1


# Placeholder classes for newer methods
class RSLMethod(Enum):
    """Roughness sublayer method."""

    NONE = 0
    VARIABLE = 1


class RSLLevel(Enum):
    """RSL reference level."""

    Z = 0
    ZD = 1


class GSModel(Enum):
    """Surface conductance model."""

    J11 = 0
    W16 = 1


# Mock SUEWSConfig for basic validation
class SUEWSConfig:
    """Minimal SUEWS configuration model."""

    def __init__(self, **kwargs):
        self.data = kwargs
        self._validate()

    @classmethod
    def model_validate(cls, data: Dict[str, Any], context: Dict[str, Any] = None):
        """Mimic pydantic's model_validate."""
        return cls(**data)

    def _validate(self):
        """Basic validation logic."""
        # Check required sections
        required = ["grid", "forcing", "runcontrol"]
        for section in required:
            if section not in self.data:
                raise ValueError(f"Missing required section: {section}")

        # Check surface fractions
        if "grid" in self.data:
            grid = self.data["grid"]
            frac_keys = [k for k in grid.keys() if k.startswith("frac_")]
            if frac_keys:
                total = sum(grid.get(k, 0) for k in frac_keys)
                if abs(total - 1.0) > 0.01:
                    raise ValueError(f"Surface fractions must sum to 1.0, got {total}")


# Mock ValidationController
ValidationController = None
