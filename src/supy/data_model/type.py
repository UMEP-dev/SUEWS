from typing import TypeVar, Optional, Generic, Union, Any
from pydantic import ConfigDict, BaseModel, Field, model_validator
import numpy as np
import pandas as pd
from enum import Enum


class SurfaceType(str, Enum):
    PAVED = "paved"
    BLDGS = "bldgs"
    EVETR = "evetr"
    DECTR = "dectr"
    GRASS = "grass"
    BSOIL = "bsoil"
    WATER = "water"


T = TypeVar("T")


class Reference(BaseModel):
    desc: Optional[str] = Field(
        default=None, description="Description of the reference source"
    )
    ID: Optional[str] = Field(
        default=None, description="Identifier for the reference (e.g., citation key)"
    )
    DOI: Optional[str] = Field(
        default=None, description="Digital Object Identifier for the reference"
    )


class RefValue(BaseModel, Generic[T]):
    """A class that wraps a value with an optional reference.

    This class allows storing a value along with its reference information (e.g. DOI).
    It handles numeric type conversion and implements comparison operators.

    When used in Field definitions for physical quantities, units should be specified:

    Examples:
        # Physical quantity with units
        temperature: RefValue[float] = Field(
            default=RefValue(15.0,
            description="Air temperature", json_schema_extra={"unit": "degC"}
        )

        # Dimensionless ratio
        albedo: RefValue[float] = Field(
            default=RefValue(0.2,
            description="Surface albedo", json_schema_extra={"unit": "dimensionless"}
        )

        # Configuration parameter (no unit needed)
        method: RefValue[int] = Field(
            default=RefValue(1,
            description="Calculation method selection"
        )

    Unit Conventions:
        - Use SI base units: m, kg, s, K, W, etc.
        - Use ASCII notation for compounds: m^2, kg m^-3, W m^-1 K^-1
        - Use degC for temperatures, K for temperature differences
        - Use "dimensionless" for ratios, fractions, albedos, etc.

    Attributes:
        value (T): The wrapped value of generic type T
        ref (Optional[Reference]): Optional reference information for the value
    """

    value: Optional[T]
    ref: Optional[Reference] = None

    def __init__(self, value: T, ref: Optional[Reference] = None):
        # Convert numpy numeric types to Python native types
        if isinstance(value, (np.float64, np.float32)):
            value = float(value)
        elif isinstance(value, (np.int64, np.int32)):
            value = int(value)
        super().__init__(value=value, ref=ref)

    @classmethod
    def wrap(cls, value: Union[T, "RefValue[T]"]) -> "RefValue[T]":
        """Auto-wrap simple values in RefValue, return RefValue unchanged"""
        if isinstance(value, cls):
            return value
        return cls(value)

    def __str__(self):
        """String representation showing just the value"""
        return f"{self.value}"

    def __repr__(self):
        """Representation showing just the value"""
        return f"{self.value}"

    def __eq__(self, other):
        """Equal comparison operator"""
        if isinstance(other, RefValue):
            return self.value == other.value
        return self.value == other

    def __ne__(self, other):
        """Not equal comparison operator"""
        if isinstance(other, RefValue):
            return self.value != other.value
        return self.value != other

    def __ge__(self, other):
        """Greater than or equal comparison operator that allows None."""
        if self.value is None or (isinstance(other, RefValue) and other.value is None):
            return True
        if isinstance(other, RefValue):
            return self.value >= other.value
        return self.value >= other

    def __gt__(self, other):
        """Greater than comparison operator that allows None."""
        if self.value is None or (isinstance(other, RefValue) and other.value is None):
            return True
        if isinstance(other, RefValue):
            return self.value > other.value
        return self.value > other

    def __le__(self, other):
        """Less than or equal comparison operator that allows None."""
        if self.value is None or (isinstance(other, RefValue) and other.value is None):
            return True
        if isinstance(other, RefValue):
            return self.value <= other.value
        return self.value <= other

    def __lt__(self, other):
        """Less than comparison operator that allows None."""
        if self.value is None or (isinstance(other, RefValue) and other.value is None):
            return True
        if isinstance(other, RefValue):
            return self.value < other.value
        return self.value < other




def FlexibleRefValue(type_param):
    """Create a Union type that accepts both RefValue and simple values"""
    return Union[RefValue[type_param], type_param]


def init_df_state(grid_id: int) -> pd.DataFrame:
    idx = pd.Index([grid_id], name="grid")
    col = pd.MultiIndex.from_tuples([("gridiv", "0")], names=["var", "ind_dim"])
    df_state = pd.DataFrame(index=idx, columns=col)
    df_state.loc[grid_id, ("gridiv", "0")] = grid_id
    return df_state
