"""Core Pydantic models for SUEWS output variables.

This module defines the data structures for output variable metadata,
transitioning from Fortran-first to Python-first architecture.
"""

from pydantic import BaseModel, Field
from enum import Enum
from typing import List, Dict, Optional, Callable
import pandas as pd
import numpy as np


class AggregationMethod(str, Enum):
    """How variables are aggregated during resampling.

    Attributes:
        TIME: Time columns (no aggregation, just timestamp)
        AVERAGE: Time-averaged using mean
        SUM: Cumulative sum over period
        LAST: Last value in the period
    """

    TIME = "T"
    AVERAGE = "A"
    SUM = "S"
    LAST = "L"


class OutputLevel(int, Enum):
    """Output priority levels for selective output.

    Attributes:
        DEFAULT: Core variables always included (level 0)
        EXTENDED: Extended variable set (level 1)
        SNOW_DETAILED: Snow-specific detailed output (level 2)
    """

    DEFAULT = 0
    EXTENDED = 1
    SNOW_DETAILED = 2


class OutputGroup(str, Enum):
    """Logical grouping of output variables.

    Attributes:
        DATETIME: Date and time columns
        SUEWS: Core SUEWS energy and water balance outputs
        SNOW: Snow-specific outputs (per surface type)
        ESTM: Element Surface Temperature Model outputs
        RSL: Roughness Sublayer profile outputs
        BL: Boundary Layer outputs
        DEBUG: Debug and diagnostic outputs
        BEERS: BEERS radiation model outputs
        DAILYSTATE: Daily accumulated state variables
    """

    DATETIME = "datetime"
    SUEWS = "SUEWS"
    SNOW = "snow"
    ESTM = "ESTM"
    RSL = "RSL"
    BL = "BL"
    DEBUG = "debug"
    BEERS = "BEERS"
    DAILYSTATE = "DailyState"


class OutputVariable(BaseModel):
    """Definition of a single output variable.

    Attributes:
        name: Variable name (column header)
        unit: Physical units
        description: Long-form description
        aggregation: Resampling aggregation method
        group: Output group membership
        level: Output priority level
        format: Fortran format specifier (for compatibility)
    """

    name: str = Field(description="Variable name (column header)")
    unit: str = Field(description="Physical units")
    description: str = Field(description="Long-form description")
    aggregation: AggregationMethod = Field(description="Resampling aggregation method")
    group: OutputGroup = Field(description="Output group membership")
    level: OutputLevel = Field(description="Output priority level")
    format: str = Field(default="f104", description="Fortran format specifier")

    class Config:
        use_enum_values = True


class OutputVariableRegistry(BaseModel):
    """Complete registry of all output variables.

    This class provides the central registry for all SUEWS output variables,
    replacing the Fortran-based runtime extraction with Python-first definitions.
    """

    variables: List[OutputVariable] = Field(
        default_factory=list, description="All registered output variables"
    )

    def by_group(self, group: OutputGroup) -> List[OutputVariable]:
        """Get all variables in a specific group.

        Args:
            group: Output group to filter by

        Returns:
            List of variables in the specified group
        """
        return [v for v in self.variables if v.group == group]

    def by_level(self, max_level: OutputLevel) -> List[OutputVariable]:
        """Get variables up to specified output level.

        Args:
            max_level: Maximum output level to include

        Returns:
            List of variables with level <= max_level
        """
        # Handle both enum and int values (Pydantic may convert with use_enum_values)
        max_val = max_level.value if isinstance(max_level, OutputLevel) else max_level
        return [
            v
            for v in self.variables
            if (v.level.value if isinstance(v.level, OutputLevel) else v.level)
            <= max_val
        ]

    def by_name(self, name: str) -> Optional[OutputVariable]:
        """Get variable by name.

        Args:
            name: Variable name to search for

        Returns:
            OutputVariable if found, None otherwise
        """
        return next((v for v in self.variables if v.name == name), None)

    def get_aggregation_rules(self) -> Dict[str, Dict[str, Callable]]:
        """Generate aggregation rules dict for resample_output().

        Returns:
            Dictionary mapping group -> {variable: aggregation_function}
            Compatible with pandas resample().agg()
        """
        dict_var_aggm = {}
        for group in OutputGroup:
            group_vars = self.by_group(group)
            if group_vars:
                group_key = group.value if isinstance(group, OutputGroup) else group
                dict_var_aggm[group_key] = {
                    v.name: self._get_agg_func(
                        v.aggregation
                        if isinstance(v.aggregation, AggregationMethod)
                        else AggregationMethod(v.aggregation)
                    )
                    for v in group_vars
                }
        return dict_var_aggm

    @staticmethod
    def _get_agg_func(method: AggregationMethod) -> Callable:
        """Map aggregation method to pandas function.

        Args:
            method: Aggregation method enum

        Returns:
            Callable suitable for pandas resample().agg()
        """
        mapping = {
            AggregationMethod.AVERAGE: "mean",
            AggregationMethod.SUM: "sum",
            AggregationMethod.LAST: lambda x: x.iloc[-1] if len(x) > 0 else np.nan,
            AggregationMethod.TIME: lambda x: x.iloc[-1] if len(x) > 0 else np.nan,
        }
        return mapping[method]

    def to_dataframe(self) -> pd.DataFrame:
        """Convert registry to DataFrame format (compatible with old df_var).

        Returns:
            DataFrame with MultiIndex (group, var) and columns (aggm, outlevel, func)
        """
        data = []
        for var in self.variables:
            # Handle both enum and primitive values (Pydantic may convert with use_enum_values)
            group_val = (
                var.group.value if isinstance(var.group, OutputGroup) else var.group
            )
            aggm_val = (
                var.aggregation.value
                if isinstance(var.aggregation, AggregationMethod)
                else var.aggregation
            )
            level_val = (
                var.level.value if isinstance(var.level, OutputLevel) else var.level
            )

            data.append({
                "group": group_val,
                "var": var.name,
                "aggm": aggm_val,
                "outlevel": str(level_val),
            })

        df = pd.DataFrame(data)
        df_indexed = df.set_index(["group", "var"])

        # Add func column for compatibility
        df_indexed["func"] = df_indexed["aggm"].apply(
            lambda x: self._get_agg_func(AggregationMethod(x))
        )

        return df_indexed

    class Config:
        arbitrary_types_allowed = True
