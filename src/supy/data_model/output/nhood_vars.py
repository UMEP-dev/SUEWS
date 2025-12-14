"""Neighbourhood (NHood) output variables.

These are experimental neighbourhood-scale output variables.
"""

from .variables import (
    OutputVariable,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)


NHOOD_VARIABLES = [
    OutputVariable(
        name="iter_count",
        unit="-",
        description="iteration count of convergence loop",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.NHOOD,
        level=OutputLevel.DEFAULT,
        format="f104",
    ),
]
