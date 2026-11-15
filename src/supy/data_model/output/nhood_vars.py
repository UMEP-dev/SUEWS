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
        unit="1",
        description=(
            "Iteration count of the convergence loop; not yet written by the "
            "kernel, so always missing"
        ),
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.NHOOD,
        level=OutputLevel.DEFAULT,
    ),
]
