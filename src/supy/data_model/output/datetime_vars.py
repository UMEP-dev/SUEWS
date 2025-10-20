"""Datetime output variables.

These variables represent the timestamp information in SUEWS output files.
"""

from .variables import (
    OutputVariable,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)


DATETIME_VARIABLES = [
    OutputVariable(
        name="Year",
        unit="YYYY",
        description="Year",
        aggregation=AggregationMethod.TIME,
        group=OutputGroup.DATETIME,
        level=OutputLevel.DEFAULT,
        format="i0004",
    ),
    OutputVariable(
        name="DOY",
        unit="DOY",
        description="Day of Year",
        aggregation=AggregationMethod.TIME,
        group=OutputGroup.DATETIME,
        level=OutputLevel.DEFAULT,
        format="i0004",
    ),
    OutputVariable(
        name="Hour",
        unit="HH",
        description="Hour",
        aggregation=AggregationMethod.TIME,
        group=OutputGroup.DATETIME,
        level=OutputLevel.DEFAULT,
        format="i0004",
    ),
    OutputVariable(
        name="Min",
        unit="MM",
        description="Minute",
        aggregation=AggregationMethod.TIME,
        group=OutputGroup.DATETIME,
        level=OutputLevel.DEFAULT,
        format="i0004",
    ),
    OutputVariable(
        name="Dectime",
        unit="-",
        description="Decimal time",
        aggregation=AggregationMethod.TIME,
        group=OutputGroup.DATETIME,
        level=OutputLevel.DEFAULT,
        format="f08.4",
    ),
]
