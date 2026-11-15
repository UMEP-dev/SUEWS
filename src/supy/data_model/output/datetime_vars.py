"""Datetime output variables.

These variables provide temporal information and are included in all output groups
as the first columns to identify the timestamp of each record.
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
        unit="1",
        description="Year (4-digit integer)",
        aggregation=AggregationMethod.TIME,
        group=OutputGroup.DATETIME,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="DOY",
        unit="1",
        description="Day of year (1-366)",
        aggregation=AggregationMethod.TIME,
        group=OutputGroup.DATETIME,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Hour",
        unit="1",
        description="Hour of day (0-23)",
        aggregation=AggregationMethod.TIME,
        group=OutputGroup.DATETIME,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Min",
        unit="1",
        description="Minute of hour (0-59)",
        aggregation=AggregationMethod.TIME,
        group=OutputGroup.DATETIME,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Dectime",
        unit="1",
        description="Decimal time (fractional day of year)",
        aggregation=AggregationMethod.TIME,
        group=OutputGroup.DATETIME,
        level=OutputLevel.DEFAULT,
    ),
]
