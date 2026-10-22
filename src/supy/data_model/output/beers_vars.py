"""BEERS radiation model output variables.

These variables provide detailed radiation outputs from the BEERS
(Built Environment Energy and Radiation model - Successor of SOLWEIG).
"""

from .variables import (
    OutputVariable,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)


BEERS_VARIABLES = [
    OutputVariable(
        name="azimuth",
        unit="degrees",
        description="Solar azimuth angle",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="altitude",
        unit="degrees",
        description="Solar altitude angle",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="GlobalRad",
        unit="W m-2",
        description="Global shortwave radiation",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="DiffuseRad",
        unit="W m-2",
        description="Diffuse shortwave radiation on a horizontal surface",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="DirectRad",
        unit="W m-2",
        description="Direct-beam shortwave radiation on a surface normal to the sun; can exceed GlobalRad at low sun",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Kdown2d",
        unit="W m-2",
        description="Incoming shortwave radiation at the point of interest",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Kup2d",
        unit="W m-2",
        description="Outgoing shortwave radiation at the point of interest",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Ksouth",
        unit="W m-2",
        description="Shortwave radiation from the south at the point of interest",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Kwest",
        unit="W m-2",
        description="Shortwave radiation from the west at the point of interest",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Knorth",
        unit="W m-2",
        description="Shortwave radiation from the north at the point of interest",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Keast",
        unit="W m-2",
        description="Shortwave radiation from the east at the point of interest",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Ldown2d",
        unit="W m-2",
        description="Incoming longwave radiation at the point of interest",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Lup2d",
        unit="W m-2",
        description="Outgoing longwave radiation at the point of interest",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Lsouth",
        unit="W m-2",
        description="Longwave radiation from the south at the point of interest",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Lwest",
        unit="W m-2",
        description="Longwave radiation from the west at the point of interest",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Lnorth",
        unit="W m-2",
        description="Longwave radiation from the north at the point of interest",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Least",
        unit="W m-2",
        description="Longwave radiation from the east at the point of interest",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Tmrt",
        unit="degC",
        description="Mean radiant temperature",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="I0",
        unit="W m-2",
        description="Clear-sky incoming shortwave radiation at the surface",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="CI",
        unit="-",
        description="Clearness index (global over clear-sky shortwave radiation), used to estimate Ldown",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="SH_Ground",
        unit="-",
        description=(
            "Sunlit fraction of the ground (0=fully shaded, 1=fully sunlit; 0 at night)"
        ),
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="SH_Walls",
        unit="-",
        description=(
            "Sunlit fraction of the canyon walls (0=fully shaded; at most 0.5 "
            "as one wall is always shaded; 0 at night)"
        ),
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="SVF_Ground",
        unit="-",
        description="Sky view factor at the ground, from the canyon height-to-width ratio",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="SVF_Roof",
        unit="-",
        description="Sky view factor at the roof, from the canyon height-to-width ratio",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="SVF_BdVeg",
        unit="-",
        description=(
            "Sky view factor at the ground accounting for both buildings and "
            "vegetation; vegetation is not yet represented, so it equals SVF_Ground"
        ),
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Emis_Sky",
        unit="-",
        description="Clear-sky emissivity from Prata (1996)",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Ta",
        unit="degC",
        description="Air temperature from the forcing",
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Tg",
        unit="degC",
        description=(
            "Excess of the ground surface temperature over the air temperature Ta, "
            "from the BEERS daytime surface-temperature parameterisation; 0 at night"
        ),
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Tw",
        unit="degC",
        description=(
            "Excess of the wall surface temperature over the air temperature Ta, "
            "from the BEERS daytime surface-temperature parameterisation; 0 at night"
        ),
        aggregation=AggregationMethod.AVERAGE,
        group=OutputGroup.BEERS,
        level=OutputLevel.DEFAULT,
    ),
]
