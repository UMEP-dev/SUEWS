"""DailyState output variables.

These variables provide daily accumulated state information including
heating/cooling degree days, growing degree days, LAI, and other daily metrics.
"""

from .variables import (
    OutputVariable,
    AggregationMethod,
    OutputGroup,
    OutputLevel,
)


DAILYSTATE_VARIABLES = [
    # Heating/Cooling Degree Days
    OutputVariable(
        name="HDD1_h",
        unit="degC",
        description="Heating degree days (accumulation during calculation)",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="HDD2_c",
        unit="degC",
        description="Cooling degree days",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="HDD3_Tmean",
        unit="degC",
        description="Heating degree days based on mean temperature",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="HDD4_T5d",
        unit="degC",
        description="Heating degree days based on 5-day running mean temperature",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Precipitation and days since rain
    OutputVariable(
        name="P_day",
        unit="mm",
        description="Daily precipitation",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="DaysSR",
        unit="days",
        description="Days since rain",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Growing Degree Days by vegetation type
    OutputVariable(
        name="GDD_EveTr",
        unit="degC",
        description="Growing degree days for evergreen trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="GDD_DecTr",
        unit="degC",
        description="Growing degree days for deciduous trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="GDD_Grass",
        unit="degC",
        description="Growing degree days for grass",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Senescence Degree Days by vegetation type
    OutputVariable(
        name="SDD_EveTr",
        unit="degC",
        description="Senescence degree days for evergreen trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="SDD_DecTr",
        unit="degC",
        description="Senescence degree days for deciduous trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="SDD_Grass",
        unit="degC",
        description="Senescence degree days for grass",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Temperature extremes
    OutputVariable(
        name="Tmin",
        unit="degC",
        description="Daily minimum temperature",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Tmax",
        unit="degC",
        description="Daily maximum temperature",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Daylight hours
    OutputVariable(
        name="DLHrs",
        unit="hours",
        description="Daylight hours",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # LAI by vegetation type
    OutputVariable(
        name="LAI_EveTr",
        unit="m2 m-2",
        description="Leaf area index for evergreen trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="LAI_DecTr",
        unit="m2 m-2",
        description="Leaf area index for deciduous trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="LAI_Grass",
        unit="m2 m-2",
        description="Leaf area index for grass",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Vegetation properties
    OutputVariable(
        name="DecidCap",
        unit="mm",
        description=(
            "Water storage capacity of deciduous trees, varying with LAI between its "
            "minimum and maximum"
        ),
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Porosity",
        unit="-",
        description=(
            "Porosity of deciduous trees, varying inversely with LAI; used in the plan "
            "and frontal area indices"
        ),
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Albedo by vegetation type
    OutputVariable(
        name="AlbEveTr",
        unit="-",
        description="Albedo for evergreen trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="AlbDecTr",
        unit="-",
        description="Albedo for deciduous trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="AlbGrass",
        unit="-",
        description="Albedo for grass",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Modelled daily irrigation water use by vegetation type: 1 = total,
    # 2 = automatic (including the ponding demand), 3 = manual; zero unless
    # water use is modelled
    OutputVariable(
        name="WU_EveTr1",
        unit="mm",
        description="Modelled daily total irrigation water use for evergreen trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_EveTr2",
        unit="mm",
        description="Modelled daily automatic irrigation water use for evergreen trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_EveTr3",
        unit="mm",
        description="Modelled daily manual irrigation water use for evergreen trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_DecTr1",
        unit="mm",
        description="Modelled daily total irrigation water use for deciduous trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_DecTr2",
        unit="mm",
        description="Modelled daily automatic irrigation water use for deciduous trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_DecTr3",
        unit="mm",
        description="Modelled daily manual irrigation water use for deciduous trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_Grass1",
        unit="mm",
        description="Modelled daily total irrigation water use for grass",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_Grass2",
        unit="mm",
        description="Modelled daily automatic irrigation water use for grass",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_Grass3",
        unit="mm",
        description="Modelled daily manual irrigation water use for grass",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # LUMPS LAI
    OutputVariable(
        name="LAIlumps",
        unit="m2 m-2",
        description="Leaf area index (LUMPS calculation)",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Snow albedo
    OutputVariable(
        name="AlbSnow",
        unit="-",
        description="Snow albedo",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Snow density by surface type
    OutputVariable(
        name="DensSnow_Paved",
        unit="kg m-3",
        description="Snow density for paved surface",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="DensSnow_Bldgs",
        unit="kg m-3",
        description="Snow density for building surface",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="DensSnow_EveTr",
        unit="kg m-3",
        description="Snow density for evergreen tree surface",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="DensSnow_DecTr",
        unit="kg m-3",
        description="Snow density for deciduous tree surface",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="DensSnow_Grass",
        unit="kg m-3",
        description="Snow density for grass surface",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="DensSnow_BSoil",
        unit="kg m-3",
        description="Snow density for bare soil surface",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="DensSnow_Water",
        unit="kg m-3",
        description="Snow density for water surface",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # OHM coefficients at the end of the day: QS = a1*Q* + a2*dQ*/dt + a3
    OutputVariable(
        name="a1",
        unit="-",
        description=(
            "Surface-weighted OHM coefficient a1 (multiplies net all-wave radiation)"
        ),
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="a2",
        unit="h",
        description=(
            "Surface-weighted OHM coefficient a2 (multiplies the rate of change of net "
            "all-wave radiation)"
        ),
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="a3",
        unit="W m-2",
        description="Surface-weighted OHM coefficient a3 (constant term)",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="a1_bldg",
        unit="-",
        description=(
            "Dynamic OHM coefficient a1 for buildings (multiplies net all-wave "
            "radiation); zero unless dynamic OHM is used"
        ),
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="a2_bldg",
        unit="h",
        description=(
            "Dynamic OHM coefficient a2 for buildings (multiplies the rate of change of"
            " net all-wave radiation); zero unless dynamic OHM is used"
        ),
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="a3_bldg",
        unit="W m-2",
        description=(
            "Dynamic OHM coefficient a3 for buildings (constant term); zero unless "
            "dynamic OHM is used"
        ),
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
]
