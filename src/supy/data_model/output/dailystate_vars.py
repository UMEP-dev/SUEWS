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


# LAST selects the sparse daily record; it does not describe the operation
# already performed by update_DailyState_Day/update_HDD/update_WaterUse.
# Explicit CF methods below describe those kernel-produced daily statistics.
DAILYSTATE_VARIABLES = [
    # Heating/Cooling Degree Days
    OutputVariable(
        name="HDD1_h",
        units_metadata="temperature: difference",
        cell_methods="time: mean",
        unit="degC",
        description="Heating degree days: daily mean of the amount by which air temperature falls below the heating base temperature; air temperature is the forcing air temperature, or T2 when roughness_sublayer_level is basic, or the RSL profile temperature T_10 when it is detailed",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="HDD2_c",
        units_metadata="temperature: difference",
        cell_methods="time: mean",
        unit="degC",
        description="Cooling degree days: daily mean of the amount by which air temperature exceeds the cooling base temperature; air temperature is the forcing air temperature, or T2 when roughness_sublayer_level is basic, or the RSL profile temperature T_10 when it is detailed",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="HDD3_Tmean",
        cell_methods="time: mean",
        unit="degC",
        description="Daily mean air temperature: the forcing air temperature, or T2 when roughness_sublayer_level is basic, or the RSL profile temperature T_10 when it is detailed",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="HDD4_T5d",
        cell_methods="time: mean",
        unit="degC",
        description="5-day running mean of the daily mean air temperature in HDD3_Tmean (fewer days at the start of a run)",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Precipitation and days since rain
    OutputVariable(
        name="P_day",
        cell_methods="time: mean",
        unit="kg m-2 s-1",
        description="Daily total precipitation",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="DaysSR",
        unit="d",
        description="Days since rain",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Growing Degree Days by vegetation type
    OutputVariable(
        name="GDD_EveTr",
        units_metadata="temperature: difference",
        unit="degC",
        description="Growing degree days for evergreen trees, from the mean of the daily minimum and maximum air temperature (Tmin, Tmax)",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="GDD_DecTr",
        units_metadata="temperature: difference",
        unit="degC",
        description="Growing degree days for deciduous trees, from the mean of the daily minimum and maximum air temperature (Tmin, Tmax)",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="GDD_Grass",
        units_metadata="temperature: difference",
        unit="degC",
        description="Growing degree days for grass, from the mean of the daily minimum and maximum air temperature (Tmin, Tmax)",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Senescence Degree Days by vegetation type
    OutputVariable(
        name="SDD_EveTr",
        units_metadata="temperature: difference",
        unit="degC",
        description="Senescence degree days for evergreen trees, from the mean of the daily minimum and maximum air temperature (Tmin, Tmax)",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="SDD_DecTr",
        units_metadata="temperature: difference",
        unit="degC",
        description="Senescence degree days for deciduous trees, from the mean of the daily minimum and maximum air temperature (Tmin, Tmax)",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="SDD_Grass",
        units_metadata="temperature: difference",
        unit="degC",
        description="Senescence degree days for grass, from the mean of the daily minimum and maximum air temperature (Tmin, Tmax)",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Temperature extremes
    OutputVariable(
        name="Tmin",
        cell_methods="time: minimum",
        unit="degC",
        description="Daily minimum air temperature: the forcing air temperature, or T2 when roughness_sublayer_level is basic, or the RSL profile temperature T_10 when it is detailed",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="Tmax",
        cell_methods="time: maximum",
        unit="degC",
        description="Daily maximum air temperature: the forcing air temperature, or T2 when roughness_sublayer_level is basic, or the RSL profile temperature T_10 when it is detailed",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Daylight hours
    OutputVariable(
        name="DLHrs",
        cell_methods="time: sum",
        unit="h",
        description="Day length: hours with incoming shortwave radiation above 10 W m-2",
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
        unit="kg m-2",
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
        unit="1",
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
        unit="1",
        description="Albedo for evergreen trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="AlbDecTr",
        unit="1",
        description="Albedo for deciduous trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="AlbGrass",
        unit="1",
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
        cell_methods="time: mean",
        unit="kg m-2 s-1",
        description="Modelled daily total irrigation water use for evergreen trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_EveTr2",
        cell_methods="time: mean",
        unit="kg m-2 s-1",
        description="Modelled daily automatic irrigation water use for evergreen trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_EveTr3",
        cell_methods="time: mean",
        unit="kg m-2 s-1",
        description="Modelled daily manual irrigation water use for evergreen trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_DecTr1",
        cell_methods="time: mean",
        unit="kg m-2 s-1",
        description="Modelled daily total irrigation water use for deciduous trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_DecTr2",
        cell_methods="time: mean",
        unit="kg m-2 s-1",
        description="Modelled daily automatic irrigation water use for deciduous trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_DecTr3",
        cell_methods="time: mean",
        unit="kg m-2 s-1",
        description="Modelled daily manual irrigation water use for deciduous trees",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_Grass1",
        cell_methods="time: mean",
        unit="kg m-2 s-1",
        description="Modelled daily total irrigation water use for grass",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_Grass2",
        cell_methods="time: mean",
        unit="kg m-2 s-1",
        description="Modelled daily automatic irrigation water use for grass",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    OutputVariable(
        name="WU_Grass3",
        cell_methods="time: mean",
        unit="kg m-2 s-1",
        description="Modelled daily manual irrigation water use for grass",
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # LUMPS LAI
    OutputVariable(
        name="LAIlumps",
        unit="1",
        description=(
            "Vegetation phenology factor for LUMPS (0 to 1): the "
            "surface-weighted leaf area index of the vegetated surfaces over their "
            "surface-weighted maximum leaf area index"
        ),
        aggregation=AggregationMethod.LAST,
        group=OutputGroup.DAILYSTATE,
        level=OutputLevel.DEFAULT,
    ),
    # Snow albedo
    OutputVariable(
        name="AlbSnow",
        unit="1",
        description=(
            "Albedo of the snowpack, one value shared by all snow-covered "
            "surfaces: it decreases as the snow ages (linearly below 0 degC, "
            "towards the minimum snow albedo at or above 0 degC), returns to "
            "the maximum after enough fresh snowfall, and is 0 when no "
            "surface has snow"
        ),
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
        unit="1",
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
        unit="1",
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
