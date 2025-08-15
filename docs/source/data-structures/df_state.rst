
.. _df_state_var:

``df_state`` variables
============================

.. warning::

   **DEVELOPER REFERENCE ONLY**: This page documents internal ``df_state`` variables used by the SuPy Python API for developers.
   
   **For SUEWS users**: Please refer to the :doc:`YAML Configuration Format </inputs/yaml/index>` for user-facing parameter documentation.
   
   These variables are automatically managed by SuPy and should not be modified directly unless you are developing or debugging the model internals.

.. note:: Data structure of ``df_state`` is explained :ref:`here </data-structure/supy-io.ipynb#df_state_init:-model-initial-states>`.

.. option:: aerodynamicresistancemethod

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: ah_min

    :Description:
        Minimum QF values.
    :Dimensionality:
        (2,)
    :Dimensionality Remarks:
        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `AHMin_WD`, `AHMin_WE`


.. option:: ah_slope_cooling

    :Description:
        Cooling slope of QF calculation.
    :Dimensionality:
        (2,)
    :Dimensionality Remarks:
        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `AHSlope_Cooling_WD`, `AHSlope_Cooling_WE`


.. option:: ah_slope_heating

    :Description:
        Heating slope of QF calculation.
    :Dimensionality:
        (2,)
    :Dimensionality Remarks:
        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `AHSlope_Heating_WD`, `AHSlope_Heating_WE`


.. option:: ahprof_24hr

    :Description:
        Hourly profile values used in energy use calculation.
    :Dimensionality:
        (24, 2)
    :Dimensionality Remarks:
        24: hours of a day

        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `EnergyUseProfWD`, `EnergyUseProfWE`


.. option:: air_ext_lw

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: air_ext_sw

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: air_ssa_lw

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: air_ssa_sw

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: alb

    :Description:
        Effective surface albedo (middle of the day value) for summertime.
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `AlbedoMax`


.. option:: albdectr_id

    :Description:
        Albedo of deciduous surface `DecTr` on day 0 of run
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `albDecTr0`


.. option:: albevetr_id

    :Description:
        Albedo of evergreen surface `EveTr` on day 0 of run
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `albEveTr0`


.. option:: albgrass_id

    :Description:
        Albedo of grass surface `Grass` on day 0 of run
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `albGrass0`


.. option:: albmax_dectr

    :Description:
        Effective surface albedo (middle of the day value) for summertime.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `AlbedoMax`


.. option:: albmax_evetr

    :Description:
        Effective surface albedo (middle of the day value) for summertime.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `AlbedoMax`


.. option:: albmax_grass

    :Description:
        Effective surface albedo (middle of the day value) for summertime.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `AlbedoMax`


.. option:: albmin_dectr

    :Description:
        Effective surface albedo (middle of the day value) for wintertime (not including snow).
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `AlbedoMin`


.. option:: albmin_evetr

    :Description:
        Effective surface albedo (middle of the day value) for wintertime (not including snow).
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `AlbedoMin`


.. option:: albmin_grass

    :Description:
        Effective surface albedo (middle of the day value) for wintertime (not including snow).
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `AlbedoMin`


.. option:: alpha_bioco2

    :Description:
        The mean apparent ecosystem quantum. Represents the initial slope of the light-response curve.
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `alpha`


.. option:: alpha_enh_bioco2

    :Description:
        Part of the `alpha` coefficient related to the fraction of vegetation.
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `alpha_enh`


.. option:: alt

    :Description:
        Altitude of grids [m].
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `Alt`


.. option:: baset

    :Description:
        Base Temperature for initiating growing degree days (GDD) for leaf growth. [°C]
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `BaseT`


.. option:: baset_cooling

    :Description:
        Critical cooling temperature.
    :Dimensionality:
        (2,)
    :Dimensionality Remarks:
        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `TCritic_Cooling_WD`, `TCritic_Cooling_WE`


.. option:: baset_hc

    :Description:
        Base temperature for heating degree days [°C]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `BaseT_HC`


.. option:: baset_heating

    :Description:
        Critical heating temperature.
    :Dimensionality:
        (2,)
    :Dimensionality Remarks:
        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `TCritic_Heating_WD`, `TCritic_Heating_WE`


.. option:: basete

    :Description:
        Base temperature for initiating sensesance degree days (SDD) for leaf off. [°C]
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `BaseTe`


.. option:: basetmethod

    :Description:
        Determines method for base temperature used in HDD/CDD calculations.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `BaseTMethod`


.. option:: beta_bioco2

    :Description:
        The light-saturated gross photosynthesis of the canopy. [umol |m^-2| |s^-1| ]
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `beta`


.. option:: beta_enh_bioco2

    :Description:
        Part of the `beta` coefficient related to the fraction of vegetation.
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `beta_enh`


.. option:: bldgh

    :Description:
        Mean building height [m]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `H_Bldgs`


.. option:: capmax_dec

    :Description:
        Maximum water storage capacity for upper surfaces (i.e. canopy)
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `StorageMax`


.. option:: capmin_dec

    :Description:
        Minimum water storage capacity for upper surfaces (i.e. canopy).
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `StorageMin`


.. option:: chanohm

    :Description:
        Bulk transfer coefficient for this surface to use in AnOHM [-]
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `AnOHM_Ch`


.. option:: co2pointsource

    :Description:
        CO2 emission factor [kg |km^-1|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `CO2PointSource`


.. option:: cpanohm

    :Description:
        Volumetric heat capacity for this surface to use in AnOHM [J |m^-3|]
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `AnOHM_Cp`


.. option:: crwmax

    :Description:
        Maximum water holding capacity of snow [mm]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `CRWMax`


.. option:: crwmin

    :Description:
        Minimum water holding capacity of snow [mm]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `CRWMin`


.. option:: daywat

    :Description:
        Irrigation flag: 1 for on and 0 for off.
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: {Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday}
    :SUEWS-related variables:
        `DayWat(1)`, `DayWat(2)`, `DayWat(3)`, `DayWat(4)`, `DayWat(5)`, `DayWat(6)`, `DayWat(7)`


.. option:: daywatper

    :Description:
        Fraction of properties using irrigation for each day of a week.
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: {Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday}
    :SUEWS-related variables:
        `DayWatPer(1)`, `DayWatPer(2)`, `DayWatPer(3)`, `DayWatPer(4)`, `DayWatPer(5)`, `DayWatPer(6)`, `DayWatPer(7)`


.. option:: decidcap_id

    :Description:
        Storage capacity of deciduous surface `DecTr` on day 0 of run.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `decidCap0`


.. option:: dectreeh

    :Description:
        Mean height of deciduous trees [m]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `H_DecTr`


.. option:: diagmethod

    :Description:
        Defines how near surface diagnostics are calculated.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `DiagMethod`


.. option:: diagnose

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: diagqn

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: diagqs

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: drainrt

    :Description:
        Drainage rate of bucket for LUMPS [mm |h^-1|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `LUMPS_DrRate`


.. option:: ef_umolco2perj

    :Description:
        Emission factor for fuels used for building heating.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `EF_umolCO2perJ`


.. option:: emis

    :Description:
        Effective surface emissivity.
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `Emissivity`


.. option:: emissionsmethod

    :Description:
        Determines method for QF calculation.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `EmissionsMethod`


.. option:: enddls

    :Description:
        End of the day light savings [DOY]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `EndDLS`


.. option:: enef_v_jkm

    :Description:
        Emission factor for heat [J k |m^-1| ].
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `EnEF_v_Jkm`


.. option:: evapmethod

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: evetreeh

    :Description:
        Mean height of evergreen trees [m]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `H_EveTr`


.. option:: faibldg

    :Description:
        Frontal area index for buildings [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `FAI_Bldgs`


.. option:: faidectree

    :Description:
        Frontal area index for deciduous trees [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `FAI_DecTr`


.. option:: faievetree

    :Description:
        Frontal area index for evergreen trees [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `FAI_EveTr`


.. option:: faut

    :Description:
        Fraction of irrigated area that is irrigated using automated systems
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `Faut`


.. option:: fcef_v_kgkm

    :Description:
        CO2 emission factor for weekdays [kg |km^-1|];;CO2 emission factor for weekends [kg |km^-1|]
    :Dimensionality:
        (2,)
    :Dimensionality Remarks:
        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `FcEF_v_kgkmWD`, `FcEF_v_kgkmWE`


.. option:: flowchange

    :Description:
        Difference in input and output flows for water surface [mm |h^-1|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `FlowChange`


.. option:: frfossilfuel_heat

    :Description:
        Fraction of fossil fuels used for building heating [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `FrFossilFuel_Heat`


.. option:: frfossilfuel_nonheat

    :Description:
        Fraction of fossil fuels used for building energy use [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `FrFossilFuel_NonHeat`


.. option:: g1

    :Description:
        Related to maximum surface conductance [mm |s^-1|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `G1`


.. option:: g2

    :Description:
        Related to Kdown dependence [W |m^-2|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `G2`


.. option:: g3

    :Description:
        Related to VPD dependence [units depend on `gsModel`]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `G3`


.. option:: g4

    :Description:
        Related to VPD dependence [units depend on `gsModel`]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `G4`


.. option:: g5

    :Description:
        Related to temperature dependence [°C]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `G5`


.. option:: g6

    :Description:
        Related to soil moisture dependence [|mm^-1|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `G6`


.. option:: gddfull

    :Description:
        The growing degree days (GDD) needed for full capacity of the leaf area index (LAI) [°C].
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `GDDFull`


.. option:: ground_albedo_dir_mult_fact

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: gsmodel

    :Description:
        Formulation choice for conductance calculation.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `gsModel`


.. option:: h_maintain

    :Description:
        water depth to maintain used in automatic irrigation (e.g., ponding water due to flooding irrigation in rice crop-field) [mm].
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `H_maintain`


.. option:: humactivity_24hr

    :Description:
        Hourly profile values used in human activity calculation.
    :Dimensionality:
        (24, 2)
    :Dimensionality Remarks:
        24: hours of a day

        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `ActivityProfWD`, `ActivityProfWE`


.. option:: ie_a

    :Description:
        Coefficient for automatic irrigation model.
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `Ie_a1`, `Ie_a2`, `Ie_a3`


.. option:: ie_end

    :Description:
        Day when irrigation ends [DOY]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `Ie_end`


.. option:: ie_m

    :Description:
        Coefficient for manual irrigation model.
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `Ie_m1`, `Ie_m2`, `Ie_m3`


.. option:: ie_start

    :Description:
        Day when irrigation starts [DOY]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `Ie_start`


.. option:: internalwateruse_h

    :Description:
        Internal water use [mm |h^-1|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `InternalWaterUse`


.. option:: irrfracbldgs

    :Description:
        Fraction of `Bldgs` that is irrigated [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `IrrFr_Bldgs`


.. option:: irrfracbsoil

    :Description:
        Fraction of `BSoil` that is irrigated [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `IrrFr_BSoil`


.. option:: irrfracdectr

    :Description:
        Fraction of `DecTr` that is irrigated [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `IrrFr_DecTr`


.. option:: irrfracevetr

    :Description:
        Fraction of `EveTr` that is irrigated [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `IrrFr_EveTr`


.. option:: irrfracgrass

    :Description:
        Fraction of `Grass` that is irrigated [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `IrrFr_Grass`


.. option:: irrfracpaved

    :Description:
        Fraction of `Paved` that is irrigated [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `IrrFr_Paved`


.. option:: irrfracwater

    :Description:
        Fraction of `Water` that is irrigated [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `IrrFr_Water`


.. option:: kkanohm

    :Description:
        Thermal conductivity for this surface to use in AnOHM [W m |K^-1|]
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `AnOHM_Kk`


.. option:: kmax

    :Description:
        Maximum incoming shortwave radiation [W |m^-2|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `Kmax`


.. option:: lai_id

    :Description:
        Initial LAI values.
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `LAIinitialDecTr`, `LAIinitialEveTr`, `LAIinitialGrass`


.. option:: laicalcyes

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: laimax

    :Description:
        full leaf-on summertime value
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `LAIMax`


.. option:: laimin

    :Description:
        leaf-off wintertime value
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `LAIMin`


.. option:: laipower

    :Description:
        parameters required by LAI calculation.
    :Dimensionality:
        (4, 3)
    :Dimensionality Remarks:
        4: {`LeafGrowthPower1`, `LeafGrowthPower2`, `LeafOffPower1`, `LeafOffPower2`}

        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `LeafGrowthPower1`, `LeafGrowthPower2`, `LeafOffPower1`, `LeafOffPower2`


.. option:: laitype

    :Description:
        LAI calculation choice.
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `LAIEq`


.. option:: lat

    :Description:
        Latitude [deg].
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `lat`


.. option:: lng

    :Description:
        longitude [deg]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `lng`


.. option:: maxconductance

    :Description:
        The maximum conductance of each vegetation or surface type. [mm |s^-1|]
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `MaxConductance`


.. option:: maxfcmetab

    :Description:
        Maximum (day) CO2 from human metabolism. [W |m^-2|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `MaxFCMetab`


.. option:: maxqfmetab

    :Description:
        Maximum value for human heat emission. [W |m^-2|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `MaxQFMetab`


.. option:: min_res_bioco2

    :Description:
        Minimum soil respiration rate (for cold-temperature limit) [umol |m^-2| |s^-1|].
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `min_respi`


.. option:: minfcmetab

    :Description:
        Minimum (night) CO2 from human metabolism. [W |m^-2|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `MinFCMetab`


.. option:: minqfmetab

    :Description:
        Minimum value for human heat emission. [W |m^-2|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `MinQFMetab`


.. option:: n_stream_lw_urban

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: n_stream_sw_urban

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: n_vegetation_region_urban

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: narp_emis_snow

    :Description:
        Effective surface emissivity.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `Emissivity`


.. option:: narp_trans_site

    :Description:
        Atmospheric transmissivity for NARP [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `NARP_Trans`


.. option:: netradiationmethod

    :Description:
        Determines method for calculation of radiation fluxes.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `NetRadiationMethod`


.. option:: ohm_coef

    :Description:
        Coefficients for OHM calculation.
    :Dimensionality:
        (8, 4, 3)
    :Dimensionality Remarks:
        8: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`, one extra land cover type (currently NOT used)}

        4: {SummerWet, SummerDry, WinterWet, WinterDry}

        3: {a1, a2, a3}
    :SUEWS-related variables:
        `a1`, `a2`, `a3`


.. option:: ohm_threshsw

    :Description:
        Temperature threshold determining whether summer/winter OHM coefficients are applied [°C]
    :Dimensionality:
        (8,)
    :Dimensionality Remarks:
        8: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`, one extra land cover type (currently NOT used)}
    :SUEWS-related variables:
        `OHMThresh_SW`


.. option:: ohm_threshwd

    :Description:
        Soil moisture threshold determining whether wet/dry OHM coefficients are applied [-]
    :Dimensionality:
        (8,)
    :Dimensionality Remarks:
        8: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`, one extra land cover type (currently NOT used)}
    :SUEWS-related variables:
        `OHMThresh_WD`


.. option:: ohmincqf

    :Description:
        Determines whether the storage heat flux calculation uses |Qstar| or ( |Qstar| +QF).
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `OHMIncQF`


.. option:: pipecapacity

    :Description:
        Storage capacity of pipes [mm]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `PipeCapacity`


.. option:: popdensdaytime

    :Description:
        Daytime population density (i.e. workers, tourists) [people |ha^-1|]
    :Dimensionality:
        (2,)
    :Dimensionality Remarks:
        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `PopDensDay`


.. option:: popdensnighttime

    :Description:
        Night-time population density (i.e. residents) [people |ha^-1|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `PopDensNight`


.. option:: popprof_24hr

    :Description:
        Hourly profile values used in dynamic population estimation.
    :Dimensionality:
        (24, 2)
    :Dimensionality Remarks:
        24: hours of a day

        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `PopProfWD`, `PopProfWE`


.. option:: pormax_dec

    :Description:
        full leaf-on summertime value Used only for `DecTr` (can affect roughness calculation)
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `PorosityMax`


.. option:: pormin_dec

    :Description:
        leaf-off wintertime value Used only for `DecTr` (can affect roughness calculation)
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `PorosityMin`


.. option:: porosity_id

    :Description:
        Porosity of deciduous vegetation on day 0 of run.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `porosity0`


.. option:: preciplimit

    :Description:
        Temperature limit when precipitation falls as snow [°C]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `PrecipLimSnow`


.. option:: preciplimitalb

    :Description:
        Limit for hourly precipitation when the ground is fully covered with snow [mm]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `PrecipLimAlb`


.. option:: qf0_beu

    :Description:
        Building energy use [W |m^-2|]
    :Dimensionality:
        (2,)
    :Dimensionality Remarks:
        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `QF0_BEU_WD`, `QF0_BEU_WE`


.. option:: qf_a

    :Description:
        Base value for QF calculation.
    :Dimensionality:
        (2,)
    :Dimensionality Remarks:
        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `QF_A_WD`, `QF_A_WE`


.. option:: qf_b

    :Description:
        Parameter related to heating degree days.
    :Dimensionality:
        (2,)
    :Dimensionality Remarks:
        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `QF_B_WD`, `QF_B_WE`


.. option:: qf_c

    :Description:
        Parameter related to heating degree days.
    :Dimensionality:
        (2,)
    :Dimensionality Remarks:
        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `QF_C_WD`, `QF_C_WE`


.. option:: radmeltfact

    :Description:
        Hourly radiation melt factor of snow [mm |w^-1| |h^-1|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `RadMeltFactor`


.. option:: raincover

    :Description:
        Limit when surface totally covered with water for LUMPS [mm]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `LUMPS_Cover`


.. option:: rainmaxres

    :Description:
        Maximum water bucket reservoir [mm] Used for LUMPS surface wetness control.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `LUMPS_MaxRes`


.. option:: resp_a

    :Description:
        Respiration coefficient a.
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `resp_a`


.. option:: resp_b

    :Description:
        Respiration coefficient b - related to air temperature dependency.
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `resp_b`


.. option:: roughlenheatmethod

    :Description:
        Determines method for calculating roughness length for heat.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `RoughLenHeatMethod`


.. option:: roughlenmommethod

    :Description:
        Determines how aerodynamic roughness length (z0m) and zero displacement height (zdm) are calculated.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `RoughLenMomMethod`


.. option:: runofftowater

    :Description:
        Fraction of above-ground runoff flowing to water surface during flooding [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `RunoffToWater`


.. option:: s1

    :Description:
        A parameter related to soil moisture dependence [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `S1`


.. option:: s2

    :Description:
        A parameter related to soil moisture dependence [mm]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `S2`


.. option:: sathydraulicconduct

    :Description:
        Hydraulic conductivity for saturated soil [mm |s^-1|]
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `SatHydraulicCond`


.. option:: sddfull

    :Description:
        The sensesence degree days (SDD) needed to initiate leaf off. [°C]
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `SDDFull`


.. option:: sfr_surf

    :Description:
        Surface cover fractions.
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `Fr_Bldgs`, `Fr_Bsoil`, `Fr_DecTr`, `Fr_EveTr`, `Fr_Grass`, `Fr_Paved`, `Fr_Water`


.. option:: smdmethod

    :Description:
        Determines method for calculating soil moisture deficit (SMD).
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `SMDMethod`


.. option:: snowalb

    :Description:
        Initial snow albedo
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `SnowAlb0`


.. option:: snowalbmax

    :Description:
        Effective surface albedo (middle of the day value) for summertime.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `AlbedoMax`


.. option:: snowalbmin

    :Description:
        Effective surface albedo (middle of the day value) for wintertime (not including snow).
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `AlbedoMin`


.. option:: snowdens

    :Description:
        Initial snow density of each land cover.
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `SnowDensBldgs`, `SnowDensPaved`, `SnowDensDecTr`, `SnowDensEveTr`, `SnowDensGrass`, `SnowDensBSoil`, `SnowDensWater`


.. option:: snowdensmax

    :Description:
        Maximum snow density [kg |m^-3|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `SnowDensMax`


.. option:: snowdensmin

    :Description:
        Fresh snow density [kg |m^-3|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `SnowDensMin`


.. option:: snowfrac

    :Description:
        Initial plan area fraction of snow on each land cover`
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `SnowFracBldgs`, `SnowFracPaved`, `SnowFracDecTr`, `SnowFracEveTr`, `SnowFracGrass`, `SnowFracBSoil`, `SnowFracWater`


.. option:: snowlimbldg

    :Description:
        Limit of the snow water equivalent for snow removal from roads and roofs [mm]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `SnowLimRemove`


.. option:: snowlimpaved

    :Description:
        Limit of the snow water equivalent for snow removal from roads and roofs [mm]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `SnowLimRemove`


.. option:: snowpack

    :Description:
        Initial snow water equivalent on each land cover
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `SnowPackBldgs`, `SnowPackPaved`, `SnowPackDecTr`, `SnowPackEveTr`, `SnowPackGrass`, `SnowPackBSoil`, `SnowPackWater`


.. option:: snowpacklimit

    :Description:
        Limit for the snow water equivalent when snow cover starts to be patchy [mm]
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `SnowLimPatch`


.. option:: snowprof_24hr

    :Description:
        Hourly profile values used in snow clearing.
    :Dimensionality:
        (24, 2)
    :Dimensionality Remarks:
        24: hours of a day

        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `SnowClearingProfWD`, `SnowClearingProfWE`


.. option:: snowuse

    :Description:
        Determines whether the snow part of the model runs.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `SnowUse`


.. option:: snowwater

    :Description:
        Initial amount of liquid water in the snow on each land cover
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `SnowWaterBldgsState`, `SnowWaterPavedState`, `SnowWaterDecTrState`, `SnowWaterEveTrState`, `SnowWaterGrassState`, `SnowWaterBSoilState`, `SnowWaterWaterState`


.. option:: soildepth

    :Description:
        Depth of soil beneath the surface [mm]
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `SoilDepth`


.. option:: soilstore_surf

    :Description:
        Initial water stored in soil beneath `Bldgs` surface [mm];;Initial water stored in soil beneath `Paved` surface [mm];;Initial water stored in soil beneath `DecTr` surface [mm];;Initial water stored in soil beneath `EveTr` surface [mm];;Initial water stored in soil beneath `Grass` surface [mm];;Initial water stored in soil beneath `BSoil` surface [mm]
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `SoilstoreBldgsState`, `SoilstorePavedState`, `SoilstoreDecTrState`, `SoilstoreEveTrState`, `SoilstoreGrassState`, `SoilstoreBSoilState`


.. option:: soilstorecap_surf

    :Description:
        Limit value for `SoilDepth` [mm]
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `SoilStoreCap`


.. option:: stabilitymethod

    :Description:
        Defines which atmospheric stability functions are used.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `StabilityMethod`


.. option:: startdls

    :Description:
        Start of the day light savings [DOY]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `StartDLS`


.. option:: state_surf

    :Description:
        Initial wetness condition on `Bldgs`;;Initial wetness condition on `Paved`;;Initial wetness condition on `DecTr`;;Initial wetness condition on `EveTr`;;Initial wetness condition on `Grass`;;Initial wetness condition on `BSoil`;;Initial wetness condition on `Water`
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `BldgsState`, `PavedState`, `DecTrState`, `EveTrState`, `GrassState`, `BSoilState`, `WaterState`


.. option:: statelimit_surf

    :Description:
        Upper limit to the surface state. [mm]
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `StateLimit`


.. option:: storageheatmethod

    :Description:
        Determines method for calculating storage heat flux ΔQS.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `StorageHeatMethod`


.. option:: storedrainprm

    :Description:
        Coefficients used in drainage calculation.
    :Dimensionality:
        (6, 7)
    :Dimensionality Remarks:
        6: { `StorageMin`, `DrainageEq`, `DrainageCoef1`, `DrainageCoef2`, `StorageMax`, current storage}

        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `DrainageCoef1`, `DrainageCoef2`, `DrainageEq`, `StorageMax`, `StorageMin`


.. option:: surfacearea

    :Description:
        Area of the grid [|m^2|].
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `SurfaceArea` - note the unit in SUEWS SiteSelect.txt is ha


.. option:: sw_dn_direct_frac

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: tau_a

    :Description:
        Time constant for snow albedo aging in cold snow [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `tau_a`


.. option:: tau_f

    :Description:
        Time constant for snow albedo aging in melting snow [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `tau_f`


.. option:: tau_r

    :Description:
        Time constant for snow density ageing [-]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `tau_r`


.. option:: tempmeltfact

    :Description:
        Hourly temperature melt factor of snow [mm |K^-1| |h^-1|]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `TempMeltFactor`


.. option:: th

    :Description:
        Upper air temperature limit [°C]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `TH`


.. option:: theta_bioco2

    :Description:
        The convexity of the curve at light saturation.
    :Dimensionality:
        (3,)
    :Dimensionality Remarks:
        3: { `EveTr`, `DecTr`, `Grass`}
    :SUEWS-related variables:
        `theta`


.. option:: timezone

    :Description:
        Time zone [h] for site relative to UTC (east is positive). This should be set according to the times given in the meteorological forcing file(s).
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `Timezone`


.. option:: tl

    :Description:
        Lower air temperature limit [°C]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `TL`


.. option:: trafficrate

    :Description:
        Traffic rate used for CO2 flux calculation.
    :Dimensionality:
        (2,)
    :Dimensionality Remarks:
        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `TrafficRate_WD`, `TrafficRate_WE`


.. option:: trafficunits

    :Description:
        Units for the traffic rate for the study area. Not used in v2018a.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `TrafficUnits`


.. option:: traffprof_24hr

    :Description:
        Hourly profile values used in traffic activity calculation.
    :Dimensionality:
        (24, 2)
    :Dimensionality Remarks:
        24: hours of a day

        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `TraffProfWD`, `TraffProfWE`


.. option:: tstep

    :Description:
        Specifies the model time step [s].
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `Tstep`


.. option:: use_sw_direct_albedo

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: veg_contact_fraction_const

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: veg_fsd_const

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: veg_ssa_lw

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: veg_ssa_sw

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: veg_type

    :Description:
        Internal use. Please DO NOT modify
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        None


.. option:: waterdist

    :Description:
        Fraction of water redistribution
    :Dimensionality:
        (8, 6)
    :Dimensionality Remarks:
        8: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`, one extra land cover type (currently NOT used)}

        6: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`}
    :SUEWS-related variables:
        `ToBSoil`, `ToBldgs`, `ToDecTr`, `ToEveTr`, `ToGrass`, `ToPaved`, `ToRunoff`, `ToSoilStore`, `ToWater`


.. option:: waterusemethod

    :Description:
        Defines how external water use is calculated.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `WaterUseMethod`


.. option:: wetthresh_surf

    :Description:
        Depth of water which determines whether evaporation occurs from a partially wet or completely wet surface [mm].
    :Dimensionality:
        (7,)
    :Dimensionality Remarks:
        7: { `Paved`, `Bldgs`, `EveTr`, `DecTr`, `Grass`, `BSoil`, `Water`}
    :SUEWS-related variables:
        `WetThreshold`


.. option:: wuprofa_24hr

    :Description:
        Hourly profile values used in automatic irrigation.
    :Dimensionality:
        (24, 2)
    :Dimensionality Remarks:
        24: hours of a day

        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `WaterUseProfAutoWD`, `WaterUseProfAutoWE`


.. option:: wuprofm_24hr

    :Description:
        Hourly profile values used in manual irrigation.
    :Dimensionality:
        (24, 2)
    :Dimensionality Remarks:
        24: hours of a day

        2: {Weekday, Weekend}
    :SUEWS-related variables:
        `WaterUseProfManuWD`, `WaterUseProfManuWE`


.. option:: z

    :Description:
        Measurement height [m] for all atmospheric forcing variables set in `SSss_YYYY_data_tt.txt`.
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `z`


.. option:: z0m_in

    :Description:
        Roughness length for momentum [m]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `z0`


.. option:: zdm_in

    :Description:
        Zero-plane displacement [m]
    :Dimensionality:
        0
    :Dimensionality Remarks:
        Scalar
    :SUEWS-related variables:
        `zd`

