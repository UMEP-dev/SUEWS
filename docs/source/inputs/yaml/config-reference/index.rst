.. _yaml_config_reference:

YAML Configuration Reference
============================

This documentation follows the hierarchical structure of SUEWS YAML configuration files.

* :doc:`SUEWS Config <suewsconfig>`

  - :option:`name`

  - :option:`schema_version`

  - :option:`description`

  - :doc:`Model Configuration <model>`

    * :doc:`Model Control <modelcontrol>`

      - :option:`tstep`

      - :option:`forcing_file`

      - :option:`start_time`

      - :option:`end_time`

      - :doc:`Output Configuration <outputconfig>`

        - :option:`format`

        - :option:`freq`

        - :option:`groups`

    * :doc:`Physics Methods <modelphysics>`

      .. hlist::
         :columns: 3

         * :option:`netradiationmethod`
         * :option:`emissionsmethod`
         * :option:`storageheatmethod`
         * :option:`ohmincqf`
         * :option:`roughlenmommethod`
         * :option:`roughlenheatmethod`
         * :option:`stabilitymethod`
         * :option:`smdmethod`
         * :option:`waterusemethod`
         * :option:`rslmethod`
         * :option:`faimethod`
         * :option:`rsllevel`
         * :option:`gsmodel`
         * :option:`snowuse`
         * :option:`stebbsmethod`

  - :doc:`Site Configuration <site>`

    * :option:`name`

    * :option:`gridiv`

    * :doc:`Site Properties <siteproperties>`

      .. hlist::
         :columns: 3

         * :option:`lat`
         * :option:`lng`
         * :option:`alt`
         * :option:`timezone`
         * :option:`surfacearea`
         * :option:`z`
         * :option:`z0m_in`
         * :option:`zdm_in`
         * :option:`pipecapacity`
         * :option:`runofftowater`
         * :option:`narp_trans_site`
         * :option:`n_buildings`
         * :option:`h_std`
         * :option:`lambda_c`

      - :doc:`LUMPS Parameters <lumpsparams>`

        - :option:`raincover`

        - :option:`rainmaxres`

        - :option:`drainrt`

        - :option:`veg_type`

      - :doc:`SPARTACUS <spartacusparams>`

        .. hlist::
           :columns: 3

           * :option:`air_ext_lw`
           * :option:`air_ext_sw`
           * :option:`air_ssa_lw`
           * :option:`air_ssa_sw`
           * :option:`ground_albedo_dir_mult_fact`
           * :option:`n_stream_lw_urban`
           * :option:`n_stream_sw_urban`
           * :option:`n_vegetation_region_urban`
           * :option:`sw_dn_direct_frac`
           * :option:`use_sw_direct_albedo`
           * :option:`veg_contact_fraction_const`
           * :option:`veg_fsd_const`
           * :option:`veg_ssa_lw`
           * :option:`veg_ssa_sw`

      - :doc:`STEBBS <stebbsproperties>`

        .. collapse:: 64 parameters
           :class: collapse-sm

           - :option:`WallInternalConvectionCoefficient`
           - :option:`InternalMassConvectionCoefficient`
           - :option:`FloorInternalConvectionCoefficient`
           - :option:`WindowInternalConvectionCoefficient`
           - :option:`WallExternalConvectionCoefficient`
           - :option:`WindowExternalConvectionCoefficient`
           - :option:`GroundDepth`
           - :option:`ExternalGroundConductivity`
           - :option:`IndoorAirDensity`
           - :option:`IndoorAirCp`
           - :option:`WallBuildingViewFactor`
           - :option:`WallGroundViewFactor`
           - :option:`WallSkyViewFactor`
           - :option:`MetabolicRate`
           - :option:`LatentSensibleRatio`
           - :option:`ApplianceRating`
           - :option:`TotalNumberofAppliances`
           - :option:`ApplianceUsageFactor`
           - :option:`HeatingSystemEfficiency`
           - :option:`MaxCoolingPower`
           - :option:`CoolingSystemCOP`
           - :option:`VentilationRate`
           - :option:`IndoorAirStartTemperature`
           - :option:`IndoorMassStartTemperature`
           - :option:`WallIndoorSurfaceTemperature`
           - :option:`WallOutdoorSurfaceTemperature`
           - :option:`WindowIndoorSurfaceTemperature`
           - :option:`WindowOutdoorSurfaceTemperature`
           - :option:`GroundFloorIndoorSurfaceTemperature`
           - :option:`GroundFloorOutdoorSurfaceTemperature`
           - :option:`WaterTankTemperature`
           - :option:`InternalWallWaterTankTemperature`
           - :option:`ExternalWallWaterTankTemperature`
           - :option:`WaterTankWallThickness`
           - :option:`MainsWaterTemperature`
           - :option:`WaterTankSurfaceArea`
           - :option:`HotWaterHeatingSetpointTemperature`
           - :option:`HotWaterTankWallEmissivity`
           - :option:`DomesticHotWaterTemperatureInUseInBuilding`
           - :option:`InternalWallDHWVesselTemperature`
           - :option:`ExternalWallDHWVesselTemperature`
           - :option:`DHWVesselWallThickness`
           - :option:`DHWWaterVolume`
           - :option:`DHWSurfaceArea`
           - :option:`DHWVesselEmissivity`
           - :option:`HotWaterFlowRate`
           - :option:`DHWDrainFlowRate`
           - :option:`DHWSpecificHeatCapacity`
           - :option:`HotWaterTankSpecificHeatCapacity`
           - :option:`DHWVesselSpecificHeatCapacity`
           - :option:`DHWDensity`
           - :option:`HotWaterTankWallDensity`
           - :option:`DHWVesselDensity`
           - :option:`HotWaterTankBuildingWallViewFactor`
           - :option:`HotWaterTankInternalMassViewFactor`
           - :option:`HotWaterTankWallConductivity`
           - :option:`HotWaterTankInternalWallConvectionCoefficient`
           - :option:`HotWaterTankExternalWallConvectionCoefficient`
           - :option:`DHWVesselWallConductivity`
           - :option:`DHWVesselInternalWallConvectionCoefficient`
           - :option:`DHWVesselExternalWallConvectionCoefficient`
           - :option:`DHWVesselWallEmissivity`
           - :option:`HotWaterHeatingEfficiency`
           - :option:`MinimumVolumeOfDHWinUse`

      - :doc:`Archetype Properties <archetypeproperties>`

        .. collapse:: 40 parameters
           :class: collapse-sm

           - :option:`BuildingType`
           - :option:`BuildingName`
           - :option:`BuildingCount`
           - :option:`Occupants`
           - :option:`stebbs_Height`
           - :option:`FootprintArea`
           - :option:`WallExternalArea`
           - :option:`RatioInternalVolume`
           - :option:`WWR`
           - :option:`WallThickness`
           - :option:`WallEffectiveConductivity`
           - :option:`WallDensity`
           - :option:`WallCp`
           - :option:`Wallx1`
           - :option:`WallExternalEmissivity`
           - :option:`WallInternalEmissivity`
           - :option:`WallTransmissivity`
           - :option:`WallAbsorbtivity`
           - :option:`WallReflectivity`
           - :option:`FloorThickness`
           - :option:`GroundFloorEffectiveConductivity`
           - :option:`GroundFloorDensity`
           - :option:`GroundFloorCp`
           - :option:`WindowThickness`
           - :option:`WindowEffectiveConductivity`
           - :option:`WindowDensity`
           - :option:`WindowCp`
           - :option:`WindowExternalEmissivity`
           - :option:`WindowInternalEmissivity`
           - :option:`WindowTransmissivity`
           - :option:`WindowAbsorbtivity`
           - :option:`WindowReflectivity`
           - :option:`InternalMassDensity`
           - :option:`InternalMassCp`
           - :option:`InternalMassEmissivity`
           - :option:`MaxHeatingPower`
           - :option:`WaterTankWaterVolume`
           - :option:`MaximumHotWaterHeatingPower`
           - :option:`HeatingSetpointTemperature`
           - :option:`CoolingSetpointTemperature`

      - :doc:`Conductance <conductance>`

        .. hlist::
           :columns: 3

           * :option:`g_max`
           * :option:`g_k`
           * :option:`g_q_base`
           * :option:`g_q_shape`
           * :option:`g_t`
           * :option:`g_sm`
           * :option:`kmax`
           * :option:`s1`
           * :option:`s2`
           * :option:`tl`
           * :option:`th`

      - :doc:`Irrigation <irrigationparams>`

        .. hlist::
           :columns: 3

           * :option:`h_maintain`
           * :option:`faut`
           * :option:`ie_start`
           * :option:`ie_end`
           * :option:`internalwateruse_h`
           * :option:`daywatper`
           * :option:`daywat`
           * :option:`wuprofa_24hr`
           * :option:`wuprofm_24hr`

      - :doc:`Anthropogenic Emissions <anthropogenicemissions>`

        - :option:`startdls`

        - :option:`enddls`

        - :doc:`Anthropogenic Heat <anthropogenicheat>`

        - :doc:`CO2 Emissions <co2params>`

      - :doc:`Snow <snowparams>`

        .. collapse:: 17 parameters
           :class: collapse-sm

           - :option:`crwmax`
           - :option:`crwmin`
           - :option:`narp_emis_snow`
           - :option:`preciplimit`
           - :option:`preciplimitalb`
           - :option:`snowalbmax`
           - :option:`snowalbmin`
           - :option:`snowdensmin`
           - :option:`snowdensmax`
           - :option:`snowlimbldg`
           - :option:`snowlimpaved`
           - :option:`snowprof_24hr`
           - :option:`tau_a`
           - :option:`tau_f`
           - :option:`tau_r`
           - :option:`tempmeltfact`
           - :option:`radmeltfact`

      - :doc:`Land Cover <landcover>`

        - :doc:`Paved Surfaces <pavedproperties>`

        - :doc:`Buildings <bldgsproperties>`

        - :doc:`Evergreen Trees <evetrproperties>`

        - :doc:`Deciduous Trees <dectrproperties>`

        - :doc:`Grass <grassproperties>`

        - :doc:`Bare Soil <bsoilproperties>`

        - :doc:`Water Surfaces <waterproperties>`

      - :doc:`Vertical Layers <verticallayers>`

        - :option:`nlayer`

        - :option:`height`

        - :option:`veg_frac`

        - :option:`veg_scale`

        - :option:`building_frac`

        - :option:`building_scale`

        - :doc:`Roof Layer <rooflayer>`

        - :doc:`Wall Layer <walllayer>`

    * :doc:`Initial States <initialstates>`

      - :option:`snowalb`

      - :doc:`Paved Surface Initial State <initialstatepaved>`

        .. hlist::
           :columns: 3

           * :option:`state`
           * :option:`soilstore`
           * :option:`snowfrac`
           * :option:`snowpack`
           * :option:`icefrac`
           * :option:`snowwater`
           * :option:`snowdens`
           * :option:`temperature`
           * :option:`tsfc`
           * :option:`tin`

      - :doc:`Buildings Surface Initial State <initialstatebldgs>`

        .. hlist::
           :columns: 3

           * :option:`state`
           * :option:`soilstore`
           * :option:`snowfrac`
           * :option:`snowpack`
           * :option:`icefrac`
           * :option:`snowwater`
           * :option:`snowdens`
           * :option:`temperature`
           * :option:`tsfc`
           * :option:`tin`

      - :doc:`Evergreen Tree Initial State <initialstateevetr>`

        .. hlist::
           :columns: 3

           * :option:`state`
           * :option:`soilstore`
           * :option:`snowfrac`
           * :option:`snowpack`
           * :option:`icefrac`
           * :option:`snowwater`
           * :option:`snowdens`
           * :option:`temperature`
           * :option:`tsfc`
           * :option:`tin`
           * :option:`alb_id`
           * :option:`lai_id`
           * :option:`gdd_id`
           * :option:`sdd_id`
           * :option:`wu`

      - :doc:`Deciduous Tree Initial State <initialstatedectr>`

        .. collapse:: 17 parameters
           :class: collapse-sm

           - :option:`state`
           - :option:`soilstore`
           - :option:`snowfrac`
           - :option:`snowpack`
           - :option:`icefrac`
           - :option:`snowwater`
           - :option:`snowdens`
           - :option:`temperature`
           - :option:`tsfc`
           - :option:`tin`
           - :option:`alb_id`
           - :option:`lai_id`
           - :option:`gdd_id`
           - :option:`sdd_id`
           - :option:`wu`
           - :option:`porosity_id`
           - :option:`decidcap_id`

      - :doc:`Grass Initial State <initialstategrass>`

        .. hlist::
           :columns: 3

           * :option:`state`
           * :option:`soilstore`
           * :option:`snowfrac`
           * :option:`snowpack`
           * :option:`icefrac`
           * :option:`snowwater`
           * :option:`snowdens`
           * :option:`temperature`
           * :option:`tsfc`
           * :option:`tin`
           * :option:`alb_id`
           * :option:`lai_id`
           * :option:`gdd_id`
           * :option:`sdd_id`
           * :option:`wu`

      - :doc:`Bare Soil Surface Initial State <initialstatebsoil>`

        .. hlist::
           :columns: 3

           * :option:`state`
           * :option:`soilstore`
           * :option:`snowfrac`
           * :option:`snowpack`
           * :option:`icefrac`
           * :option:`snowwater`
           * :option:`snowdens`
           * :option:`temperature`
           * :option:`tsfc`
           * :option:`tin`

      - :doc:`Water Surface Initial State <initialstatewater>`

        .. hlist::
           :columns: 3

           * :option:`state`
           * :option:`soilstore`
           * :option:`snowfrac`
           * :option:`snowpack`
           * :option:`icefrac`
           * :option:`snowwater`
           * :option:`snowdens`
           * :option:`temperature`
           * :option:`tsfc`
           * :option:`tin`

      - :doc:`Roofs Initial State <surfaceinitialstate>`

        .. hlist::
           :columns: 3

           * :option:`state`
           * :option:`soilstore`
           * :option:`snowfrac`
           * :option:`snowpack`
           * :option:`icefrac`
           * :option:`snowwater`
           * :option:`snowdens`
           * :option:`temperature`
           * :option:`tsfc`
           * :option:`tin`

      - :doc:`Walls Initial State <surfaceinitialstate>`

        .. hlist::
           :columns: 3

           * :option:`state`
           * :option:`soilstore`
           * :option:`snowfrac`
           * :option:`snowpack`
           * :option:`icefrac`
           * :option:`snowwater`
           * :option:`snowdens`
           * :option:`temperature`
           * :option:`tsfc`
           * :option:`tin`


.. toctree::
   :hidden:
   :maxdepth: 3

   anthropogenicemissions
   anthropogenicheat
   archetypeproperties
   bldgsproperties
   bsoilproperties
   buildinglayer
   co2params
   conductance
   dayprofile
   dectrproperties
   evetrproperties
   grassproperties
   hourlyprofile
   initialstatebldgs
   initialstatebsoil
   initialstatedectr
   initialstateevetr
   initialstategrass
   initialstatepaved
   initialstateveg
   initialstatewater
   initialstates
   irrigationparams
   laiparams
   laipowercoefficients
   lumpsparams
   landcover
   model
   modelcontrol
   modelphysics
   nonvegetatedsurfaceproperties
   ohmcoefficients
   ohm_coefficient_season_wetness
   outputconfig
   pavedproperties
   rooflayer
   spartacusparams
   suewsconfig
   site
   siteproperties
   snowparams
   stebbsproperties
   storagedrainparams
   surfaceinitialstate
   surfaceproperties
   thermallayers
   vegetatedsurfaceproperties
   vegetationparams
   verticallayers
   walllayer
   waterdistribution
   waterproperties
   weeklyprofile