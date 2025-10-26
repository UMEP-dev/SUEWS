! Main module following naming standard: matches filename
MODULE module_phys_anthro
   USE get_prof_module, ONLY: get_Prof_SpecTime_inst, get_Prof_SpecTime_mean

   IMPLICIT NONE

   !===================================================================================
   !Simple Anthropogenic Heat and Carbon Dioxide Parameterization routines
   !This subroutine is still under development and in the equations concerning CO2 fluxes
   !there are bugs and missing comments.
   !Last modified
   ! TS 01 Jul 2018 - replaced annual HDD array with a simplied daily array HDD_id_use
   ! MH 29 Jun 2017 -  Finalised the code to calculate the anthropogenic emissions of heat and CO2
   ! HCW 21 Apr 2017 - renamed from SUEWS_SAHP.f95. Now includes CO2 fluxes as well as QF.
   !                   Tidied code. Combined three subroutines into 1 to avoid repetition.
   ! HCW 24 Feb 2017 - Added new anthropogenic heat flux calculation (AnthropEmissionsMethod=3)
   ! HCW 25 Aug 2016 - Outputs base QF (part without temp. dependence)
   ! LJ 27 Jan 2016  - Removal of Tabs
   ! HCW 20 Jan 2015 - v2015 applies a profile at each model timestep
   !                   these have been interpolated from the hourly profile input data (SUEWS_Profiles)
   !                   vthey are now normalised (sum to 1) in InitializeSurfaceCharacteristics
   !                   N.B. previous versions were not applying weekday/weekend profiles correctly
   !
   ! EmissionsMethod = 0 - Use values in met forcing file, or default QF
   ! EmissionsMethod = 1 - Method according to Loridan et al. (2011) : SAHP
   ! EmissionsMethod = 2 - Method according to Jarvi et al. (2011)   : SAHP_2
   !
   !===================================================================================

CONTAINS

   SUBROUTINE AnthropogenicEmissions( &
      CO2PointSource, EmissionsMethod, &
      it, imin, DLS, DayofWeek_id, &
      EF_umolCO2perJ, FcEF_v_kgkm, EnEF_v_Jkm, TrafficUnits, &
      FrFossilFuel_Heat, FrFossilFuel_NonHeat, &
      MinFCMetab, MaxFCMetab, MinQFMetab, MaxQFMetab, &
      PopDensDaytime, PopDensNighttime, &
      Tair, HDD_id, Qf_A, Qf_B, Qf_C, &
      AH_MIN, AH_SLOPE_Heating, AH_SLOPE_Cooling, &
      BaseT_Heating, BaseT_Cooling, &
      TrafficRate, &
      QF0_BEU, QF_SAHP, &
      Fc_anthro, Fc_metab, Fc_traff, Fc_build, Fc_point, &
      AHProf_24hr, HumActivity_24hr, TraffProf_24hr, PopProf_24hr, SurfaceArea)

      ! Simple anthropogenic heat parameterisation and co2 calculation
      ! Calculates QF_SAHP and Fc_anthro

      IMPLICIT NONE

      INTEGER, INTENT(in) :: &
         EmissionsMethod, &
         it, & !Hour
         imin, & !Minutes
         DLS !day lightsavings =1+1h=0

      INTEGER, DIMENSION(3), INTENT(in) :: DayofWeek_id !1 - day of week; 2 - month; 3 - season

      REAL(KIND(1D0)), DIMENSION(12), INTENT(in) :: HDD_id !Heating Degree Days (see SUEWS_DailyState.f95)

      REAL(KIND(1D0)), DIMENSION(2), INTENT(in) :: &
         Qf_A, Qf_B, Qf_C, & !Qf coefficients
         AH_MIN, & !Minimum anthropogenic heat flux
         AH_SLOPE_Heating, & !Slope of the anthropogenic heat flux calculation
         AH_SLOPE_Cooling, &
         FcEF_v_kgkm, & !CO2 Emission factor
         PopDensDaytime, & !Daytime population density [ha-1] (i.e. workers)
         BaseT_Heating, & !Critical temperature
         BaseT_Cooling, & !Critical cooling temperature
         TrafficRate, & !Traffic rate
         QF0_BEU

      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: AHProf_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: HumActivity_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: TraffProf_24hr
      REAL(KIND(1D0)), DIMENSION(0:23, 2), INTENT(in) :: PopProf_24hr

      REAL(KIND(1D0)), INTENT(in) :: &
         CO2PointSource, & !Point source [kgC day-1]
         EF_umolCO2perJ, &
         EnEF_v_Jkm, & !Energy Emission factor
         TrafficUnits, & !Traffic Units choice
         FrFossilFuel_Heat, & !Fraction of Fossil Fuel heat
         FrFossilFuel_NonHeat, & !Fraction of Fossil Fuel non heat
         MinFCMetab, & !Minimum FC Metabolism
         MaxFCMetab, & !Maximum FC Metabolism
         MinQFMetab, & !Minimum QF Metabolism
         MaxQFMetab, & !Maximum QF Metabolism
         PopDensNighttime, & !Nighttime population density [ha-1] (i.e. residents)
         SurfaceArea, & !Surface area [m-2]
         Tair !Ambient air temperature [degC], which can be from either the forcing or diagnostics (i.e. T2)

      REAL(KIND(1D0)), INTENT(out) :: &
         QF_SAHP, &
         Fc_anthro, &
         Fc_metab, Fc_traff, Fc_build, Fc_point

      INTEGER :: &
         iu, & !1=weekday OR 2=weekend
         ih

      REAL(KIND(1D0)) :: &
         ! get_Prof_SpecTime_inst, &
         ! get_Prof_SpecTime_mean, & !external function to get profile value at specified timestamp
         HDD_daily, & !daily HDD
         CDD_daily, & !daily HDD
         Tair_avg_daily, & !daily mean air temperature
         NumCapita(2), &
         DP_x_RhoPop, DP_x_RhoPop_traff, &
         QF_build, QF_metab, QF_traff, &
         QF_SAHP_base, & !Anthropogenic heat flux calculated by SAHP (temp independent part)
         QF_SAHP_heating, & !Anthropogenic heat flux calculated by SAHP (heating part only)
         QF_SAHP_cooling, & !AC contribution
         PopDorNorT, & ! Population
         ActDorNorT, & ! Human activity
         TraffDorNorT, & ! Traffic
         AHDorNorT ! Anthropogenic heat

      ! initialisation
      QF_traff = 0
      QF_SAHP_heating = 0
      QF_SAHP_cooling = 0

      ! Transfer HDD/CDD values to local explict variables
      HDD_daily = HDD_id(7)
      CDD_daily = HDD_id(8)
      ! NB: temporarily use 5-day running mean to test the performance
      Tair_avg_daily = HDD_id(10)

      ! Tair_avg_daily= HDD_id_use(3) ! this is daily

      ! use average of both daytime and nighttime population values
      ! TS 20 Sep 2019: moved from  here to simplify the interface
      IF (PopDensDaytime(1) >= 0 .AND. PopDensNighttime >= 0) NumCapita(1) = (PopDensDaytime(1) + PopDensNighttime)/2 !If both, use average
      IF (PopDensDaytime(2) >= 0 .AND. PopDensNighttime >= 0) NumCapita(2) = (PopDensDaytime(2) + PopDensNighttime)/2 !If both, use average

      !NumCapita = (PopDensDaytime + PopDensNighttime)/2

      !-----------------------------------------------------------------------
      ! Account for Daylight saving
      ih = it - DLS
      IF (ih < 0) ih = 23

      ! Set weekday/weekend counter
      iu = 1 !Set to 1=weekday
      IF (DayofWeek_id(1) == 1 .OR. DayofWeek_id(1) == 7) iu = 2 !Set to 2=weekend

      ! Calculate energy emissions and CO2 from human metabolism -------------
      ! Pop dens (cap ha-1 -> cap m-2) x activity level (W cap-1)
      ! PopDorNorT   = PopProf_tstep((NSH*(ih+1-1)+imin*NSH/60+1),iu)      ! 1=night, 2=day, 1-2=transition
      ! ActDorNorT   = HumActivity_tstep((NSH*(ih+1-1)+imin*NSH/60+1),iu)  ! 1=night, 2=day, 1-2=transition
      ! TraffDorNorT = TraffProf_tstep((NSH*(ih+1-1)+imin*NSH/60+1),iu)    ! normalise so the AVERAGE of the multipliers is equal to 1
      ! AHDorNorT    = AHProf_tstep((NSH*(ih+1-1)+imin*NSH/60+1),iu)       ! normalise so the AVERAGE of the multipliers is equal to 1
      ! PRINT*, ''
      ! PRINT*, 'AHDorNorT old:',AHDorNorT
      PopDorNorT = get_Prof_SpecTime_inst(ih, imin, 0, PopProf_24hr(:, iu)) ! 1=night, 2=day, 1-2=transition
      ActDorNorT = get_Prof_SpecTime_inst(ih, imin, 0, HumActivity_24hr(:, iu)) ! 1=night, 2=day, 1-2=transition
      TraffDorNorT = get_Prof_SpecTime_mean(ih, imin, 0, TraffProf_24hr(:, iu)) ! normalise so the AVERAGE of the multipliers is equal to 1
      AHDorNorT = get_Prof_SpecTime_mean(ih, imin, 0, AHProf_24hr(:, iu)) ! normalise so the AVERAGE of the multipliers is equal to 1

      ! Diurnal profile times population density [cap ha-1]
      DP_x_RhoPop = AHDorNorT*NumCapita(iu)

      QF_metab = (PopDensNighttime*MinQFMetab*((2 - ActDorNorT) + (2 - PopDorNorT))/2 + &
                  PopDensDaytime(iu)*MaxQFMetab*((ActDorNorT - 1) + (PopDorNorT - 1))/2)/10000 !W m-2

      Fc_metab = (PopDensNighttime*MinFCMetab*((2 - ActDorNorT) + (2 - PopDorNorT))/2 + &
                  PopDensDaytime(iu)*MaxFCMetab*((ActDorNorT - 1) + (PopDorNorT - 1))/2)/10000 !umol m-2 s-1

      !----------------------------------------------------------------------------------------------------
      !Different methods to calculate the anthopogenic heat and CO2 emissions.
      !1-3: CO2 emission is calculated from QF which can be calculated with three methods
      !41-43: CO2 emission is calculated with local information. QF methods are used for housing and human metabolism

      IF (EmissionsMethod == 1 .OR. EmissionsMethod == 4 .OR. &
          EmissionsMethod == 11 .OR. EmissionsMethod == 14 .OR. &
          EmissionsMethod == 21 .OR. EmissionsMethod == 24 .OR. &
          EmissionsMethod == 31 .OR. EmissionsMethod == 34 .OR. &
          EmissionsMethod == 41 .OR. EmissionsMethod == 44) THEN ! (formerly SAHP_1 subroutine)
         ! Loridan et al. (2011) JAMC Eq 13: linear relation with air temperature
         ! Weekday/weekend differences due to profile only
         ! Now scales with population density

         IF (Tair < BaseT_Heating(iu)) THEN
            ! QF_SAHP = (AH_MIN(iu) + AH_SLOPE_Heating(iu)*(BaseT_Heating(iu) - Temp_C))*AHDorNorT
            QF_SAHP_heating = (AH_SLOPE_Heating(iu)*(BaseT_Heating(iu) - Tair))*AHDorNorT
         ELSE
            QF_SAHP_heating = 0
            !    QF_SAHP = AH_MIN(iu)*AHDorNorT
         END IF

         ! Need to be checked later, not recommended to use
         QF_SAHP_base = AH_MIN(iu)*AHDorNorT ! Temperature-independent contribution
         QF_SAHP_cooling = 0 ! No AC contribution with this method

      ELSEIF (EmissionsMethod == 2 .OR. EmissionsMethod == 5 .OR. &
              EmissionsMethod == 12 .OR. EmissionsMethod == 15 .OR. &
              EmissionsMethod == 22 .OR. EmissionsMethod == 25 .OR. &
              EmissionsMethod == 32 .OR. EmissionsMethod == 35 .OR. &
              EmissionsMethod == 42 .OR. EmissionsMethod == 45) THEN ! (formerly SAHP_2 subroutine)
         ! Jarvi et al. (2011) JH Eq 3 using HDD and CDD
         ! Weekday/weekend differences due to profile and coefficients QF_a,b,c
         ! Scales with population density
         ! QF_SAHP = (Qf_a(iu) + Qf_b(iu)*CDD_daily + Qf_c(iu)*HDD_daily)*DP_x_RhoPop  !This contains QF from all three sources: buildings, metabolism and traffic!
         QF_SAHP_base = (Qf_a(iu))*DP_x_RhoPop ! Temperature-independent contribution from buildings, traffic and human metabolism
         QF_SAHP_heating = (Qf_c(iu)*HDD_daily)*DP_x_RhoPop ! Heating contribution
         QF_SAHP_cooling = (Qf_b(iu)*CDD_daily)*DP_x_RhoPop ! Cooling (AC) contribution

      ELSEIF (EmissionsMethod == 3 .OR. EmissionsMethod == 6 .OR. &
              EmissionsMethod == 13 .OR. EmissionsMethod == 16 .OR. &
              EmissionsMethod == 23 .OR. EmissionsMethod == 26 .OR. &
              EmissionsMethod == 33 .OR. EmissionsMethod == 36 .OR. &
              EmissionsMethod == 43 .OR. EmissionsMethod == 46) THEN
         ! Updated Loridan et al. (2011) method using daily (not instantaneous) air temperature (HDD_id_use(3))
         ! Linear relation with air temperature
         ! Weekday/weekend differences due to profile only
         ! Scales with population density

         ! Need to be checked later, not recommended to use
         QF_SAHP_base = AH_MIN(iu)*AHDorNorT ! Temperature-independent contribution

         IF (Tair_avg_daily < BaseT_Heating(iu)) THEN ! Heating
            QF_SAHP_heating = (AH_SLOPE_Heating(iu)*(BaseT_Heating(iu) - Tair_avg_daily))*AHDorNorT ! Heating contribution
            QF_SAHP_cooling = 0

         ELSEIF (Tair_avg_daily > BaseT_Cooling(iu)) THEN ! Air-conditioning
            QF_SAHP_heating = 0
            QF_SAHP_cooling = (AH_SLOPE_Cooling(iu)*(Tair_avg_daily - BaseT_Cooling(iu)))*AHDorNorT ! AC contribution

         ELSE
            QF_SAHP_heating = 0
            QF_SAHP_cooling = 0
         END IF

      END IF

      ! total QF as the sum of components
      QF_SAHP = QF_SAHP_base + QF_SAHP_heating + QF_SAHP_cooling

      IF (EmissionsMethod >= 1 .AND. EmissionsMethod <= 3 .OR. &
          EmissionsMethod >= 11 .AND. EmissionsMethod <= 13 .OR. &
          EmissionsMethod >= 21 .AND. EmissionsMethod <= 23 .OR. &
          EmissionsMethod >= 31 .AND. EmissionsMethod <= 33 .OR. &
          EmissionsMethod >= 41 .AND. EmissionsMethod <= 43) THEN

         ! Calculate QF from buildings.
         ! First remove (if possibe) human metabolism from the total value given by SAHP.
         IF ((QF_SAHP_base - QF_metab) > 0) THEN
            QF_build = QF_SAHP_base*QF0_BEU(iu) + QF_SAHP_heating + QF_SAHP_cooling !QF0_BEU = QF0_BuildingEnergyUse = Fraction of base value coming from buildings
            !relative to traffic as metabolism is separately calculated
         ELSE
            CALL ErrorHint(69, 'QF metab exceeds base QF.', QF_metab, QF_SAHP_base)

            QF_build = QF_SAHP_heating + QF_SAHP_cooling + (QF_SAHP_base - QF_metab) !If human metabolism greater than Base QF, remove this from the heating/cooling contribution also
         END IF

         ! Consider the various components of QF_build to calculate Fc_build
         ! Assume all A/C electric, so QF_SAHP_ac is not associated with any local CO2 emissions
         ! HDD part is building energy use, split between electricity (no local emissions CO2) and combustion (CO2) heating...
         Fc_build = QF_SAHP_heating*FrFossilFuel_Heat*EF_umolCO2perJ

         ! ... and there is also a temperature-independent contribution from building energy use.
         IF ((QF_SAHP_base - QF_metab) > 0) THEN
            Fc_build = Fc_build + QF_SAHP_base*QF0_BEU(iu)*FrFossilFuel_NonHeat*EF_umolCO2perJ
         END IF

         ! Remainder of temperature-independent part must be traffic
         !QF_traff = (QF_SAHP_base-QF_metab)*(1.0-QF0_BEU(iu))
         QF_traff = QF_SAHP_base*(1.0 - QF0_BEU(iu)) - QF_metab

         ! Divide QF by energy emission factor and multiply by CO2 factor
         Fc_traff = QF_traff/EnEF_v_Jkm*FcEF_v_kgkm(iu)*1E3*1E6/44

         !CO2 point source [kg C day-1] * 1e3 g kg-1 * 1e6 umol mol-1 /(12 g mol-1 * m-2)
         IF (CO2PointSource > 0) THEN
            Fc_point = CO2PointSource*1E3*1E6/(12*60*60*24*SurfaceArea)
         ELSE
            Fc_point = 0
         END IF

         ! Sum components to give anthropogenic CO2 flux [umol m-2 s-1]
         Fc_anthro = Fc_metab + Fc_traff + Fc_build + Fc_point

      ELSEIF (EmissionsMethod >= 4 .AND. EmissionsMethod <= 6 .OR. &
              EmissionsMethod >= 14 .AND. EmissionsMethod <= 16 .OR. &
              EmissionsMethod >= 24 .AND. EmissionsMethod <= 26 .OR. &
              EmissionsMethod >= 34 .AND. EmissionsMethod <= 36 .OR. &
              EmissionsMethod >= 44 .AND. EmissionsMethod <= 46) THEN
         ! Calculate QF and Fc using building energy use and transport statistics

         ! Calculate energy released from traffic
         IF (TrafficUnits == 1) THEN ! [veh km m-2 day-1]
            ! Calculate using mean traffic rate [veh km m-2 day-1] * emission factor [J km-1]
            QF_traff = TrafficRate(iu)/(60*60*24)*EnEF_v_Jkm*TraffDorNorT
            ! Use mean traffic rate [veh km m-2 s-1] * emission factor [kg km-1] * 1e3 g kg-1 /44 g mol-1 * 1e6 umol mol-1
            Fc_traff = TrafficRate(iu)/(60*60*24)*FcEF_v_kgkm(iu)*1E3*1E6/44*TraffDorNorT

         ELSEIF (TrafficUnits == 2) THEN ! [veh km cap-1 day-1]
            DP_x_RhoPop_traff = TraffDorNorT*NumCapita(iu)/10000 ! [cap m-2]
            ! Use mean traffic rate [veh km cap-1 day-1] * emission factor [J km-1]
            QF_traff = TrafficRate(iu)/(60*60*24)*EnEF_v_Jkm*DP_x_RhoPop_traff
            ! Use mean traffic rate [veh km cap-1 day-1] * emission factor [kg km-1] * 1e3 g kg-1 /44 g mol-1 * 1e6 umol mol-1
            Fc_traff = TrafficRate(iu)/(60*60*24)*FcEF_v_kgkm(iu)*1E3*1E6/44*DP_x_RhoPop_traff

         ELSE ! If TrafficUnits doesn't match possible units
            CALL ErrorHint(75, 'Check TrafficUnits', TrafficUnits, -999D1)

         END IF

         ! Energy released from buildings only
         ! Should buildings have their own profile? Now using population profile
         ! Check for zero population density to avoid division by zero
         IF (DP_x_RhoPop > 0.0D0) THEN
            QF_build = ((QF_SAHP_base*QF0_BEU(iu) + QF_SAHP_heating + QF_SAHP_cooling)/DP_x_RhoPop)* &
                       (PopDensNighttime*(2 - PopDorNorT) + PopDensDaytime(iu)*(PopDorNorT - 1))
         ELSE
            ! If population density is zero, building energy flux should also be zero
            QF_build = 0.0D0
         END IF

         ! Consider the various components of QF_build to calculate Fc_build
         Fc_build = QF_SAHP_heating*FrFossilFuel_Heat*EF_umolCO2perJ
         ! ... and there is also a temperature-independent contribution from building energy use
         Fc_build = Fc_build + QF_SAHP_base*QF0_BEU(iu)*FrFossilFuel_NonHeat*EF_umolCO2perJ

         !CO2 point source [kg C day-1] * 1e3 g kg-1 * 1e6 umol mol-1 /(12 g mol-1 * m-2)
         IF (CO2PointSource > 0) THEN
            Fc_point = CO2PointSource*1E3*1E6/(12*60*60*24*SurfaceArea)
         ELSE
            Fc_point = 0
         END IF

         ! Add other QF components to QF_SAHP_base (because if AnthropEmissionsMethod = 4, QF calculated above is building part only)
         QF_SAHP_base = QF_SAHP_base + QF_traff + QF_metab

         ! Sum components to give anthropogenic heat flux [W m-2]
         QF_SAHP = QF_metab + QF_traff + QF_build

         ! Sum components to give anthropogenic CO2 flux [umol m-2 s-1]
         Fc_anthro = Fc_metab + Fc_traff + Fc_build + Fc_point

      END IF

      RETURN
   END SUBROUTINE AnthropogenicEmissions

END MODULE module_phys_anthro

! Backward compatibility alias
MODULE AnEmsn_module
   USE module_phys_anthro
END MODULE AnEmsn_module
!========================================================================================
