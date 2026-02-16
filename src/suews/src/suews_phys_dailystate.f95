! Main module following naming standard: matches filename
MODULE module_phys_dailystate
   USE module_ctrl_const_allocate, ONLY: &
      ndays, nsurf, nvegsurf, ivConif, ivDecid, ivGrass, DecidSurf, ncolumnsDataOutDailyState
   USE module_ctrl_error_state, ONLY: set_supy_error, supy_error_flag
   USE, INTRINSIC :: ieee_arithmetic, ONLY: IEEE_IS_NAN

   IMPLICIT NONE

CONTAINS

   ! Calculation of daily state variables
   ! Responds to what has happened in the past (temperature, rainfall, etc)
   ! Updates each time step, but for many variables, correct values are calculated only at the end of each day!
   ! --> for these variables, the rest of the code MUST use values from the previous day
   ! N.B. Some of this code is repeated in SUEWS_Initial
   ! --> so if changes are made here, SUEWS_Initial may also need to be updated accordingly
   ! N.B. Currently, daily variables are calculated using 00:00-23:55 timestamps (for 5-min resolution); should use 00:05-00:00
   !
   ! Last modified:
   !  TS 09 Jul 2018  - Modified HDD array to hold values for actual calculation
   !  TS 18 Sep 2017  - Added explicit interface
   !  TS 07 Jun 2017  - Improve the format of output with more friendly alignment
   !  HCW 04 Jul 2016 - GridID can now be up to 10 digits long
   !  HCW 25 May 2016 - Added extra columns to daily state file (albedo for EveTr and Grass)
   !  HCW 24 May 2016 - Bug fixed in naming of SUEWS_cal_DailyState file (now uses GridIDmatrix(Gridiv) rather than Gridiv)
   !  LJ 27 Jan 2016  - Removal of tabs
   !  HCW 20 Aug 2015 - Sign of the porosity change corrected so that porosity is greatest when LAI is smallest
   !  HCW 03 Jul 2015 - Increased output resolution of P/day in SUEWS_cal_DailyState file to avoid rounding errors.
   !                    Albedo of EveTr and Grass now adjusted based on change in LAI for EveTr and Grass
   !                    (rather than DecTr)
   !  HCW 29 Jun 2015 - Added albChange for EveTr and Grass surfaces
   !  HCW 11 Jun 2015 - Bug fix from 05 Jun now fixed in a different way -
   !                    DecidCap is now treated the same as DecidAlb so should be able to cope with multiple grids.
   !  HCW 05 Jun 2015 - Bug fix - set all current storage capacities (StoreDrainPrm(6,)) to min. value, then set for DecTr
   !  LJ 11 Mar 2015  - Removed switch as no longer necessary
   !  HCW 06 Mar 2015 - iy used instead of year which does not have a value here
   !  HCW 20 Feb 2015 - Added StoreDrainPrm(6,is) for the current storage capacity
   !  Updated and corrected SUEWS_cal_DailyState output file
   !  LJ 05 Feb 2015  - SUEWS_cal_DailyState saving fixed. Now header is printed and the file closed and opened as suggested.
   ! N.B. Bug in daily Precip - needs fixing!!! - HCW thinks this is fixed 20 Feb 2015
   !  HCW 26 Jan 2015 - sfr_surf and IrrFracs deleted from WUDay calculations, so that WUDay is not spread over
   !  the total area
   !  HCW 23 Jan 2015 - WUDay now has 9 columns (EveTr, DecTr, Grass; automatic, manual, total)
   !  HCW 27 Nov 2014 - Handles values for different grids (Gridiv & ir arguments)
   ! Added the calculation of surface temperature
   !  LJ 22 Feb 2013  - Snow albedo aging and calculation of snow density added,
   !  LJ 22 Jul 2013  - Calculation of LAI senescence from previous day length added
   ! sg feb 2012 - rewritten from LUMPS_LAI so done in real time
   !
   ! To Do
   !   - Account for change of year in 5-day running mean
   !   - Check LAI calcs (N/S hemisphere similarities; use of day length)
   !       - Take out doy limits (140,170, etc) and code as parameters
   !   - Could add different coefficients (Ie_m, Ie_a) for each vegetation type
   !==============================================================================

   SUBROUTINE SUEWS_cal_DailyState( &
      timer, config, forcing, siteInfo, &
      modState) ! input/output:
      ! atmState, & !inout
      ! phenState, & !inout
      ! anthroEmisState, & !inout
      ! hydroState) !inout

      ! USE module_phys_snow, ONLY: SnowUpdate
      USE module_util_datetime, ONLY: datetime, timedelta
      USE module_ctrl_type, ONLY: SUEWS_SITE, SUEWS_TIMER, SUEWS_FORCING, anthroEMIS_PRM, &
                               PHENOLOGY_STATE, anthroEmis_STATE, SUEWS_CONFIG, &
                               IRRIGATION_PRM, LC_PAVED_PRM, LC_BLDG_PRM, &
                               LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                               LC_BSOIL_PRM, LC_WATER_PRM, &
                               HYDRO_STATE, atm_STATE, SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(INOUT) :: modState


      INTEGER :: LAICalcYes ! 1 = calculate LAI internally (GDD), 0 = use forcing%LAI_obs [-]

      REAL(KIND(1D0)), DIMENSION(2) :: BaseT_Heating

      REAL(KIND(1D0)), DIMENSION(4, nvegsurf) :: LAIPower !Coeffs for LAI equation: 1,2 - leaf growth; 3,4 - leaf off

      TYPE(PHENOLOGY_STATE) :: phenState_prev


      LOGICAL :: first_tstep_Q ! if this is the first tstep of a day
      LOGICAL :: last_tstep_Q ! if this is the last tstep of a day
      TYPE(datetime) :: time_now, time_prev, time_next

      REAL(KIND(1D0)) :: tair ! air temperature [degC]

      ! Define a logical flag
      LOGICAL :: execute_subroutines

      ASSOCIATE ( &
         phenState => modState%phenState, &
         anthroEmisState => modState%anthroEmisState, &
         hydroState => modState%hydroState, &
         atmState => modState%atmState, &
         flagState => modState%flagState &
         )

         ! save initial values
         phenState_prev = phenState

         ! LAI calculation switch: pulled from config (model.physics.laimethod in YAML).
         ! 0 = use forcing%LAI_obs (observed/prescribed), 1 = compute internally via GDD/SDD.
         LAICalcYes = config%LAImethod

         ASSOCIATE ( &
            lat => siteInfo%lat, &
            iy => timer%iy, &
            id => timer%id, &
            it => timer%it, &
            imin => timer%imin, &
            isec => timer%isec, &
            tstep => timer%tstep, &
            tstep_prev => timer%tstep_prev, &
            dt_since_start => timer%dt_since_start, &
            nsh_real => timer%nsh_real, &
            DayofWeek_id => timer%DayofWeek_id, &
            avkdn => forcing%kdown, &
            Temp_C => forcing%Temp_C, &
            Precip => forcing%rain, &
            LAI_dectr => forcing%LAI_dectr, &
            LAI_evetr => forcing%LAI_evetr, &
            LAI_grass => forcing%LAI_grass, &
            ahemisPrm => siteInfo%anthroEmis, &
            irrPrm => siteInfo%irrigation, &
            pavedPrm => siteInfo%lc_paved, &
            bldgPrm => siteInfo%lc_bldg, &
            grassPrm => siteInfo%lc_grass, &
            dectrPrm => siteInfo%lc_dectr, &
            evetrPrm => siteInfo%lc_evetr, &
            bsoilPrm => siteInfo%lc_bsoil, &
            waterPrm => siteInfo%lc_water, &
            i_iter => flagState%i_iter &
            )

            ASSOCIATE ( &
               BaseT_Heating => [ahemisPrm%anthro_heat%BaseT_Heating_working, &
                                 ahemisPrm%anthro_heat%BaseT_Heating_holiday], &
               BaseT_Cooling => [ahemisPrm%anthro_heat%BaseT_Cooling_working, &
                                 ahemisPrm%anthro_heat%BaseT_Cooling_holiday], &
               Tmin_id_prev => phenState_prev%Tmin_id, &
               Tmax_id_prev => phenState_prev%Tmax_id, &
               lenDay_id_prev => phenState_prev%lenDay_id, &
               DecidCap_id_prev => phenState_prev%DecidCap_id, &
               StoreDrainPrm_prev => phenState_prev%storage_drain_params, &
               LAI_id_prev => phenState_prev%LAI_id, &
               GDD_id_prev => phenState_prev%GDD_id, &
               SDD_id_prev => phenState_prev%SDD_id, &
               albDecTr_id_prev => phenState_prev%albDecTr_id, &
               albEveTr_id_prev => phenState_prev%albEveTr_id, &
               albGrass_id_prev => phenState_prev%albGrass_id, &
               porosity_id_prev => phenState_prev%porosity_id, &
               Tmin_id => phenState%Tmin_id, &
               Tmax_id => phenState%Tmax_id, &
               lenDay_id => phenState%lenDay_id, &
               DecidCap_id => phenState%DecidCap_id, &
               StoreDrainPrm => phenState%storage_drain_params, &
               LAI_id => phenState%LAI_id, &
               GDD_id => phenState%GDD_id, &
               SDD_id => phenState%SDD_id, &
               albDecTr_id => phenState%albDecTr_id, &
               albEveTr_id => phenState%albEveTr_id, &
               albGrass_id => phenState%albGrass_id, &
               porosity_id => phenState%porosity_id, &
               HDD_id => anthroEmisState%HDD_id, &
               state_surf => hydroState%state_surf, &
               soilstore_surf => hydroState%soil_store_surf, &
               WUDay_id => hydroState%WUDay_id, &
               WaterUseMethod => config%WaterUseMethod, &
               Ie_start => irrPrm%Ie_start, &
               Ie_end => irrPrm%Ie_end, &
               Faut => irrPrm%f_aut, &
               Ie_a => irrPrm%Ie_a, &
               Ie_m => irrPrm%Ie_m, &
               H_maintain => irrPrm%H_maintain, &
               DayWatPer => [irrPrm%irr_daywater%monday_percent, &
                             irrPrm%irr_daywater%tuesday_percent, &
                             irrPrm%irr_daywater%wednesday_percent, &
                             irrPrm%irr_daywater%thursday_percent, &
                             irrPrm%irr_daywater%friday_percent, &
                             irrPrm%irr_daywater%saturday_percent, &
                             irrPrm%irr_daywater%sunday_percent], &
               DayWat => [irrPrm%irr_daywater%monday_flag, &
                          irrPrm%irr_daywater%tuesday_flag, &
                          irrPrm%irr_daywater%wednesday_flag, &
                          irrPrm%irr_daywater%thursday_flag, &
                          irrPrm%irr_daywater%friday_flag, &
                          irrPrm%irr_daywater%saturday_flag, &
                          irrPrm%irr_daywater%sunday_flag], &
               AlbMax_EveTr => evetrPrm%Alb_Max, &
               AlbMin_EveTr => evetrPrm%Alb_Min, &
               evetrLAIPower => evetrPrm%lai%lai_power, &
               AlbMax_DecTr => dectrPrm%Alb_Max, &
               AlbMin_DecTr => dectrPrm%Alb_Min, &
               CapMax_dec => dectrPrm%capacity_max_deciduous, &
               CapMin_dec => dectrPrm%capacity_min_deciduous, &
               PorMax_dec => dectrPrm%porosity_max_deciduous, &
               PorMin_dec => dectrPrm%porosity_min_deciduous, &
               dectrLAIPower => dectrPrm%lai%lai_power, &
               AlbMax_Grass => grassPrm%Alb_Max, &
               AlbMin_Grass => grassPrm%Alb_Min, &
               LAIType => [evetrPrm%lai%lai_type, &
                           dectrPrm%lai%lai_type, &
                           grassPrm%lai%lai_type], &
               BaseT => [evetrPrm%lai%base_temperature, &
                         dectrPrm%lai%base_temperature, &
                         grassPrm%lai%base_temperature], &
               BaseTe => [evetrPrm%lai%base_temperature_senescence, &
                          dectrPrm%lai%base_temperature_senescence, &
                          grassPrm%lai%base_temperature_senescence], &
               GDDFull => [evetrPrm%lai%gdd_full, &
                           dectrPrm%lai%gdd_full, &
                           grassPrm%lai%gdd_full], &
               SDDFull => [evetrPrm%lai%sdd_full, &
                           dectrPrm%lai%sdd_full, &
                           grassPrm%lai%sdd_full], &
               LAIMin => [evetrPrm%lai%lai_min, &
                          dectrPrm%lai%lai_min, &
                          grassPrm%lai%lai_min], &
               LAIMax => [evetrPrm%lai%lai_max, &
                          dectrPrm%lai%lai_max, &
                          grassPrm%lai%lai_max], &
               SoilStoreCap => [pavedPrm%soil%soil_store_capacity, &
                                bldgPrm%soil%soil_store_capacity, &
                                evetrPrm%soil%soil_store_capacity, &
                                dectrPrm%soil%soil_store_capacity, &
                                grassPrm%soil%soil_store_capacity, &
                                bsoilPrm%soil%soil_store_capacity, &
                                waterPrm%soil%soil_store_capacity], &
               grassLAIPower => grassPrm%lai%lai_power &
               )

               ! before
               ! Set the flag based on i_iter
               ! execute_subroutines = (i_iter == 1)
               execute_subroutines = .TRUE.

               LAIPower(:, 1) = evetrLAIPower
               LAIPower(:, 2) = dectrLAIPower
               LAIPower(:, 3) = grassLAIPower



               ! get timestamps
               time_now = datetime(year=iy) + timedelta(days=id - 1, hours=it, minutes=imin, seconds=isec)
               ! WRF-SUEWS COUPLING: tstep_prev allows for adaptive timesteps in WRF
               ! In standalone SUEWS, tstep_prev always equals tstep
               time_prev = time_now - timedelta(seconds=tstep_prev)
               time_next = time_now + timedelta(seconds=tstep)

               ! test if time at now is the first/last tstep of today
               first_tstep_Q = time_now%getDay() /= time_prev%getDay()
               last_tstep_Q = time_now%getDay() /= time_next%getDay()

               ! --------------------------------------------------------------------------------
               ! On first timestep of each day, define whether the day each a workday or weekend
               IF ((first_tstep_Q) .AND. execute_subroutines) THEN
                  CALL update_DailyState_Start( &
                     it, imin, & !input
                     HDD_id) !inout

                  ! reset certain GDD columns
                  Tmin_id = Temp_C !Daily min T in column 3
                  Tmax_id = Temp_C !Daily max T in column 4
                  lenDay_id = 0 !Cumulate daytime hours
               END IF

               ! --------------------------------------------------------------------------------
               !> assign Tair with either the forcing air temperature or the local diagnostic air temperature
               IF (config%RSLLevel == 1) THEN
                  Tair = atmState%t2_C
               ELSE IF (config%RSLLevel == 2) THEN
                  Tair = atmState%T_half_bldg_C
               ELSE
                  Tair = Temp_C
               END IF

               ! regular update at all timesteps of a day
               IF (execute_subroutines) THEN
                  CALL update_DailyState_Day( &
                     DayofWeek_id, &
                     avkdn, & !input
                     Tair, &
                     Precip, &
                     BaseT_Heating, BaseT_Cooling, &
                     nsh_real, &
                     Tmin_id, Tmax_id, lenDay_id, & !inout
                     HDD_id) !inout
               END IF

               ! Update snow density, albedo surface fraction
               ! TODO: to recover snow related functions
               ! IF (SnowUse == 1) CALL SnowUpdate( &
               !    nsurf, tstep, Temp_C, tau_a, tau_f, tau_r, &!input
               !    SnowDensMax, SnowDensMin, SnowAlbMax, SnowAlbMin, SnowPack, &
               !    SnowAlb, SnowDens)!inout

               ! --------------------------------------------------------------------------------
               ! On last timestep, perform the daily calculations -------------------------------
               ! Daily values not correct until end of each day,
               !  so main program should use values from the previous day
               IF (last_tstep_Q) THEN
                  ! Calculate heating degree days ------------------------------------------
                  IF (execute_subroutines) THEN
                     CALL update_HDD( &
                        dt_since_start, it, imin, tstep, & !input
                        HDD_id) !inout
                  END IF

                  ! Calculate modelled daily water use ------------------------------------------
                  CALL update_WaterUse( &
                     id, WaterUseMethod, DayofWeek_id, lat, Faut, HDD_id, & !input
                     state_surf, soilstore_surf, SoilStoreCap, H_maintain, & !input
                     Ie_a, Ie_m, Ie_start, Ie_end, DayWatPer, DayWat, &
                     WUDay_id) !output

                  !------------------------------------------------------------------------------
                  ! Calculation of LAI from growing degree days
                  ! This was revised and checked on 16 Feb 2014 by LJ
                  !------------------------------------------------------------------------------
                  IF (execute_subroutines) THEN
                     CALL update_GDDLAI( &
                        id, LAICalcYes, & !input
                        lat, [lai_evetr, lai_dectr, lai_grass], &
                        Tmin_id, Tmax_id, lenDay_id, &
                        BaseT, BaseTe, &
                        GDDFull, SDDFull, &
                        LAIMin, LAIMax, LAIPower, LAIType, &
                        LAI_id_prev, &
                        GDD_id, SDD_id, & !inout
                        LAI_id) !output
                     IF (supy_error_flag) RETURN

                     CALL update_Veg( &
                        LAImax, LAIMin, & !input
                        AlbMax_DecTr, AlbMax_EveTr, AlbMax_Grass, &
                        AlbMin_DecTr, AlbMin_EveTr, AlbMin_Grass, &
                        CapMax_dec, CapMin_dec, &
                        PorMax_dec, PorMin_dec, &
                        LAI_id, LAI_id_prev, &
                        DecidCap_id, & !inout
                        albDecTr_id, &
                        albEveTr_id, &
                        albGrass_id, &
                        porosity_id, &
                        StoreDrainPrm)
                  END IF
               END IF !End of section done only at the end of each day (i.e. only once per day)

            END ASSOCIATE
         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE SUEWS_cal_DailyState
   SUBROUTINE update_DailyState_Day( &
      DayofWeek_id, &
      avkdn, & !input
      Tair, &
      Precip, &
      BaseT_Heating, BaseT_Cooling, &
      nsh_real, &
      Tmin_id, Tmax_id, lenDay_id, & !inout
      HDD_id) !inout
      ! use time, only: id, id_prev_t
      USE module_util_time, ONLY: cal_weekday_index
      IMPLICIT NONE

      INTEGER, DIMENSION(3), INTENT(in) :: DayofWeek_id

      REAL(KIND(1D0)), INTENT(IN) :: avkdn
      REAL(KIND(1D0)), INTENT(IN) :: Tair ! Ambient air temperature [degC], this can be from either forcing or diagnostic
      REAL(KIND(1D0)), INTENT(IN) :: Precip
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: BaseT_Heating
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: BaseT_Cooling
      REAL(KIND(1D0)), INTENT(IN) :: nsh_real
      REAL(KIND(1D0)), INTENT(INOUT) :: Tmin_id
      REAL(KIND(1D0)), INTENT(INOUT) :: Tmax_id
      REAL(KIND(1D0)), INTENT(INOUT) :: lenDay_id
      REAL(KIND(1D0)), DIMENSION(12), INTENT(INOUT) :: HDD_id !Heating Degree Days (see SUEWS_DailyState.f95)
      INTEGER :: iu ! flag for weekday/weekend

      REAL(KIND(1D0)) :: dT_heating
      REAL(KIND(1D0)) :: dT_cooling

      REAL(KIND(1D0)) :: BaseT_Heating_use
      REAL(KIND(1D0)) :: BaseT_Cooling_use

      ! Set weekday/weekend counter (GH#1559: centralised in module_util_time)
      iu = cal_weekday_index(DayofWeek_id(1))

      ! Use weekday/weekend-specific base temperatures for heating/cooling degree day calculations
      BaseT_Heating_use = BaseT_Heating(iu)
      BaseT_Cooling_use = BaseT_Cooling(iu)

      ! Daily min and max temp (these get updated through the day) ---------------------
      Tmin_id = MIN(Tair, Tmin_id) !Daily min T in column 3
      Tmax_id = MAX(Tair, Tmax_id) !Daily max T in column 4
      IF (avkdn > 10) THEN
         lenDay_id = lenDay_id + 1/nsh_real !Cumulate daytime hours !Divide by nsh (HCW 01 Dec 2014)
      END IF

      ! Calculations related to heating and cooling degree days (HDD) ------------------
      ! See Sailor & Vasireddy (2006) EMS Eq 1,2 (theirs is hourly timestep)
      dT_heating = BaseT_Heating_use - Tair
      dT_cooling = Tair - BaseT_Cooling_use

      HDD_id(1) = HDD_id(1) + MERGE(dT_heating, 0D0, dT_heating >= 0) !Heating
      HDD_id(2) = HDD_id(2) + MERGE(dT_cooling, 0D0, dT_cooling >= 0) !Cooling
      HDD_id(3) = HDD_id(3) + Tair !Will become daily average temperature
      !      4 ------------------------------------!   !5-day running mean
      HDD_id(5) = HDD_id(5) + Precip !Daily precip total
      !      6 ------------------------------------!   !Days since rain

   END SUBROUTINE update_DailyState_Day

   SUBROUTINE update_Veg( &
      LAImax, LAIMin, & !input
      AlbMax_DecTr, AlbMax_EveTr, AlbMax_Grass, &
      AlbMin_DecTr, AlbMin_EveTr, AlbMin_Grass, &
      CapMax_dec, CapMin_dec, &
      PorMax_dec, PorMin_dec, &
      LAI_id, LAI_id_prev, &
      DecidCap_id, & !inout
      albDecTr_id, &
      albEveTr_id, &
      albGrass_id, &
      porosity_id, &
      StoreDrainPrm) !output

      IMPLICIT NONE

      ! INTEGER,INTENT(IN)::id
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: LAImax
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: LAIMin

      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_DecTr
      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_EveTr
      REAL(KIND(1D0)), INTENT(IN) :: AlbMax_Grass
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_DecTr
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_EveTr
      REAL(KIND(1D0)), INTENT(IN) :: AlbMin_Grass
      REAL(KIND(1D0)), INTENT(IN) :: CapMax_dec
      REAL(KIND(1D0)), INTENT(IN) :: CapMin_dec
      REAL(KIND(1D0)), INTENT(IN) :: PorMax_dec
      REAL(KIND(1D0)), INTENT(IN) :: PorMin_dec
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: LAI_id, LAI_id_prev

      REAL(KIND(1D0)), INTENT(INOUT) :: DecidCap_id
      REAL(KIND(1D0)), INTENT(INOUT) :: albDecTr_id
      REAL(KIND(1D0)), INTENT(INOUT) :: albEveTr_id
      REAL(KIND(1D0)), INTENT(INOUT) :: albGrass_id
      REAL(KIND(1D0)), INTENT(INOUT) :: porosity_id

      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(inout) :: StoreDrainPrm

      INTEGER :: iv

      REAL(KIND(1D0)) :: albChangeDecTr
      REAL(KIND(1D0)) :: albChangeEveTr
      REAL(KIND(1D0)) :: albChangeGrass
      REAL(KIND(1D0)) :: CapChange

      REAL(KIND(1D0)) :: deltaLAIDecTr
      REAL(KIND(1D0)) :: deltaLAIEveTr
      REAL(KIND(1D0)) :: deltaLAIGrass
      REAL(KIND(1D0)) :: porChange
      !::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      ! Calculate the development of vegetation cover
      ! Albedo changes with LAI for each vegetation type
      ! Storage capacity and porosity are updated based on DecTr LAI only (seasonal variation in Grass and EveTr assumed small)
      ! If only LUMPS is used, set deciduous capacities to 0
      ! QUESTION: Assume porosity Change based on GO99- Heisler?
      deltaLAIDecTr = 0
      deltaLAIEveTr = 0
      deltaLAIGrass = 0
      CapChange = 0
      porChange = 0
      albChangeDecTr = 0
      albChangeEveTr = 0
      albChangeGrass = 0

      iv = ivDecid
      IF ((LAI_id(iv) - LAI_id_prev(iv)) /= 0) THEN
         deltaLAIDecTr = (LAI_id(iv) - LAI_id_prev(iv))/(LAImax(iv) - LAIMin(iv))
         albChangeDecTr = (AlbMax_DecTr - AlbMin_DecTr)*deltaLAIDecTr
         CapChange = (CapMin_dec - CapMax_dec)*deltaLAIDecTr
         porChange = (PorMin_dec - PorMax_dec)*deltaLAIDecTr
      END IF

      iv = ivConif
      IF ((LAI_id(iv) - LAI_id_prev(iv)) /= 0) THEN
         deltaLAIEveTr = (LAI_id(iv) - LAI_id_prev(iv))/(LAImax(iv) - LAIMin(iv))
         albChangeEveTr = (AlbMax_EveTr - AlbMin_EveTr)*deltaLAIEveTr
      END IF

      iv = ivGrass
      IF ((LAI_id(iv) - LAI_id_prev(iv)) /= 0) THEN
         deltaLAIGrass = (LAI_id(iv) - LAI_id_prev(iv))/(LAImax(iv) - LAIMin(iv))
         ! Grass has reversed LAI-albedo relationship: higher LAI -> lower albedo
         ! (bright soil/litter background replaced by absorbing canopy)
         albChangeGrass = (AlbMin_Grass - AlbMax_Grass)*deltaLAIGrass
      END IF

      iv = ivDecid

      !write(*,*) deltaLAI, deltaLAIEveTr, deltaLAIGrass

      DecidCap_id = DecidCap_id - CapChange
      StoreDrainPrm(6, DecidSurf) = DecidCap_id !Change current storage capacity of deciduous trees
      porosity_id = porosity_id + porChange !- changed to + by HCW 20 Aug 2015 (porosity greatest when LAI smallest)
      porosity_id = MIN(MAX(porosity_id, MAX(PorMin_dec, 0.1)), MIN(PorMax_dec, 0.9)) ! limit porosity to valid range, TS 26 Jun 2023

      ! update albedo values while limiting these to valid ranges
      albDecTr_id = MIN(MAX(albDecTr_id + albChangeDecTr, AlbMin_DecTr), AlbMax_DecTr)
      albEveTr_id = MIN(MAX(albEveTr_id + albChangeEveTr, AlbMin_EveTr), AlbMax_EveTr)
      albGrass_id = MIN(MAX(albGrass_id + albChangeGrass, AlbMin_Grass), AlbMax_Grass)

   END SUBROUTINE update_Veg

   SUBROUTINE update_GDDLAI( &
      id, LAICalcYes, & !input
      lat, LAI_obs, &
      Tmin_id_prev, Tmax_id_prev, lenDay_id_prev, &
      BaseT_GDD, BaseT_SDD, &
      GDDFull, SDDFull, &
      LAIMin, LAIMax, LAIPower, LAIType, &
      LAI_id_prev, &
      GDD_id, SDD_id, & !inout
      LAI_id_next) !output
      
      implicit none

      !------------------------------------------------------------------------------
      ! Calculation of LAI from growing degree days
      ! This was revised and checked on 16 Feb 2014 by LJ
      !------------------------------------------------------------------------------

      integer, intent(in) :: id
      integer, intent(in) :: LAICalcYes

      real(kind(1D0)), intent(in) :: lat
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: LAI_obs
      real(kind(1D0)), intent(in) :: Tmin_id_prev
      real(kind(1D0)), intent(in) :: Tmax_id_prev
      real(kind(1D0)), intent(in) :: lenDay_id_prev

      ! --- Vegetation phenology ---------------------------------------------------------------------
      ! Parameters provided in input information for each vegetation surface (SUEWS_Veg.txt)
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: BaseT_GDD !Base temperature for growing degree days [degC]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: BaseT_SDD !Base temperature for senescence degree days [degC]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: GDDFull !Growing degree days needed for full capacity [degC]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: SDDFull !Senescence degree days needed to initiate leaf off [degC]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: laimin !Min LAI [m2 m-2]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: laimax !Max LAI [m2 m-2]
      real(kind(1D0)), dimension(4, nvegsurf), intent(in) :: LAIPower !Coeffs for LAI equation: 1,2 - leaf growth; 3,4 - leaf off
      !! N.B. currently DecTr only, although input provided for all veg types
      integer, dimension(nvegsurf), intent(in) :: LAIType !LAI equation to use: original (0) or new (1)

      real(kind(1D0)), dimension(3), intent(inout) :: GDD_id !Growing Degree Days (see SUEWS_DailyState.f95)
      real(kind(1D0)), dimension(3), intent(inout) :: SDD_id !Senescence Degree Days (see SUEWS_DailyState.f95)
      real(kind(1D0)), dimension(nvegsurf), intent(out) :: LAI_id_next !LAI for each veg surface [m2 m-2]
      real(kind(1D0)), dimension(nvegsurf), intent(in) :: LAI_id_prev ! LAI of previous day

      real(kind(1D0)) :: mean_temp ! Mean temperature of previous day
      real(kind(1D0)) :: delta_SDD !Switches and checks for GDD
      real(kind(1D0)) :: delta_GDD !Switches and checks for GDD
      real(kind(1D0)) :: indHelp !Switches and checks for GDD
      real(kind(1D0)), dimension(3) :: GDD_id_prev ! GDD of previous day
      real(kind(1D0)), dimension(3) :: SDD_id_prev ! SDD of previous day

      integer :: critDays = 50 !Critical limit for GDD when GDD or SDD is set to zero
      integer :: iv
      
      integer, parameter :: LAI_ORIGINAL = 0
      integer, parameter :: LAI_NEW = 1

      integer, parameter :: SEN_DAYLENGTH = 1
      integer, parameter :: SEN_SDD = 2

      logical :: valid_observed_lai
      
      ! translate values of previous day to local variables
      GDD_id_prev = GDD_id
      SDD_id_prev = SDD_id

      if (LAICalcYes == 0) then
         call observed_lai(valid_observed_lai)
         if (.not. valid_observed_lai) return
      end if

      mean_temp = calc_mean_temp(Tmin_id_prev, Tmax_id_prev)
      
      ! Loop through vegetation types (iv)
      do iv = 1, NVegSurf

         call calc_delta_gdd_sdd( &
            tmin_prev=Tmin_id_prev, &
            tmax_prev=Tmax_id_prev, &
            base_t_gdd=BaseT_GDD(iv), &
            base_t_sdd=BaseT_SDD(iv), &
            delta_gdd=delta_GDD, &
            delta_sdd=delta_SDD, &
            ind_help=indHelp &
         )
         if (LAItype(iv) == 2) then
            delta_GDD = -delta_GDD
            delta_SDD = -delta_SDD
         end if

         call apply_delta_gdd_sdd( &
            gdd_prev=GDD_id_prev(iv), &
            sdd_prev=SDD_id_prev(iv), &
            delta_gdd=delta_GDD, &
            delta_sdd=delta_SDD, &
            gdd_id=GDD_id(iv), &
            sdd_id=SDD_id(iv) &
         )
         
         ! Possibility for cold spring
         IF (SDD_id(iv) <= SDDFull(iv) .AND. indHelp < 0) THEN
            GDD_id(iv) = 0
         END IF

         if (laitype(iv) /= 2) then
            call limit_gdd_sdd( &
               GDD_id=GDD_id(iv), &
               SDD_id=SDD_id(iv), &
               GDDFull=GDDFull(iv), &
               SDDFull=SDDFull(iv), &
               critDays=critDays &
            )
         else
            if (SDD_id(iv) <= SDDFull(iv)) then !After senescence now start growing leaves
               SDD_id(iv) = SDDFull(iv) !Leaves off so add back earlier
               if (GDD_id(iv) > critDays) SDD_id(iv) = 0
            end if
            if (GDD_id(iv) >= GDDFull(iv)) then !Start senescence
               GDD_id(iv) = GDDFull(iv) !Leaves should not grow so delete yes from earlier
               if (SDD_id(iv) < -critDays) GDD_id(iv) = 0
            end if
         end if

         ! With these limits SDD, GDD is set to zero
         if (LAItype(iv) /= 2) then
            if (SDD_id(iv) < -critDays .and. SDD_id(iv) > SDDFull(iv)) GDD_id(iv) = 0
            if (GDD_id(iv) > critDays .and. GDD_id(iv) < GDDFull(iv)) SDD_id(iv) = 0
         else
            if (GDD_id(iv) > critDays .and. GDD_id(iv) < GDDFull(iv)) SDD_id(iv) = 0
            if (SDD_id(iv) < -critDays .and. SDD_id(iv) > SDDFull(iv)) GDD_id(iv) = 0
         end if

         ! Now calculate LAI itself
         if (lat >= 0) THEN !Northern hemispere
            if (LAItype(iv) < 1.5) then
               call reset_degree_day_states( &
                  id=id, &
                  sdd_reset_day=140, &
                  crit_days=critDays, &
                  summer_day=170, &
                  winter_day=170, &
                  southern_hemisphere=.false., &
                  sdd_id=SDD_id(iv), &
                  gdd_id=GDD_id(iv) &
               )
               if (LAICalcYes /= 0) then
                  call calculate_lai( &
                     senescence_mode=SEN_DAYLENGTH, &
                     id=id, &
                     SDD_id=SDD_id(iv), &
                     GDD_id=GDD_id(iv), &
                     critDays=critDays, &
                     LAItype=LAItype(iv), &
                     LAIPower=LAIPower(:, iv), &
                     GDDFull=GDDFull(iv), &
                     SDDFull=SDDFull(iv), &
                     lenDay_id_prev=lenDay_id_prev, &
                     laimax=laimax(iv), &
                     laimin=laimin(iv), &
                     LAI_id_prev=LAI_id_prev(iv), &
                     LAI_id_next=LAI_id_next(iv) &
                  )
               end if
            else if (laitype(iv) == 2) ! Inverted LAI behaviour (for evergreen trees)
               !If GDD is not zero by mid May, this is forced
               if (id == 140 .and. GDD_id(iv) /= 0) GDD_id(iv) = 0
               ! Set GDD to zero in summer time
               if (SDD_id(iv) < -critDays .and. id < 170) GDD_id(iv) = 0
               ! Set SDD zero in winter time
               if (GDD_id(iv) > critDays .and. id > 170) SDD_id(iv) = 0
               
               if (SDD_id(iv) < 0 .and. SDD_id(iv) > SDDFull(iv)) then !Leaves can still fall
                  LAI_id_next(iv) = (LAI_id_prev(iv)*LAIPower(3, iv)*(1 - SDD_id(iv))*LAIPower(4, iv)) + LAI_id_prev(iv)
                  !! Use day length to start senescence at high latitudes (N hemisphere)
               else if (lenDay_id_prev <= 12 .and. GDD_id(iv) < GDDFull(iv)) then !Start growth
                  LAI_id_next(iv) = (LAI_id_prev(iv)**LAIPower(1, iv)*GDD_id(iv)*LAIPower(2, iv)) + LAI_id_prev(iv)
               else
                  LAI_id_next(iv) = LAI_id_prev(iv)
               end if
            else if (LAItype(iv) == 3) then ! For managed grass - if max LAI set to min
               if (LAI_id_prev(iv) == LAIMax(iv)) then
                  LAI_id_next(iv) = LAIMin(iv)
                  GDD_id(iv) = 0
               else if (GDD_id(iv) > 0 .and. GDD_id(iv) < GDDFull(iv)) then !Leaves can still grow
                  LAI_id_next(iv) = (LAI_id_prev(iv)**LAIPower(1, iv)*GDD_id(iv)*LAIPower(2, iv)) + LAI_id_prev(iv)
               else if (lenDay_id_prev <= 12 .and. SDD_id(iv) > SDDFull(iv)) then !Start senescence
                  LAI_id_next(iv) = (LAI_id_prev(iv)*LAIPower(3, iv)*(1 - SDD_id(iv))*LAIPower(4, iv)) + LAI_id_prev(iv)
               else
                  LAI_id_next(iv) = LAI_id_prev(iv)
               end if
            else if (LAItype(iv) == 4) then ! For managed grass - if MAX GDD set to LAI min
               if (GDD_id(iv) == GDDFull(iv)) then
                  LAI_id_next(iv) = LAIMin(iv)
                  GDD_id(iv) = 0
               else if (lenDay_id_prev <= 12 .and. SDD_id(iv) > SDDFull(iv)) then !Start senescence
                     LAI_id_next(iv) = (LAI_id_prev(iv)*LAIPower(3, iv)*(1 - SDD_id(iv))*LAIPower(4, iv)) + LAI_id_prev(iv)
               else if (GDD_id(iv) > 0 .and. GDD_id(iv) < GDDFull(iv)) then !Leaves can still grow
                  LAI_id_next(iv) = (LAI_id_prev(iv)**LAIPower(1, iv)*GDD_id(iv)*LAIPower(2, iv)) + LAI_id_prev(iv)
               else
                  LAI_id_next(iv) = LAI_id_prev(iv)
               end if
            end if
         else !Southern hemisphere !! N.B. not identical to N hemisphere - return to later
            call reset_degree_day_states( &
               id=id, &
               sdd_reset_day=300, &
               crit_days=critDays, &
               summer_day=250, &
               winter_day=250, &
               southern_hemisphere=.true., &
               sdd_id=SDD_id(iv), &
               gdd_id=GDD_id(iv) &
            )
            if (LAICalcYes /= 0) then
               call calculate_lai( &
                  senescence_mode=SEN_SDD, &
                  id=id, &
                  SDD_id=SDD_id(iv), &
                  GDD_id=GDD_id(iv), &
                  critDays=critDays, &
                  LAItype=LAItype(iv), &
                  LAIPower=LAIPower(:, iv), &
                  GDDFull=GDDFull(iv), &
                  SDDFull=SDDFull(iv), &
                  lenDay_id_prev=lenDay_id_prev, &
                  laimax=laimax(iv), &
                  laimin=laimin(iv), &
                  LAI_id_prev=LAI_id_prev(iv), &
                  LAI_id_next=LAI_id_next(iv) &
               )
            end if

         end if !N or S hemisphere
            
      end do !End of loop over veg surfaces

      !------------------------------------------------------------------------------

   CONTAINS
   
      subroutine observed_lai(valid)

         implicit none

         logical, intent(out) :: valid

         valid = .false.

         if (any(ieee_is_nan(LAI_obs)) .or. any(LAI_obs < 0.0D0)) then
            ! Invalid LAI_obs slipped past pre-flight; raise an error before
            ! mutating phenology state and assign a safe sentinel to the output.
            LAI_id_next = -999.0D0
            call set_supy_error( &
               105, &
               'update_GDDLAI: laimethod=0 requires non-missing lai_* or lai >= 0 at every timestep')
            return
         end if

         ! Observed-LAI override: when LAICalcYes == 0, every timestep's forcing
         ! value must be a non-missing, non-negative observation (LAI_obs >= 0).
         
         ! A genuine zero observation (e.g. complete winter dieback) is valid and
         ! passes through unchanged. Missing/NaN values and strictly negative
         ! values - including the -999 missing sentinel - are rejected; choosing
         ! this path commits the user to providing an observation for every
         ! timestep. Observed LAI is intentionally not clipped to LAImin/LAImax,
         ! because those bounds describe the internal GDD/SDD phenology path.

         ! The Python pre-flight validator (supy._check.check_forcing) enforces
         ! this contract before a run starts; the guard below is a defensive
         ! backstop for callers that bypass preflight. Reports via
         ! module_ctrl_error_state so SuPy can surface a clean exception —
         ! never WRITE(*,...) + STOP, which kills the embedding Python process.

         ! Copy the effective observed LAI for EveTr, DecTr and Grass into the
         ! daily state without applying the GDD/SDD envelope.
         do iv = 1, NVegSurf
            LAI_id_next(iv) = LAI_obs(iv)
         end do

         valid = .true.

      end subroutine observed_lai

      function calc_mean_temp(temp1, temp2) result(mean_temp)
         implicit none

         real(kind(1D0)), intent(in) :: temp1
         real(kind(1D0)), intent(in) :: temp2
         real(kind(1D0)) :: mean_temp

         mean_temp = (temp1 + temp2)/2
      
      END FUNCTION calc_mean_temp

      subroutine calc_delta_gdd_sdd( &
            tmin_prev, tmax_prev, base_t_gdd, base_t_sdd, &
            delta_gdd, delta_sdd, ind_help)

         implicit none

         real(kind(1D0)), intent(in)  :: tmin_prev
         real(kind(1D0)), intent(in)  :: tmax_prev
         real(kind(1D0)), intent(in)  :: base_t_gdd
         real(kind(1D0)), intent(in)  :: base_t_sdd

         real(kind(1D0)), intent(out) :: delta_gdd
         real(kind(1D0)), intent(out) :: delta_sdd
         real(kind(1D0)), intent(out) :: ind_help

         ! Calculate GDD and SDD
         delta_gdd = calc_delta_degree_days( &
            tmin_prev, tmax_prev, base_t_gdd)

         delta_sdd = calc_delta_degree_days( &
            tmin_prev, tmax_prev, base_t_sdd)

         ! SDD cannot be positive
         if (delta_sdd > 0) delta_sdd = 0

         ! Help switch to allow GDD to go to zero in spring-time
         ind_help = 0

         ! GDD cannot be negative
         if (delta_gdd < 0) then
            ind_help = delta_gdd
            delta_gdd = 0
         end if

      end subroutine calc_delta_gdd_sdd

      function calc_delta_degree_days(Tmin_prev, Tmax_prev, base_t) result(delta_dd)

         implicit none

         real(kind(1D0)), intent(in) :: Tmin_prev
         real(kind(1D0)), intent(in) :: Tmax_prev
         real(kind(1D0)), intent(in) :: base_t

         real(kind(1D0)) :: delta_dd

         delta_dd = (Tmin_prev + Tmax_prev) / 2 - base_t

      end function calc_delta_degree_days

      subroutine apply_delta_gdd_sdd(gdd_prev, sdd_prev, &
                                    delta_gdd, delta_sdd, &
                                    gdd_id, sdd_id)

         implicit none

         real(kind(1D0)), intent(in) :: gdd_prev
         real(kind(1D0)), intent(in) :: sdd_prev
         real(kind(1D0)), intent(in) :: delta_gdd
         real(kind(1D0)), intent(in) :: delta_sdd

         real(kind(1D0)), intent(out) :: gdd_id
         real(kind(1D0)), intent(out) :: sdd_id

         ! Calculate cumulative growing and senescence degree days
         gdd_id = gdd_prev + delta_gdd
         sdd_id = sdd_prev + delta_sdd

      end subroutine apply_delta_gdd_sdd

      subroutine limit_gdd_sdd( &
            GDD_id, SDD_id, GDDFull, SDDFull, critDays)

         implicit none

         real(kind(1D0)), intent(inout) :: GDD_id
         real(kind(1D0)), intent(inout) :: SDD_id
         real(kind(1D0)), intent(in)    :: GDDFull
         real(kind(1D0)), intent(in)    :: SDDFull
         
         integer, intent(in)    :: critDays

         !Start senescence
         if (GDD_id >= GDDFull) then
            GDD_id = GDDFull !Leaves should not grow so delete yes from earlier
            if (SDD_id < -critDays) GDD_id = 0
         end if

         !After senescence now start growing leaves
         if (SDD_id <= SDDFull) then
            SDD_id = SDDFull !Leaves off so add back earlier
            if (GDD_id > critDays) SDD_id = 0
         end if

      end subroutine limit_gdd_sdd

      subroutine calculate_lai( &
            senescence_mode, &
            id, SDD_id, GDD_id, critDays, LAItype, LAIPower, GDDFull, SDDFull, &
            lenDay_id_prev, LAI_id_prev, laimax, laimin, LAI_id_next)

         implicit none

         integer, intent(in) :: senescence_mode
         integer, intent(in) :: id

         real(kind(1D0)), intent(inout) :: SDD_id
         real(kind(1D0)), intent(inout) :: GDD_id
         
         integer, intent(in) :: critDays
         integer, intent(in) :: LAItype
         
         real(kind(1D0)), dimension(4), intent(in) :: LAIPower

         real(kind(1D0)), intent(in) :: GDDFull
         real(kind(1D0)), intent(in) :: SDDFull
         real(kind(1D0)), intent(in) :: lenDay_id_prev
         real(kind(1D0)), intent(in) :: LAI_id_prev
         real(kind(1D0)), intent(in) :: laimax
         real(kind(1D0)), intent(in) :: laimin
         real(kind(1D0)), intent(out) :: LAI_id_next

         logical :: start_senescence

         if (GDD_id > 0 .and. GDD_id < GDDFull) then !Leaves can still grow
            call calculate_gdd( &
               LAI_id_prev=LAI_id_prev, &
               LAIPower=LAIPower, &
               GDD_id=GDD_id, &
               LAI_id_next=LAI_id_next &
            )
         
         else if (LAItype <= LAI_ORIGINAL) THEN !Original LAI type

            if (SDD_id < 0 .and. SDD_id > SDDFull) then !Start senescence
               call calculate_sdd_type0( &
                  LAI_id_prev=LAI_id_prev, &
                  LAIPower=LAIPower, &
                  SDD_id=SDD_id, &
                  LAI_id_next=LAI_id_next &
               )

            else
               LAI_id_next = LAI_id_prev

            end if

         else

            !! Use day length to start senescence at high latitudes (controlled in senescence_mode)
            start_senescence = check_start_senescence( &
               senescence_mode=senescence_mode, &
               lenDay_id_prev=lenDay_id_prev, &
               SDD_id=SDD_id, &
               SDDFull=SDDFull &
            )

            if (start_senescence) then !Start senescence
               call calculate_sdd_type1( &
                  LAI_id_prev=LAI_id_prev, &
                  LAIPower=LAIPower, &
                  SDD_id=SDD_id, &
                  LAI_id_next=LAI_id_next &
               )
            else
               LAI_id_next = LAI_id_prev
            end if

         end if

         ! Keep internally computed phenology within the configured canopy envelope.
         call limit_lai( &
            LAI_id_next=LAI_id_next, &
            LAImax=LAImax, &
            LAImin=LAImin &
         )

      end subroutine calculate_lai

      subroutine reset_degree_day_states( &
         id, sdd_reset_day, crit_days, summer_day, winter_day, &
         southern_hemisphere, sdd_id, gdd_id)

         implicit none

         integer, intent(in) :: id
         integer, intent(in) :: sdd_reset_day
         integer, intent(in) :: crit_days
         integer, intent(in) :: summer_day
         integer, intent(in) :: winter_day
         logical, intent(in) :: southern_hemisphere

         real(kind(1D0)), intent(inout) :: sdd_id
         real(kind(1D0)), intent(inout) :: gdd_id

         ! if SDD is not zero by the transition day, force it
         if (id == sdd_reset_day .and. sdd_id /= 0) sdd_id = 0

         if (southern_hemisphere) then

            ! Set SDD to zero in southern summer
            if (gdd_id > crit_days .and. id > summer_day) sdd_id = 0

            ! Set GDD zero in southern winter
            if (sdd_id < -crit_days .and. id < winter_day) gdd_id = 0

         else

            ! Set SDD to zero in northern summer
            if (gdd_id > crit_days .and. id < summer_day) sdd_id = 0

            ! Set GDD zero in northern winter
            if (sdd_id < -crit_days .and. id > winter_day) gdd_id = 0

         end if

      end subroutine reset_degree_day_states

      function check_start_senescence(senescence_mode, lenDay_id_prev, SDD_id, SDDFull) result(start_senescence)
         
         implicit none
         
         integer, intent(in) :: senescence_mode

         real(kind(1D0)), intent(in) :: lenDay_id_prev
         real(kind(1D0)), intent(in) :: SDD_id
         real(kind(1D0)), intent(in) :: SDDFull

         logical :: start_senescence
         
         select case (senescence_mode)

            case (SEN_DAYLENGTH)
               start_senescence = ((lenDay_id_prev <= 12) .and. (SDD_id > SDDFull))

            case (SEN_SDD)
               start_senescence = ((SDD_id < 0) .and. (SDD_id > SDDFull))

            case default
               ! Invalid option falls back to SEN_SDD. No error yet registered.
               ! default currently not possible as function calls hard-coded
               start_senescence = ((SDD_id < 0) .and. (SDD_id > SDDFull))

         end select

      end function check_start_senescence

      subroutine calculate_gdd( &
            LAI_id_prev, LAIPower, GDD_id, LAI_id_next)

         implicit none

         real(kind(1D0)), intent(in) :: LAI_id_prev
         real(kind(1D0)), dimension(4), intent(in) :: LAIPower
         real(kind(1D0)), intent(in) :: GDD_id
         real(kind(1D0)), intent(out) :: LAI_id_next

         LAI_id_next = (LAI_id_prev**LAIPower(1) * &
                        GDD_id * LAIPower(2)) + LAI_id_prev

      end subroutine calculate_gdd
   
      subroutine calculate_sdd_type0( &
            LAI_id_prev, LAIPower, SDD_id, LAI_id_next)

         implicit none

         real(kind(1D0)), intent(in) :: LAI_id_prev
         real(kind(1D0)), dimension(4), intent(in) :: LAIPower
         real(kind(1D0)), intent(in) :: SDD_id
         real(kind(1D0)), intent(out) :: LAI_id_next

         LAI_id_next = (LAI_id_prev**LAIPower(3) * SDD_id * LAIPower(4)) + LAI_id_prev

      end subroutine calculate_sdd_type0
   
      subroutine calculate_sdd_type1( & ! 
            LAI_id_prev, LAIPower, SDD_id, LAI_id_next)

         implicit none

         real(kind(1D0)), intent(in) :: LAI_id_prev
         real(kind(1D0)), dimension(4), intent(in) :: LAIPower
         real(kind(1D0)), intent(in) :: SDD_id
         real(kind(1D0)), intent(out) :: LAI_id_next

         LAI_id_next = (LAI_id_prev * LAIPower(3) * (1 - SDD_id) * LAIPower(4)) + LAI_id_prev

      end subroutine calculate_sdd_type1

      subroutine limit_lai(lai_id_next, laimax, laimin)

         ! Keep internally computed phenology within the configured canopy envelope.

         implicit none

         real(kind(1D0)), intent(inout) :: lai_id_next
         real(kind(1D0)), intent(in) :: laimax
         real(kind(1D0)), intent(in) :: laimin

         if (lai_id_next > LAImax) then
            lai_id_next = laimax
         else if (lai_id_next < LAImin) then
            lai_id_next = laimin
         end if

      end subroutine limit_lai

   end subroutine update_GDDLAI

   SUBROUTINE update_WaterUse( &
      id, WaterUseMethod, DayofWeek_id, lat, FrIrriAuto, HDD_id, & !input
      state_surf, soilstore_surf, SoilStoreCap_surf, H_maintain, & !input
      Ie_a, Ie_m, Ie_start, Ie_end, DayWatPer, DayWat, & !input
      WUDay_id) !output

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: id
      INTEGER, INTENT(IN) :: WaterUseMethod
      INTEGER, INTENT(IN) :: Ie_start !Starting time of water use (DOY)
      INTEGER, INTENT(IN) :: Ie_end !Ending time of water use (DOY)
      INTEGER, DIMENSION(3), INTENT(IN) :: DayofWeek_id

      REAL(KIND(1D0)), INTENT(IN) :: lat
      REAL(KIND(1D0)), INTENT(IN) :: FrIrriAuto !Fraction of irrigated area using automatic irrigation

      REAL(KIND(1D0)), DIMENSION(12), INTENT(IN) :: HDD_id
      REAL(KIND(1D0)), DIMENSION(NVegSurf), INTENT(IN) :: Ie_a !Coefficients for automatic irrigation models
      REAL(KIND(1D0)), DIMENSION(NVegSurf), INTENT(IN) :: Ie_m !Coefficients for manual irrigation models
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: DayWatPer !% of houses following daily water
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: DayWat !Days of watering allowed

      ! ponding control related
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: state_surf ! surface wetness [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: soilstore_surf ! soil water store [mm]
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SoilStoreCap_surf !Capacity of soil store for each surface [mm]
      REAL(KIND(1D0)), INTENT(IN) :: H_maintain ! ponding water depth to maintain [mm]

      REAL(KIND(1D0)), DIMENSION(9), INTENT(OUT) :: WUDay_id !Daily water use for EveTr, DecTr, Grass [mm] (see SUEWS_DailyState.f95)

      REAL(KIND(1D0)), DIMENSION(3) :: h_need !water level to maintain: surface+soil [mm]
      REAL(KIND(1D0)), DIMENSION(3) :: store_total !current water level: surface+soil [mm]
      REAL(KIND(1D0)), DIMENSION(3) :: WUDay_P !water used to maintain ponding level [mm]
      REAL(KIND(1D0)), DIMENSION(3) :: WUDay_A !automatic irrigation [mm]
      REAL(KIND(1D0)), DIMENSION(3) :: WUDay_M !manual irrigation [mm]
      REAL(KIND(1D0)), DIMENSION(3) :: WUDay_total !Coefficients for manual irrigation models

      INTEGER :: wd !Water use calculation is done when calc = 1
      INTEGER :: calc !Water use calculation is done when calc = 1
      INTEGER :: i

      REAL(KIND(1D0)) :: temp_avg
      REAL(KIND(1D0)) :: days_since_rain

      ! transfer HDD values
      temp_avg = HDD_id(9)
      days_since_rain = HDD_id(12)

      ! initialise WUDay_id
      WUDay_id = 0
      WUDay_P = 0
      WUDay_A = 0
      WUDay_M = 0

      IF (WaterUseMethod == 0) THEN !If water use is to be modelled (rather than observed)

         wd = DayofWeek_id(1)

         IF (DayWat(wd) == 1.0) THEN !1 indicates watering permitted on this day
            calc = 0
            IF (lat >= 0) THEN !Northern Hemisphere
               IF (id >= Ie_start - 1 .AND. id <= Ie_end + 1) calc = 1 !Day between irrigation period
            ELSE !Southern Hemisphere
               calc = 1
               IF (id >= Ie_end .AND. id <= Ie_start) calc = 0 !Day between irrigation period
            END IF

            IF (calc == 1) THEN
               ! Model daily water use based on days_since_rain (days since rain) and temp_avg (average temp)
               ! WUDay is the amount of water [mm] per day, applied to each of the irrigated areas
               ! N.B. These are the same for each vegetation type at the moment

               ! ---- irrigation amount to maintain a certain water availability----
               ! NB: H_maintain can be either positive or negative
               h_need = SoilStoreCap_surf(3:5) + H_maintain
               store_total = state_surf(3:5) + soilstore_surf(3:5)
               WUDay_P = h_need - store_total
               WUDay_P = MERGE(WUDay_P, 0D0, WUDay_P > 0)

               ! ---- automatic irrigation ----
               WUDay_A = FrIrriAuto*(Ie_a(1) + Ie_a(2)*temp_avg + Ie_a(3)*days_since_rain)*DayWatPer(wd)
               WUDay_A = MERGE(WUDay_A, 0D0, WUDay_A > 0)
               ! add ponding-demand to auto-irrigation
               WUDay_A = WUDay_A + WUDay_P

               ! ---- Manual irrigation----
               WUDay_M = (1 - FrIrriAuto)*(Ie_m(1) + Ie_m(2)*temp_avg + Ie_m(3)*days_since_rain)*DayWatPer(wd)
               WUDay_M = MERGE(WUDay_M, 0D0, WUDay_M > 0)

               ! ---- total irrigation
               WUDay_total = WUDay_P + WUDay_A + WUDay_M

               ! transfer values to WUDay_id
               WUDay_id([((i - 1)*3 + 1, i=1, 3)]) = WUDay_total
               WUDay_id([((i - 1)*3 + 2, i=1, 3)]) = WUDay_A
               WUDay_id([((i - 1)*3 + 3, i=1, 3)]) = WUDay_M

            ELSE !If no irrigation on this day
               WUDay_id = 0
            END IF
         END IF
      END IF
      ! print *, 'WUDay_id in update_WaterUse', WUDay_id

   END SUBROUTINE update_WaterUse

   SUBROUTINE update_HDD( &
      dt_since_start, it, imin, tstep, & !input
      HDD_id) !inout
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: dt_since_start, it, imin, tstep

      REAL(KIND(1D0)), DIMENSION(12), INTENT(INOUT) :: HDD_id
      ! REAL(KIND(1d0)),DIMENSION(6),INTENT(OUT):: HDD_id_use

      INTEGER :: days_prev
      REAL(KIND(1D0)) :: tstepcount

      ! count of timesteps performed during day
      tstepcount = (it*60 + imin)*60/tstep*1.
      ! Heating degree days (HDD) -------------
      HDD_id(1) = HDD_id(1)/tstepcount !Heating
      HDD_id(2) = HDD_id(2)/tstepcount !Cooling
      HDD_id(3) = HDD_id(3)/tstepcount !Average temp

      ! Calculate a quasi-5-day-running-mean temp
      days_prev = MIN(4, & ! dt_since_start >= 4 days
                      FLOOR(dt_since_start/(24*60*60)*1.)) ! dt_since_start < 4 days
      HDD_id(4) = (HDD_id(4)*days_prev + HDD_id(3))/(days_prev + 1)

      ! Calculate number of days since rain
      IF (HDD_id(5) > 0) THEN !Rain occurred
         HDD_id(6) = 0
      ELSE
         HDD_id(6) = HDD_id(6) + 1 !Days since rain
      END IF

      ! save updated HDD_id(1:6) values to the last-half part (i.e., HDD_id(7:12))
      HDD_id(6 + 1:6 + 6) = HDD_id(1:6)

   END SUBROUTINE update_HDD

   SUBROUTINE update_DailyState_Start( &
      it, imin, & !input
      HDD_id) !output
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: it
      INTEGER, INTENT(IN) :: imin

      REAL(KIND(1D0)), DIMENSION(6), INTENT(INOUT) :: HDD_id
      REAL(KIND(1D0)) :: HDD_id_mav, HDD_id_daysSR

      ! reset HDD_id to ZERO except for:
      ! 5-day moving average
      HDD_id_mav = HDD_id(4)
      ! Days Since Rain
      HDD_id_daysSR = HDD_id(6)
      IF (it == 0 .AND. imin == 0) THEN
         HDD_id = 0
         HDD_id(4) = HDD_id_mav
         HDD_id(6) = HDD_id_daysSR
      END IF

   END SUBROUTINE update_DailyState_Start

   SUBROUTINE SUEWS_update_DailyState( &
      id, datetimeline, & !input
      Gridiv, NumberOfGrids, &
      DailyStateLine, &
      dataOutDailyState) !inout

      IMPLICIT NONE

      ! INTEGER,INTENT(IN) ::iy
      INTEGER, INTENT(IN) :: id
      ! INTEGER,INTENT(IN) ::it
      ! INTEGER,INTENT(IN) ::imin

      REAL(KIND(1D0)), DIMENSION(5), INTENT(IN) :: datetimeline

      INTEGER, INTENT(IN) :: Gridiv
      INTEGER, INTENT(IN) :: NumberOfGrids
      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDailyState - 5), INTENT(IN) :: DailyStateLine
      REAL(KIND(1D0)), DIMENSION(ndays, ncolumnsDataOutDailyState, NumberOfGrids), INTENT(INOUT) :: dataOutDailyState

      ! write out to dataOutDailyState
      dataOutDailyState(id, 1:5, Gridiv) = datetimeline
      ! DailyStateLine will be -999 unless realistic values are calculated at the last timestep of each day
      dataOutDailyState(id, 6:ncolumnsDataOutDailyState, Gridiv) = DailyStateLine

   END SUBROUTINE SUEWS_update_DailyState

   ! transfer results to a one-line output for SUEWS_cal_DailyState
   SUBROUTINE update_DailyStateLine( &
      timer, config, forcing, siteInfo, & ! input
      modState, & ! input/output:
      DailyStateLine) !out

      USE module_ctrl_type, ONLY: &
         SUEWS_SITE, SUEWS_TIMER, SUEWS_CONFIG, SUEWS_FORCING, &
         PHENOLOGY_STATE, anthroEmis_STATE, &
         SNOW_STATE, SUEWS_TIMER, HYDRO_STATE, &
         OHM_STATE, PHENOLOGY_STATE, &
         SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_TIMER), INTENT(IN) :: timer
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(inout) :: modState

      REAL(KIND(1D0)), DIMENSION(ncolumnsDataOutDailyState - 5), INTENT(OUT) :: DailyStateLine

      ASSOCIATE ( &
         phenState => modState%phenState, &
         anthroEmisState => modState%anthroEmisState, &
         hydroState => modState%hydroState, &
         snowState => modState%snowState, &
         OHMState => modState%OHMState &
         )
         ASSOCIATE ( &
            GDD_id => phenState%GDD_id, &
            LAI_id => phenState%LAI_id, &
            SDD_id => phenState%SDD_id, &
            Tmin_id => phenState%Tmin_id, &
            Tmax_id => phenState%Tmax_id, &
            lenday_id => phenState%lenday_id, &
            DecidCap_id => phenState%DecidCap_id, &
            albDecTr_id => phenState%albDecTr_id, &
            albEveTr_id => phenState%albEveTr_id, &
            albGrass_id => phenState%albGrass_id, &
            porosity_id => phenState%porosity_id, &
            WUDay_id => hydroState%WUDay_id, &
            SnowAlb => snowState%snow_albedo, &
            SnowDens => snowState%snow_density, &
            HDD_id => anthroEmisState%HDD_id, &
            VegPhenLumps => phenState%veg_phen_lumps, &
            a1 => OHMState%a1, &
            a2 => OHMState%a2, &
            a3 => OHMState%a3, &
            a1_bldg => ohmState%a1_bldg, &
            a2_bldg => ohmState%a2_bldg, &
            a3_bldg => ohmState%a3_bldg, &
            it => timer%it, &
            imin => timer%imin, &
            nsh_real => timer%nsh_real &
            )

            ! initialise DailyStateLine
            DailyStateLine = -999
            ! Check if this is the last timestep of the day
            IF (it == 23 .AND. imin == INT((nsh_real - 1)/nsh_real*60)) THEN
               ! Write actual data only at the last timestep of each day
               DailyStateLine = [ &
                  HDD_id(1:6), &
                  GDD_id, &
                  SDD_id, &
                  Tmin_id, &
                  Tmax_id, &
                  lenday_id, &
                  LAI_id, &
                  DecidCap_id, &
                  Porosity_id, &
                  AlbEveTr_id, &
                  AlbDecTr_id, &
                  AlbGrass_id, &
                  WUDay_id, &
                  VegPhenLumps, &
                  SnowAlb, &
                  SnowDens, &
                  a1, &
                  a2, &
                  a3, &
                  a1_bldg, &
                  a2_bldg, &
                  a3_bldg]
            END IF

         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE update_DailyStateLine

END MODULE module_phys_dailystate

! Backward compatibility alias (deprecated - will be removed in future version)
! TODO: Remove in version 2026.1.0 (deprecated since 2025.10.0)
MODULE DailyState_module
   USE module_phys_dailystate
END MODULE DailyState_module
