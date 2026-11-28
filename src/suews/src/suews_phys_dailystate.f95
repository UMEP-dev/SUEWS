! Main module following naming standard: matches filename
MODULE module_phys_dailystate
   USE, INTRINSIC :: ieee_arithmetic, ONLY: ieee_is_finite
   USE module_ctrl_const_allocate, ONLY: &
      nsurf, nvegsurf, ivConif, ivDecid, ivGrass, ConifSurf, DecidSurf, GrassSurf, &
      ncolumnsDataOutDailyState
   USE module_ctrl_const_calendar, ONLY: &
      NDAYS_WEEK, DOW_SUNDAY, DOW_MONDAY, DOW_TUESDAY, DOW_WEDNESDAY, &
      DOW_THURSDAY, DOW_FRIDAY, DOW_SATURDAY
   USE module_ctrl_error_state, ONLY: set_supy_error, supy_error_flag

   use module_phys_lai, only: update_gddlai, has_valid_absent_lai_params, is_missing_sentinel
   USE module_ctrl_const_physconst, ONLY: sec_per_day
   IMPLICIT NONE

CONTAINS

   ! Calculation of daily state variables
   ! Responds to what has happened in the past (temperature, rainfall, etc)
   ! Updates each time step, but for many variables, correct values are calculated only at the end of each day!
   ! --> for these variables, the rest of the code MUST use values from the previous day
   ! N.B. Some of this code is repeated in SUEWS_Initial
   ! --> so if changes are made here, SUEWS_Initial may also need to be updated accordingly
   ! Daily variables accumulate over right-closed days (D 00:00, D+1 00:00] and emit at D+1 00:00
   ! (for 5-min resolution, 00:05 to 00:00 next day).
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
      clock, config, forcing, siteInfo, &
      modState) ! input/output:
      ! atmState, & !inout
      ! phenState, & !inout
      ! anthroEmisState, & !inout
      ! hydroState) !inout

      ! USE module_phys_snow, ONLY: SnowUpdate
      USE module_ctrl_type, ONLY: SUEWS_SITE, SUEWS_CLOCK, SUEWS_FORCING, anthroEMIS_PRM, &
                                  PHENOLOGY_STATE, anthroEmis_STATE, SUEWS_CONFIG, &
                                  IRRIGATION_PRM, LC_PAVED_PRM, LC_BLDG_PRM, &
                                  LC_EVETR_PRM, LC_DECTR_PRM, LC_GRASS_PRM, &
                                  LC_BSOIL_PRM, LC_WATER_PRM, &
                                  HYDRO_STATE, atm_STATE, SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
      TYPE(SUEWS_CONFIG), INTENT(IN) :: config
      TYPE(SUEWS_FORCING), INTENT(IN) :: forcing
      TYPE(SUEWS_SITE), INTENT(IN) :: siteInfo

      TYPE(SUEWS_STATE), INTENT(INOUT) :: modState

      INTEGER :: LAICalcYes ! 1 = calculate LAI internally (GDD), 0 = use forcing%LAI_obs [-]
      INTEGER :: emissions_sub ! EmissionsMethod modulo 10 sub-method [-]
      LOGICAL :: calc_hdd ! whether heating base temperatures / HDD are active [-]
      LOGICAL :: calc_cdd ! whether cooling base temperatures / CDD are active [-]

      REAL(KIND(1D0)), DIMENSION(2) :: BaseT_Heating
      REAL(KIND(1D0)), DIMENSION(2) :: BaseT_Cooling

      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: sfr_veg
      LOGICAL, DIMENSION(nvegsurf) :: run_veg_lai
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: BaseT
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: BaseTe
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: GDDBase
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: SDDBase
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: LAIMin
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: LAIMax
      REAL(KIND(1D0)), DIMENSION(4, nvegsurf) :: LAIPower !Coeffs for LAI equation: 1,2 - leaf growth; 3,4 - leaf off
      REAL(KIND(1D0)), DIMENSION(NDAYS_WEEK) :: DayWat ! Days of watering allowed (DOW_SUNDAY..DOW_SATURDAY) [-]
      REAL(KIND(1D0)), DIMENSION(NDAYS_WEEK) :: DayWatPer ! Fraction of properties irrigating (DOW_SUNDAY..DOW_SATURDAY) [-]

      TYPE(PHENOLOGY_STATE) :: phenState_prev

      LOGICAL :: first_tstep_day_Q ! if this is the first tstep of a right-closed day
      LOGICAL :: first_tstep_Q ! if this is the first tstep of a day or of the run
      LOGICAL :: first_tstep_run_Q ! if this is the first tstep of the run
      LOGICAL :: last_tstep_Q ! if this is the closing tstep of a right-closed day (D+1 00:00)

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
            tstep => clock%timer%tstep, &
            tstep_prev => clock%timer%tstep_prev, &
            dt_since_start => clock%timer%dt_since_start, &
            nsh_real => clock%nsh_real, &
            id_day => clock%id_day, &
            DayofWeek_day => clock%dayofWeek_day, &
            elapsed_sec_of_day => clock%elapsed_sec_of_day, &
            irr_month => clock%deliv_month_st, &
            irr_doy => clock%deliv_doy_st, &
            irr_dow => clock%deliv_dow_st, &
            irrig_new_day_Q => clock%first_step_of_deliv_day_st, &
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
               AlbMax_EveTr => evetrPrm%Alb_Max, &
               AlbMin_EveTr => evetrPrm%Alb_Min, &
               AlbMax_DecTr => dectrPrm%Alb_Max, &
               AlbMin_DecTr => dectrPrm%Alb_Min, &
               CapMax_dec => dectrPrm%capacity_max_deciduous, &
               CapMin_dec => dectrPrm%capacity_min_deciduous, &
               PorMax_dec => dectrPrm%porosity_max_deciduous, &
               PorMin_dec => dectrPrm%porosity_min_deciduous, &
               AlbMax_Grass => grassPrm%Alb_Max, &
               AlbMin_Grass => grassPrm%Alb_Min, &
               SoilStoreCap => [pavedPrm%soil%soil_store_capacity, &
                                bldgPrm%soil%soil_store_capacity, &
                                evetrPrm%soil%soil_store_capacity, &
                                dectrPrm%soil%soil_store_capacity, &
                                grassPrm%soil%soil_store_capacity, &
                                bsoilPrm%soil%soil_store_capacity, &
                                waterPrm%soil%soil_store_capacity] &
               )

               ! Pack weekday irrigation schedules by named day-of-week index
               ! (DOW_SUNDAY=1 .. DOW_SATURDAY=7) to match DayofWeek_id(1) (gh#2049).
               DayWat(DOW_SUNDAY) = irrPrm%irr_daywater%sunday_flag
               DayWat(DOW_MONDAY) = irrPrm%irr_daywater%monday_flag
               DayWat(DOW_TUESDAY) = irrPrm%irr_daywater%tuesday_flag
               DayWat(DOW_WEDNESDAY) = irrPrm%irr_daywater%wednesday_flag
               DayWat(DOW_THURSDAY) = irrPrm%irr_daywater%thursday_flag
               DayWat(DOW_FRIDAY) = irrPrm%irr_daywater%friday_flag
               DayWat(DOW_SATURDAY) = irrPrm%irr_daywater%saturday_flag

               DayWatPer(DOW_SUNDAY) = irrPrm%irr_daywater%sunday_percent
               DayWatPer(DOW_MONDAY) = irrPrm%irr_daywater%monday_percent
               DayWatPer(DOW_TUESDAY) = irrPrm%irr_daywater%tuesday_percent
               DayWatPer(DOW_WEDNESDAY) = irrPrm%irr_daywater%wednesday_percent
               DayWatPer(DOW_THURSDAY) = irrPrm%irr_daywater%thursday_percent
               DayWatPer(DOW_FRIDAY) = irrPrm%irr_daywater%friday_percent
               DayWatPer(DOW_SATURDAY) = irrPrm%irr_daywater%saturday_percent

               ! before
               ! Set the flag based on i_iter
               ! execute_subroutines = (i_iter == 1)
               execute_subroutines = .TRUE.

               ! Gate heating/cooling base-temperature reads on the active
               ! anthropogenic emissions sub-method (#2363):
               ! - EmissionsMethod == 0 (observed QF): neither HDD nor CDD base temps used
               ! - sub-methods 1, 4 (L11 instantaneous): heating only (no cooling term)
               ! - sub-methods 2, 3, 5, 6 (J11 and L11 daily): both heating and cooling
               emissions_sub = MOD(config%EmissionsMethod, 10)
               calc_hdd = (config%EmissionsMethod > 0) .AND. (emissions_sub >= 1 .AND. emissions_sub <= 6)
               calc_cdd = (config%EmissionsMethod > 0) .AND. &
                          (emissions_sub == 2 .OR. emissions_sub == 3 .OR. &
                           emissions_sub == 5 .OR. emissions_sub == 6)
               BaseT_Heating = 0.0D0
               BaseT_Cooling = 0.0D0
               IF (calc_hdd) THEN
                  BaseT_Heating = [ahemisPrm%anthro_heat%BaseT_Heating_working, &
                                   ahemisPrm%anthro_heat%BaseT_Heating_holiday]
               END IF
               IF (calc_cdd) THEN
                  BaseT_Cooling = [ahemisPrm%anthro_heat%BaseT_Cooling_working, &
                                   ahemisPrm%anthro_heat%BaseT_Cooling_holiday]
               END IF

               sfr_veg = [siteInfo%sfr_surf(ConifSurf), &
                          siteInfo%sfr_surf(DecidSurf), &
                          siteInfo%sfr_surf(GrassSurf)]

               ! Copy LAI bounds only for vegetation surfaces present (consumed
               ! by update_Veg under both observed and modelled LAI), and copy
               ! GDD/SDD phenology parameters only when LAI is modelled (#2363).
               ! For absent surfaces (sfr == 0) under modelled LAI, preserve
               ! existing diagnostic DailyState evolution when their phenology
               ! parameters are valid and finite, and skip them cleanly when
               ! sentinel/non-finite parameters are supplied.
               BaseT = 0.0D0
               BaseTe = 0.0D0
               GDDBase = 0.0D0
               SDDBase = 0.0D0
               LAIMin = 0.0D0
               LAIMax = 0.0D0
               LAIPower = 0.0D0
               run_veg_lai = .FALSE.

               IF (sfr_veg(ivConif) > 0.0D0) THEN
                  LAIMin(ivConif) = evetrPrm%lai%lai_min
                  LAIMax(ivConif) = evetrPrm%lai%lai_max
               ELSE IF (.NOT. is_missing_sentinel(evetrPrm%lai%lai_min) .AND. &
                        .NOT. is_missing_sentinel(evetrPrm%lai%lai_max)) THEN
                  LAIMin(ivConif) = evetrPrm%lai%lai_min
                  LAIMax(ivConif) = evetrPrm%lai%lai_max
               ELSE
                  LAIMin(ivConif) = -999.0D0
                  LAIMax(ivConif) = -999.0D0
               END IF
               IF (sfr_veg(ivDecid) > 0.0D0) THEN
                  LAIMin(ivDecid) = dectrPrm%lai%lai_min
                  LAIMax(ivDecid) = dectrPrm%lai%lai_max
               ELSE IF (.NOT. is_missing_sentinel(dectrPrm%lai%lai_min) .AND. &
                        .NOT. is_missing_sentinel(dectrPrm%lai%lai_max)) THEN
                  LAIMin(ivDecid) = dectrPrm%lai%lai_min
                  LAIMax(ivDecid) = dectrPrm%lai%lai_max
               ELSE
                  LAIMin(ivDecid) = -999.0D0
                  LAIMax(ivDecid) = -999.0D0
               END IF
               IF (sfr_veg(ivGrass) > 0.0D0) THEN
                  LAIMin(ivGrass) = grassPrm%lai%lai_min
                  LAIMax(ivGrass) = grassPrm%lai%lai_max
               ELSE IF (.NOT. is_missing_sentinel(grassPrm%lai%lai_min) .AND. &
                        .NOT. is_missing_sentinel(grassPrm%lai%lai_max)) THEN
                  LAIMin(ivGrass) = grassPrm%lai%lai_min
                  LAIMax(ivGrass) = grassPrm%lai%lai_max
               ELSE
                  LAIMin(ivGrass) = -999.0D0
                  LAIMax(ivGrass) = -999.0D0
               END IF

               IF (LAICalcYes /= 0) THEN
                  run_veg_lai(ivConif) = (sfr_veg(ivConif) > 0.0D0)
                  IF (.NOT. run_veg_lai(ivConif)) THEN
                     run_veg_lai(ivConif) = has_valid_absent_lai_params( &
                        evetrPrm%lai%base_temperature, &
                        evetrPrm%lai%base_temperature_senescence, &
                        evetrPrm%lai%gdd_base, &
                        evetrPrm%lai%sdd_base, &
                        evetrPrm%lai%lai_min, &
                        evetrPrm%lai%lai_max, &
                        evetrPrm%lai%lai_power, &
                        LAI_id_prev(ivConif))
                  END IF
                  IF (run_veg_lai(ivConif)) THEN
                     LAIMin(ivConif) = evetrPrm%lai%lai_min
                     LAIMax(ivConif) = evetrPrm%lai%lai_max
                     BaseT(ivConif) = evetrPrm%lai%base_temperature
                     BaseTe(ivConif) = evetrPrm%lai%base_temperature_senescence
                     GDDBase(ivConif) = evetrPrm%lai%gdd_base
                     SDDBase(ivConif) = evetrPrm%lai%sdd_base
                     LAIPower(:, ivConif) = evetrPrm%lai%lai_power
                  END IF

                  run_veg_lai(ivDecid) = (sfr_veg(ivDecid) > 0.0D0)
                  IF (.NOT. run_veg_lai(ivDecid)) THEN
                     run_veg_lai(ivDecid) = has_valid_absent_lai_params( &
                        dectrPrm%lai%base_temperature, &
                        dectrPrm%lai%base_temperature_senescence, &
                        dectrPrm%lai%gdd_base, &
                        dectrPrm%lai%sdd_base, &
                        dectrPrm%lai%lai_min, &
                        dectrPrm%lai%lai_max, &
                        dectrPrm%lai%lai_power, &
                        LAI_id_prev(ivDecid))
                  END IF
                  IF (run_veg_lai(ivDecid)) THEN
                     LAIMin(ivDecid) = dectrPrm%lai%lai_min
                     LAIMax(ivDecid) = dectrPrm%lai%lai_max
                     BaseT(ivDecid) = dectrPrm%lai%base_temperature
                     BaseTe(ivDecid) = dectrPrm%lai%base_temperature_senescence
                     GDDBase(ivDecid) = dectrPrm%lai%gdd_base
                     SDDBase(ivDecid) = dectrPrm%lai%sdd_base
                     LAIPower(:, ivDecid) = dectrPrm%lai%lai_power
                  END IF

                  run_veg_lai(ivGrass) = (sfr_veg(ivGrass) > 0.0D0)
                  IF (.NOT. run_veg_lai(ivGrass)) THEN
                     run_veg_lai(ivGrass) = has_valid_absent_lai_params( &
                        grassPrm%lai%base_temperature, &
                        grassPrm%lai%base_temperature_senescence, &
                        grassPrm%lai%gdd_base, &
                        grassPrm%lai%sdd_base, &
                        grassPrm%lai%lai_min, &
                        grassPrm%lai%lai_max, &
                        grassPrm%lai%lai_power, &
                        LAI_id_prev(ivGrass))
                  END IF
                  IF (run_veg_lai(ivGrass)) THEN
                     LAIMin(ivGrass) = grassPrm%lai%lai_min
                     LAIMax(ivGrass) = grassPrm%lai%lai_max
                     BaseT(ivGrass) = grassPrm%lai%base_temperature
                     BaseTe(ivGrass) = grassPrm%lai%base_temperature_senescence
                     GDDBase(ivGrass) = grassPrm%lai%gdd_base
                     SDDBase(ivGrass) = grassPrm%lai%sdd_base
                     LAIPower(:, ivGrass) = grassPrm%lai%lai_power
                  END IF
               END IF

               ! Right-closed day boundaries and accumulated-day calendar identity
               ! come from the per-step clock (gh#2333, gh#2382).
               ! The first step of a run also starts a day, so a run starting
               ! mid-day or at 00:00 begins its first (possibly partial) day from
               ! that step rather than from the initial state (gh#1919). The
               ! bridge starts a fresh run with dt_since_start = tstep; a run
               ! continued from a checkpoint carries it on, so a chunk boundary
               ! inside a day does not restart that day.
               first_tstep_run_Q = clock%first_step_of_run
               first_tstep_day_Q = clock%first_step_of_day
               first_tstep_Q = first_tstep_day_Q .OR. first_tstep_run_Q
               last_tstep_Q = clock%last_step_of_day

               ! Reject intra-day or midnight-skipping timestep changes explicitly.
               ! DailyState accumulates fixed-step means via update_HDD (using tstep)
               ! and emits DailyStateLine only on each day's closing step, so changing
               ! tstep within an open day or jumping past an unclosed day boundary
               ! cannot preserve exact daily means or emit day_prev's record.
               ! Clean between-day changes after a closed day boundary remain allowed.
               IF ((.NOT. first_tstep_run_Q) .AND. tstep_prev > 0 .AND. (tstep /= tstep_prev)) THEN
                  IF ((.NOT. first_tstep_day_Q) .OR. (.NOT. clock%prev_step_closed_day)) THEN
                     CALL set_supy_error( &
                        118, &
                        'SUEWS_cal_DailyState: changing tstep within a day or across an unclosed day boundary is not supported')
                     RETURN
                  END IF
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

               ! --------------------------------------------------------------------------------
               ! On first timestep of each day (or run), reset daily accumulators
               IF ((first_tstep_Q) .AND. execute_subroutines) THEN
                  CALL update_DailyState_Start( &
                     first_tstep_Q, & !input
                     HDD_id(1:6)) !inout

                  ! reset certain GDD columns from the same Tair used for the rest of the day (gh#1882)
                  Tmin_id = Tair !Daily min T in column 3
                  Tmax_id = Tair !Daily max T in column 4
                  lenDay_id = 0 !Cumulate daytime hours
               END IF

               ! regular update at all timesteps of a day
               IF (execute_subroutines) THEN
                  CALL update_DailyState_Day( &
                     DayofWeek_day, &
                     avkdn, & !input
                     Tair, &
                     Precip, &
                     BaseT_Heating, BaseT_Cooling, &
                     nsh_real, &
                     Tmin_id, Tmax_id, lenDay_id, & !inout
                     HDD_id, & !inout
                     calc_hdd=calc_hdd, calc_cdd=calc_cdd)
               END IF

               ! Update snow density, albedo surface fraction
               ! TODO: to recover snow related functions
               ! IF (SnowUse == 1) CALL SnowUpdate( &
               !    nsurf, tstep, Temp_C, tau_a, tau_f, tau_r, &!input
               !    SnowDensMax, SnowDensMin, SnowAlbMax, SnowAlbMin, SnowPack, &
               !    SnowAlb, SnowDens)!inout

               ! --------------------------------------------------------------------------------
               ! Calculate modelled daily irrigation budget at the opening step of each
               ! right-closed local-standard delivery day (or on a fresh cold start),
               ! before SUEWS_cal_WaterUse runs, and hold WUDay_id frozen through D+1 00:00
               ! (gh#2381).
               IF (irrig_new_day_Q) THEN
                  CALL update_WaterUse( &
                     irr_doy, WaterUseMethod, [irr_dow, irr_month, 0], lat, Faut, HDD_id, & !input
                     state_surf, soilstore_surf, SoilStoreCap, H_maintain, & !input
                     Ie_a, Ie_m, Ie_start, Ie_end, DayWatPer, DayWat, &
                     WUDay_id) !output
               END IF

               ! --------------------------------------------------------------------------------
               ! On closing timestep of each right-closed day (D+1 00:00), perform daily updates
               ! Daily values not correct until end of each day,
               !  so main program should use values from the previous day
               IF (last_tstep_Q) THEN
                  ! Calculate heating degree days ------------------------------------------
                  IF (execute_subroutines) THEN
                     CALL update_HDD( &
                        dt_since_start, elapsed_sec_of_day, tstep, & !input
                        HDD_id) !inout
                  END IF

                  !------------------------------------------------------------------------------
                  ! Calculation of LAI from growing degree days
                  ! This was revised and checked on 16 Feb 2014 by LJ
                  !------------------------------------------------------------------------------
                  IF (execute_subroutines) THEN
                     CALL update_gddlai( &
                        id=id_day, lai_calc_yes=LAICalcYes, & !input
                        lat=lat, lai_obs=[lai_evetr, lai_dectr, lai_grass], &
                        t_min_id_prev=Tmin_id, t_max_id_prev=Tmax_id, &
                        base_t_gdd=BaseT, base_t_sdd=BaseTe, &
                        gdd_base=GDDBase, sdd_base=SDDBase, &
                        lai_min=LAIMin, lai_max=LAIMax, &
                        lai_power=LAIPower, &
                        lai_id_prev=LAI_id_prev, &
                        gdd_id=GDD_id, sdd_id=SDD_id, & !inout
                        lai_id_next=LAI_id, & !output
                        sfr_veg=sfr_veg, &
                        run_veg=run_veg_lai)
                     IF (supy_error_flag()) RETURN

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
                        StoreDrainPrm, &
                        sfr_veg=sfr_veg)
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
      HDD_id, & !inout
      calc_hdd, calc_cdd)
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
      LOGICAL, INTENT(IN), OPTIONAL :: calc_hdd
      LOGICAL, INTENT(IN), OPTIONAL :: calc_cdd
      INTEGER :: iu ! flag for weekday/weekend
      LOGICAL :: do_hdd, do_cdd

      REAL(KIND(1D0)) :: dT_heating
      REAL(KIND(1D0)) :: dT_cooling

      REAL(KIND(1D0)) :: BaseT_Heating_use
      REAL(KIND(1D0)) :: BaseT_Cooling_use

      do_hdd = .TRUE.
      IF (PRESENT(calc_hdd)) do_hdd = calc_hdd
      do_cdd = .TRUE.
      IF (PRESENT(calc_cdd)) do_cdd = calc_cdd

      ! Daily min and max temp (these get updated through the day) ---------------------
      Tmin_id = MIN(Tair, Tmin_id) !Daily min T in column 3
      Tmax_id = MAX(Tair, Tmax_id) !Daily max T in column 4
      IF (avkdn > 10) THEN
         lenDay_id = lenDay_id + 1/nsh_real !Cumulate daytime hours !Divide by nsh (HCW 01 Dec 2014)
      END IF

      ! Calculations related to heating and cooling degree days (HDD) ------------------
      ! See Sailor & Vasireddy (2006) EMS Eq 1,2 (theirs is hourly timestep)
      IF (do_hdd .OR. do_cdd) THEN
         ! Set weekday/weekend counter (GH#1559: centralised in module_util_time)
         iu = cal_weekday_index(DayofWeek_id(1))
         IF (do_hdd) THEN
            BaseT_Heating_use = BaseT_Heating(iu)
            dT_heating = BaseT_Heating_use - Tair
            HDD_id(1) = HDD_id(1) + MERGE(dT_heating, 0D0, dT_heating >= 0) !Heating
         END IF
         IF (do_cdd) THEN
            BaseT_Cooling_use = BaseT_Cooling(iu)
            dT_cooling = Tair - BaseT_Cooling_use
            HDD_id(2) = HDD_id(2) + MERGE(dT_cooling, 0D0, dT_cooling >= 0) !Cooling
         END IF
      END IF
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
      StoreDrainPrm, &
      sfr_veg) !output

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
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN), OPTIONAL :: sfr_veg

      INTEGER :: iv
      LOGICAL, DIMENSION(nvegsurf) :: active_veg
      LOGICAL, DIMENSION(nvegsurf) :: update_veg_surf

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

      active_veg = .TRUE.
      IF (PRESENT(sfr_veg)) active_veg = (sfr_veg > 0.0D0)

      ! For absent surfaces (sfr == 0), preserve existing diagnostic albedo,
      ! storage-capacity, and porosity evolution when all consumed bounds are
      ! valid and non-sentinel, and skip cleanly when any bound is NaN or -999
      ! (checking finiteness via is_missing_sentinel before ordering comparisons
      ! so quiet NaNs never raise IEEE_INVALID) (#2363).
      update_veg_surf = active_veg
      IF (.NOT. update_veg_surf(ivDecid)) THEN
         IF (.NOT. is_missing_sentinel(LAIMin(ivDecid)) .AND. &
             .NOT. is_missing_sentinel(LAImax(ivDecid)) .AND. &
             .NOT. is_missing_sentinel(AlbMin_DecTr) .AND. &
             .NOT. is_missing_sentinel(AlbMax_DecTr) .AND. &
             .NOT. is_missing_sentinel(CapMin_dec) .AND. &
             .NOT. is_missing_sentinel(CapMax_dec) .AND. &
             .NOT. is_missing_sentinel(PorMin_dec) .AND. &
             .NOT. is_missing_sentinel(PorMax_dec) .AND. &
             .NOT. is_missing_sentinel(LAI_id(ivDecid)) .AND. &
             .NOT. is_missing_sentinel(LAI_id_prev(ivDecid))) THEN
            update_veg_surf(ivDecid) = .TRUE.
         END IF
      END IF
      IF (.NOT. update_veg_surf(ivConif)) THEN
         IF (.NOT. is_missing_sentinel(LAIMin(ivConif)) .AND. &
             .NOT. is_missing_sentinel(LAImax(ivConif)) .AND. &
             .NOT. is_missing_sentinel(AlbMin_EveTr) .AND. &
             .NOT. is_missing_sentinel(AlbMax_EveTr) .AND. &
             .NOT. is_missing_sentinel(LAI_id(ivConif)) .AND. &
             .NOT. is_missing_sentinel(LAI_id_prev(ivConif))) THEN
            update_veg_surf(ivConif) = .TRUE.
         END IF
      END IF
      IF (.NOT. update_veg_surf(ivGrass)) THEN
         IF (.NOT. is_missing_sentinel(LAIMin(ivGrass)) .AND. &
             .NOT. is_missing_sentinel(LAImax(ivGrass)) .AND. &
             .NOT. is_missing_sentinel(AlbMin_Grass) .AND. &
             .NOT. is_missing_sentinel(AlbMax_Grass) .AND. &
             .NOT. is_missing_sentinel(LAI_id(ivGrass)) .AND. &
             .NOT. is_missing_sentinel(LAI_id_prev(ivGrass))) THEN
            update_veg_surf(ivGrass) = .TRUE.
         END IF
      END IF

      ! Use nested IF blocks (Fortran does not guarantee short-circuiting of
      ! .AND.) so absent vegetation surfaces with poisoned bounds never evaluate
      ! NaN comparisons or arithmetic (#2363). Active vegetation updates remain
      ! enabled for both observed and modelled LAI.
      iv = ivDecid
      IF (update_veg_surf(iv)) THEN
         IF (ieee_is_finite(LAI_id(iv)) .AND. ieee_is_finite(LAI_id_prev(iv))) THEN
            IF ((LAI_id(iv) - LAI_id_prev(iv)) /= 0.0D0) THEN
               IF (ieee_is_finite(LAIMin(iv)) .AND. ieee_is_finite(LAImax(iv))) THEN
                  IF (LAIMin(iv) >= 0.0D0 .AND. (LAImax(iv) - LAIMin(iv)) > 0.0D0) THEN
                     deltaLAIDecTr = (LAI_id(iv) - LAI_id_prev(iv))/(LAImax(iv) - LAIMin(iv))
                     albChangeDecTr = (AlbMax_DecTr - AlbMin_DecTr)*deltaLAIDecTr
                     CapChange = (CapMin_dec - CapMax_dec)*deltaLAIDecTr
                     porChange = (PorMin_dec - PorMax_dec)*deltaLAIDecTr
                  END IF
               END IF
            END IF
         END IF
      END IF

      iv = ivConif
      IF (update_veg_surf(iv)) THEN
         IF (ieee_is_finite(LAI_id(iv)) .AND. ieee_is_finite(LAI_id_prev(iv))) THEN
            IF ((LAI_id(iv) - LAI_id_prev(iv)) /= 0.0D0) THEN
               IF (ieee_is_finite(LAIMin(iv)) .AND. ieee_is_finite(LAImax(iv))) THEN
                  IF (LAIMin(iv) >= 0.0D0 .AND. (LAImax(iv) - LAIMin(iv)) > 0.0D0) THEN
                     deltaLAIEveTr = (LAI_id(iv) - LAI_id_prev(iv))/(LAImax(iv) - LAIMin(iv))
                     albChangeEveTr = (AlbMax_EveTr - AlbMin_EveTr)*deltaLAIEveTr
                  END IF
               END IF
            END IF
         END IF
      END IF

      iv = ivGrass
      IF (update_veg_surf(iv)) THEN
         IF (ieee_is_finite(LAI_id(iv)) .AND. ieee_is_finite(LAI_id_prev(iv))) THEN
            IF ((LAI_id(iv) - LAI_id_prev(iv)) /= 0.0D0) THEN
               IF (ieee_is_finite(LAIMin(iv)) .AND. ieee_is_finite(LAImax(iv))) THEN
                  IF (LAIMin(iv) >= 0.0D0 .AND. (LAImax(iv) - LAIMin(iv)) > 0.0D0) THEN
                     deltaLAIGrass = (LAI_id(iv) - LAI_id_prev(iv))/(LAImax(iv) - LAIMin(iv))
                     albChangeGrass = (AlbMax_Grass - AlbMin_Grass)*deltaLAIGrass
                  END IF
               END IF
            END IF
         END IF
      END IF

      IF (update_veg_surf(ivDecid)) THEN
         DecidCap_id = DecidCap_id - CapChange
         StoreDrainPrm(6, DecidSurf) = DecidCap_id !Change current storage capacity of deciduous trees
         porosity_id = porosity_id + porChange !- changed to + by HCW 20 Aug 2015 (porosity greatest when LAI smallest)
         porosity_id = MIN(MAX(porosity_id, MAX(PorMin_dec, 0.1)), MIN(PorMax_dec, 0.9)) ! limit porosity to valid range, TS 26 Jun 2023

         ! update albedo values while limiting these to valid ranges
         albDecTr_id = MIN(MAX(albDecTr_id + albChangeDecTr, AlbMin_DecTr), AlbMax_DecTr)
      END IF
      IF (update_veg_surf(ivConif)) THEN
         albEveTr_id = MIN(MAX(albEveTr_id + albChangeEveTr, AlbMin_EveTr), AlbMax_EveTr)
      END IF
      IF (update_veg_surf(ivGrass)) THEN
         albGrass_id = MIN(MAX(albGrass_id + albChangeGrass, AlbMin_Grass), AlbMax_Grass)
      END IF

   END SUBROUTINE update_Veg

   PURE LOGICAL FUNCTION is_in_irrigation_season(doy, lat, ie_start, ie_end) RESULT(in_season)
      ! Inclusive irrigation season gate matching documented Ie_start / Ie_end (gh#2381):
      ! - Both bounds zero (ie_start == 0 .AND. ie_end == 0): window disabled (gh#2329).
      ! - Northern Hemisphere (lat >= 0): [ie_start, ie_end] (closed all year if ie_start > ie_end).
      ! - Southern Hemisphere (lat < 0): [ie_start, ie_end] when ie_start <= ie_end (e.g. 1..366),
      !   or inclusive wrap-around across New Year (doy >= ie_start .OR. doy <= ie_end) when ie_start > ie_end.
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: doy, ie_start, ie_end
      REAL(KIND(1D0)), INTENT(IN) :: lat

      IF (ie_start == 0 .AND. ie_end == 0) THEN
         in_season = .FALSE.
      ELSE IF (lat >= 0.0D0) THEN
         in_season = (doy >= ie_start .AND. doy <= ie_end)
      ELSE IF (ie_start <= ie_end) THEN
         in_season = (doy >= ie_start .AND. doy <= ie_end)
      ELSE
         in_season = (doy >= ie_start .OR. doy <= ie_end)
      END IF
   END FUNCTION is_in_irrigation_season

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
      REAL(KIND(1D0)), DIMENSION(NDAYS_WEEK), INTENT(IN) :: DayWatPer ! Fraction of properties following daily water (DOW_SUNDAY..DOW_SATURDAY) [-]
      REAL(KIND(1D0)), DIMENSION(NDAYS_WEEK), INTENT(IN) :: DayWat ! Days of watering allowed (DOW_SUNDAY..DOW_SATURDAY) [-]

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

      INTEGER :: wd ! Day of week index (DOW_SUNDAY..DOW_SATURDAY)
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
            IF (is_in_irrigation_season(id, lat, Ie_start, Ie_end)) THEN
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
               ! WUDay_A already includes the ponding demand WUDay_P (gh#1864)
               WUDay_total = WUDay_A + WUDay_M

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
      dt_since_start, elapsed_sec_of_day, tstep, & !input
      HDD_id) !inout
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: dt_since_start, elapsed_sec_of_day, tstep

      REAL(KIND(1D0)), DIMENSION(12), INTENT(INOUT) :: HDD_id
      ! REAL(KIND(1d0)),DIMENSION(6),INTENT(OUT):: HDD_id_use

      INTEGER :: days_prev
      INTEGER :: steps_in_day
      REAL(KIND(1D0)) :: tstepcount

      ! Count of timesteps accumulated in this right-closed day (D 00:00, D+1 00:00]:
      ! ceiling(elapsed_sec_of_day / tstep) gives the full-day step count across
      ! both midnight-aligned (e.g. 86400/300 = 288, 86400/3600 = 24) and offset
      ! grids (e.g. 00:02..23:57 -> ceiling(86220/300) = 288), capped by
      ! dt_since_start/tstep on a partial first day (including a run starting at
      ! 00:00 where dt_since_start/tstep == 1) (gh#1878, gh#1919).
      steps_in_day = MAX(1, MIN((elapsed_sec_of_day + tstep - 1)/tstep, dt_since_start/tstep))
      tstepcount = REAL(steps_in_day, KIND(1D0))
      ! Heating degree days (HDD) -------------
      HDD_id(1) = HDD_id(1)/tstepcount !Heating
      HDD_id(2) = HDD_id(2)/tstepcount !Cooling
      HDD_id(3) = HDD_id(3)/tstepcount !Average temp

      ! Calculate a quasi-5-day-running-mean temp:
      ! Count completed prior days using (dt_since_start - tstep) so Day 1
      ! (dt_since_start == sec_per_day for a full 00:05..00:00 day) has
      ! days_prev == 0 and does not blend with the initial HDD_id(4).
      days_prev = MIN(4, & ! >= 4 prior days
                      MAX(0, FLOOR(REAL(dt_since_start - tstep, KIND(1D0))/sec_per_day)))
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
      first_tstep_Q, & !input
      HDD_id) !output
      IMPLICIT NONE
      LOGICAL, INTENT(IN) :: first_tstep_Q ! first step of a right-closed day or of the run (gh#1919)

      REAL(KIND(1D0)), DIMENSION(6), INTENT(INOUT) :: HDD_id
      REAL(KIND(1D0)) :: HDD_id_mav, HDD_id_daysSR

      ! reset HDD_id to ZERO except for:
      ! 5-day moving average
      HDD_id_mav = HDD_id(4)
      ! Days Since Rain
      HDD_id_daysSR = HDD_id(6)
      IF (first_tstep_Q) THEN
         HDD_id = 0
         HDD_id(4) = HDD_id_mav
         HDD_id(6) = HDD_id_daysSR
      END IF

   END SUBROUTINE update_DailyState_Start

   ! transfer results to a one-line output for SUEWS_cal_DailyState
   SUBROUTINE update_DailyStateLine( &
      clock, config, forcing, siteInfo, & ! input
      modState, & ! input/output:
      DailyStateLine) !out

      USE module_ctrl_type, ONLY: &
         SUEWS_SITE, SUEWS_CLOCK, SUEWS_CONFIG, SUEWS_FORCING, &
         PHENOLOGY_STATE, anthroEmis_STATE, &
         SNOW_STATE, HYDRO_STATE, &
         OHM_STATE, &
         SUEWS_STATE

      IMPLICIT NONE

      TYPE(SUEWS_CLOCK), INTENT(IN) :: clock
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
            a3_bldg => ohmState%a3_bldg &
            )

            ! initialise DailyStateLine
            DailyStateLine = -999
            ! Emit day D's DailyState on its closing timestep (D+1 00:00 on
            ! midnight-aligned grids): the same clock predicate DailyState uses,
            ! so the two cannot disagree (gh#2333, gh#2382).
            IF (clock%last_step_of_day) THEN
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
