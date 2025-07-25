MODULE DailyState_module
   USE allocateArray, ONLY: &
      ndays, nsurf, nvegsurf, ivConif, ivDecid, ivGrass, DecidSurf, ncolumnsDataOutDailyState

   IMPLICIT NONE
   ! INTEGER,PARAMETER::ndays=366
   ! INTEGER,PARAMETER::nvegsurf=3
   ! INTEGER,PARAMETER::ncolumnsDataOutDailyState=46

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

      ! USE Snow_module, ONLY: SnowUpdate
      USE datetime_module, ONLY: datetime, timedelta
      USE SUEWS_DEF_DTS, ONLY: SUEWS_SITE, SUEWS_TIMER, SUEWS_FORCING, anthroEMIS_PRM, &
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

      ! INTEGER :: WaterUseMethod
      INTEGER, PARAMETER :: BaseTMethod = 2 ! base t method [-]
      REAL(KIND(1D0)), PARAMETER :: BaseT_HC = 18.2 !base temperature for heating degree dayb [degC] ! to be fully removed TODO

      ! TYPE(IRRIGATION_PRM), INTENT(IN) :: irrPrm
      ! INTEGER :: Ie_start !Starting time of water use (DOY)
      ! INTEGER :: Ie_end !Ending time of water use (DOY)
      ! REAL(KIND(1D0)) :: Faut

      INTEGER, PARAMETER :: LAICalcYes = 1 ! boolean to determine if calculate LAI [-]

      ! INTEGER :: dectrLAIType
      ! INTEGER :: evetrLAIType
      ! INTEGER :: grassLAIType
      ! INTEGER, DIMENSION(nvegsurf) :: LAIType !LAI equation to use: original (0) or new (1)

      ! REAL(KIND(1D0)), INTENT(IN) :: nsh_real
      ! REAL(KIND(1D0)) :: avkdn
      ! REAL(KIND(1D0)) :: Temp_C
      ! REAL(KIND(1D0)) :: Precip
      ! REAL(KIND(1D0)) :: LAI_obs

      ! TYPE(anthroEMIS_PRM), INTENT(IN) :: ahemisPrm
      ! REAL(KIND(1D0)) :: BaseT_Heating_working
      ! REAL(KIND(1D0)) :: BaseT_Heating_holiday
      REAL(KIND(1D0)), DIMENSION(2) :: BaseT_Heating

      ! REAL(KIND(1D0)) :: BaseT_Cooling_working
      ! REAL(KIND(1D0)) :: BaseT_Cooling_holiday
      ! REAL(KIND(1D0)), DIMENSION(2) :: BaseT_Cooling

      ! REAL(KIND(1D0)) :: lat

      ! REAL(KIND(1D0)), INTENT(IN)::tau_a
      ! REAL(KIND(1D0)), INTENT(IN)::tau_f
      ! REAL(KIND(1D0)), INTENT(IN)::tau_r
      ! REAL(KIND(1D0)), INTENT(IN)::SnowDensMax
      ! REAL(KIND(1D0)), INTENT(IN)::SnowDensMin
      ! REAL(KIND(1D0)), INTENT(in)::SnowAlbMax
      ! REAL(KIND(1D0)), INTENT(IN)::SnowAlbMin
      ! REAL(KIND(1D0)) :: AlbMax_EveTr
      ! REAL(KIND(1D0)) :: AlbMax_DecTr
      ! REAL(KIND(1D0)) :: AlbMax_Grass
      ! REAL(KIND(1D0)) :: AlbMin_EveTr
      ! REAL(KIND(1D0)) :: AlbMin_DecTr
      ! REAL(KIND(1D0)) :: AlbMin_Grass
      ! REAL(KIND(1D0)) :: CapMax_dec
      ! REAL(KIND(1D0)) :: CapMin_dec
      ! REAL(KIND(1D0)) :: PorMax_dec
      ! REAL(KIND(1D0)) :: PorMin_dec
      ! REAL(KIND(1d0)),INTENT(IN) ::VegPhenLumps

      ! REAL(KIND(1D0)), DIMENSION(3) :: Ie_a
      ! REAL(KIND(1D0)), DIMENSION(3) :: Ie_m !Coefficients for automatic and manual irrigation models

      ! REAL(KIND(1D0)) :: DayWatPer_mon
      ! REAL(KIND(1D0)) :: DayWatPer_tues
      ! REAL(KIND(1D0)) :: DayWatPer_wed
      ! REAL(KIND(1D0)) :: DayWatPer_thur
      ! REAL(KIND(1D0)) :: DayWatPer_fri
      ! REAL(KIND(1D0)) :: DayWatPer_sat
      ! REAL(KIND(1D0)) :: DayWatPer_sun
      ! REAL(KIND(1D0)), DIMENSION(7) :: DayWatPer !% of houses following daily water

      ! REAL(KIND(1D0)) :: DayWat_mon
      ! REAL(KIND(1D0)) :: DayWat_tues
      ! REAL(KIND(1D0)) :: DayWat_wed
      ! REAL(KIND(1D0)) :: DayWat_thur
      ! REAL(KIND(1D0)) :: DayWat_fri
      ! REAL(KIND(1D0)) :: DayWat_sat
      ! REAL(KIND(1D0)) :: DayWat_sun
      ! REAL(KIND(1D0)), DIMENSION(7) :: DayWat !Days of watering allowed

      ! ponding-water related
      ! REAL(KIND(1D0)) :: H_maintain ! ponding water depth to maintain [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: state_id ! surface wetness [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_id ! soil water store [mm]
      ! REAL(KIND(1D0)), DIMENSION(nsurf) :: SoilStoreCap !Capacity of soil store for each surface [mm]

      ! REAL(KIND(1d0)), DIMENSION(nsurf), INTENT(IN)      ::SnowPack
      ! REAL(KIND(1D0)) :: evetrBaseT
      ! REAL(KIND(1D0)) :: dectrBaseT
      ! REAL(KIND(1D0)) :: grassBaseT
      ! REAL(KIND(1D0)), DIMENSION(nvegsurf) :: BaseT !Base temperature for growing degree days [degC]

      ! REAL(KIND(1D0)) :: evetrBaseTe
      ! REAL(KIND(1D0)) :: dectrBaseTe
      ! REAL(KIND(1D0)) :: grassBaseTe
      ! REAL(KIND(1D0)), DIMENSION(nvegsurf) :: BaseTe !Base temperature for senescence degree days [degC]

      ! REAL(KIND(1D0)) :: evetrGDDFull
      ! REAL(KIND(1D0)) :: dectrGDDFull
      ! REAL(KIND(1D0)) :: grassGDDFull
      ! REAL(KIND(1D0)), DIMENSION(nvegsurf) :: GDDFull !Growing degree days needed for full capacity [degC]

      ! REAL(KIND(1D0)) :: evetrSDDFull
      ! REAL(KIND(1D0)) :: dectrSDDFull
      ! REAL(KIND(1D0)) :: grassSDDFull
      ! REAL(KIND(1D0)), DIMENSION(nvegsurf) :: SDDFull !Senescence degree days needed to initiate leaf off [degC]

      ! REAL(KIND(1D0)) :: evetrLAIMin
      ! REAL(KIND(1D0)) :: dectrLAIMin
      ! REAL(KIND(1D0)) :: grassLAIMin
      ! REAL(KIND(1D0)), DIMENSION(nvegsurf) :: LAIMin !Min LAI [m2 m-2]

      ! REAL(KIND(1D0)) :: evetrLAIMax
      ! REAL(KIND(1D0)) :: dectrLAIMax
      ! REAL(KIND(1D0)) :: grassLAIMax
      ! REAL(KIND(1D0)), DIMENSION(nvegsurf) :: LAIMax !Max LAI [m2 m-2]

      ! REAL(KIND(1D0)), DIMENSION(4) :: evetrLAIPower
      ! REAL(KIND(1D0)), DIMENSION(4) :: dectrLAIPower
      ! REAL(KIND(1D0)), DIMENSION(4) :: grassLAIPower
      REAL(KIND(1D0)), DIMENSION(4, nvegsurf) :: LAIPower !Coeffs for LAI equation: 1,2 - leaf growth; 3,4 - leaf off

      ! REAL(KIND(1d0)), INTENT(INOUT)::SnowAlb
      ! TYPE(PHENOLOGY_STATE), INTENT(INOUT) :: phenState
      TYPE(PHENOLOGY_STATE) :: phenState_prev
      ! Growing Degree Days
      ! REAL(KIND(1D0)), DIMENSION(3) :: GDD_id ! Growing Degree Days (see SUEWS_DailyState.f95)
      ! REAL(KIND(1D0)), DIMENSION(3) :: GDD_id_prev ! Growing Degree Days (see SUEWS_DailyState.f95)
      !REAL(KIND(1D0)), DIMENSION(3) :: GDD_id_next ! Growing Degree Days (see SUEWS_DailyState.f95)

      ! Senescence Degree Days
      ! REAL(KIND(1D0)), DIMENSION(3) :: SDD_id ! Senescence Degree Days (see SUEWS_DailyState.f95)
      ! REAL(KIND(1D0)), DIMENSION(3) :: SDD_id_prev ! Senescence Degree Days (see SUEWS_DailyState.f95)
      !REAL(KIND(1D0)), DIMENSION(3), INTENT(OUT) :: SDD_id_next ! Senescence Degree Days (see SUEWS_DailyState.f95)

      ! Daily min temp [degC]
      ! REAL(KIND(1D0)) :: Tmin_id
      ! REAL(KIND(1D0)) :: Tmin_id_prev
      !REAL(KIND(1D0)), INTENT(out) :: Tmin_id_next

      ! Daily max temp [degC]
      ! REAL(KIND(1D0)) :: Tmax_id
      ! REAL(KIND(1D0)) :: Tmax_id_prev
      !REAL(KIND(1D0)), INTENT(out) :: Tmax_id_next

      ! Daytime hours [h]
      ! REAL(KIND(1D0)) :: lenDay_id
      ! REAL(KIND(1D0)) :: lenDay_id_prev
      !REAL(KIND(1D0)), INTENT(out) :: lenDay_id_next

      ! LAI for each veg surface [m2 m-2]
      ! REAL(KIND(1D0)), DIMENSION(3) :: LAI_id ! LAI for each veg surface [m2 m-2]
      ! REAL(KIND(1D0)), DIMENSION(3) :: LAI_id_prev ! LAI for each veg surface [m2 m-2]
      !REAL(KIND(1D0)), DIMENSION(3), INTENT(OUT) :: LAI_id_next ! LAI for each veg surface [m2 m-2]

      ! ------------- Key to daily arrays ----------------------------------------------
      ! TS, 27 Dec 2018: updated the annotation for 2018b and WRF-SUEWS coupling

      ! Heating Degree Days
      ! TYPE(anthroEmis_STATE), INTENT(INout) :: anthroEmisState
      ! TYPE(anthroEmis_STATE) :: anthroEmisState_prev
      ! REAL(KIND(1D0)), DIMENSION(12) :: HDD_id ! Heating Degree Days (see SUEWS_DailyState.f95)
      ! REAL(KIND(1D0)), DIMENSION(12) :: HDD_id_prev ! Heating Degree Days (see SUEWS_DailyState.f95)
      !REAL(KIND(1D0)), DIMENSION(12), INTENT(OUT) :: HDD_id_next ! Heating Degree Days (see SUEWS_DailyState.f95)
      ! HDD_id:
      ! first half used for update through the day
      ! HDD_id(1) ---- Heating [degC]: used for accumulation during calculation
      ! HDD_id(2) ---- Cooling [degC]: used for accumulation during calculation
      ! HDD_id(3) ---- Daily mean temp [degC]: used for accumulation during calculation
      ! HDD_id(4) ---- 5-day running mean temp [degC]: used for actual calculation
      ! HDD_id(5) ---- Daily precip total [mm]
      ! HDD_id(6) ---- Days since rain [d]
      ! second half used for storage of the first half for the prevous day
      ! HDD_id(6+1) ---- Heating [degC]: used for accumulation during calculation
      ! HDD_id(6+2) ---- Cooling [degC]: used for accumulation during calculation
      ! HDD_id(6+3) ---- Daily mean temp [degC]: used for accumulation during calculation
      ! HDD_id(6+4) ---- 5-day running mean temp [degC]: used for actual calculation
      ! HDD_id(6+5) ---- Daily precip total [mm]
      ! HDD_id(6+6) ---- Days since rain [d]
      ! --------------------------------------------------------------------------------

      ! --------------------------------------------------------------------------------
      !Daily water use for EveTr, DecTr, Grass [mm] (see SUEWS_DailyState.f95)
      ! TYPE(HYDRO_STATE), INTENT(IN) :: hydroState
      ! TYPE(HYDRO_STATE), INTENT(inout) :: hydroState
      ! REAL(KIND(1D0)), DIMENSION(9), INTENT(OUT) :: WUDay_id ! Water use related array
      ! WUDay_id:
      ! WUDay_id(1) - Daily water use total for Irr EveTr (automatic+manual) [mm]
      ! WUDay_id(2) - Automatic irrigation for Irr EveTr [mm]
      ! WUDay_id(3) - Manual irrigation for Irr EveTr [mm]
      ! WUDay_id(4) - Daily water use total for Irr DecTr (automatic+manual) [mm]
      ! WUDay_id(5) - Automatic irrigation for Irr DecTr [mm]
      ! WUDay_id(6) - Manual irrigation for Irr DecTr [mm]
      ! WUDay_id(7) - Daily water use total for Irr Grass (automatic+manual) [mm]
      ! WUDay_id(8) - Automatic irrigation for Irr Grass [mm]
      ! WUDay_id(9) - Manual irrigation for Irr Grass [mm]
      ! --------------------------------------------------------------------------------

      ! REAL(KIND(1d0)), DIMENSION(nsurf), INTENT(INOUT)::SnowDens
      ! INTEGER, DIMENSION(3), INTENT(in) :: DayofWeek_id

      ! REAL(KIND(1D0)), INTENT(OUT) :: deltaLAI

      ! REAL(KIND(1D0)) :: DecidCap_id
      ! REAL(KIND(1D0)) :: DecidCap_id_prev
      !REAL(KIND(1D0)), INTENT(OUT) :: DecidCap_id_next
      ! REAL(KIND(1D0)) :: albDecTr_id
      ! REAL(KIND(1D0)) :: albDecTr_id_prev
      ! REAL(KIND(1D0)), INTENT(OUT) :: albDecTr_id_next
      ! REAL(KIND(1D0)) :: albEveTr_id
      ! REAL(KIND(1D0)) :: albEveTr_id_prev
      !REAL(KIND(1D0)), INTENT(OUT) :: albEveTr_id_next
      ! REAL(KIND(1D0)) :: albGrass_id
      ! REAL(KIND(1D0)) :: albGrass_id_prev
      !REAL(KIND(1D0)), INTENT(OUT) :: albGrass_id_next
      ! REAL(KIND(1D0)) :: porosity_id
      ! REAL(KIND(1D0)), INTENT(INOUT) :: porosity_id_prev
      ! REAL(KIND(1D0)) :: porosity_id_prev
      !REAL(KIND(1D0)), INTENT(INOUT) :: porosity_id_next
      ! REAL(KIND(1D0)), DIMENSION(6, nsurf) :: StoreDrainPrm
      ! REAL(KIND(1D0)), DIMENSION(6, nsurf) :: StoreDrainPrm_prev
      !REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(out) :: StoreDrainPrm_next

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
            LAI_obs => forcing%LAI_obs, &
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
               BaseT_Heating => [ahemisPrm%anthroheat%BaseT_Heating_working, &
                                 ahemisPrm%anthroheat%BaseT_Heating_holiday], &
               BaseT_Cooling => [ahemisPrm%anthroheat%BaseT_Cooling_working, &
                                 ahemisPrm%anthroheat%BaseT_Cooling_holiday], &
               Tmin_id_prev => phenState_prev%Tmin_id, &
               Tmax_id_prev => phenState_prev%Tmax_id, &
               lenDay_id_prev => phenState_prev%lenDay_id, &
               DecidCap_id_prev => phenState_prev%DecidCap_id, &
               StoreDrainPrm_prev => phenState_prev%StoreDrainPrm, &
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
               StoreDrainPrm => phenState%StoreDrainPrm, &
               LAI_id => phenState%LAI_id, &
               GDD_id => phenState%GDD_id, &
               SDD_id => phenState%SDD_id, &
               albDecTr_id => phenState%albDecTr_id, &
               albEveTr_id => phenState%albEveTr_id, &
               albGrass_id => phenState%albGrass_id, &
               porosity_id => phenState%porosity_id, &
               HDD_id => anthroEmisState%HDD_id, &
               state_surf => hydroState%state_surf, &
               soilstore_surf => hydroState%soilstore_surf, &
               WUDay_id => hydroState%WUDay_id, &
               WaterUseMethod => config%WaterUseMethod, &
               Ie_start => irrPrm%Ie_start, &
               Ie_end => irrPrm%Ie_end, &
               Faut => irrPrm%Faut, &
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
               evetrLAIPower => evetrPrm%lai%laipower, &
               AlbMax_DecTr => dectrPrm%Alb_Max, &
               AlbMin_DecTr => dectrPrm%Alb_Min, &
               CapMax_dec => dectrPrm%CapMax_dec, &
               CapMin_dec => dectrPrm%CapMin_dec, &
               PorMax_dec => dectrPrm%PorMax_dec, &
               PorMin_dec => dectrPrm%PorMin_dec, &
               dectrLAIPower => dectrPrm%lai%laipower, &
               AlbMax_Grass => grassPrm%Alb_Max, &
               AlbMin_Grass => grassPrm%Alb_Min, &
               LAIType => [evetrPrm%lai%laitype, &
                           dectrPrm%lai%laitype, &
                           grassPrm%lai%laitype], &
               BaseT => [evetrPrm%lai%BaseT, &
                         dectrPrm%lai%BaseT, &
                         grassPrm%lai%BaseT], &
               BaseTe => [evetrPrm%lai%BaseTe, &
                          dectrPrm%lai%BaseTe, &
                          grassPrm%lai%BaseTe], &
               GDDFull => [evetrPrm%lai%gddfull, &
                           dectrPrm%lai%gddfull, &
                           grassPrm%lai%gddfull], &
               SDDFull => [evetrPrm%lai%sddfull, &
                           dectrPrm%lai%sddfull, &
                           grassPrm%lai%sddfull], &
               LAIMin => [evetrPrm%lai%laimin, &
                          dectrPrm%lai%laimin, &
                          grassPrm%lai%laimin], &
               LAIMax => [evetrPrm%lai%laimax, &
                          dectrPrm%lai%laimax, &
                          grassPrm%lai%laimax], &
               SoilStoreCap => [pavedPrm%soil%soilstorecap, &
                                bldgPrm%soil%soilstorecap, &
                                evetrPrm%soil%soilstorecap, &
                                dectrPrm%soil%soilstorecap, &
                                grassPrm%soil%soilstorecap, &
                                bsoilPrm%soil%soilstorecap, &
                                waterPrm%soil%soilstorecap], &
               grassLAIPower => grassPrm%lai%laipower &
               )

               ! before
               ! Set the flag based on i_iter
               ! execute_subroutines = (i_iter == 1)
               execute_subroutines = .TRUE.

               LAIPower(:, 1) = evetrLAIPower
               LAIPower(:, 2) = dectrLAIPower
               LAIPower(:, 3) = grassLAIPower

               ! transfer values
               ! LAI_id = LAI_id_prev
               ! GDD_id = GDD_id_prev
               ! SDD_id = SDD_id_prev
               ! Tmin_id = Tmin_id_prev
               ! Tmax_id = Tmax_id_prev
               ! lenDay_id = lenDay_id_prev
               ! StoreDrainPrm = StoreDrainPrm_prev
               ! DecidCap_id = DecidCap_id_prev
               ! albDecTr_id = albDecTr_id_prev
               ! albEveTr_id = albEveTr_id_prev
               ! albGrass_id = albGrass_id_prev
               ! porosity_id = porosity_id_prev
               ! HDD_id = HDD_id_prev

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
               IF (config%localClimateMethod == 1) THEN
                  Tair = atmState%t2_C
               ELSE IF (config%localClimateMethod == 2) THEN
                  Tair = atmState%T_hbh_C
               ELSE
                  Tair = Temp_C
               END IF

               ! regular update at all timesteps of a day
               IF (execute_subroutines) THEN
                  CALL update_DailyState_Day( &
                     BaseTMethod, &
                     DayofWeek_id, &
                     avkdn, & !input
                     Tair, &
                     Precip, &
                     BaseT_HC, &
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

                  ! PRINT*, ''
                  ! PRINT*, 'WUDay(id)',WUDay(id,:)
                  ! PRINT*, 'WUDay_id after update_WaterUse',WUDay_id

                  !------------------------------------------------------------------------------
                  ! Calculation of LAI from growing degree days
                  ! This was revised and checked on 16 Feb 2014 by LJ
                  !------------------------------------------------------------------------------
                  ! save initial LAI_id
                  ! LAI_id_in = LAI_id

                  IF (execute_subroutines) THEN
                     CALL update_GDDLAI( &
                        id, LAICalcYes, & !input
                        lat, LAI_obs, &
                        Tmin_id, Tmax_id, lenDay_id, &
                        BaseT, BaseTe, &
                        GDDFull, SDDFull, &
                        LAIMin, LAIMax, LAIPower, LAIType, &
                        LAI_id_prev, &
                        GDD_id, SDD_id, & !inout
                        LAI_id) !output

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

               ! translate values back
               ! phenState%LAI_id = LAI_id
               ! phenState%GDD_id = GDD_id
               ! phenState%SDD_id = SDD_id
               ! phenState%Tmin_id = Tmin_id
               ! phenState%Tmax_id = Tmax_id
               ! phenState%lenDay_id = lenDay_id
               ! phenState%StoreDrainPrm = StoreDrainPrm
               ! phenState%DecidCap_id = DecidCap_id
               ! phenState%albDecTr_id = albDecTr_id
               ! phenState%albEveTr_id = albEveTr_id
               ! phenState%albGrass_id = albGrass_id
               ! phenState%porosity_id = porosity_id
               ! anthroEmisState%HDD_id = HDD_id
               ! PRINT*, 'after_DailyState', iy,id,it,imin
               ! PRINT*, 'HDD(id)', HDD(id,:)
               ! PRINT*, 'HDD_id', HDD_id

               ! RETURN

            END ASSOCIATE
         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE SUEWS_cal_DailyState

   ! SUBROUTINE update_DailyState_End( &
   !    id, it, imin, tstep, dt_since_start, & !input
   !    Tmin_id, Tmax_id, lenDay_id, &
   !    LAIType, Ie_end, Ie_start, LAICalcYes, &
   !    WaterUseMethod, DayofWeek_id, &
   !    AlbMax_DecTr, AlbMax_EveTr, AlbMax_Grass, AlbMin_DecTr, AlbMin_EveTr, AlbMin_Grass, &
   !    BaseT, BaseTe, CapMax_dec, CapMin_dec, DayWat, DayWatPer, Faut, GDDFull, &
   !    Ie_a, Ie_m, LAIMax, LAIMin, LAIPower, lat, PorMax_dec, PorMin_dec, SDDFull, LAI_obs, &
   !    state_id, soilstore_id, SoilStoreCap, H_maintain, & !input
   !    GDD_id, SDD_id, & !inout
   !    HDD_id, &
   !    LAI_id, &
   !    DecidCap_id, &
   !    albDecTr_id, &
   !    albEveTr_id, &
   !    albGrass_id, &
   !    porosity_id, &
   !    StoreDrainPrm, &
   !    WUDay_id) !output
   !    IMPLICIT NONE

   !    INTEGER, INTENT(IN) :: id
   !    INTEGER, INTENT(IN) :: it
   !    INTEGER, INTENT(IN) :: imin
   !    INTEGER, INTENT(IN) :: tstep
   !    INTEGER, INTENT(IN) :: dt_since_start
   !    INTEGER, INTENT(IN) :: LAIType(nvegsurf)
   !    INTEGER, INTENT(IN) :: Ie_end
   !    INTEGER, INTENT(IN) :: Ie_start
   !    INTEGER, INTENT(IN) :: LAICalcYes
   !    INTEGER, INTENT(IN) :: WaterUseMethod
   !    INTEGER, INTENT(in) :: DayofWeek_id(3)

   !    REAL(KIND(1D0)), INTENT(IN) :: AlbMax_DecTr
   !    REAL(KIND(1D0)), INTENT(IN) :: AlbMax_EveTr
   !    REAL(KIND(1D0)), INTENT(IN) :: AlbMax_Grass
   !    REAL(KIND(1D0)), INTENT(IN) :: AlbMin_DecTr
   !    REAL(KIND(1D0)), INTENT(IN) :: AlbMin_EveTr
   !    REAL(KIND(1D0)), INTENT(IN) :: AlbMin_Grass
   !    REAL(KIND(1D0)), INTENT(IN) :: BaseT(nvegsurf)
   !    REAL(KIND(1D0)), INTENT(IN) :: BaseTe(nvegsurf)
   !    REAL(KIND(1D0)), INTENT(IN) :: CapMax_dec
   !    REAL(KIND(1D0)), INTENT(IN) :: CapMin_dec
   !    REAL(KIND(1D0)) :: DayWat(7)
   !    REAL(KIND(1D0)) :: DayWatPer(7)
   !    REAL(KIND(1D0)), INTENT(IN) :: Faut
   !    REAL(KIND(1D0)), INTENT(IN) :: GDDFull(nvegsurf)
   !    REAL(KIND(1D0)), INTENT(IN) :: Ie_a(3)
   !    REAL(KIND(1D0)), INTENT(IN) :: Ie_m(3)
   !    REAL(KIND(1D0)), INTENT(IN) :: LAIMax(nvegsurf)
   !    REAL(KIND(1D0)), INTENT(IN) :: LAIMin(nvegsurf)
   !    REAL(KIND(1D0)), INTENT(IN) :: LAIPower(4, nvegsurf)
   !    REAL(KIND(1D0)), INTENT(IN) :: lat
   !    REAL(KIND(1D0)), INTENT(IN) :: PorMax_dec
   !    REAL(KIND(1D0)), INTENT(IN) :: PorMin_dec
   !    REAL(KIND(1D0)), INTENT(IN) :: SDDFull(nvegsurf)
   !    REAL(KIND(1D0)), INTENT(IN) :: LAI_obs
   !    REAL(KIND(1D0)), INTENT(IN) :: Tmin_id
   !    REAL(KIND(1D0)), INTENT(IN) :: Tmax_id
   !    REAL(KIND(1D0)), INTENT(IN) :: lenDay_id
   !    REAL(KIND(1D0)), INTENT(IN) :: H_maintain ! ponding water depth to maintain
   !    REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: state_id ! surface wetness [mm]
   !    REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: soilstore_id ! soil water store [mm]
   !    REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: SoilStoreCap !Capacity of soil store for each surface [mm]

   !    REAL(KIND(1D0)), DIMENSION(3), INTENT(INOUT) :: GDD_id ! Growing Degree Days (see SUEWS_DailyState.f95)
   !    REAL(KIND(1D0)), DIMENSION(3), INTENT(INOUT) :: SDD_id ! Senescence Degree Days (see SUEWS_DailyState.f95)
   !    REAL(KIND(1D0)), DIMENSION(12), INTENT(INOUT) :: HDD_id
   !    REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: LAI_id ! LAI for each veg surface [m2 m-2]

   !    ! REAL(KIND(1d0)),DIMENSION(6),INTENT(INOUT)::HDD_id_use ! HDD of previous day
   !    REAL(KIND(1D0)), DIMENSION(nvegsurf) :: LAI_id_in ! LAI of previous day

   !    REAL(KIND(1D0)), DIMENSION(9), INTENT(OUT) :: WUDay_id

   !    REAL(KIND(1D0)), INTENT(INOUT) :: DecidCap_id
   !    REAL(KIND(1D0)), INTENT(INOUT) :: albDecTr_id
   !    REAL(KIND(1D0)), INTENT(INOUT) :: albEveTr_id
   !    REAL(KIND(1D0)), INTENT(INOUT) :: albGrass_id
   !    REAL(KIND(1D0)), INTENT(INOUT) :: porosity_id

   !    REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(inout) :: StoreDrainPrm

   !    ! Calculate heating degree days ------------------------------------------
   !    CALL update_HDD( &
   !       dt_since_start, it, imin, tstep, & !input
   !       HDD_id) !inout

   !    ! Calculate modelled daily water use ------------------------------------------
   !    CALL update_WaterUse( &
   !       id, WaterUseMethod, DayofWeek_id, lat, Faut, HDD_id, & !input
   !       state_id, soilstore_id, SoilStoreCap, H_maintain, & !input
   !       Ie_a, Ie_m, Ie_start, Ie_end, DayWatPer, DayWat, &
   !       WUDay_id) !output

   !    ! PRINT*, ''
   !    ! PRINT*, 'WUDay(id)',WUDay(id,:)
   !    ! PRINT*, 'WUDay_id',WUDay_id

   !    !------------------------------------------------------------------------------
   !    ! Calculation of LAI from growing degree days
   !    ! This was revised and checked on 16 Feb 2014 by LJ
   !    !------------------------------------------------------------------------------
   !    ! save initial LAI_id
   !    LAI_id_in = LAI_id

   !    CALL update_GDDLAI( &
   !       id, LAICalcYes, & !input
   !       lat, LAI_obs, &
   !       Tmin_id, Tmax_id, lenDay_id, &
   !       BaseT, BaseTe, &
   !       GDDFull, SDDFull, &
   !       LAIMin, LAIMax, LAIPower, LAIType, &
   !       LAI_id_in, &
   !       GDD_id, SDD_id, & !inout
   !       LAI_id) !output

   !    CALL update_Veg( &
   !       LAImax, LAIMin, & !input
   !       AlbMax_DecTr, AlbMax_EveTr, AlbMax_Grass, &
   !       AlbMin_DecTr, AlbMin_EveTr, AlbMin_Grass, &
   !       CapMax_dec, CapMin_dec, &
   !       PorMax_dec, PorMin_dec, &
   !       LAI_id, LAI_id_in, &
   !       DecidCap_id, & !inout
   !       albDecTr_id, &
   !       albEveTr_id, &
   !       albGrass_id, &
   !       porosity_id, &
   !       StoreDrainPrm)

   !    ! PRINT*, 'DecidCap',DecidCap(id),DecidCap_id
   !    ! PRINT*, 'albDecTr',albDecTr(id),albDecTr_id
   !    ! PRINT*, 'albEveTr',albEveTr(id),albEveTr_id
   !    ! PRINT*, 'albGrass',albGrass(id),albGrass_id
   !    ! PRINT*, 'porosity',porosity(id),porosity_id

   ! END SUBROUTINE update_DailyState_End

   SUBROUTINE update_DailyState_Day( &
      BaseTMethod, &
      DayofWeek_id, &
      avkdn, & !input
      Tair, &
      Precip, &
      BaseT_HC, &
      BaseT_Heating, BaseT_Cooling, &
      nsh_real, &
      Tmin_id, Tmax_id, lenDay_id, & !inout
      HDD_id) !inout
      ! use time, only: id, id_prev_t
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: BaseTMethod
      INTEGER, DIMENSION(3), INTENT(in) :: DayofWeek_id

      REAL(KIND(1D0)), INTENT(IN) :: avkdn
      REAL(KIND(1D0)), INTENT(IN) :: Tair ! Ambient air temperature [degC], this can be from either forcing or diagnostic
      REAL(KIND(1D0)), INTENT(IN) :: Precip
      REAL(KIND(1D0)), INTENT(IN) :: BaseT_HC
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: BaseT_Heating
      REAL(KIND(1D0)), DIMENSION(2), INTENT(IN) :: BaseT_Cooling
      REAL(KIND(1D0)), INTENT(IN) :: nsh_real
      REAL(KIND(1D0)), INTENT(INOUT) :: Tmin_id
      REAL(KIND(1D0)), INTENT(INOUT) :: Tmax_id
      REAL(KIND(1D0)), INTENT(INOUT) :: lenDay_id
      ! REAL(KIND(1d0)), INTENT(out)::Tmin_id_next
      ! REAL(KIND(1d0)), INTENT(out)::Tmax_id_next
      ! REAL(KIND(1d0)), INTENT(out)::lenDay_id_next

      ! REAL(KIND(1d0))::tstepcount
      ! REAL(KIND(1d0)),DIMENSION(-4:366,6),INTENT(INOUT):: HDD
      ! REAL(KIND(1d0)), DIMENSION(5), INTENT(INOUT):: GDD_id !Growing Degree Days (see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(12), INTENT(INOUT) :: HDD_id !Heating Degree Days (see SUEWS_DailyState.f95)
      ! REAL(KIND(1d0)),DIMENSION(5),INTENT(OUT):: GDD_id_prev !Growing Degree Days (see SUEWS_DailyState.f95)
      INTEGER :: iu ! flag for weekday/weekend

      REAL(KIND(1D0)) :: dT_heating
      REAL(KIND(1D0)) :: dT_cooling

      REAL(KIND(1D0)) :: BaseT_Heating_use
      REAL(KIND(1D0)) :: BaseT_Cooling_use

      ! Set weekday/weekend counter
      iu = 1 !Set to 1=weekday
      IF (DayofWeek_id(1) == 1 .OR. DayofWeek_id(1) == 7) iu = 2 !Set to 2=weekend

      SELECT CASE (BaseTMethod)
      CASE (1)
         BaseT_Heating_use = BaseT_HC
         BaseT_Cooling_use = BaseT_HC
      CASE (2)
         BaseT_Heating_use = BaseT_Heating(iu)
         BaseT_Cooling_use = BaseT_Cooling(iu)

      CASE default
         CALL ErrorHint(75, "RunControl.nml", -999, -999, -999)

      END SELECT

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
         albChangeGrass = (AlbMax_Grass - AlbMin_Grass)*deltaLAIGrass
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
      ! TS 12 Nov 2024: revert the behaviour in restricting albDecTr_id, albEveTr_id, albGrass_id to valid ranges #285
      ! albDecTr_id = albDecTr_id + albChangeDecTr
      ! albEveTr_id = albEveTr_id + albChangeEveTr
      ! albGrass_id = albGrass_id + albChangeGrass

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
      IMPLICIT NONE

      !------------------------------------------------------------------------------
      ! Calculation of LAI from growing degree days
      ! This was revised and checked on 16 Feb 2014 by LJ
      !------------------------------------------------------------------------------

      INTEGER, INTENT(IN) :: id
      INTEGER, INTENT(IN) :: LAICalcYes

      REAL(KIND(1D0)), INTENT(IN) :: lat
      REAL(KIND(1D0)), INTENT(IN) :: LAI_obs
      REAL(KIND(1D0)), INTENT(IN) :: Tmin_id_prev
      REAL(KIND(1D0)), INTENT(IN) :: Tmax_id_prev
      REAL(KIND(1D0)), INTENT(IN) :: lenDay_id_prev

      ! --- Vegetation phenology ---------------------------------------------------------------------
      ! Parameters provided in input information for each vegetation surface (SUEWS_Veg.txt)
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: BaseT_GDD !Base temperature for growing degree days [degC]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: BaseT_SDD !Base temperature for senescence degree days [degC]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: GDDFull !Growing degree days needed for full capacity [degC]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: SDDFull !Senescence degree days needed to initiate leaf off [degC]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: LAIMin !Min LAI [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: LAIMax !Max LAI [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(4, nvegsurf), INTENT(IN) :: LAIPower !Coeffs for LAI equation: 1,2 - leaf growth; 3,4 - leaf off
      !! N.B. currently DecTr only, although input provided for all veg types
      INTEGER, DIMENSION(nvegsurf), INTENT(IN) :: LAIType !LAI equation to use: original (0) or new (1)

      REAL(KIND(1D0)), DIMENSION(3), INTENT(INOUT) :: GDD_id !Growing Degree Days (see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(3), INTENT(INOUT) :: SDD_id !Senescence Degree Days (see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(OUT) :: LAI_id_next !LAI for each veg surface [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: LAI_id_prev ! LAI of previous day

      REAL(KIND(1D0)) :: delta_SDD !Switches and checks for GDD
      REAL(KIND(1D0)) :: delta_GDD !Switches and checks for GDD
      REAL(KIND(1D0)) :: indHelp !Switches and checks for GDD
      REAL(KIND(1D0)), DIMENSION(3) :: GDD_id_prev ! GDD of previous day
      REAL(KIND(1D0)), DIMENSION(3) :: SDD_id_prev ! SDD of previous day

      INTEGER :: critDays
      INTEGER :: iv

      ! translate values of previous day to local variables
      GDD_id_prev = GDD_id
      SDD_id_prev = SDD_id
      ! LAI_id_prev = LAI_id_next

      critDays = 50 !Critical limit for GDD when GDD or SDD is set to zero

      ! Loop through vegetation types (iv)
      DO iv = 1, NVegSurf
         ! Calculate GDD for each day from the minimum and maximum air temperature
         delta_GDD = ((Tmin_id_prev + Tmax_id_prev)/2 - BaseT_GDD(iv)) !Leaf on
         delta_SDD = ((Tmin_id_prev + Tmax_id_prev)/2 - BaseT_SDD(iv)) !Leaf off

         indHelp = 0 !Help switch to allow GDD to go to zero in sprint-time !! QUESTION: What does this mean? HCW

         IF (delta_GDD < 0) THEN !GDD cannot be negative
            indHelp = delta_GDD !Amount of negative GDD
            delta_GDD = 0
         END IF

         IF (delta_SDD > 0) delta_SDD = 0 !SDD cannot be positive

         ! Calculate cumulative growing and senescence degree days
         GDD_id(iv) = GDD_id_prev(iv) + delta_GDD
         SDD_id(iv) = SDD_id_prev(iv) + delta_SDD

         ! Possibility for cold spring
         IF (SDD_id(iv) <= SDDFull(iv) .AND. indHelp < 0) THEN
            GDD_id(iv) = 0
         END IF

         IF (GDD_id(iv) >= GDDFull(iv)) THEN !Start senescence
            GDD_id(iv) = GDDFull(iv) !Leaves should not grow so delete yes from earlier
            IF (SDD_id(iv) < -critDays) GDD_id(iv) = 0
         END IF

         IF (SDD_id(iv) <= SDDFull(iv)) THEN !After senescence now start growing leaves
            SDD_id(iv) = SDDFull(iv) !Leaves off so add back earlier
            IF (GDD_id(iv) > critDays) SDD_id(iv) = 0
         END IF

         ! With these limits SDD, GDD is set to zero
         IF (SDD_id(iv) < -critDays .AND. SDD_id(iv) > SDDFull(iv)) GDD_id(iv) = 0
         IF (GDD_id(iv) > critDays .AND. GDD_id(iv) < GDDFull(iv)) SDD_id(iv) = 0

         ! Now calculate LAI itself
         IF (lat >= 0) THEN !Northern hemispere
            !If SDD is not zero by mid May, this is forced
            IF (id == 140 .AND. SDD_id(iv) /= 0) SDD_id(iv) = 0
            ! Set SDD to zero in summer time
            IF (GDD_id(iv) > critDays .AND. id < 170) SDD_id(iv) = 0
            ! Set GDD zero in winter time
            IF (SDD_id(iv) < -critDays .AND. id > 170) GDD_id(iv) = 0

            IF (LAItype(iv) < 0.5) THEN !Original LAI type
               IF (GDD_id(iv) > 0 .AND. GDD_id(iv) < GDDFull(iv)) THEN !Leaves can still grow
                  LAI_id_next(iv) = (LAI_id_prev(iv)**LAIPower(1, iv)*GDD_id(iv)*LAIPower(2, iv)) + LAI_id_prev(iv)
               ELSEIF (SDD_id(iv) < 0 .AND. SDD_id(iv) > SDDFull(iv)) THEN !Start senescence
                  LAI_id_next(iv) = (LAI_id_prev(iv)**LAIPower(3, iv)*SDD_id(iv)*LAIPower(4, iv)) + LAI_id_prev(iv)
               ELSE
                  LAI_id_next(iv) = LAI_id_prev(iv)
               END IF
            ELSEIF (LAItype(iv) >= 0.5) THEN
               IF (GDD_id(iv) > 0 .AND. GDD_id(iv) < GDDFull(iv)) THEN !Leaves can still grow
                  LAI_id_next(iv) = (LAI_id_prev(iv)**LAIPower(1, iv)*GDD_id(iv)*LAIPower(2, iv)) + LAI_id_prev(iv)
                  !! Use day length to start senescence at high latitudes (N hemisphere)
               ELSEIF (lenDay_id_prev <= 12 .AND. SDD_id(iv) > SDDFull(iv)) THEN !Start senescence
                  LAI_id_next(iv) = (LAI_id_prev(iv)*LAIPower(3, iv)*(1 - SDD_id(iv))*LAIPower(4, iv)) + LAI_id_prev(iv)
               ELSE
                  LAI_id_next(iv) = LAI_id_prev(iv)
               END IF
            END IF

         ELSEIF (lat < 0) THEN !Southern hemisphere !! N.B. not identical to N hemisphere - return to later
            !If SDD is not zero by late Oct, this is forced
            IF (id == 300 .AND. SDD_id(iv) /= 0) SDD_id(iv) = 0
            ! Set SDD to zero in summer time
            IF (GDD_id(iv) > critDays .AND. id > 250) SDD_id(iv) = 0
            ! Set GDD zero in winter time
            IF (SDD_id(iv) < -critDays .AND. id < 250) GDD_id(iv) = 0

            IF (LAItype(iv) < 0.5) THEN !Original LAI type
               IF (GDD_id(iv) > 0 .AND. GDD_id(iv) < GDDFull(iv)) THEN
                  LAI_id_next(iv) = (LAI_id_prev(iv)**LAIPower(1, iv)*GDD_id(iv)*LAIPower(2, iv)) + LAI_id_prev(iv)
               ELSEIF (SDD_id(iv) < 0 .AND. SDD_id(iv) > SDDFull(iv)) THEN
                  LAI_id_next(iv) = (LAI_id_prev(iv)**LAIPower(3, iv)*SDD_id(iv)*LAIPower(4, iv)) + LAI_id_prev(iv)
               ELSE
                  LAI_id_next(iv) = LAI_id_prev(iv)
               END IF
            ELSE
               IF (GDD_id(iv) > 0 .AND. GDD_id(iv) < GDDFull(iv)) THEN
                  LAI_id_next(iv) = (LAI_id_prev(iv)**LAIPower(1, iv)*GDD_id(iv)*LAIPower(2, iv)) + LAI_id_prev(iv)
                  !! Day length not used to start senescence in S hemisphere (not much land)
               ELSEIF (SDD_id(iv) < 0 .AND. SDD_id(iv) > SDDFull(iv)) THEN
                  LAI_id_next(iv) = (LAI_id_prev(iv)*LAIPower(3, iv)*(1 - SDD_id(iv))*LAIPower(4, iv)) + LAI_id_prev(iv)
               ELSE
                  LAI_id_next(iv) = LAI_id_prev(iv)
               END IF
            END IF
         END IF !N or S hemisphere

         ! Check LAI within limits; if not set to limiting value
         IF (LAI_id_next(iv) > LAImax(iv)) THEN
            LAI_id_next(iv) = LAImax(iv)
         ELSEIF (LAI_id_next(iv) < LAImin(iv)) THEN
            LAI_id_next(iv) = LAImin(iv)
         END IF

      END DO !End of loop over veg surfaces

      IF (LAICalcYes == 0) THEN ! moved to SUEWS_cal_DailyState, TS 18 Sep 2017
         ! LAI(id-1,:)=LAI_obs ! check -- this is going to be a problem as it is not for each vegetation class
         LAI_id_next = LAI_obs
      END IF
      !------------------------------------------------------------------------------

   END SUBROUTINE update_GDDLAI

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
   SUBROUTINE update_DailyStateLine_DTS( &
      timer, config, forcing, siteInfo, & ! input
      modState, & ! input/output:
      DailyStateLine) !out

      USE SUEWS_DEF_DTS, ONLY: &
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
            SnowAlb => snowState%SnowAlb, &
            SnowDens => snowState%SnowDens, &
            HDD_id => anthroEmisState%HDD_id, &
            VegPhenLumps => phenState%VegPhenLumps, &
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
            IF (it == 23 .AND. imin == (nsh_real - 1)/nsh_real*60) THEN
               ! Write actual data only at the last timesstep of each day
               ! DailyStateLine(1:2)   = [iy,id]
               ! DailyStateLine(1:6) = HDD_id
               ! DailyStateLine(6 + 1:6 + 5) = GDD_id
               ! DailyStateLine(11 + 1:11 + 3) = LAI_id
               ! DailyStateLine(14 + 1:14 + 5) = [DecidCap_id, Porosity_id, AlbEveTr_id, AlbDecTr_id, AlbGrass_id]
               ! DailyStateLine(19 + 1:19 + 9) = WUDay_id(1:9)
               ! DailyStateLine(28 + 1) = deltaLAI
               ! DailyStateLine(29 + 1) = VegPhenLumps
               ! DailyStateLine(30 + 1:30 + 8) = [SnowAlb, SnowDens(1:7)]
               ! DailyStateLine(38 + 1:38 + 3) = [a1, a2, a3]
               DailyStateLine = [HDD_id(1:6), GDD_id, SDD_id, Tmin_id, Tmax_id, lenday_id, LAI_id, DecidCap_id, Porosity_id, &
                                 AlbEveTr_id, AlbDecTr_id, AlbGrass_id, WUDay_id, VegPhenLumps, SnowAlb, SnowDens, &
                                 a1, a2, a3, a1_bldg, a2_bldg, a3_bldg]

            END IF

         END ASSOCIATE
      END ASSOCIATE

   END SUBROUTINE update_DailyStateLine_DTS

END MODULE DailyState_module
