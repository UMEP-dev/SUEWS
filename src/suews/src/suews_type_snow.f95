module module_type_snow

    USE module_ctrl_const_allocate, ONLY: &
       nsurf

    ! use module_ctrl_type

    implicit none

    TYPE, PUBLIC :: SNOW_PRM
       REAL(KIND(1D0)) :: water_holding_capacity_max = 0.0D0 ! Maximum water holding capacity of snow [mm]
       REAL(KIND(1D0)) :: water_holding_capacity_min = 0.0D0 ! Minimum water holding capacity of snow [mm]
       REAL(KIND(1D0)) :: narp_emis_snow = 0.0D0 ! Snow emissivity in NARP model [-]
       REAL(KIND(1D0)) :: temperature_rain_snow_threshold = 0.0D0 ! Temperature limit when precipitation falls as snow [degC]
       REAL(KIND(1D0)) :: precipitation_threshold_albedo_reset = 0.0D0 ! Limit for hourly precipitation when ground is fully covered with snow [mm]
       REAL(KIND(1D0)) :: snow_albedo_max = 0.0D0 ! Maximum snow albedo (fresh snow) [-]
       REAL(KIND(1D0)) :: snow_albedo_min = 0.0D0 ! Minimum snow albedo (aged snow) [-]
       REAL(KIND(1D0)) :: snow_density_max = 0.0D0 ! Maximum snow density [kg m-3]
       REAL(KIND(1D0)) :: snow_density_min = 0.0D0 ! Fresh snow density [kg m-3]
       REAL(KIND(1D0)) :: snow_depth_limit_building = 0.0D0 ! Snow water equivalent limit for removal from building roofs [mm]
       REAL(KIND(1D0)) :: snow_depth_limit_paved = 0.0D0 ! Snow water equivalent limit for removal from roads [mm]
       REAL(KIND(1D0)), DIMENSION(nsurf) :: snowpack_limit = 0.0D0 ! Limit for SWE when snow cover starts to be patchy [mm]
       REAL(KIND(1D0)), DIMENSION(0:23) :: snowprof_24hr_working = 0.0D0 ! Hourly snow clearing profile (working day) [-]
       REAL(KIND(1D0)), DIMENSION(0:23) :: snowprof_24hr_holiday = 0.0D0 ! Hourly snow clearing profile (holiday) [-]
       REAL(KIND(1D0)) :: tau_a = 0.0D0 ! Time constant for snow albedo ageing in cold snow [-]
       REAL(KIND(1D0)) :: tau_f = 0.0D0 ! Time constant for snow albedo ageing in melting snow [-]
       REAL(KIND(1D0)) :: tau_r = 0.0D0 ! Time constant for snow density ageing [-]
       REAL(KIND(1D0)) :: temperature_melt_factor = 0.0D0 ! Hourly temperature melt factor of snow [mm K-1 h-1]
       REAL(KIND(1D0)) :: radiation_melt_factor = 0.0D0 ! Hourly radiation melt factor of snow [mm W-1 h-1]
    END TYPE SNOW_PRM

    TYPE, PUBLIC :: SNOW_STATE
       REAL(KIND(1D0)) :: snowfall_cum = 0.0D0 ! Cumulative snowfall [mm]
       REAL(KIND(1D0)) :: snow_albedo = 0.0D0 ! Albedo of snow [-]
       REAL(KIND(1D0)) :: chSnow_per_interval = 0.0D0 ! Change in snow state per time interval [mm]
       REAL(KIND(1D0)) :: mwh = 0.0D0 ! Snowmelt [mm]
       REAL(KIND(1D0)) :: melt_water_store = 0.0D0 ! Overall melt water [mm]
       REAL(KIND(1D0)) :: qn_snow = 0.0D0 ! Net all-wave radiation on snow surface [W m-2]
       REAL(KIND(1D0)) :: qm = 0.0D0 ! Snowmelt-related heat [W m-2]
       REAL(KIND(1D0)) :: qm_freeze = 0.0D0 ! Heat related to freezing of surface store [W m-2]
       REAL(KIND(1D0)) :: qm_rain = 0.0D0 ! Melt heat for rain on snow [W m-2]
       REAL(KIND(1D0)) :: swe = 0.0D0 ! Overall snow water equivalent [mm]
       REAL(KIND(1D0)) :: z0v_snow = 0.0D0 ! Roughness for heat [m]
       REAL(KIND(1D0)) :: ra_snow = 0.0D0 ! Aerodynamic resistance for snow [s m-1]
       REAL(KIND(1D0)) :: s_ice_hpa = 0.0D0 ! Saturated vapour pressure on snow [hPa]
       REAL(KIND(1D0)), DIMENSION(2) :: snow_removal = 0.0D0 ! Snow removal [mm]
       REAL(KIND(1D0)), DIMENSION(nsurf) :: ice_frac = 0.0D0 ! Fraction of ice in snowpack [-]
       REAL(KIND(1D0)), DIMENSION(nsurf) :: snow_density = 0.0D0 ! Snow density [kg m-3]
       REAL(KIND(1D0)), DIMENSION(nsurf) :: snow_fraction = 0.0D0 ! Snow fraction [-]
       REAL(KIND(1D0)), DIMENSION(nsurf) :: snow_pack = 0.0D0 ! Snow water equivalent on each land cover [mm]
       REAL(KIND(1D0)), DIMENSION(nsurf) :: snow_water = 0.0D0 ! Snow water [mm]
       REAL(KIND(1D0)), DIMENSION(nsurf) :: kup_ind_snow = 0.0D0 ! Outgoing shortwave on snowpack [W m-2]
       REAL(KIND(1D0)), DIMENSION(NSURF) :: qn_ind_snow = 0.0D0 ! Net all-wave radiation on snowpack [W m-2]
       REAL(KIND(1D0)), DIMENSION(NSURF) :: delta_qi = 0.0D0 ! Storage heat flux of snow surfaces [W m-2]
       REAL(KIND(1D0)), DIMENSION(nsurf) :: Tsurf_ind_snow = 0.0D0 ! Snowpack surface temperature [degC]
       ! flag for iteration safety - NO
       ! Multiple variables (snowpack, snowwater, snowfallCum, etc) are extensive quantities
       LOGICAL :: iter_safe = .FALSE.
    END TYPE SNOW_STATE

end module module_type_snow
