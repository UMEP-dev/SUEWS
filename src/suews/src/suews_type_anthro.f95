module module_type_anthro

    ! use module_ctrl_type

    implicit none

    TYPE, PUBLIC :: anthroHEAT_PRM
       REAL(KIND(1D0)) :: qf0_beu_working = 0.0D0 ! Fraction of base value coming from buildings (working day) [-]
       REAL(KIND(1D0)) :: qf0_beu_holiday = 0.0D0 ! Fraction of base value coming from buildings (holiday) [-]
       REAL(KIND(1D0)) :: qf_a_working = 0.0D0 ! Base value for QF (working day) [W m-2]
       REAL(KIND(1D0)) :: qf_a_holiday = 0.0D0 ! Base value for QF (holiday) [W m-2]
       REAL(KIND(1D0)) :: qf_b_working = 0.0D0 ! Parameter related to heating degree days (working day) [W m-2 K-1 (Cap ha-1 )-1]
       REAL(KIND(1D0)) :: qf_b_holiday = 0.0D0 ! Parameter related to heating degree days (holiday) [W m-2 K-1 (Cap ha-1 )-1]
       REAL(KIND(1D0)) :: qf_c_working = 0.0D0 ! Parameter related to cooling degree days (working day) [W m-2 K-1 (Cap ha-1 )-1]
       REAL(KIND(1D0)) :: qf_c_holiday = 0.0D0 ! Parameter related to cooling degree days (holiday) [W m-2 K-1 (Cap ha-1 )-1]
       REAL(KIND(1D0)) :: baset_cooling_working = 0.0D0 ! Base temperature for cooling degree days (working day) [degC]
       REAL(KIND(1D0)) :: baset_cooling_holiday = 0.0D0 ! Base temperature for cooling degree days (holiday) [degC]
       REAL(KIND(1D0)) :: baset_heating_working = 0.0D0 ! Base temperature for heating degree days (working day) [degC]
       REAL(KIND(1D0)) :: baset_heating_holiday = 0.0D0 ! Base temperature for heating degree days (holiday) [degC]
       REAL(KIND(1D0)) :: ah_min_working = 0.0D0 ! minimum QF values (working day) [W m-2]
       REAL(KIND(1D0)) :: ah_min_holiday = 0.0D0 ! minimum QF values (holiday) [W m-2]
       REAL(KIND(1D0)) :: ah_slope_cooling_working = 0.0D0 ! cooling slope for the anthropogenic heat flux calculation (working day) [W m-2 K-1]
       REAL(KIND(1D0)) :: ah_slope_cooling_holiday = 0.0D0 ! cooling slope for the anthropogenic heat flux calculation (holiday) [W m-2 K-1]
       REAL(KIND(1D0)) :: ah_slope_heating_working = 0.0D0 ! heating slope for the anthropogenic heat flux calculation (working day) [W m-2 K-1]
       REAL(KIND(1D0)) :: ah_slope_heating_holiday = 0.0D0 ! heating slope for the anthropogenic heat flux calculation (holiday) [W m-2 K-1]
       REAL(KIND(1D0)), DIMENSION(0:23) :: ahprof_24hr_working = 0.0D0 ! Hourly profile values used in energy use calculation (working day) [-]
       REAL(KIND(1D0)), DIMENSION(0:23) :: ahprof_24hr_holiday = 0.0D0 ! Hourly profile values used in energy use calculation (holiday) [-]
       REAL(KIND(1D0)) :: pop_density_daytime_working = 0.0D0 ! Daytime population density [people ha-1] (working day)
       REAL(KIND(1D0)) :: pop_density_daytime_holiday = 0.0D0 ! Daytime population density [people ha-1] (holiday)
       REAL(KIND(1D0)) :: pop_density_nighttime = 0.0D0
       REAL(KIND(1D0)), DIMENSION(0:23) :: popprof_24hr_working = 0.0D0 !Hourly profile values used in dynamic population estimation[-] (working day)
       REAL(KIND(1D0)), DIMENSION(0:23) :: popprof_24hr_holiday = 0.0D0 !Hourly profile values used in dynamic population estimation[-] (holiday)
    END TYPE anthroHEAT_PRM

    TYPE, PUBLIC :: anthroEMIS_PRM
       INTEGER :: start_dls = 0 ! start of daylight saving  [DOY]
       INTEGER :: end_dls = 0 ! end of daylight saving [DOY]
       TYPE(anthroHEAT_PRM) :: anthro_heat
       REAL(KIND(1D0)) :: ef_umol_co2_per_j = 0.0D0
       REAL(KIND(1D0)) :: en_ef_v_jkm = 0.0D0
       REAL(KIND(1D0)) :: fr_fossil_fuel_heat = 0.0D0
       REAL(KIND(1D0)) :: fr_fossil_fuel_non_heat = 0.0D0
       REAL(KIND(1D0)), DIMENSION(2) :: fc_ef_v_kgkm = 0.0D0
       REAL(KIND(1D0)), DIMENSION(0:23) :: hum_activity_24hr_working = 0.0D0
       REAL(KIND(1D0)), DIMENSION(0:23) :: hum_activity_24hr_holiday = 0.0D0
       REAL(KIND(1D0)) :: max_fc_metab = 0.0D0
       REAL(KIND(1D0)) :: max_qf_metab = 0.0D0
       REAL(KIND(1D0)) :: min_fc_metab = 0.0D0
       REAL(KIND(1D0)) :: min_qf_metab = 0.0D0
       REAL(KIND(1D0)) :: traffic_rate_working = 0.0D0
       REAL(KIND(1D0)) :: traffic_rate_holiday = 0.0D0
       REAL(KIND(1D0)) :: traffic_units = 0.0D0
       REAL(KIND(1D0)), DIMENSION(0:23) :: traff_prof_24hr_working = 0.0D0
       REAL(KIND(1D0)), DIMENSION(0:23) :: traff_prof_24hr_holiday = 0.0D0
    END TYPE anthroEMIS_PRM

    TYPE, PUBLIC :: anthroEmis_STATE
       ! TODO: #242 split HDD_id into individual explicit variables
       REAL(KIND(1D0)), DIMENSION(12) :: HDD_id = 0.0D0 !Heating Degree Days [degC d]
       ! HDD_id:
       ! first half used for update through the day
       ! HDD_id(1) ---- Heating [degC]: used for accumulation during calculation
       ! HDD_id(2) ---- Cooling [degC]: used for accumulation during calculation
       ! HDD_id(3) ---- Daily mean temp [degC]: used for accumulation during calculation
       ! HDD_id(4) ----
       ! HDD_id(5) ---- Daily precip total [mm]
       ! HDD_id(6) ---- Days since rain [d]
       ! second half used for storage of the first half for the prevous day
       ! HDD_id(6+1) ---- Heating [degC]: used for accumulation during calculation
       ! HDD_id(6+2) ---- Cooling [degC]: used for accumulation during calculation
       ! HDD_id(6+3) ---- Daily mean temp [degC]: used for accumulation during calculation
       ! HDD_id(6+4) ---- 5-day running mean temp [degC]: used for actual calculation
       ! HDD_id(6+5) ---- Daily precip total [mm]
       ! HDD_id(6+6) ---- Days since rain [d]

       REAL(KIND(1D0)) :: Fc = 0.0D0 !total co2 flux [umol m-2 s-1]
       REAL(KIND(1D0)) :: Fc_anthro = 0.0D0 !anthropogenic co2 flux  [umol m-2 s-1]
       REAL(KIND(1D0)) :: Fc_biogen = 0.0D0 !biogenic CO2 flux [umol m-2 s-1]
       REAL(KIND(1D0)) :: Fc_build = 0.0D0 ! anthropogenic co2 flux  [umol m-2 s-1]

       REAL(KIND(1D0)) :: Fc_metab = 0.0D0 ! co2 emission from metabolism component [umol m-2 s-1]
       REAL(KIND(1D0)) :: Fc_photo = 0.0D0 !co2 flux from photosynthesis [umol m
       REAL(KIND(1D0)) :: Fc_point = 0.0D0 ! co2 emission from point source [umol m-2 s-1]
       REAL(KIND(1D0)) :: Fc_respi = 0.0D0 !co2 flux from respiration [umol m-2 s-1]
       REAL(KIND(1D0)) :: Fc_traff = 0.0D0 ! co2 emission from traffic component [umol m-2 s-1]

       ! flag for iteration safety - NO
       ! HDD_id includes extensive quantities and thus cannot be used for iteration safety
       LOGICAL :: iter_safe = .FALSE.
    END TYPE anthroEmis_STATE

end module module_type_anthro
