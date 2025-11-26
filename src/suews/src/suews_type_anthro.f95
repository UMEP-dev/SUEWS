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
       REAL(KIND(1D0)) :: popdensdaytime_working = 0.0D0 ! Daytime population density [people ha-1] (working day)
       REAL(KIND(1D0)) :: popdensdaytime_holiday = 0.0D0 ! Daytime population density [people ha-1] (holiday)
       REAL(KIND(1D0)) :: popdensnighttime = 0.0D0
       REAL(KIND(1D0)), DIMENSION(0:23) :: popprof_24hr_working = 0.0D0 !Hourly profile values used in dynamic population estimation[-] (working day)
       REAL(KIND(1D0)), DIMENSION(0:23) :: popprof_24hr_holiday = 0.0D0 !Hourly profile values used in dynamic population estimation[-] (holiday)
    END TYPE anthroHEAT_PRM

    TYPE, PUBLIC :: anthroEMIS_PRM
       INTEGER :: startdls = 0 ! start of daylight saving  [DOY]
       INTEGER :: enddls = 0 ! end of daylight saving [DOY]
       TYPE(anthroHEAT_PRM) :: anthroheat
       REAL(KIND(1D0)) :: EF_umolCO2perJ = 0.0D0
       REAL(KIND(1D0)) :: EnEF_v_Jkm = 0.0D0
       REAL(KIND(1D0)) :: FrFossilFuel_Heat = 0.0D0
       REAL(KIND(1D0)) :: FrFossilFuel_NonHeat = 0.0D0
       REAL(KIND(1D0)), DIMENSION(2) :: FcEF_v_kgkm = 0.0D0
       REAL(KIND(1D0)), DIMENSION(0:23) :: HumActivity_24hr_working = 0.0D0
       REAL(KIND(1D0)), DIMENSION(0:23) :: HumActivity_24hr_holiday = 0.0D0
       REAL(KIND(1D0)) :: MaxFCMetab = 0.0D0
       REAL(KIND(1D0)) :: MaxQFMetab = 0.0D0
       REAL(KIND(1D0)) :: MinFCMetab = 0.0D0
       REAL(KIND(1D0)) :: MinQFMetab = 0.0D0
       REAL(KIND(1D0)) :: TrafficRate_working = 0.0D0
       REAL(KIND(1D0)) :: TrafficRate_holiday = 0.0D0
       REAL(KIND(1D0)) :: TrafficUnits = 0.0D0
       REAL(KIND(1D0)), DIMENSION(0:23) :: TraffProf_24hr_working = 0.0D0
       REAL(KIND(1D0)), DIMENSION(0:23) :: TraffProf_24hr_holiday = 0.0D0
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
