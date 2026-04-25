module module_type_waterdist

    ! use module_ctrl_type

    implicit none

    TYPE, PUBLIC :: SURF_STORE_PRM
       REAL(KIND(1D0)) :: store_min = 0.0D0 ! Minimum storage capacity [mm]
       REAL(KIND(1D0)) :: store_max = 0.0D0 ! Maximum storage capacity [mm]
       REAL(KIND(1D0)) :: store_cap = 0.0D0 ! Current storage capacity [mm]
       INTEGER :: drain_eq = 0 ! Drainage equation choice [-]
       REAL(KIND(1D0)) :: drain_coef_1 = 0.0D0 ! Drainage coefficient 1 [-]
       REAL(KIND(1D0)) :: drain_coef_2 = 0.0D0 ! Drainage coefficient 2 [-]
    END TYPE SURF_STORE_PRM

    TYPE, PUBLIC :: WATER_DIST_PRM
       REAL(KIND(1D0)) :: to_paved = 0.0D0 ! Fraction of water distributed to paved surface [-]
       REAL(KIND(1D0)) :: to_bldg = 0.0D0 ! Fraction of water distributed to building surface [-]
       REAL(KIND(1D0)) :: to_evetr = 0.0D0 ! Fraction of water distributed to evergreen tree surface [-]
       REAL(KIND(1D0)) :: to_dectr = 0.0D0 ! Fraction of water distributed to deciduous tree surface [-]
       REAL(KIND(1D0)) :: to_grass = 0.0D0 ! Fraction of water distributed to grass surface [-]
       REAL(KIND(1D0)) :: to_bsoil = 0.0D0 ! Fraction of water distributed to bare soil surface [-]
       REAL(KIND(1D0)) :: to_water = 0.0D0 ! Fraction of water distributed to water surface [-]
       REAL(KIND(1D0)) :: to_soilstore = 0.0D0 ! Fraction of water distributed to soil store [-]
    END TYPE WATER_DIST_PRM

    TYPE, PUBLIC :: IRRIG_daywater
       ! TODO: Change flags to int or bool, not REAL!
       REAL(KIND(1D0)) :: monday_flag = 0.0D0 ! Irrigation flag: 1 for on and 0 for off [-]
       REAL(KIND(1D0)) :: monday_percent = 0.0D0 ! Fraction of properties using irrigation [-]
       REAL(KIND(1D0)) :: tuesday_flag = 0.0D0 ! Irrigation flag: 1 for on and 0 for off [-]
       REAL(KIND(1D0)) :: tuesday_percent = 0.0D0 ! Fraction of properties using irrigation [-]
       REAL(KIND(1D0)) :: wednesday_flag = 0.0D0 ! Irrigation flag: 1 for on and 0 for off [-]
       REAL(KIND(1D0)) :: wednesday_percent = 0.0D0 ! Fraction of properties using irrigation [-]
       REAL(KIND(1D0)) :: thursday_flag = 0.0D0 ! Irrigation flag: 1 for on and 0 for off [-]
       REAL(KIND(1D0)) :: thursday_percent = 0.0D0 ! Fraction of properties using irrigation [-]
       REAL(KIND(1D0)) :: friday_flag = 0.0D0 ! Irrigation flag: 1 for on and 0 for off [-]
       REAL(KIND(1D0)) :: friday_percent = 0.0D0 ! Fraction of properties using irrigation [-]
       REAL(KIND(1D0)) :: saturday_flag = 0.0D0 ! Irrigation flag: 1 for on and 0 for off [-]
       REAL(KIND(1D0)) :: saturday_percent = 0.0D0 ! Fraction of properties using irrigation [-]
       REAL(KIND(1D0)) :: sunday_flag = 0.0D0 ! Irrigation flag: 1 for on and 0 for off [-]
       REAL(KIND(1D0)) :: sunday_percent = 0.0D0 ! Fraction of properties using irrigation [-]
    END TYPE IRRIG_daywater

    TYPE, PUBLIC :: IRRIGATION_PRM
       REAL(KIND(1D0)) :: h_maintain = 0.0D0 ! Ponding water depth to maintain [mm]
       REAL(KIND(1D0)) :: faut = 0.0D0 ! Fraction of irrigated area using automatic irrigation [-]
       REAL(KIND(1D0)), DIMENSION(3) :: ie_a = 0.0D0 ! Coefficient for automatic irrigation model [-]
       REAL(KIND(1D0)), DIMENSION(3) :: ie_m = 0.0D0 ! Coefficient for manual irrigation model [-]
       INTEGER :: ie_start = 0 ! Starting time of water use [DOY]
       INTEGER :: ie_end = 0 ! Ending time of water use [DOY]
       REAL(KIND(1D0)) :: internalwateruse_h = 0.0D0 ! Internal water use [mm h-1]
       TYPE(IRRIG_daywater) :: irr_daywater ! Daily irrigation parameters
       REAL(KIND(1D0)), DIMENSION(0:23) :: wuprofa_24hr_working = 0.0D0 ! Hourly profile for automatic irrigation (working day) [-]
       REAL(KIND(1D0)), DIMENSION(0:23) :: wuprofa_24hr_holiday = 0.0D0 ! Hourly profile for automatic irrigation (holiday) [-]
       REAL(KIND(1D0)), DIMENSION(0:23) :: wuprofm_24hr_working = 0.0D0 ! Hourly profile for manual irrigation (working day) [-]
       REAL(KIND(1D0)), DIMENSION(0:23) :: wuprofm_24hr_holiday = 0.0D0 ! Hourly profile for manual irrigation (holiday) [-]
    END TYPE IRRIGATION_PRM

end module module_type_waterdist
