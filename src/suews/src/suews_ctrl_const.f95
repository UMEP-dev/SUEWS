MODULE module_ctrl_const_mathconst

   IMPLICIT NONE

   REAL(KIND(1D0)), PARAMETER :: pi = 3.141592653589793D0 ! circle circumference / diameter
   REAL(KIND(1D0)), PARAMETER :: DEG2RAD = pi/180.0D0 ! [rad degree-1]
   REAL(KIND(1D0)), PARAMETER :: RAD2DEG = 180.0D0/pi ! [degree rad-1]

END MODULE module_ctrl_const_mathconst

!----------------------------------------------------------------------------------
MODULE module_ctrl_const_physconst

   IMPLICIT NONE

   REAL(KIND(1D0)), PARAMETER :: sec_per_hour = 3600.0D0, sec_per_day = 86400.0D0, min_per_day = 1440.0D0 ! SI time conversions
   REAL(KIND(1D0)), PARAMETER :: arcsec_per_deg = 3600.0D0 ! arcseconds per degree
   REAL(KIND(1D0)), PARAMETER :: cp_air_anohm = 1006.38D0 ! AnOHM air heat capacity [J kg-1 K-1]; differs from cp_air
   REAL(KIND(1D0)), PARAMETER :: lv_anohm = 2264705.0D0 ! AnOHM Lv [J kg-1]; differs from lv_bolton and lv_wetbulb
   REAL(KIND(1D0)), PARAMETER :: C2K = 273.15D0 ! SI Celsius-to-Kelvin offset [K]
   REAL(KIND(1D0)), PARAMETER :: SBConst = 5.670374419D-8 ! CODATA Stefan-Boltzmann constant [W m-2 K-4]
   REAL(KIND(1D0)), PARAMETER :: JtoumolPAR = 4.6D0 ! Convert PAR from W m-2 to umol m-2 s-1
   REAL(KIND(1D0)), PARAMETER :: KdntoPAR = 0.46D0 ! Conversion from Kdn to PAR, originally from Tsubo and Walker (2005), used in Bellucco et al. (2017)
   REAL(KIND(1D0)), PARAMETER :: lv_bolton = 2501000.0D0 ! Bolton (1980) Lv intercept [J kg-1]; empirical intercept, distinct from AnOHM and wet-bulb formulae
   REAL(KIND(1D0)), PARAMETER :: lv_wetbulb = 2500.25D0 ! Wet-bulb Lv intercept [kJ kg-1]; differs from lv_bolton after unit conversion
   REAL(KIND(1D0)), PARAMETER :: cp_air = 1005.0D0 ! specific heat capacity of dry air [J kg-1 K-1]; dry-air intercept in the meteorological formula
   REAL(KIND(1D0)), PARAMETER :: HPA_TO_PA = 100.0D0 ! SI hecto prefix: Pa per hPa
   REAL(KIND(1D0)), PARAMETER :: p_ref_pa_d0 = 101325.0D0 ! Standard atmosphere [Pa], SI defining value
   REAL(KIND(1D0)), PARAMETER :: eps_fp = 1.0D-12 !Epsilon for floating-point near-zero comparisons

END MODULE module_ctrl_const_physconst

!----------------------------------------------------------------------------------
MODULE module_ctrl_const_calendar

   IMPLICIT NONE

   ! Number of days in a week and 1-based weekday indices matching Day_Of_Week
   ! (suews_util_time.f95) and legacy SUEWS_Irrigation DayWat(1:7) ordering.
   INTEGER, PARAMETER :: NDAYS_WEEK = 7
   INTEGER, PARAMETER :: DOW_SUNDAY = 1
   INTEGER, PARAMETER :: DOW_MONDAY = 2
   INTEGER, PARAMETER :: DOW_TUESDAY = 3
   INTEGER, PARAMETER :: DOW_WEDNESDAY = 4
   INTEGER, PARAMETER :: DOW_THURSDAY = 5
   INTEGER, PARAMETER :: DOW_FRIDAY = 6
   INTEGER, PARAMETER :: DOW_SATURDAY = 7

   ! Day-type indices for two-day-type diurnal profiles (working day / weekend).
   INTEGER, PARAMETER :: DAYTYPE_WEEKDAY = 1
   INTEGER, PARAMETER :: DAYTYPE_WEEKEND = 2

END MODULE module_ctrl_const_calendar

!==================================================================================================
MODULE module_ctrl_const_allocate

   USE module_ctrl_const_mathconst, ONLY: DEG2RAD, RAD2DEG

   IMPLICIT NONE

   ! ---- Surface types ---------------------------------------------------------------------------
   INTEGER, PARAMETER :: nsurf = 7 !Total number of surfaces
   INTEGER, PARAMETER :: NVegSurf = 3 !Number of surfaces that are vegetated
   INTEGER, PARAMETER :: ndepth = 5 !Number of depth levels for facets

   ! SPARTACUS related
   INTEGER, PARAMETER :: nspec = 1 !Number of spectral bands
   INTEGER, PARAMETER :: nsw = 1 !Number of shortwave bands
   INTEGER, PARAMETER :: nlw = 1 !Number of longwave bands
   INTEGER, PARAMETER :: ncol = 1 !Number of tiles used: SUEWS does not have multiple tiles so ncol=1

   INTEGER, PARAMETER :: PavSurf = 1, & !When all surfaces considered together (1-7)
                         BldgSurf = 2, &
                         ConifSurf = 3, &
                         DecidSurf = 4, &
                         GrassSurf = 5, & !New surface classes: Grass = 5th/7 surfaces
                         BSoilSurf = 6, & !New surface classes: Bare soil = 6th/7 surfaces
                         WaterSurf = 7, &
                         ExcessSurf = 8, & !Runoff or subsurface soil in WGWaterDist
                         ivConif = 1, & !When only vegetated surfaces considered (1-3)
                         ivDecid = 2, &
                         ivGrass = 3

   ! STEBBS Related
   INTEGER, PARAMETER :: nbtypes = 1 ! Number of building archetypes. TODO: Make dynamic

   ! ---- Set number of columns in output files ---------------------------------------------------
   INTEGER, PARAMETER :: ncolumnsDataOutSUEWS = 90 + 7 + 7 + 7 + 7, & !Main output file (_5.txt): base + Ts_surf + Ts_surf_dyohm + qn_surf + qs_surf
                         ncolumnsDataOutSnow = 103, &
                         ncolumnsdataOutSOLWEIG = 31, &
                         ncolumnsDataOutBEERS = 34, &
                         ncolumnsdataOutBL = 22, &
                         ncolumnsDataOutESTM = 5 + 27, &
                         ncolumnsDataOutEHC = 5 + 7*2 + 15*(1 + 4 + 2)*2 + 7, &
                         ncolumnsDataOutDailyState = 52, &
                         ncolumnsDataOutRSL = 30*4 + 5 + 13 + 2, &
                         ncolumnsDataOutDebug = 1 + 5 + 103 + 14 + 5 + 4 + 3 + 1, &
                         ncolumnsDataOutSPARTACUS = 5 + 8 + 8*15 + 3 + 6*15 + 2*15 + 4 + 1 + 2*15 + 2 + 8*15, &
                         ncolumnsDataOutSTEBBS = 5 + 82, &
                         ncolumnsDataOutNHood = 5 + 1

   ! ---- Define arrays at daily timestep ---------------------------------------------------------
   INTEGER, PARAMETER :: ndays = 366 !Max no. days in a year used to specify size of daily arrays

END MODULE module_ctrl_const_allocate

!==================================================================================================
MODULE module_ctrl_const_datain

   IMPLICIT NONE

   INTEGER :: SuppressWarnings = 1 ! Set to 1 to suppress warning output

END MODULE module_ctrl_const_datain

!==================================================================================================
MODULE module_ctrl_const_default

   IMPLICIT NONE

   REAL(KIND(1D0)) :: notUsed = -55.55
   INTEGER :: notUsedI = -55

END MODULE module_ctrl_const_default

!===================================================================================
MODULE module_ctrl_const_gas

   IMPLICIT NONE

   REAL(KIND(1D0)) :: comp = 0.9995
   REAL(KIND(1D0)) :: epsil = 0.62197 !ratio molecular weight of water vapour/dry air (kg/mol/kg/mol)
   REAL(KIND(1D0)) :: epsil_gkg = 621.97 !ratio molecular weight of water vapour/dry air in g/kg
   REAL(KIND(1D0)) :: dry_gas = 8.31451 !Dry gas constant (J/k/mol)
   REAL(KIND(1D0)) :: gas_ct_wat = 461.05 !Gas constant for water (J/kg/K)
   REAL(KIND(1D0)) :: molar = 0.028965 !Dry air molar fraction in kg/mol
   REAL(KIND(1D0)) :: molar_wat_vap = 0.0180153 !Molar fraction of water vapour in kg/mol
   REAL(KIND(1D0)) :: gas_ct_dry = 8.31451/0.028965 !j/kg/k=dry_gas/molar
   REAL(KIND(1D0)) :: gas_ct_wv = 8.31451/0.0180153 !j/kg/kdry_gas/molar_wat_vap

END MODULE module_ctrl_const_gas

!----------------------------------------------------------------------------------

! Integer option codes shared with the Python schema and Rust bridge.
MODULE module_ctrl_const_options
   IMPLICIT NONE
   PRIVATE

   INTEGER, PARAMETER, PUBLIC :: SHM_OBSERVED = 0
   INTEGER, PARAMETER, PUBLIC :: SHM_OHM = 1
   INTEGER, PARAMETER, PUBLIC :: SHM_ANOHM = 3
   INTEGER, PARAMETER, PUBLIC :: SHM_ESTM = 4
   INTEGER, PARAMETER, PUBLIC :: SHM_EHC = 5
   INTEGER, PARAMETER, PUBLIC :: SHM_DYOHM = 6
   INTEGER, PARAMETER, PUBLIC :: SHM_STEBBS = 7
   INTEGER, PARAMETER, PUBLIC :: SHM_DYOHM_BUILDING = 8

   INTEGER, PARAMETER, PUBLIC :: OHM_QF_EXCLUDE = 0
   INTEGER, PARAMETER, PUBLIC :: OHM_QF_INCLUDE = 1

   INTEGER, PARAMETER, PUBLIC :: DIAG_OFF = 0
   INTEGER, PARAMETER, PUBLIC :: DIAG_ON = 1

   INTEGER, PARAMETER, PUBLIC :: STEBBS_NONE = 0
   INTEGER, PARAMETER, PUBLIC :: STEBBS_DEFAULT = 1
   INTEGER, PARAMETER, PUBLIC :: STEBBS_PROVIDED = 2

   INTEGER, PARAMETER, PUBLIC :: RC_DEFAULT = 0
   INTEGER, PARAMETER, PUBLIC :: RC_PROVIDED = 1
   INTEGER, PARAMETER, PUBLIC :: RC_PARAMETERISE = 2

   INTEGER, PARAMETER, PUBLIC :: SETPOINT_CONSTANT = 0
   INTEGER, PARAMETER, PUBLIC :: SETPOINT_DEPENDENT = 1
   INTEGER, PARAMETER, PUBLIC :: SETPOINT_SCHEDULED = 2
END MODULE module_ctrl_const_options
