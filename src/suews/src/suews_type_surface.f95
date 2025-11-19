module module_type_surface
   use module_ctrl_const_allocate, only: nsurf

   implicit none
   INTEGER, PARAMETER, PUBLIC :: ANOHM_MAX_SAMPLES = 48

   TYPE, PUBLIC :: LUMPS_PRM
      REAL(KIND(1D0)) :: raincover = 0.0D0 ! limit when surface totally covered with water for LUMPS [mm]
      REAL(KIND(1D0)) :: rainmaxres = 0.0D0 ! maximum water bucket reservoir. Used for LUMPS surface wetness control. [mm]
      REAL(KIND(1D0)) :: drainrt = 0.0D0 ! Drainage rate of the water bucket [mm hr-1]
      INTEGER :: veg_type ! Defines how vegetation is calculated for LUMPS [-]
   END TYPE LUMPS_PRM

   TYPE, PUBLIC :: OHM_COEF_LC
      REAL(KIND(1D0)) :: summer_dry = 0.0D0
      REAL(KIND(1D0)) :: summer_wet = 0.0D0
      REAL(KIND(1D0)) :: winter_dry = 0.0D0
      REAL(KIND(1D0)) :: winter_wet = 0.0D0
   END TYPE OHM_COEF_LC

   TYPE, PUBLIC :: OHM_PRM
      REAL(KIND(1D0)) :: chanohm = 0.0D0 ! Bulk transfer coefficient for this surface to use in AnOHM [J m-3 K-1]
      REAL(KIND(1D0)) :: cpanohm = 0.0D0 ! Volumetric heat capacity for this surface to use in AnOHM  [J m-3 K-1]
      REAL(KIND(1D0)) :: kkanohm = 0.0D0 ! Thermal conductivity for this surface to use in AnOHM [W m-1 K-1]
      REAL(KIND(1D0)) :: ohm_threshsw = 0.0D0 ! Temperature threshold determining whether summer/winter OHM coefficients are applied [degC]
      REAL(KIND(1D0)) :: ohm_threshwd = 0.0D0 ! Soil moisture threshold determining whether wet/dry OHM coefficients are applied [degC]
      TYPE(OHM_COEF_LC), DIMENSION(3) :: ohm_coef_lc
   END TYPE OHM_PRM

   TYPE, PUBLIC :: OHM_STATE
      REAL(KIND(1D0)) :: qn_av = 0.0D0 ! weighted average of net all-wave radiation [W m-2]
      REAL(KIND(1D0)) :: dqndt = 0.0D0 ! rate of change of net radiation [W m-2 h-1]
      REAL(KIND(1D0)) :: qn_s_av = 0.0D0 ! weighted average of qn over snow [W m-2]
      REAL(KIND(1D0)) :: dqnsdt = 0.0D0 ! Rate of change of net radiation [W m-2 h-1]
      REAL(KIND(1D0)) :: a1 = 0.0D0 !AnOHM coefficients of grid [-]
      REAL(KIND(1D0)) :: a2 = 0.0D0 ! AnOHM coefficients of grid [h]
      REAL(KIND(1D0)) :: a3 = 0.0D0 !AnOHM coefficients of grid [W m-2]
      ! all variables are intensive and thus can be used for iteration safety
      REAL(KIND(1D0)) :: t2_prev = 0.0D0 ! previous day midnight air temperature [degC]
      REAL(KIND(1D0)) :: ws_rav = 0.0D0 ! running average of wind speed [m s-1]
      REAL(KIND(1D0)) :: tair_prev = 0.0D0
      REAL(KIND(1D0)) :: qn_rav = 0.0D0 ! running average of net radiation [W m-2]
      REAL(KIND(1D0)) :: a1_bldg = 0.0D0 ! Dynamic OHM coefficients of buildings
      REAL(KIND(1D0)) :: a2_bldg = 0.0D0 ! Dynamic OHM coefficients of buildings
      REAL(KIND(1D0)) :: a3_bldg = 0.0D0 ! Dynamic OHM coefficients of buildings
      REAL(KIND(1D0)) :: a1_paved = 0.0D0! Dynamic OHM coefficients of paved
      REAL(KIND(1D0)) :: a2_paved = 0.0D0! Dynamic OHM coefficients of paved
      REAL(KIND(1D0)) :: a3_paved = 0.0D0! Dynamic OHM coefficients of paved
      REAL(KIND(1D0)) :: a1_evetr = 0.0D0! Dynamic OHM coefficients of evetree
      REAL(KIND(1D0)) :: a2_evetr = 0.0D0! Dynamic OHM coefficients of evetree
      REAL(KIND(1D0)) :: a3_evetr = 0.0D0! Dynamic OHM coefficients of evetree
      REAL(KIND(1D0)) :: a1_dectr = 0.0D0! Dynamic OHM coefficients of dectree
      REAL(KIND(1D0)) :: a2_dectr = 0.0D0! Dynamic OHM coefficients of dectree
      REAL(KIND(1D0)) :: a3_dectr = 0.0D0! Dynamic OHM coefficients of dectree
      REAL(KIND(1D0)) :: a1_grass = 0.0D0! Dynamic OHM coefficients of grass
      REAL(KIND(1D0)) :: a2_grass = 0.0D0! Dynamic OHM coefficients of grass
      REAL(KIND(1D0)) :: a3_grass = 0.0D0! Dynamic OHM coefficients of grass
      REAL(KIND(1D0)) :: a1_bsoil = 0.0D0! Dynamic OHM coefficients of bare soil
      REAL(KIND(1D0)) :: a2_bsoil = 0.0D0! Dynamic OHM coefficients of bare soil
      REAL(KIND(1D0)) :: a3_bsoil = 0.0D0! Dynamic OHM coefficients of bare soil
      REAL(KIND(1D0)) :: a1_water = 0.0D0! Dynamic OHM coefficients of water
      REAL(KIND(1D0)) :: a2_water = 0.0D0! Dynamic OHM coefficients of water
      REAL(KIND(1D0)) :: a3_water = 0.0D0! Dynamic OHM coefficients of water
      INTEGER :: anohm_working_day = -999
      INTEGER :: anohm_working_count = 0
      REAL(KIND(1D0)), DIMENSION(ANOHM_MAX_SAMPLES) :: anohm_working_tHr = -999.0D0
      REAL(KIND(1D0)), DIMENSION(ANOHM_MAX_SAMPLES) :: anohm_working_sd = -999.0D0
      REAL(KIND(1D0)), DIMENSION(ANOHM_MAX_SAMPLES) :: anohm_working_ta = -999.0D0
      REAL(KIND(1D0)), DIMENSION(ANOHM_MAX_SAMPLES) :: anohm_working_rh = -999.0D0
      REAL(KIND(1D0)), DIMENSION(ANOHM_MAX_SAMPLES) :: anohm_working_pres = -999.0D0
      REAL(KIND(1D0)), DIMENSION(ANOHM_MAX_SAMPLES) :: anohm_working_ws = -999.0D0
      REAL(KIND(1D0)), DIMENSION(ANOHM_MAX_SAMPLES) :: anohm_working_ah = -999.0D0
      INTEGER :: anohm_coeff_day = -999
      INTEGER :: anohm_coeff_count = 0
      LOGICAL :: anohm_coeff_ready = .FALSE.
      REAL(KIND(1D0)), DIMENSION(ANOHM_MAX_SAMPLES) :: anohm_coeff_tHr = -999.0D0
      REAL(KIND(1D0)), DIMENSION(ANOHM_MAX_SAMPLES) :: anohm_coeff_sd = -999.0D0
      REAL(KIND(1D0)), DIMENSION(ANOHM_MAX_SAMPLES) :: anohm_coeff_ta = -999.0D0
      REAL(KIND(1D0)), DIMENSION(ANOHM_MAX_SAMPLES) :: anohm_coeff_rh = -999.0D0
      REAL(KIND(1D0)), DIMENSION(ANOHM_MAX_SAMPLES) :: anohm_coeff_pres = -999.0D0
      REAL(KIND(1D0)), DIMENSION(ANOHM_MAX_SAMPLES) :: anohm_coeff_ws = -999.0D0
      REAL(KIND(1D0)), DIMENSION(ANOHM_MAX_SAMPLES) :: anohm_coeff_ah = -999.0D0
      REAL(KIND(1D0)), DIMENSION(nsurf) :: anohm_a1_surf = 0.0D0
      REAL(KIND(1D0)), DIMENSION(nsurf) :: anohm_a2_surf = 0.0D0
      REAL(KIND(1D0)), DIMENSION(nsurf) :: anohm_a3_surf = 0.0D0
      ! flag for iteration safety - YES
      LOGICAL :: iter_safe = .TRUE.
   END TYPE OHM_STATE

   TYPE, PUBLIC :: CONDUCTANCE_PRM
      REAL(KIND(1D0)) :: g_max = 0.0D0 !Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)) :: g_k = 0.0D0
      REAL(KIND(1D0)) :: g_q_base = 0.0D0
      REAL(KIND(1D0)) :: g_q_shape = 0.0D0
      REAL(KIND(1D0)) :: g_t = 0.0D0
      REAL(KIND(1D0)) :: g_sm = 0.0D0 ! Fitted parameters related to surface res. calculations
      REAL(KIND(1D0)) :: kmax = 0.0D0 ! annual maximum hourly solar radiation [W m-2]
      ! TODO Should this not be moved to the physics options!
      INTEGER :: gsmodel = 0 ! choice of gs parameterisation (1 = Ja11, 2 = Wa16) [-]
      REAL(KIND(1D0)) :: s1 = 0.0D0 ! a parameter related to soil moisture dependence [-]
      REAL(KIND(1D0)) :: s2 = 0.0D0 ! a parameter related to soil moisture dependence [mm]
      REAL(KIND(1D0)) :: TH = 0.0D0 ! upper air temperature limit [degC]
      REAL(KIND(1D0)) :: TL = 0.0D0 ! lower air temperature limit [degC]
   END TYPE CONDUCTANCE_PRM

   TYPE, PUBLIC :: ROUGHNESS_STATE
      ! this type is used to collect the intermediate results in the SUEWS model

      ! calculated values of FAI
      REAL(KIND(1D0)) :: FAIBldg_use = 0.0D0 ! frontal area index of buildings [-]
      REAL(KIND(1D0)) :: FAIEveTree_use = 0.0D0 ! frontal area index of evergreen trees [-]
      REAL(KIND(1D0)) :: FAIDecTree_use = 0.0D0 ! frontal area index of deciduous trees [-]

      REAL(KIND(1D0)) :: FAI = 0.0D0 ! frontal area index [-]
      REAL(KIND(1D0)) :: PAI = 0.0D0 ! plan area index [-]
      REAL(KIND(1D0)) :: Zh = 0.0D0 ! effective height of bluff bodies [m]
      REAL(KIND(1D0)) :: z0m = 0.0D0 ! aerodynamic roughness length [m]
      REAL(KIND(1D0)) :: z0v = 0.0D0 ! roughness for heat [m]
      REAL(KIND(1D0)) :: zdm = 0.0D0 ! zero-plance displacement [m]
      REAL(KIND(1D0)) :: ZZD = 0.0D0 ! z-zdm [m]

      ! flag for iteration safety - YES
      ! all variables are intensive and thus can be used for iteration safety
      LOGICAL :: iter_safe = .TRUE.

   END TYPE ROUGHNESS_STATE

end module module_type_surface
