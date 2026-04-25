module module_type_vegetation

   USE module_ctrl_const_allocate, ONLY: &
      nsurf, nvegsurf

   implicit none

   TYPE, PUBLIC :: bioCO2_PRM
      REAL(KIND(1D0)) :: beta_bioco2 = 0.0D0 ! The light-saturated gross photosynthesis of the canopy [umol m-2 s-1 ]
      REAL(KIND(1D0)) :: beta_enh_bioco2 = 0.0D0 ! Part of the beta coefficient related to the fraction of vegetation [umol m-2 s-1 ]
      REAL(KIND(1D0)) :: alpha_bioco2 = 0.0D0 ! The mean apparent ecosystem quantum. Represents the initial slope of the light-response curve [-]
      REAL(KIND(1D0)) :: alpha_enh_bioco2 = 0.0D0 ! Part of the alpha coefficient related to the fraction of vegetation [-]
      REAL(KIND(1D0)) :: resp_a = 0.0D0 !Respiration coefficient a
      REAL(KIND(1D0)) :: resp_b = 0.0D0 !Respiration coefficient b - related to air temperature dependency
      REAL(KIND(1D0)) :: theta_bioco2 = 0.0D0 ! The convexity of the curve at light saturation [-]
      REAL(KIND(1D0)) :: min_res_bioCO2 = 0.0D0 ! Minimum soil respiration rate (for cold-temperature limit) [umol m-2 s-1]
   END TYPE bioCO2_PRM

   TYPE, PUBLIC :: LAI_PRM
      REAL(KIND(1D0)) :: baset = 0.0D0 ! Base temperature for growing degree days (GDD) [degC]
      REAL(KIND(1D0)) :: gddfull = 0.0D0 ! Growing degree days for full capacity [degC]
      REAL(KIND(1D0)) :: basete = 0.0D0 ! Base temperature for senescence degree days (SDD) [degC]
      REAL(KIND(1D0)) :: sddfull = 0.0D0 ! Senescence degree days for full capacity [degC]
      REAL(KIND(1D0)) :: laimin = 0.0D0 ! Minimum LAI [m2 m-2]
      REAL(KIND(1D0)) :: laimax = 0.0D0 ! Maximum LAI [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(4) :: laipower = 0.0D0 ! Coefficients for LAI equation: 1,2 - leaf growth; 3,4 - leaf off [-]
      INTEGER :: laitype = 0 ! LAI equation to use: original (0) or new (1) [-]
   END TYPE LAI_PRM

   TYPE, PUBLIC :: PHENOLOGY_STATE
      REAL(KIND(1D0)), DIMENSION(NSURF) :: alb = 0.0D0 ! Effective surface albedo (calculated in the current time step) [-]
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: lai_id = 0.0D0 ! LAI [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: GDD_id = 0.0D0 ! Growing degree days [degC]
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: SDD_id = 0.0D0 ! Senescence degree days [degC]
      REAL(KIND(1D0)) :: VegPhenLumps = 0.0D0 ! Phenology indicator used by LUMPS [-] (NOT USED - TO BE REMOVED)
      REAL(KIND(1D0)) :: porosity_id = 0.0D0 ! Porosity of each surface type [-]
      REAL(KIND(1D0)) :: decidcap_id = 0.0D0 ! Storage capacity of deciduous surface (DecTr) [mm]
      REAL(KIND(1D0)) :: albDecTr_id = 0.0D0 ! Albedo of deciduous trees [-]
      REAL(KIND(1D0)) :: albEveTr_id = 0.0D0 ! Albedo of evergreen trees [-]
      REAL(KIND(1D0)) :: albGrass_id = 0.0D0 ! Albedo of grass [-]
      REAL(KIND(1D0)) :: Tmin_id = 0.0D0 ! Daily minimum temperature [degC]
      REAL(KIND(1D0)) :: Tmax_id = 0.0D0 ! Daily maximum temperature [degC]
      REAL(KIND(1D0)) :: lenDay_id = 0.0D0 ! Daytime length [h]
      REAL(KIND(1D0)) :: TempVeg = 0.0D0 ! Temporary vegetative surface fraction adjusted by rainfall [-]
      REAL(KIND(1D0)), DIMENSION(6, NSURF) :: StoreDrainPrm = 0.0D0 ! Coefficients used in drainage calculation [-]
      REAL(KIND(1D0)) :: gfunc = 0.0D0 ! Stomatal conductance function [-]
      REAL(KIND(1D0)) :: gsc = 0.0D0 ! Surface layer conductance [s m-1]
      REAL(KIND(1D0)) :: g_kdown = 0.0D0 ! Surface conductance function for shortwave radiation [-]
      REAL(KIND(1D0)) :: g_dq = 0.0D0 ! Surface conductance function for specific humidity [-]
      REAL(KIND(1D0)) :: g_ta = 0.0D0 ! Surface conductance function for air temperature [-]
      REAL(KIND(1D0)) :: g_smd = 0.0D0 ! Surface conductance function for soil moisture deficit [-]
      REAL(KIND(1D0)) :: g_lai = 0.0D0 ! Surface conductance function for LAI [-]
      ! flag for iteration safety - NO
      ! GDD_id, SDD_id are extensive quantities and thus cannot be used for iteration safety
      LOGICAL :: iter_safe = .FALSE.
   END TYPE PHENOLOGY_STATE

end module module_type_vegetation
