module module_type_atmosphere

   USE module_ctrl_const_allocate, ONLY: &
      nsurf

   implicit none

   TYPE, PUBLIC :: solar_State
      REAL(KIND(1D0)) :: azimuth_deg = 0.0D0 !solar azimuth [angle]
      REAL(KIND(1D0)) :: ZENITH_deg = 0.0D0 !solar zenith angle [deg]

      ! flag for iteration safety - YES
      ! all variables are intensive and thus can be used for iteration safety
      LOGICAL :: iter_safe = .TRUE.
   END TYPE solar_State

   TYPE, PUBLIC :: atm_state
      REAL(KIND(1D0)) :: fcld = 0.0D0 !estomated cloud fraction [-]
      REAL(KIND(1D0)) :: avcp = 0.0D0 !Specific heat capacity
      REAL(KIND(1D0)) :: dens_dry = 0.0D0 !Dry air density kg m-3
      REAL(KIND(1D0)) :: avdens = 0.0D0 !Average air density
      REAL(KIND(1D0)) :: dq = 0.0D0 !Specific humidity deficit
      REAL(KIND(1D0)) :: Ea_hPa = 0.0D0 !Water vapour pressure in hPa
      REAL(KIND(1D0)) :: Es_hPa = 0.0D0 !Saturation vapour pressure in hPa
      REAL(KIND(1D0)) :: lv_J_kg = 0.0D0 !Latent heat of vaporization in [J kg-1]
      REAL(KIND(1D0)) :: lvS_J_kg = 0.0D0 !latent heat of sublimation [J kg-1]
      REAL(KIND(1D0)) :: tlv = 0.0D0 !Latent heat of vaporization per timestep [J kg-1 s-1] (tlv=lv_J_kg/tstep_real)
      REAL(KIND(1D0)) :: psyc_hPa = 0.0D0 !Psychometric constant in hPa
      REAL(KIND(1D0)) :: psycIce_hPa = 0.0D0 !Psychometric constant in hPa for snow
      REAL(KIND(1D0)) :: s_Pa = 0.0D0 !Vapour pressure versus temperature slope in Pa
      REAL(KIND(1D0)) :: s_hpa = 0.0D0 !Vapour pressure versus temperature slope in hPa
      REAL(KIND(1D0)) :: sIce_hpa = 0.0D0 !Vapour pressure versus temperature slope in hPa above ice/snow
      REAL(KIND(1D0)) :: vpd_hPa = 0.0D0 !Vapour pressure deficit in hPa
      REAL(KIND(1D0)) :: vpd_pa = 0.0D0 !Vapour pressure deficit in Pa
      REAL(KIND(1D0)) :: U10_ms = 0.0D0 !average wind speed at 10m [W m-1]
      REAL(KIND(1D0)) :: U_hbh = 0.0D0 ! wind speed at half building height [m s-1]
      REAL(KIND(1D0)) :: T2_C = 0.0D0 !modelled 2 meter air temperature [degC]
      REAL(KIND(1D0)) :: T_hbh_C = 0.0D0 ! air temperature at half building height [Deg C]
      REAL(KIND(1D0)) :: q2_gkg = 0.0D0 ! Air specific humidity at 2 m [g kg-1]
      REAL(KIND(1D0)) :: RH2 = 0.0D0 ! air relative humidity at 2m [-]
      REAL(KIND(1D0)) :: L_mod = 0.0D0 !Obukhov length [m]
      REAL(KIND(1D0)) :: zL = 0.0D0 ! Stability scale [-]
      REAL(KIND(1D0)) :: RA_h = 0.0D0 ! aerodynamic resistance [s m-1]
      REAL(KIND(1D0)) :: RS = 0.0D0 ! surface resistance [s m-1]
      REAL(KIND(1D0)) :: UStar = 0.0D0 !friction velocity [m s-1]
      REAL(KIND(1D0)) :: TStar = 0.0D0 !T*, temperature scale [-]
      REAL(KIND(1D0)) :: RB = 0.0D0 !boundary layer resistance shuttleworth
      REAL(KIND(1D0)) :: Tair_av = 0.0D0 ! 5-day moving average of air temperature [degC]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: rss_surf = 0.0D0 ! surface resistance adjusted by surface wetness state[s m-1]

      ! flag for iteration safety - YES
      ! all variables are intensive and thus can be used for iteration safety
      LOGICAL :: iter_safe = .TRUE.
   END TYPE atm_state

end module module_type_atmosphere
