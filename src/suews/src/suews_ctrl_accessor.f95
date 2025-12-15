!> @file suews_ctrl_accessor.f95
!> @brief Accessor functions for nested state arrays in SUEWS_STATE
!>
!> This module provides getter and setter functions to access allocatable
!> arrays within nested state types (HEAT_STATE, HYDRO_STATE, SNOW_STATE)
!> from Python via f90wrap without requiring wrapping of the nested types.
!>
!> Note: 2D arrays are passed with explicit dimensions to work around
!> f90wrap limitations with assumed-shape arrays.
!>
!> @author SUEWS Development Team
!> @date 2025

MODULE module_ctrl_accessor
   USE module_ctrl_type, ONLY: SUEWS_STATE, HEAT_STATE, HYDRO_STATE, SNOW_STATE, &
                               flag_STATE, OHM_STATE, solar_State, ROUGHNESS_STATE, &
                               NHOOD_STATE, STEBBS_STATE
   USE module_type_atmosphere, ONLY: atm_state
   USE module_type_anthro, ONLY: anthroEmis_STATE
   USE module_type_vegetation, ONLY: PHENOLOGY_STATE
   USE module_ctrl_const_allocate, ONLY: nsurf, nvegsurf
   IMPLICIT NONE

CONTAINS

   !===============================================
   ! HEAT_STATE Accessors
   !===============================================

   !> Get dimensions of HEAT_STATE allocatable arrays
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] nlayer Number of layers (first dimension of temp_roof)
   !> @param[out] ndepth Number of depth levels (second dimension of temp_roof)
   !> @param[out] nsurf_out Number of surfaces (first dimension of temp_surf)
   SUBROUTINE get_heat_state_dims(state, nlayer, ndepth, nsurf_out)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(OUT) :: nlayer, ndepth, nsurf_out

      IF (ALLOCATED(state%heatState%temp_roof)) THEN
         nlayer = SIZE(state%heatState%temp_roof, 1)
         ndepth = SIZE(state%heatState%temp_roof, 2)
      ELSE
         nlayer = 0
         ndepth = 0
      END IF
      nsurf_out = nsurf
   END SUBROUTINE get_heat_state_dims

   !> Get temperature arrays from HEAT_STATE (with explicit dimensions)
   !> @param[in] state The SUEWS_STATE object
   !> @param[in] nlayer Number of layers
   !> @param[in] ndepth Number of depth levels
   !> @param[in] nsurf_in Number of surfaces
   !> @param[out] temp_roof Interface temperature in roof [degC]
   !> @param[out] temp_wall Interface temperature in wall [degC]
   !> @param[out] temp_surf Interface temperature in surface [degC]
   SUBROUTINE get_heat_state_temp(state, nlayer, ndepth, nsurf_in, &
                                  temp_roof, temp_wall, temp_surf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: nlayer, ndepth, nsurf_in
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(OUT) :: temp_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(OUT) :: temp_wall
      REAL(KIND(1D0)), DIMENSION(nsurf_in, ndepth), INTENT(OUT) :: temp_surf

      temp_roof = state%heatState%temp_roof
      temp_wall = state%heatState%temp_wall
      temp_surf = state%heatState%temp_surf
   END SUBROUTINE get_heat_state_temp

   !> Set temperature arrays in HEAT_STATE (with explicit dimensions)
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] nlayer Number of layers
   !> @param[in] ndepth Number of depth levels
   !> @param[in] nsurf_in Number of surfaces
   !> @param[in] temp_roof Interface temperature in roof [degC]
   !> @param[in] temp_wall Interface temperature in wall [degC]
   !> @param[in] temp_surf Interface temperature in surface [degC]
   SUBROUTINE set_heat_state_temp(state, nlayer, ndepth, nsurf_in, &
                                  temp_roof, temp_wall, temp_surf)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      INTEGER, INTENT(IN) :: nlayer, ndepth, nsurf_in
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(IN) :: temp_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(IN) :: temp_wall
      REAL(KIND(1D0)), DIMENSION(nsurf_in, ndepth), INTENT(IN) :: temp_surf

      state%heatState%temp_roof = temp_roof
      state%heatState%temp_wall = temp_wall
      state%heatState%temp_surf = temp_surf
   END SUBROUTINE set_heat_state_temp

   !> Get surface temperature arrays from HEAT_STATE (with explicit dimensions)
   !> @param[in] state The SUEWS_STATE object
   !> @param[in] nlayer Number of layers
   !> @param[in] nsurf_in Number of surfaces
   !> @param[out] tsfc_roof Roof surface temperature [degC]
   !> @param[out] tsfc_wall Wall surface temperature [degC]
   !> @param[out] tsfc_surf Surface temperature [degC]
   SUBROUTINE get_heat_state_tsfc(state, nlayer, nsurf_in, &
                                  tsfc_roof, tsfc_wall, tsfc_surf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: nlayer, nsurf_in
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: tsfc_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: tsfc_wall
      REAL(KIND(1D0)), DIMENSION(nsurf_in), INTENT(OUT) :: tsfc_surf

      tsfc_roof = state%heatState%tsfc_roof
      tsfc_wall = state%heatState%tsfc_wall
      tsfc_surf = state%heatState%tsfc_surf
   END SUBROUTINE get_heat_state_tsfc

   !> Set surface temperature arrays in HEAT_STATE (with explicit dimensions)
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] nlayer Number of layers
   !> @param[in] nsurf_in Number of surfaces
   !> @param[in] tsfc_roof Roof surface temperature [degC]
   !> @param[in] tsfc_wall Wall surface temperature [degC]
   !> @param[in] tsfc_surf Surface temperature [degC]
   SUBROUTINE set_heat_state_tsfc(state, nlayer, nsurf_in, &
                                  tsfc_roof, tsfc_wall, tsfc_surf)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      INTEGER, INTENT(IN) :: nlayer, nsurf_in
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: tsfc_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: tsfc_wall
      REAL(KIND(1D0)), DIMENSION(nsurf_in), INTENT(IN) :: tsfc_surf

      state%heatState%tsfc_roof = tsfc_roof
      state%heatState%tsfc_wall = tsfc_wall
      state%heatState%tsfc_surf = tsfc_surf
   END SUBROUTINE set_heat_state_tsfc

   !> Get flux arrays from HEAT_STATE (roof components, with explicit dimensions)
   !> @param[in] state The SUEWS_STATE object
   !> @param[in] nlayer Number of layers
   !> @param[out] qs_roof Storage heat flux for roof [W m-2]
   !> @param[out] qn_roof Net all-wave radiation of roof [W m-2]
   !> @param[out] qe_roof Latent heat flux of roof [W m-2]
   !> @param[out] qh_roof Sensible heat flux of roof [W m-2]
   SUBROUTINE get_heat_state_flux_roof(state, nlayer, &
                                       qs_roof, qn_roof, qe_roof, qh_roof)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: qs_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: qn_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: qe_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: qh_roof

      qs_roof = state%heatState%qs_roof
      qn_roof = state%heatState%qn_roof
      qe_roof = state%heatState%qe_roof
      qh_roof = state%heatState%qh_roof
   END SUBROUTINE get_heat_state_flux_roof

   !> Get flux arrays from HEAT_STATE (wall components, with explicit dimensions)
   !> @param[in] state The SUEWS_STATE object
   !> @param[in] nlayer Number of layers
   !> @param[out] qs_wall Storage heat flux for wall [W m-2]
   !> @param[out] qn_wall Net all-wave radiation of wall [W m-2]
   !> @param[out] qe_wall Latent heat flux of wall [W m-2]
   !> @param[out] qh_wall Sensible heat flux of wall [W m-2]
   SUBROUTINE get_heat_state_flux_wall(state, nlayer, &
                                       qs_wall, qn_wall, qe_wall, qh_wall)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: qs_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: qn_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: qe_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: qh_wall

      qs_wall = state%heatState%qs_wall
      qn_wall = state%heatState%qn_wall
      qe_wall = state%heatState%qe_wall
      qh_wall = state%heatState%qh_wall
   END SUBROUTINE get_heat_state_flux_wall

   !> Get scalar heat state values
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] qh Sensible heat flux [W m-2]
   !> @param[out] qe Latent heat flux [W m-2]
   !> @param[out] qs Storage heat flux [W m-2]
   !> @param[out] qn Net all-wave radiation [W m-2]
   !> @param[out] qf Anthropogenic heat flux [W m-2]
   !> @param[out] tsurf Surface temperature [degC]
   SUBROUTINE get_heat_state_scalars(state, qh, qe, qs, qn, qf, tsurf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: qh, qe, qs, qn, qf, tsurf

      qh = state%heatState%qh
      qe = state%heatState%qe
      qs = state%heatState%qs
      qn = state%heatState%qn
      qf = state%heatState%qf
      tsurf = state%heatState%tsurf
   END SUBROUTINE get_heat_state_scalars

   !> Set scalar heat state values
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] qh Sensible heat flux [W m-2]
   !> @param[in] qe Latent heat flux [W m-2]
   !> @param[in] qs Storage heat flux [W m-2]
   !> @param[in] qn Net all-wave radiation [W m-2]
   !> @param[in] qf Anthropogenic heat flux [W m-2]
   !> @param[in] tsurf Surface temperature [degC]
   SUBROUTINE set_heat_state_scalars(state, qh, qe, qs, qn, qf, tsurf)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: qh, qe, qs, qn, qf, tsurf

      state%heatState%qh = qh
      state%heatState%qe = qe
      state%heatState%qs = qs
      state%heatState%qn = qn
      state%heatState%qf = qf
      state%heatState%tsurf = tsurf
   END SUBROUTINE set_heat_state_scalars

   !===============================================
   ! HYDRO_STATE Accessors
   !===============================================

   !> Get dimensions of HYDRO_STATE allocatable arrays
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] nlayer Number of layers
   !> @param[out] nsurf_out Number of surfaces
   SUBROUTINE get_hydro_state_dims(state, nlayer, nsurf_out)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(OUT) :: nlayer, nsurf_out

      IF (ALLOCATED(state%hydroState%soilstore_roof)) THEN
         nlayer = SIZE(state%hydroState%soilstore_roof)
      ELSE
         nlayer = 0
      END IF
      nsurf_out = nsurf
   END SUBROUTINE get_hydro_state_dims

   !> Get soil store arrays from HYDRO_STATE (with explicit dimensions)
   !> @param[in] state The SUEWS_STATE object
   !> @param[in] nlayer Number of layers
   !> @param[out] soilstore_roof Soil moisture of roof [mm]
   !> @param[out] soilstore_wall Soil moisture of wall [mm]
   !> @param[out] soilstore_surf Soil store of surfaces [mm]
   SUBROUTINE get_hydro_state_soilstore(state, nlayer, &
                                        soilstore_roof, soilstore_wall, soilstore_surf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: soilstore_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: soilstore_wall
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: soilstore_surf

      soilstore_roof = state%hydroState%soilstore_roof
      soilstore_wall = state%hydroState%soilstore_wall
      soilstore_surf = state%hydroState%soilstore_surf
   END SUBROUTINE get_hydro_state_soilstore

   !> Set soil store arrays in HYDRO_STATE (with explicit dimensions)
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] nlayer Number of layers
   !> @param[in] soilstore_roof Soil moisture of roof [mm]
   !> @param[in] soilstore_wall Soil moisture of wall [mm]
   !> @param[in] soilstore_surf Soil store of surfaces [mm]
   SUBROUTINE set_hydro_state_soilstore(state, nlayer, &
                                        soilstore_roof, soilstore_wall, soilstore_surf)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      INTEGER, INTENT(IN) :: nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: soilstore_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: soilstore_wall
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: soilstore_surf

      state%hydroState%soilstore_roof = soilstore_roof
      state%hydroState%soilstore_wall = soilstore_wall
      state%hydroState%soilstore_surf = soilstore_surf
   END SUBROUTINE set_hydro_state_soilstore

   !> Get wetness state arrays from HYDRO_STATE (with explicit dimensions)
   !> @param[in] state The SUEWS_STATE object
   !> @param[in] nlayer Number of layers
   !> @param[out] state_roof Wetness status of roof [mm]
   !> @param[out] state_wall Wetness status of wall [mm]
   !> @param[out] state_surf Wetness status of surfaces [mm]
   SUBROUTINE get_hydro_state_wetness(state, nlayer, &
                                      state_roof, state_wall, state_surf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: state_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: state_wall
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: state_surf

      state_roof = state%hydroState%state_roof
      state_wall = state%hydroState%state_wall
      state_surf = state%hydroState%state_surf
   END SUBROUTINE get_hydro_state_wetness

   !> Set wetness state arrays in HYDRO_STATE (with explicit dimensions)
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] nlayer Number of layers
   !> @param[in] state_roof Wetness status of roof [mm]
   !> @param[in] state_wall Wetness status of wall [mm]
   !> @param[in] state_surf Wetness status of surfaces [mm]
   SUBROUTINE set_hydro_state_wetness(state, nlayer, &
                                      state_roof, state_wall, state_surf)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      INTEGER, INTENT(IN) :: nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: state_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: state_wall
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: state_surf

      state%hydroState%state_roof = state_roof
      state%hydroState%state_wall = state_wall
      state%hydroState%state_surf = state_surf
   END SUBROUTINE set_hydro_state_wetness

   !> Get evapotranspiration arrays from HYDRO_STATE (with explicit dimensions)
   !> @param[in] state The SUEWS_STATE object
   !> @param[in] nlayer Number of layers
   !> @param[out] ev_roof Evapotranspiration of roof [mm]
   !> @param[out] ev_wall Evapotranspiration of wall [mm]
   !> @param[out] ev_surf Evapotranspiration of surfaces [mm]
   SUBROUTINE get_hydro_state_evap(state, nlayer, &
                                   ev_roof, ev_wall, ev_surf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: ev_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: ev_wall
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: ev_surf

      ev_roof = state%hydroState%ev_roof
      ev_wall = state%hydroState%ev_wall
      ev_surf = state%hydroState%ev_surf
   END SUBROUTINE get_hydro_state_evap

   !> Get hydro state scalar values
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] smd Soil moisture deficit [mm]
   !> @param[out] runoff_per_tstep Runoff per timestep [mm]
   !> @param[out] ev_per_tstep Evaporation per timestep [mm]
   !> @param[out] drain_per_tstep Drainage per timestep [mm]
   SUBROUTINE get_hydro_state_scalars(state, smd, runoff_per_tstep, ev_per_tstep, drain_per_tstep)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: smd, runoff_per_tstep, ev_per_tstep, drain_per_tstep

      smd = state%hydroState%smd
      runoff_per_tstep = state%hydroState%runoff_per_tstep
      ev_per_tstep = state%hydroState%ev_per_tstep
      drain_per_tstep = state%hydroState%drain_per_tstep
   END SUBROUTINE get_hydro_state_scalars

   !===============================================
   ! SNOW_STATE Accessors
   !===============================================

   !> Get snow state fixed-size arrays (all nsurf dimension)
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] snowpack Snow water equivalent on each surface [mm]
   !> @param[out] snowfrac Snow fraction on each surface [-]
   !> @param[out] snowdens Snow density on each surface [kg m-3]
   !> @param[out] icefrac Ice fraction in snowpack [-]
   SUBROUTINE get_snow_state_arrays(state, snowpack, snowfrac, snowdens, icefrac)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: snowpack
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: snowfrac
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: snowdens
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: icefrac

      snowpack = state%snowState%snowpack
      snowfrac = state%snowState%snowfrac
      snowdens = state%snowState%snowdens
      icefrac = state%snowState%icefrac
   END SUBROUTINE get_snow_state_arrays

   !> Set snow state fixed-size arrays
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] snowpack Snow water equivalent on each surface [mm]
   !> @param[in] snowfrac Snow fraction on each surface [-]
   !> @param[in] snowdens Snow density on each surface [kg m-3]
   !> @param[in] icefrac Ice fraction in snowpack [-]
   SUBROUTINE set_snow_state_arrays(state, snowpack, snowfrac, snowdens, icefrac)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: snowpack
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: snowfrac
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: snowdens
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: icefrac

      state%snowState%snowpack = snowpack
      state%snowState%snowfrac = snowfrac
      state%snowState%snowdens = snowdens
      state%snowState%icefrac = icefrac
   END SUBROUTINE set_snow_state_arrays

   !> Get snow state scalar values
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] snowalb Snow albedo [-]
   !> @param[out] swe Overall snow water equivalent [mm]
   !> @param[out] mwh Snowmelt [mm]
   !> @param[out] qm Snowmelt-related heat [W m-2]
   SUBROUTINE get_snow_state_scalars(state, snowalb, swe, mwh, qm)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: snowalb, swe, mwh, qm

      snowalb = state%snowState%snowalb
      swe = state%snowState%swe
      mwh = state%snowState%mwh
      qm = state%snowState%qm
   END SUBROUTINE get_snow_state_scalars

   !> Set snow state scalar values
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] snowalb Snow albedo [-]
   !> @param[in] swe Overall snow water equivalent [mm]
   !> @param[in] mwh Snowmelt [mm]
   !> @param[in] qm Snowmelt-related heat [W m-2]
   SUBROUTINE set_snow_state_scalars(state, snowalb, swe, mwh, qm)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: snowalb, swe, mwh, qm

      state%snowState%snowalb = snowalb
      state%snowState%swe = swe
      state%snowState%mwh = mwh
      state%snowState%qm = qm
   END SUBROUTINE set_snow_state_scalars

   !===============================================
   ! FLAG_STATE Accessors
   !===============================================

   !> Get flag state values
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] flag_converge Convergence flag for surface temperature
   !> @param[out] i_iter Number of iterations for convergence
   !> @param[out] stebbs_bldg_init STEBBS building initialisation flag
   SUBROUTINE get_flag_state(state, flag_converge, i_iter, stebbs_bldg_init)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      LOGICAL, INTENT(OUT) :: flag_converge
      INTEGER, INTENT(OUT) :: i_iter, stebbs_bldg_init

      flag_converge = state%flagState%flag_converge
      i_iter = state%flagState%i_iter
      stebbs_bldg_init = state%flagState%stebbs_bldg_init
   END SUBROUTINE get_flag_state

   !> Set flag state values
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] flag_converge Convergence flag for surface temperature
   !> @param[in] i_iter Number of iterations for convergence
   !> @param[in] stebbs_bldg_init STEBBS building initialisation flag
   SUBROUTINE set_flag_state(state, flag_converge, i_iter, stebbs_bldg_init)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      LOGICAL, INTENT(IN) :: flag_converge
      INTEGER, INTENT(IN) :: i_iter, stebbs_bldg_init

      state%flagState%flag_converge = flag_converge
      state%flagState%i_iter = i_iter
      state%flagState%stebbs_bldg_init = stebbs_bldg_init
   END SUBROUTINE set_flag_state

   !===============================================
   ! SOLAR_STATE Accessors
   !===============================================

   !> Get solar state values
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] azimuth_deg Solar azimuth angle [deg]
   !> @param[out] zenith_deg Solar zenith angle [deg]
   SUBROUTINE get_solar_state(state, azimuth_deg, zenith_deg)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: azimuth_deg, zenith_deg

      azimuth_deg = state%solarState%azimuth_deg
      zenith_deg = state%solarState%ZENITH_deg
   END SUBROUTINE get_solar_state

   !> Set solar state values
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] azimuth_deg Solar azimuth angle [deg]
   !> @param[in] zenith_deg Solar zenith angle [deg]
   SUBROUTINE set_solar_state(state, azimuth_deg, zenith_deg)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: azimuth_deg, zenith_deg

      state%solarState%azimuth_deg = azimuth_deg
      state%solarState%ZENITH_deg = zenith_deg
   END SUBROUTINE set_solar_state

   !===============================================
   ! ROUGHNESS_STATE Accessors
   !===============================================

   !> Get roughness state values
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] FAIBldg_use Frontal area index of buildings [-]
   !> @param[out] FAIEveTree_use Frontal area index of evergreen trees [-]
   !> @param[out] FAIDecTree_use Frontal area index of deciduous trees [-]
   !> @param[out] FAI Total frontal area index [-]
   !> @param[out] PAI Plan area index [-]
   !> @param[out] Zh Effective height of bluff bodies [m]
   !> @param[out] z0m Aerodynamic roughness length [m]
   !> @param[out] z0v Roughness for heat [m]
   !> @param[out] zdm Zero-plane displacement [m]
   !> @param[out] ZZD z-zdm [m]
   SUBROUTINE get_roughness_state(state, FAIBldg_use, FAIEveTree_use, FAIDecTree_use, &
                                  FAI, PAI, Zh, z0m, z0v, zdm, ZZD)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: FAIBldg_use, FAIEveTree_use, FAIDecTree_use
      REAL(KIND(1D0)), INTENT(OUT) :: FAI, PAI, Zh, z0m, z0v, zdm, ZZD

      FAIBldg_use = state%roughnessState%FAIBldg_use
      FAIEveTree_use = state%roughnessState%FAIEveTree_use
      FAIDecTree_use = state%roughnessState%FAIDecTree_use
      FAI = state%roughnessState%FAI
      PAI = state%roughnessState%PAI
      Zh = state%roughnessState%Zh
      z0m = state%roughnessState%z0m
      z0v = state%roughnessState%z0v
      zdm = state%roughnessState%zdm
      ZZD = state%roughnessState%ZZD
   END SUBROUTINE get_roughness_state

   !> Set roughness state values
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] FAIBldg_use Frontal area index of buildings [-]
   !> @param[in] FAIEveTree_use Frontal area index of evergreen trees [-]
   !> @param[in] FAIDecTree_use Frontal area index of deciduous trees [-]
   !> @param[in] FAI Total frontal area index [-]
   !> @param[in] PAI Plan area index [-]
   !> @param[in] Zh Effective height of bluff bodies [m]
   !> @param[in] z0m Aerodynamic roughness length [m]
   !> @param[in] z0v Roughness for heat [m]
   !> @param[in] zdm Zero-plane displacement [m]
   !> @param[in] ZZD z-zdm [m]
   SUBROUTINE set_roughness_state(state, FAIBldg_use, FAIEveTree_use, FAIDecTree_use, &
                                  FAI, PAI, Zh, z0m, z0v, zdm, ZZD)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: FAIBldg_use, FAIEveTree_use, FAIDecTree_use
      REAL(KIND(1D0)), INTENT(IN) :: FAI, PAI, Zh, z0m, z0v, zdm, ZZD

      state%roughnessState%FAIBldg_use = FAIBldg_use
      state%roughnessState%FAIEveTree_use = FAIEveTree_use
      state%roughnessState%FAIDecTree_use = FAIDecTree_use
      state%roughnessState%FAI = FAI
      state%roughnessState%PAI = PAI
      state%roughnessState%Zh = Zh
      state%roughnessState%z0m = z0m
      state%roughnessState%z0v = z0v
      state%roughnessState%zdm = zdm
      state%roughnessState%ZZD = ZZD
   END SUBROUTINE set_roughness_state

   !===============================================
   ! NHOOD_STATE Accessors
   !===============================================

   !> Get neighbourhood state values
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] U_hbh_1dravg 24hr running average wind speed at half building height [m s-1]
   !> @param[out] QN_1dravg 24hr running average net all-wave radiation [W m-2]
   !> @param[out] Tair_mn_prev Previous midnight air temperature [degC]
   !> @param[out] iter_count Iteration count of convergence loop [-]
   SUBROUTINE get_nhood_state(state, U_hbh_1dravg, QN_1dravg, Tair_mn_prev, iter_count)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: U_hbh_1dravg, QN_1dravg, Tair_mn_prev, iter_count

      U_hbh_1dravg = state%nhoodState%U_hbh_1dravg
      QN_1dravg = state%nhoodState%QN_1dravg
      Tair_mn_prev = state%nhoodState%Tair_mn_prev
      iter_count = state%nhoodState%iter_count
   END SUBROUTINE get_nhood_state

   !> Set neighbourhood state values
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] U_hbh_1dravg 24hr running average wind speed at half building height [m s-1]
   !> @param[in] QN_1dravg 24hr running average net all-wave radiation [W m-2]
   !> @param[in] Tair_mn_prev Previous midnight air temperature [degC]
   !> @param[in] iter_count Iteration count of convergence loop [-]
   SUBROUTINE set_nhood_state(state, U_hbh_1dravg, QN_1dravg, Tair_mn_prev, iter_count)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: U_hbh_1dravg, QN_1dravg, Tair_mn_prev, iter_count

      state%nhoodState%U_hbh_1dravg = U_hbh_1dravg
      state%nhoodState%QN_1dravg = QN_1dravg
      state%nhoodState%Tair_mn_prev = Tair_mn_prev
      state%nhoodState%iter_count = iter_count
   END SUBROUTINE set_nhood_state

   !===============================================
   ! OHM_STATE Accessors
   !===============================================

   !> Get OHM state basic values (radiation and rates)
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] qn_av Weighted average of net all-wave radiation [W m-2]
   !> @param[out] dqndt Rate of change of net radiation [W m-2 h-1]
   !> @param[out] qn_s_av Weighted average of qn over snow [W m-2]
   !> @param[out] dqnsdt Rate of change of net radiation over snow [W m-2 h-1]
   SUBROUTINE get_ohm_state_radiation(state, qn_av, dqndt, qn_s_av, dqnsdt)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: qn_av, dqndt, qn_s_av, dqnsdt

      qn_av = state%ohmState%qn_av
      dqndt = state%ohmState%dqndt
      qn_s_av = state%ohmState%qn_s_av
      dqnsdt = state%ohmState%dqnsdt
   END SUBROUTINE get_ohm_state_radiation

   !> Set OHM state basic values (radiation and rates)
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] qn_av Weighted average of net all-wave radiation [W m-2]
   !> @param[in] dqndt Rate of change of net radiation [W m-2 h-1]
   !> @param[in] qn_s_av Weighted average of qn over snow [W m-2]
   !> @param[in] dqnsdt Rate of change of net radiation over snow [W m-2 h-1]
   SUBROUTINE set_ohm_state_radiation(state, qn_av, dqndt, qn_s_av, dqnsdt)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: qn_av, dqndt, qn_s_av, dqnsdt

      state%ohmState%qn_av = qn_av
      state%ohmState%dqndt = dqndt
      state%ohmState%qn_s_av = qn_s_av
      state%ohmState%dqnsdt = dqnsdt
   END SUBROUTINE set_ohm_state_radiation

   !> Get OHM state grid coefficients
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] a1 AnOHM coefficient of grid [-]
   !> @param[out] a2 AnOHM coefficient of grid [h]
   !> @param[out] a3 AnOHM coefficient of grid [W m-2]
   SUBROUTINE get_ohm_state_coef_grid(state, a1, a2, a3)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: a1, a2, a3

      a1 = state%ohmState%a1
      a2 = state%ohmState%a2
      a3 = state%ohmState%a3
   END SUBROUTINE get_ohm_state_coef_grid

   !> Set OHM state grid coefficients
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] a1 AnOHM coefficient of grid [-]
   !> @param[in] a2 AnOHM coefficient of grid [h]
   !> @param[in] a3 AnOHM coefficient of grid [W m-2]
   SUBROUTINE set_ohm_state_coef_grid(state, a1, a2, a3)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: a1, a2, a3

      state%ohmState%a1 = a1
      state%ohmState%a2 = a2
      state%ohmState%a3 = a3
   END SUBROUTINE set_ohm_state_coef_grid

   !> Get OHM state running averages
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] t2_prev Previous day midnight air temperature [degC]
   !> @param[out] ws_rav Running average of wind speed [m s-1]
   !> @param[out] tair_prev Previous air temperature [degC]
   !> @param[out] qn_rav Running average of net radiation [W m-2]
   SUBROUTINE get_ohm_state_averages(state, t2_prev, ws_rav, tair_prev, qn_rav)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: t2_prev, ws_rav, tair_prev, qn_rav

      t2_prev = state%ohmState%t2_prev
      ws_rav = state%ohmState%ws_rav
      tair_prev = state%ohmState%tair_prev
      qn_rav = state%ohmState%qn_rav
   END SUBROUTINE get_ohm_state_averages

   !> Set OHM state running averages
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] t2_prev Previous day midnight air temperature [degC]
   !> @param[in] ws_rav Running average of wind speed [m s-1]
   !> @param[in] tair_prev Previous air temperature [degC]
   !> @param[in] qn_rav Running average of net radiation [W m-2]
   SUBROUTINE set_ohm_state_averages(state, t2_prev, ws_rav, tair_prev, qn_rav)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: t2_prev, ws_rav, tair_prev, qn_rav

      state%ohmState%t2_prev = t2_prev
      state%ohmState%ws_rav = ws_rav
      state%ohmState%tair_prev = tair_prev
      state%ohmState%qn_rav = qn_rav
   END SUBROUTINE set_ohm_state_averages

   !> Get OHM state dynamic coefficients for all surfaces
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] a1_bldg, a2_bldg, a3_bldg Building coefficients
   !> @param[out] a1_paved, a2_paved, a3_paved Paved coefficients
   !> @param[out] a1_evetr, a2_evetr, a3_evetr Evergreen tree coefficients
   !> @param[out] a1_dectr, a2_dectr, a3_dectr Deciduous tree coefficients
   !> @param[out] a1_grass, a2_grass, a3_grass Grass coefficients
   !> @param[out] a1_bsoil, a2_bsoil, a3_bsoil Bare soil coefficients
   !> @param[out] a1_water, a2_water, a3_water Water coefficients
   SUBROUTINE get_ohm_state_coef_surf(state, &
                                      a1_bldg, a2_bldg, a3_bldg, &
                                      a1_paved, a2_paved, a3_paved, &
                                      a1_evetr, a2_evetr, a3_evetr, &
                                      a1_dectr, a2_dectr, a3_dectr, &
                                      a1_grass, a2_grass, a3_grass, &
                                      a1_bsoil, a2_bsoil, a3_bsoil, &
                                      a1_water, a2_water, a3_water)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: a1_bldg, a2_bldg, a3_bldg
      REAL(KIND(1D0)), INTENT(OUT) :: a1_paved, a2_paved, a3_paved
      REAL(KIND(1D0)), INTENT(OUT) :: a1_evetr, a2_evetr, a3_evetr
      REAL(KIND(1D0)), INTENT(OUT) :: a1_dectr, a2_dectr, a3_dectr
      REAL(KIND(1D0)), INTENT(OUT) :: a1_grass, a2_grass, a3_grass
      REAL(KIND(1D0)), INTENT(OUT) :: a1_bsoil, a2_bsoil, a3_bsoil
      REAL(KIND(1D0)), INTENT(OUT) :: a1_water, a2_water, a3_water

      a1_bldg = state%ohmState%a1_bldg
      a2_bldg = state%ohmState%a2_bldg
      a3_bldg = state%ohmState%a3_bldg
      a1_paved = state%ohmState%a1_paved
      a2_paved = state%ohmState%a2_paved
      a3_paved = state%ohmState%a3_paved
      a1_evetr = state%ohmState%a1_evetr
      a2_evetr = state%ohmState%a2_evetr
      a3_evetr = state%ohmState%a3_evetr
      a1_dectr = state%ohmState%a1_dectr
      a2_dectr = state%ohmState%a2_dectr
      a3_dectr = state%ohmState%a3_dectr
      a1_grass = state%ohmState%a1_grass
      a2_grass = state%ohmState%a2_grass
      a3_grass = state%ohmState%a3_grass
      a1_bsoil = state%ohmState%a1_bsoil
      a2_bsoil = state%ohmState%a2_bsoil
      a3_bsoil = state%ohmState%a3_bsoil
      a1_water = state%ohmState%a1_water
      a2_water = state%ohmState%a2_water
      a3_water = state%ohmState%a3_water
   END SUBROUTINE get_ohm_state_coef_surf

   !> Set OHM state dynamic coefficients for all surfaces
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] a1_bldg, a2_bldg, a3_bldg Building coefficients
   !> @param[in] a1_paved, a2_paved, a3_paved Paved coefficients
   !> @param[in] a1_evetr, a2_evetr, a3_evetr Evergreen tree coefficients
   !> @param[in] a1_dectr, a2_dectr, a3_dectr Deciduous tree coefficients
   !> @param[in] a1_grass, a2_grass, a3_grass Grass coefficients
   !> @param[in] a1_bsoil, a2_bsoil, a3_bsoil Bare soil coefficients
   !> @param[in] a1_water, a2_water, a3_water Water coefficients
   SUBROUTINE set_ohm_state_coef_surf(state, &
                                      a1_bldg, a2_bldg, a3_bldg, &
                                      a1_paved, a2_paved, a3_paved, &
                                      a1_evetr, a2_evetr, a3_evetr, &
                                      a1_dectr, a2_dectr, a3_dectr, &
                                      a1_grass, a2_grass, a3_grass, &
                                      a1_bsoil, a2_bsoil, a3_bsoil, &
                                      a1_water, a2_water, a3_water)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: a1_bldg, a2_bldg, a3_bldg
      REAL(KIND(1D0)), INTENT(IN) :: a1_paved, a2_paved, a3_paved
      REAL(KIND(1D0)), INTENT(IN) :: a1_evetr, a2_evetr, a3_evetr
      REAL(KIND(1D0)), INTENT(IN) :: a1_dectr, a2_dectr, a3_dectr
      REAL(KIND(1D0)), INTENT(IN) :: a1_grass, a2_grass, a3_grass
      REAL(KIND(1D0)), INTENT(IN) :: a1_bsoil, a2_bsoil, a3_bsoil
      REAL(KIND(1D0)), INTENT(IN) :: a1_water, a2_water, a3_water

      state%ohmState%a1_bldg = a1_bldg
      state%ohmState%a2_bldg = a2_bldg
      state%ohmState%a3_bldg = a3_bldg
      state%ohmState%a1_paved = a1_paved
      state%ohmState%a2_paved = a2_paved
      state%ohmState%a3_paved = a3_paved
      state%ohmState%a1_evetr = a1_evetr
      state%ohmState%a2_evetr = a2_evetr
      state%ohmState%a3_evetr = a3_evetr
      state%ohmState%a1_dectr = a1_dectr
      state%ohmState%a2_dectr = a2_dectr
      state%ohmState%a3_dectr = a3_dectr
      state%ohmState%a1_grass = a1_grass
      state%ohmState%a2_grass = a2_grass
      state%ohmState%a3_grass = a3_grass
      state%ohmState%a1_bsoil = a1_bsoil
      state%ohmState%a2_bsoil = a2_bsoil
      state%ohmState%a3_bsoil = a3_bsoil
      state%ohmState%a1_water = a1_water
      state%ohmState%a2_water = a2_water
      state%ohmState%a3_water = a3_water
   END SUBROUTINE set_ohm_state_coef_surf

   !===============================================
   ! ATM_STATE Accessors
   !===============================================

   !> Get atmospheric state thermodynamic properties
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] fcld Estimated cloud fraction [-]
   !> @param[out] avcp Specific heat capacity [J kg-1 K-1]
   !> @param[out] dens_dry Dry air density [kg m-3]
   !> @param[out] avdens Average air density [kg m-3]
   !> @param[out] dq Specific humidity deficit [kg kg-1]
   !> @param[out] lv_J_kg Latent heat of vaporisation [J kg-1]
   !> @param[out] lvS_J_kg Latent heat of sublimation [J kg-1]
   !> @param[out] tlv Latent heat per timestep [J kg-1 s-1]
   SUBROUTINE get_atm_state_thermo(state, fcld, avcp, dens_dry, avdens, dq, lv_J_kg, lvS_J_kg, tlv)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: fcld, avcp, dens_dry, avdens, dq, lv_J_kg, lvS_J_kg, tlv

      fcld = state%atmState%fcld
      avcp = state%atmState%avcp
      dens_dry = state%atmState%dens_dry
      avdens = state%atmState%avdens
      dq = state%atmState%dq
      lv_J_kg = state%atmState%lv_J_kg
      lvS_J_kg = state%atmState%lvS_J_kg
      tlv = state%atmState%tlv
   END SUBROUTINE get_atm_state_thermo

   !> Set atmospheric state thermodynamic properties
   !> @param[inout] state The SUEWS_STATE object
   SUBROUTINE set_atm_state_thermo(state, fcld, avcp, dens_dry, avdens, dq, lv_J_kg, lvS_J_kg, tlv)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: fcld, avcp, dens_dry, avdens, dq, lv_J_kg, lvS_J_kg, tlv

      state%atmState%fcld = fcld
      state%atmState%avcp = avcp
      state%atmState%dens_dry = dens_dry
      state%atmState%avdens = avdens
      state%atmState%dq = dq
      state%atmState%lv_J_kg = lv_J_kg
      state%atmState%lvS_J_kg = lvS_J_kg
      state%atmState%tlv = tlv
   END SUBROUTINE set_atm_state_thermo

   !> Get atmospheric state vapour pressure variables
   !> @param[in] state The SUEWS_STATE object
   SUBROUTINE get_atm_state_vapour(state, Ea_hPa, Es_hPa, psyc_hPa, psycIce_hPa, &
                                   s_Pa, s_hpa, sIce_hpa, vpd_hPa, vpd_pa)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: Ea_hPa, Es_hPa, psyc_hPa, psycIce_hPa
      REAL(KIND(1D0)), INTENT(OUT) :: s_Pa, s_hpa, sIce_hpa, vpd_hPa, vpd_pa

      Ea_hPa = state%atmState%Ea_hPa
      Es_hPa = state%atmState%Es_hPa
      psyc_hPa = state%atmState%psyc_hPa
      psycIce_hPa = state%atmState%psycIce_hPa
      s_Pa = state%atmState%s_Pa
      s_hpa = state%atmState%s_hpa
      sIce_hpa = state%atmState%sIce_hpa
      vpd_hPa = state%atmState%vpd_hPa
      vpd_pa = state%atmState%vpd_pa
   END SUBROUTINE get_atm_state_vapour

   !> Set atmospheric state vapour pressure variables
   !> @param[inout] state The SUEWS_STATE object
   SUBROUTINE set_atm_state_vapour(state, Ea_hPa, Es_hPa, psyc_hPa, psycIce_hPa, &
                                   s_Pa, s_hpa, sIce_hpa, vpd_hPa, vpd_pa)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: Ea_hPa, Es_hPa, psyc_hPa, psycIce_hPa
      REAL(KIND(1D0)), INTENT(IN) :: s_Pa, s_hpa, sIce_hpa, vpd_hPa, vpd_pa

      state%atmState%Ea_hPa = Ea_hPa
      state%atmState%Es_hPa = Es_hPa
      state%atmState%psyc_hPa = psyc_hPa
      state%atmState%psycIce_hPa = psycIce_hPa
      state%atmState%s_Pa = s_Pa
      state%atmState%s_hpa = s_hpa
      state%atmState%sIce_hpa = sIce_hpa
      state%atmState%vpd_hPa = vpd_hPa
      state%atmState%vpd_pa = vpd_pa
   END SUBROUTINE set_atm_state_vapour

   !> Get atmospheric state turbulence and stability variables
   !> @param[in] state The SUEWS_STATE object
   SUBROUTINE get_atm_state_turb(state, L_mod, zL, RA_h, RS, UStar, TStar, RB, Tair_av)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: L_mod, zL, RA_h, RS, UStar, TStar, RB, Tair_av

      L_mod = state%atmState%L_mod
      zL = state%atmState%zL
      RA_h = state%atmState%RA_h
      RS = state%atmState%RS
      UStar = state%atmState%UStar
      TStar = state%atmState%TStar
      RB = state%atmState%RB
      Tair_av = state%atmState%Tair_av
   END SUBROUTINE get_atm_state_turb

   !> Set atmospheric state turbulence and stability variables
   !> @param[inout] state The SUEWS_STATE object
   SUBROUTINE set_atm_state_turb(state, L_mod, zL, RA_h, RS, UStar, TStar, RB, Tair_av)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: L_mod, zL, RA_h, RS, UStar, TStar, RB, Tair_av

      state%atmState%L_mod = L_mod
      state%atmState%zL = zL
      state%atmState%RA_h = RA_h
      state%atmState%RS = RS
      state%atmState%UStar = UStar
      state%atmState%TStar = TStar
      state%atmState%RB = RB
      state%atmState%Tair_av = Tair_av
   END SUBROUTINE set_atm_state_turb

   !> Get atmospheric state near-surface diagnostics
   !> @param[in] state The SUEWS_STATE object
   SUBROUTINE get_atm_state_diag(state, U10_ms, U_hbh, T2_C, T_hbh_C, q2_gkg, RH2)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: U10_ms, U_hbh, T2_C, T_hbh_C, q2_gkg, RH2

      U10_ms = state%atmState%U10_ms
      U_hbh = state%atmState%U_hbh
      T2_C = state%atmState%T2_C
      T_hbh_C = state%atmState%T_hbh_C
      q2_gkg = state%atmState%q2_gkg
      RH2 = state%atmState%RH2
   END SUBROUTINE get_atm_state_diag

   !> Set atmospheric state near-surface diagnostics
   !> @param[inout] state The SUEWS_STATE object
   SUBROUTINE set_atm_state_diag(state, U10_ms, U_hbh, T2_C, T_hbh_C, q2_gkg, RH2)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: U10_ms, U_hbh, T2_C, T_hbh_C, q2_gkg, RH2

      state%atmState%U10_ms = U10_ms
      state%atmState%U_hbh = U_hbh
      state%atmState%T2_C = T2_C
      state%atmState%T_hbh_C = T_hbh_C
      state%atmState%q2_gkg = q2_gkg
      state%atmState%RH2 = RH2
   END SUBROUTINE set_atm_state_diag

   !> Get atmospheric state surface resistance array
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] rss_surf Surface resistance adjusted by wetness [s m-1]
   SUBROUTINE get_atm_state_rss_surf(state, rss_surf)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: rss_surf

      rss_surf = state%atmState%rss_surf
   END SUBROUTINE get_atm_state_rss_surf

   !> Set atmospheric state surface resistance array
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] rss_surf Surface resistance adjusted by wetness [s m-1]
   SUBROUTINE set_atm_state_rss_surf(state, rss_surf)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: rss_surf

      state%atmState%rss_surf = rss_surf
   END SUBROUTINE set_atm_state_rss_surf

   !===============================================
   ! ANTHRO_EMIS_STATE Accessors
   !===============================================

   !> Get anthropogenic emissions state HDD array
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] HDD_id Heating Degree Days array [degC d]
   SUBROUTINE get_anthro_state_hdd(state, HDD_id)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(12), INTENT(OUT) :: HDD_id

      HDD_id = state%anthroemisState%HDD_id
   END SUBROUTINE get_anthro_state_hdd

   !> Set anthropogenic emissions state HDD array
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] HDD_id Heating Degree Days array [degC d]
   SUBROUTINE set_anthro_state_hdd(state, HDD_id)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(12), INTENT(IN) :: HDD_id

      state%anthroemisState%HDD_id = HDD_id
   END SUBROUTINE set_anthro_state_hdd

   !> Get anthropogenic emissions state CO2 flux scalars
   !> @param[in] state The SUEWS_STATE object
   SUBROUTINE get_anthro_state_co2(state, Fc, Fc_anthro, Fc_biogen, Fc_build, &
                                   Fc_metab, Fc_photo, Fc_point, Fc_respi, Fc_traff)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: Fc, Fc_anthro, Fc_biogen, Fc_build
      REAL(KIND(1D0)), INTENT(OUT) :: Fc_metab, Fc_photo, Fc_point, Fc_respi, Fc_traff

      Fc = state%anthroemisState%Fc
      Fc_anthro = state%anthroemisState%Fc_anthro
      Fc_biogen = state%anthroemisState%Fc_biogen
      Fc_build = state%anthroemisState%Fc_build
      Fc_metab = state%anthroemisState%Fc_metab
      Fc_photo = state%anthroemisState%Fc_photo
      Fc_point = state%anthroemisState%Fc_point
      Fc_respi = state%anthroemisState%Fc_respi
      Fc_traff = state%anthroemisState%Fc_traff
   END SUBROUTINE get_anthro_state_co2

   !> Set anthropogenic emissions state CO2 flux scalars
   !> @param[inout] state The SUEWS_STATE object
   SUBROUTINE set_anthro_state_co2(state, Fc, Fc_anthro, Fc_biogen, Fc_build, &
                                   Fc_metab, Fc_photo, Fc_point, Fc_respi, Fc_traff)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: Fc, Fc_anthro, Fc_biogen, Fc_build
      REAL(KIND(1D0)), INTENT(IN) :: Fc_metab, Fc_photo, Fc_point, Fc_respi, Fc_traff

      state%anthroemisState%Fc = Fc
      state%anthroemisState%Fc_anthro = Fc_anthro
      state%anthroemisState%Fc_biogen = Fc_biogen
      state%anthroemisState%Fc_build = Fc_build
      state%anthroemisState%Fc_metab = Fc_metab
      state%anthroemisState%Fc_photo = Fc_photo
      state%anthroemisState%Fc_point = Fc_point
      state%anthroemisState%Fc_respi = Fc_respi
      state%anthroemisState%Fc_traff = Fc_traff
   END SUBROUTINE set_anthro_state_co2

   !===============================================
   ! PHENOLOGY_STATE Accessors
   !===============================================

   !> Get phenology state surface albedo array
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] alb Effective surface albedo [-]
   SUBROUTINE get_phen_state_alb(state, alb)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: alb

      alb = state%phenState%alb
   END SUBROUTINE get_phen_state_alb

   !> Set phenology state surface albedo array
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] alb Effective surface albedo [-]
   SUBROUTINE set_phen_state_alb(state, alb)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: alb

      state%phenState%alb = alb
   END SUBROUTINE set_phen_state_alb

   !> Get phenology state LAI and degree day arrays
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] lai_id LAI [m2 m-2]
   !> @param[out] GDD_id Growing degree days [degC]
   !> @param[out] SDD_id Senescence degree days [degC]
   SUBROUTINE get_phen_state_lai(state, lai_id, GDD_id, SDD_id)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(OUT) :: lai_id, GDD_id, SDD_id

      lai_id = state%phenState%lai_id
      GDD_id = state%phenState%GDD_id
      SDD_id = state%phenState%SDD_id
   END SUBROUTINE get_phen_state_lai

   !> Set phenology state LAI and degree day arrays
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] lai_id LAI [m2 m-2]
   !> @param[in] GDD_id Growing degree days [degC]
   !> @param[in] SDD_id Senescence degree days [degC]
   SUBROUTINE set_phen_state_lai(state, lai_id, GDD_id, SDD_id)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: lai_id, GDD_id, SDD_id

      state%phenState%lai_id = lai_id
      state%phenState%GDD_id = GDD_id
      state%phenState%SDD_id = SDD_id
   END SUBROUTINE set_phen_state_lai

   !> Get phenology state scalar values
   !> @param[in] state The SUEWS_STATE object
   SUBROUTINE get_phen_state_scalars(state, porosity_id, decidcap_id, &
                                     albDecTr_id, albEveTr_id, albGrass_id, &
                                     Tmin_id, Tmax_id, lenDay_id, TempVeg)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: porosity_id, decidcap_id
      REAL(KIND(1D0)), INTENT(OUT) :: albDecTr_id, albEveTr_id, albGrass_id
      REAL(KIND(1D0)), INTENT(OUT) :: Tmin_id, Tmax_id, lenDay_id, TempVeg

      porosity_id = state%phenState%porosity_id
      decidcap_id = state%phenState%decidcap_id
      albDecTr_id = state%phenState%albDecTr_id
      albEveTr_id = state%phenState%albEveTr_id
      albGrass_id = state%phenState%albGrass_id
      Tmin_id = state%phenState%Tmin_id
      Tmax_id = state%phenState%Tmax_id
      lenDay_id = state%phenState%lenDay_id
      TempVeg = state%phenState%TempVeg
   END SUBROUTINE get_phen_state_scalars

   !> Set phenology state scalar values
   !> @param[inout] state The SUEWS_STATE object
   SUBROUTINE set_phen_state_scalars(state, porosity_id, decidcap_id, &
                                     albDecTr_id, albEveTr_id, albGrass_id, &
                                     Tmin_id, Tmax_id, lenDay_id, TempVeg)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: porosity_id, decidcap_id
      REAL(KIND(1D0)), INTENT(IN) :: albDecTr_id, albEveTr_id, albGrass_id
      REAL(KIND(1D0)), INTENT(IN) :: Tmin_id, Tmax_id, lenDay_id, TempVeg

      state%phenState%porosity_id = porosity_id
      state%phenState%decidcap_id = decidcap_id
      state%phenState%albDecTr_id = albDecTr_id
      state%phenState%albEveTr_id = albEveTr_id
      state%phenState%albGrass_id = albGrass_id
      state%phenState%Tmin_id = Tmin_id
      state%phenState%Tmax_id = Tmax_id
      state%phenState%lenDay_id = lenDay_id
      state%phenState%TempVeg = TempVeg
   END SUBROUTINE set_phen_state_scalars

   !> Get phenology state conductance function values
   !> @param[in] state The SUEWS_STATE object
   SUBROUTINE get_phen_state_conductance(state, gfunc, gsc, g_kdown, g_dq, g_ta, g_smd, g_lai)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: gfunc, gsc, g_kdown, g_dq, g_ta, g_smd, g_lai

      gfunc = state%phenState%gfunc
      gsc = state%phenState%gsc
      g_kdown = state%phenState%g_kdown
      g_dq = state%phenState%g_dq
      g_ta = state%phenState%g_ta
      g_smd = state%phenState%g_smd
      g_lai = state%phenState%g_lai
   END SUBROUTINE get_phen_state_conductance

   !> Set phenology state conductance function values
   !> @param[inout] state The SUEWS_STATE object
   SUBROUTINE set_phen_state_conductance(state, gfunc, gsc, g_kdown, g_dq, g_ta, g_smd, g_lai)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: gfunc, gsc, g_kdown, g_dq, g_ta, g_smd, g_lai

      state%phenState%gfunc = gfunc
      state%phenState%gsc = gsc
      state%phenState%g_kdown = g_kdown
      state%phenState%g_dq = g_dq
      state%phenState%g_ta = g_ta
      state%phenState%g_smd = g_smd
      state%phenState%g_lai = g_lai
   END SUBROUTINE set_phen_state_conductance

   !> Get phenology state StoreDrainPrm array
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] StoreDrainPrm Drainage calculation coefficients [-]
   SUBROUTINE get_phen_state_drain(state, StoreDrainPrm)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(OUT) :: StoreDrainPrm

      StoreDrainPrm = state%phenState%StoreDrainPrm
   END SUBROUTINE get_phen_state_drain

   !> Set phenology state StoreDrainPrm array
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] StoreDrainPrm Drainage calculation coefficients [-]
   SUBROUTINE set_phen_state_drain(state, StoreDrainPrm)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(6, nsurf), INTENT(IN) :: StoreDrainPrm

      state%phenState%StoreDrainPrm = StoreDrainPrm
   END SUBROUTINE set_phen_state_drain

   !===============================================
   ! STEBBS_STATE Accessors
   !===============================================

   !> Get STEBBS state shortwave radiation components
   !> @param[in] state The SUEWS_STATE object
   SUBROUTINE get_stebbs_state_krad(state, Kdown2d, Kup2d, Kwest, Ksouth, Knorth, Keast)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: Kdown2d, Kup2d, Kwest, Ksouth, Knorth, Keast

      Kdown2d = state%stebbsState%Kdown2d
      Kup2d = state%stebbsState%Kup2d
      Kwest = state%stebbsState%Kwest
      Ksouth = state%stebbsState%Ksouth
      Knorth = state%stebbsState%Knorth
      Keast = state%stebbsState%Keast
   END SUBROUTINE get_stebbs_state_krad

   !> Set STEBBS state shortwave radiation components
   !> @param[inout] state The SUEWS_STATE object
   SUBROUTINE set_stebbs_state_krad(state, Kdown2d, Kup2d, Kwest, Ksouth, Knorth, Keast)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: Kdown2d, Kup2d, Kwest, Ksouth, Knorth, Keast

      state%stebbsState%Kdown2d = Kdown2d
      state%stebbsState%Kup2d = Kup2d
      state%stebbsState%Kwest = Kwest
      state%stebbsState%Ksouth = Ksouth
      state%stebbsState%Knorth = Knorth
      state%stebbsState%Keast = Keast
   END SUBROUTINE set_stebbs_state_krad

   !> Get STEBBS state longwave radiation components
   !> @param[in] state The SUEWS_STATE object
   SUBROUTINE get_stebbs_state_lrad(state, Ldown2d, Lup2d, Lwest, Lsouth, Lnorth, Least)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: Ldown2d, Lup2d, Lwest, Lsouth, Lnorth, Least

      Ldown2d = state%stebbsState%Ldown2d
      Lup2d = state%stebbsState%Lup2d
      Lwest = state%stebbsState%Lwest
      Lsouth = state%stebbsState%Lsouth
      Lnorth = state%stebbsState%Lnorth
      Least = state%stebbsState%Least
   END SUBROUTINE get_stebbs_state_lrad

   !> Set STEBBS state longwave radiation components
   !> @param[inout] state The SUEWS_STATE object
   SUBROUTINE set_stebbs_state_lrad(state, Ldown2d, Lup2d, Lwest, Lsouth, Lnorth, Least)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: Ldown2d, Lup2d, Lwest, Lsouth, Lnorth, Least

      state%stebbsState%Ldown2d = Ldown2d
      state%stebbsState%Lup2d = Lup2d
      state%stebbsState%Lwest = Lwest
      state%stebbsState%Lsouth = Lsouth
      state%stebbsState%Lnorth = Lnorth
      state%stebbsState%Least = Least
   END SUBROUTINE set_stebbs_state_lrad

   !> Get STEBBS state RSL profile arrays
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] zarray RSL layer heights [m]
   !> @param[out] dataoutLineURSL Wind speed array from RSL [m s-1]
   !> @param[out] dataoutLineTRSL Temperature array from RSL [degC]
   !> @param[out] dataoutLineqRSL Specific humidity array from RSL [g kg-1]
   SUBROUTINE get_stebbs_state_rsl(state, zarray, dataoutLineURSL, dataoutLineTRSL, dataoutLineqRSL)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), DIMENSION(30), INTENT(OUT) :: zarray, dataoutLineURSL, dataoutLineTRSL, dataoutLineqRSL

      zarray = state%stebbsState%zarray
      dataoutLineURSL = state%stebbsState%dataoutLineURSL
      dataoutLineTRSL = state%stebbsState%dataoutLineTRSL
      dataoutLineqRSL = state%stebbsState%dataoutLineqRSL
   END SUBROUTINE get_stebbs_state_rsl

   !> Set STEBBS state RSL profile arrays
   !> @param[inout] state The SUEWS_STATE object
   SUBROUTINE set_stebbs_state_rsl(state, zarray, dataoutLineURSL, dataoutLineTRSL, dataoutLineqRSL)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), DIMENSION(30), INTENT(IN) :: zarray, dataoutLineURSL, dataoutLineTRSL, dataoutLineqRSL

      state%stebbsState%zarray = zarray
      state%stebbsState%dataoutLineURSL = dataoutLineURSL
      state%stebbsState%dataoutLineTRSL = dataoutLineTRSL
      state%stebbsState%dataoutLineqRSL = dataoutLineqRSL
   END SUBROUTINE set_stebbs_state_rsl

   !> Get STEBBS state temperature scalars (building envelope)
   !> @param[in] state The SUEWS_STATE object
   SUBROUTINE get_stebbs_state_temps_envelope(state, &
         DeepSoilTemperature, OutdoorAirStartTemperature, &
         IndoorAirStartTemperature, IndoorMassStartTemperature, &
         WallIndoorSurfaceTemperature, WallOutdoorSurfaceTemperature, &
         RoofIndoorSurfaceTemperature, RoofOutdoorSurfaceTemperature, &
         WindowIndoorSurfaceTemperature, WindowOutdoorSurfaceTemperature, &
         GroundFloorIndoorSurfaceTemperature, GroundFloorOutdoorSurfaceTemperature)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: DeepSoilTemperature, OutdoorAirStartTemperature
      REAL(KIND(1D0)), INTENT(OUT) :: IndoorAirStartTemperature, IndoorMassStartTemperature
      REAL(KIND(1D0)), INTENT(OUT) :: WallIndoorSurfaceTemperature, WallOutdoorSurfaceTemperature
      REAL(KIND(1D0)), INTENT(OUT) :: RoofIndoorSurfaceTemperature, RoofOutdoorSurfaceTemperature
      REAL(KIND(1D0)), INTENT(OUT) :: WindowIndoorSurfaceTemperature, WindowOutdoorSurfaceTemperature
      REAL(KIND(1D0)), INTENT(OUT) :: GroundFloorIndoorSurfaceTemperature, GroundFloorOutdoorSurfaceTemperature

      DeepSoilTemperature = state%stebbsState%DeepSoilTemperature
      OutdoorAirStartTemperature = state%stebbsState%OutdoorAirStartTemperature
      IndoorAirStartTemperature = state%stebbsState%IndoorAirStartTemperature
      IndoorMassStartTemperature = state%stebbsState%IndoorMassStartTemperature
      WallIndoorSurfaceTemperature = state%stebbsState%WallIndoorSurfaceTemperature
      WallOutdoorSurfaceTemperature = state%stebbsState%WallOutdoorSurfaceTemperature
      RoofIndoorSurfaceTemperature = state%stebbsState%RoofIndoorSurfaceTemperature
      RoofOutdoorSurfaceTemperature = state%stebbsState%RoofOutdoorSurfaceTemperature
      WindowIndoorSurfaceTemperature = state%stebbsState%WindowIndoorSurfaceTemperature
      WindowOutdoorSurfaceTemperature = state%stebbsState%WindowOutdoorSurfaceTemperature
      GroundFloorIndoorSurfaceTemperature = state%stebbsState%GroundFloorIndoorSurfaceTemperature
      GroundFloorOutdoorSurfaceTemperature = state%stebbsState%GroundFloorOutdoorSurfaceTemperature
   END SUBROUTINE get_stebbs_state_temps_envelope

   !> Set STEBBS state temperature scalars (building envelope)
   !> @param[inout] state The SUEWS_STATE object
   SUBROUTINE set_stebbs_state_temps_envelope(state, &
         DeepSoilTemperature, OutdoorAirStartTemperature, &
         IndoorAirStartTemperature, IndoorMassStartTemperature, &
         WallIndoorSurfaceTemperature, WallOutdoorSurfaceTemperature, &
         RoofIndoorSurfaceTemperature, RoofOutdoorSurfaceTemperature, &
         WindowIndoorSurfaceTemperature, WindowOutdoorSurfaceTemperature, &
         GroundFloorIndoorSurfaceTemperature, GroundFloorOutdoorSurfaceTemperature)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: DeepSoilTemperature, OutdoorAirStartTemperature
      REAL(KIND(1D0)), INTENT(IN) :: IndoorAirStartTemperature, IndoorMassStartTemperature
      REAL(KIND(1D0)), INTENT(IN) :: WallIndoorSurfaceTemperature, WallOutdoorSurfaceTemperature
      REAL(KIND(1D0)), INTENT(IN) :: RoofIndoorSurfaceTemperature, RoofOutdoorSurfaceTemperature
      REAL(KIND(1D0)), INTENT(IN) :: WindowIndoorSurfaceTemperature, WindowOutdoorSurfaceTemperature
      REAL(KIND(1D0)), INTENT(IN) :: GroundFloorIndoorSurfaceTemperature, GroundFloorOutdoorSurfaceTemperature

      state%stebbsState%DeepSoilTemperature = DeepSoilTemperature
      state%stebbsState%OutdoorAirStartTemperature = OutdoorAirStartTemperature
      state%stebbsState%IndoorAirStartTemperature = IndoorAirStartTemperature
      state%stebbsState%IndoorMassStartTemperature = IndoorMassStartTemperature
      state%stebbsState%WallIndoorSurfaceTemperature = WallIndoorSurfaceTemperature
      state%stebbsState%WallOutdoorSurfaceTemperature = WallOutdoorSurfaceTemperature
      state%stebbsState%RoofIndoorSurfaceTemperature = RoofIndoorSurfaceTemperature
      state%stebbsState%RoofOutdoorSurfaceTemperature = RoofOutdoorSurfaceTemperature
      state%stebbsState%WindowIndoorSurfaceTemperature = WindowIndoorSurfaceTemperature
      state%stebbsState%WindowOutdoorSurfaceTemperature = WindowOutdoorSurfaceTemperature
      state%stebbsState%GroundFloorIndoorSurfaceTemperature = GroundFloorIndoorSurfaceTemperature
      state%stebbsState%GroundFloorOutdoorSurfaceTemperature = GroundFloorOutdoorSurfaceTemperature
   END SUBROUTINE set_stebbs_state_temps_envelope

   !> Get STEBBS state water tank temperatures
   !> @param[in] state The SUEWS_STATE object
   SUBROUTINE get_stebbs_state_temps_water(state, &
         WaterTankTemperature, InternalWallWaterTankTemperature, ExternalWallWaterTankTemperature, &
         MainsWaterTemperature, DomesticHotWaterTemperatureInUseInBuilding, &
         InternalWallDHWVesselTemperature, ExternalWallDHWVesselTemperature)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: WaterTankTemperature, InternalWallWaterTankTemperature
      REAL(KIND(1D0)), INTENT(OUT) :: ExternalWallWaterTankTemperature, MainsWaterTemperature
      REAL(KIND(1D0)), INTENT(OUT) :: DomesticHotWaterTemperatureInUseInBuilding
      REAL(KIND(1D0)), INTENT(OUT) :: InternalWallDHWVesselTemperature, ExternalWallDHWVesselTemperature

      WaterTankTemperature = state%stebbsState%WaterTankTemperature
      InternalWallWaterTankTemperature = state%stebbsState%InternalWallWaterTankTemperature
      ExternalWallWaterTankTemperature = state%stebbsState%ExternalWallWaterTankTemperature
      MainsWaterTemperature = state%stebbsState%MainsWaterTemperature
      DomesticHotWaterTemperatureInUseInBuilding = state%stebbsState%DomesticHotWaterTemperatureInUseInBuilding
      InternalWallDHWVesselTemperature = state%stebbsState%InternalWallDHWVesselTemperature
      ExternalWallDHWVesselTemperature = state%stebbsState%ExternalWallDHWVesselTemperature
   END SUBROUTINE get_stebbs_state_temps_water

   !> Set STEBBS state water tank temperatures
   !> @param[inout] state The SUEWS_STATE object
   SUBROUTINE set_stebbs_state_temps_water(state, &
         WaterTankTemperature, InternalWallWaterTankTemperature, ExternalWallWaterTankTemperature, &
         MainsWaterTemperature, DomesticHotWaterTemperatureInUseInBuilding, &
         InternalWallDHWVesselTemperature, ExternalWallDHWVesselTemperature)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: WaterTankTemperature, InternalWallWaterTankTemperature
      REAL(KIND(1D0)), INTENT(IN) :: ExternalWallWaterTankTemperature, MainsWaterTemperature
      REAL(KIND(1D0)), INTENT(IN) :: DomesticHotWaterTemperatureInUseInBuilding
      REAL(KIND(1D0)), INTENT(IN) :: InternalWallDHWVesselTemperature, ExternalWallDHWVesselTemperature

      state%stebbsState%WaterTankTemperature = WaterTankTemperature
      state%stebbsState%InternalWallWaterTankTemperature = InternalWallWaterTankTemperature
      state%stebbsState%ExternalWallWaterTankTemperature = ExternalWallWaterTankTemperature
      state%stebbsState%MainsWaterTemperature = MainsWaterTemperature
      state%stebbsState%DomesticHotWaterTemperatureInUseInBuilding = DomesticHotWaterTemperatureInUseInBuilding
      state%stebbsState%InternalWallDHWVesselTemperature = InternalWallDHWVesselTemperature
      state%stebbsState%ExternalWallDHWVesselTemperature = ExternalWallDHWVesselTemperature
   END SUBROUTINE set_stebbs_state_temps_water

   !> Get STEBBS state storage heat flux
   !> @param[in] state The SUEWS_STATE object
   !> @param[out] QS_stebbs Storage heat flux per footprint area [W m-2]
   SUBROUTINE get_stebbs_state_qs(state, QS_stebbs)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      REAL(KIND(1D0)), INTENT(OUT) :: QS_stebbs

      QS_stebbs = state%stebbsState%QS_stebbs
   END SUBROUTINE get_stebbs_state_qs

   !> Set STEBBS state storage heat flux
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] QS_stebbs Storage heat flux per footprint area [W m-2]
   SUBROUTINE set_stebbs_state_qs(state, QS_stebbs)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      REAL(KIND(1D0)), INTENT(IN) :: QS_stebbs

      state%stebbsState%QS_stebbs = QS_stebbs
   END SUBROUTINE set_stebbs_state_qs

   !> Get STEBBS building runtime temperatures (roof and wall external temps)
   !> Note: These are the only runtime-modified arrays in buildings(:)
   !> @param[in] state The SUEWS_STATE object
   !> @param[in] bldg_idx Building index (1-based)
   !> @param[in] nlayer Number of layers
   !> @param[out] Textroof_C Roof external surface temperature [K]
   !> @param[out] Textwall_C Wall external surface temperature [K]
   SUBROUTINE get_stebbs_building_temps(state, bldg_idx, nlayer, Textroof_C, Textwall_C)
      TYPE(SUEWS_STATE), INTENT(IN) :: state
      INTEGER, INTENT(IN) :: bldg_idx, nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(OUT) :: Textroof_C, Textwall_C

      IF (ALLOCATED(state%stebbsState%buildings)) THEN
         IF (bldg_idx <= SIZE(state%stebbsState%buildings)) THEN
            IF (ALLOCATED(state%stebbsState%buildings(bldg_idx)%Textroof_C)) THEN
               Textroof_C = state%stebbsState%buildings(bldg_idx)%Textroof_C
            END IF
            IF (ALLOCATED(state%stebbsState%buildings(bldg_idx)%Textwall_C)) THEN
               Textwall_C = state%stebbsState%buildings(bldg_idx)%Textwall_C
            END IF
         END IF
      END IF
   END SUBROUTINE get_stebbs_building_temps

   !> Set STEBBS building runtime temperatures (roof and wall external temps)
   !> Note: These are the only runtime-modified arrays in buildings(:)
   !> @param[inout] state The SUEWS_STATE object
   !> @param[in] bldg_idx Building index (1-based)
   !> @param[in] nlayer Number of layers
   !> @param[in] Textroof_C Roof external surface temperature [K]
   !> @param[in] Textwall_C Wall external surface temperature [K]
   SUBROUTINE set_stebbs_building_temps(state, bldg_idx, nlayer, Textroof_C, Textwall_C)
      TYPE(SUEWS_STATE), INTENT(INOUT) :: state
      INTEGER, INTENT(IN) :: bldg_idx, nlayer
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(IN) :: Textroof_C, Textwall_C

      IF (ALLOCATED(state%stebbsState%buildings)) THEN
         IF (bldg_idx <= SIZE(state%stebbsState%buildings)) THEN
            IF (ALLOCATED(state%stebbsState%buildings(bldg_idx)%Textroof_C)) THEN
               state%stebbsState%buildings(bldg_idx)%Textroof_C = Textroof_C
            END IF
            IF (ALLOCATED(state%stebbsState%buildings(bldg_idx)%Textwall_C)) THEN
               state%stebbsState%buildings(bldg_idx)%Textwall_C = Textwall_C
            END IF
         END IF
      END IF
   END SUBROUTINE set_stebbs_building_temps

END MODULE module_ctrl_accessor
