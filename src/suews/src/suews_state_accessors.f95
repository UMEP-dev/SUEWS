!> @file suews_state_accessors.f95
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
!> @date 2024

MODULE suews_state_accessors
   USE module_ctrl_type, ONLY: SUEWS_STATE, HEAT_STATE, HYDRO_STATE, SNOW_STATE
   USE module_ctrl_const_allocate, ONLY: nsurf
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

END MODULE suews_state_accessors
