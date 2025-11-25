module module_type_hydro

   USE module_ctrl_const_allocate, ONLY: &
      nsurf

   implicit none

   TYPE, PUBLIC :: HYDRO_STATE
      ! REAL(KIND(1D0)) :: runofftowater   ! Fraction of above-ground runoff flowing to water surface during flooding
      REAL(KIND(1D0)), DIMENSION(nsurf) :: soilstore_surf = 0.0D0 ! Initial water stored in soil beneath `Bldgs` surface
      REAL(KIND(1D0)), DIMENSION(nsurf) :: state_surf = 0.0D0 ! Initial wetness condition on SUEWS land covers.

      ! ==================================================
      ! TODO: #243 split WUDay_id into individual explicit variables
      REAL(KIND(1D0)), DIMENSION(9) :: WUDay_id = 0.0D0 ! Daily water use for EveTr, DecTr, Grass [mm]
      ! WUDay_id:
      ! WUDay_id(1) - Daily water use total for Irr EveTr (automatic+manual) [mm]
      ! WUDay_id(2) - Automatic irrigation for Irr EveTr [mm]
      ! WUDay_id(3) - Manual irrigation for Irr EveTr [mm]
      ! WUDay_id(4) - Daily water use total for Irr DecTr (automatic+manual) [mm]
      ! WUDay_id(5) - Automatic irrigation for Irr DecTr [mm]
      ! WUDay_id(6) - Manual irrigation for Irr DecTr [mm]
      ! WUDay_id(7) - Daily water use total for Irr Grass (automatic+manual) [mm]
      ! WUDay_id(8) - Automatic irrigation for Irr Grass [mm]
      ! WUDay_id(9) - Manual irrigation for Irr Grass [mm]
      ! ==================================================

      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: soilstore_roof ! Soil moisture of roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: state_roof ! wetness status of roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: soilstore_wall ! Soil moisture of wall [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: state_wall ! wetness status of wall [mm]

      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: ev_roof ! evapotranspiration of each roof layer [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: ev_wall ! evapotranspiration of each wall type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: ev0_surf = 0.0D0 ! evapotranspiration from PM of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: ev_surf = 0.0D0 ! evapotranspiration of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: wu_surf = 0.0D0 !external water use of each surface type [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: runoffSoil = 0.0D0 !Soil runoff from each soil sub-surface [mm]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: smd_surf = 0.0D0 !soil moisture deficit for each surface
      REAL(KIND(1D0)), DIMENSION(NSURF) :: drain_surf = 0.0D0 !drainage of each surface type [mm]

      REAL(KIND(1D0)) :: drain_per_tstep = 0.0D0 ! total drainage for all surface type at each timestep [mm]
      REAL(KIND(1D0)) :: ev_per_tstep = 0.0D0 ! evaporation at each time step [mm]
      REAL(KIND(1D0)) :: wu_ext = 0.0D0 !external water use [mm]
      REAL(KIND(1D0)) :: wu_int = 0.0D0 !internal water use [mm]

      REAL(KIND(1D0)) :: runoffAGveg = 0.0D0 !Above ground runoff from vegetated surfaces for all surface area [mm]
      REAL(KIND(1D0)) :: runoffAGimpervious = 0.0D0 !Above ground runoff from impervious surface for all surface area [mm]
      REAL(KIND(1D0)) :: runoff_per_tstep = 0.0D0 !runoff water at each time step [mm]
      REAL(KIND(1D0)) :: runoffPipes = 0.0 !runoff to pipes [mm]
      REAL(KIND(1D0)) :: runoffSoil_per_tstep = 0.0D0 !Runoff to deep soil per timestep [mm] (for whole surface, excluding water body)
      REAL(KIND(1D0)) :: runoffwaterbody = 0.0D0 !Above ground runoff from water body for all surface area [mm]
      REAL(KIND(1D0)) :: smd = 0.0D0 !soil moisture deficit [mm]
      REAL(KIND(1D0)) :: SoilState = 0.0D0 !Area-averaged soil moisture  for whole surface [mm]
      REAL(KIND(1D0)) :: state_per_tstep = 0.0D0 !state_id at each timestep [mm]
      REAL(KIND(1D0)) :: surf_chang_per_tstep = 0.0D0 !change in state_id (exluding snowpack) per timestep [mm]
      REAL(KIND(1D0)) :: tot_chang_per_tstep = 0.0D0 !Change in surface state_id [mm]
      REAL(KIND(1D0)) :: runoff_per_interval = 0.0D0 !run-off at each time interval [mm]
      REAL(KIND(1D0)) :: NWstate_per_tstep = 0.0D0 ! state_id at each tinestep(excluding water body) [mm]

      REAL(KIND(1D0)) :: SoilMoistCap !Maximum capacity of soil store [mm]
      REAL(KIND(1D0)) :: vsmd !Soil moisture deficit for vegetated surfaces only [mm]

      ! TODO: TS 25 Oct 2017
      ! the  variables are not used currently as grid-to-grid connection is NOT set up.
      ! set these variables as zero.
      REAL(KIND(1D0)) :: AdditionalWater = 0.0D0 !!Additional water coming from other grids [mm] (these are expressed as depths over the whole surface)
      REAL(KIND(1D0)) :: addImpervious = 0.0D0
      REAL(KIND(1D0)) :: addPipes = 0.0D0
      REAL(KIND(1D0)) :: addVeg = 0.0D0
      REAL(KIND(1D0)) :: addWaterBody = 0.0D0
      REAL(KIND(1D0)), DIMENSION(NSURF) :: AddWater = 0.0D0
      REAL(KIND(1D0)), DIMENSION(NSURF) :: frac_water2runoff = 0.0D0

      ! flag for iteration safety - NO
      ! multiple variables (e.g. soilstore_surf, state_surf, etc) include extensive quantities and thus cannot be used for iteration safety
      LOGICAL :: iter_safe = .FALSE.
   CONTAINS
      PROCEDURE :: ALLOCATE => allocHydroState_c
      PROCEDURE :: DEALLOCATE => deallocHydroState_c
   END TYPE HYDRO_STATE

CONTAINS

   SUBROUTINE allocHydroState_c(self, nlayer)
      IMPLICIT NONE
      CLASS(HYDRO_STATE), INTENT(inout) :: self
      INTEGER, INTENT(in) :: nlayer
      !
      ! CALL allocate_hydro_state(self, nlayer)
      CALL self%DEALLOCATE()
      ALLOCATE (self%soilstore_roof(nlayer))
      ALLOCATE (self%state_roof(nlayer))
      ALLOCATE (self%soilstore_wall(nlayer))
      ALLOCATE (self%state_wall(nlayer))
      ALLOCATE (self%ev_roof(nlayer))
      ALLOCATE (self%ev_wall(nlayer))
      !
   END SUBROUTINE allocHydroState_c

   SUBROUTINE deallocHydroState_c(self)
      IMPLICIT NONE
      CLASS(HYDRO_STATE), INTENT(inout) :: self
      !
      ! CALL dealloc_hydro_state(self)
      IF (ALLOCATED(self%soilstore_roof)) DEALLOCATE (self%soilstore_roof)
      IF (ALLOCATED(self%state_roof)) DEALLOCATE (self%state_roof)
      IF (ALLOCATED(self%soilstore_wall)) DEALLOCATE (self%soilstore_wall)
      IF (ALLOCATED(self%state_wall)) DEALLOCATE (self%state_wall)
      IF (ALLOCATED(self%ev_roof)) DEALLOCATE (self%ev_roof)
      IF (ALLOCATED(self%ev_wall)) DEALLOCATE (self%ev_wall)
      !
   END SUBROUTINE deallocHydroState_c

end module module_type_hydro
