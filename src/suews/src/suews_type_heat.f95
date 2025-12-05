module module_type_heat

   USE module_ctrl_const_allocate, ONLY: &
      nsurf

   implicit none

   TYPE, PUBLIC :: HEAT_STATE
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_roof ! interface temperature between depth layers in roof [degC]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_wall ! interface temperature between depth layers in wall [degC]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_surf ! interface temperature between depth layers [degC]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_surf_dyohm ! interface temperature between depth layers [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_roof ! roof surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_wall ! wall surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_surf ! surface temperature [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_surf_dyohm! surface temperature [degC]
      ! surface temperature saved at the beginning of the time step - not updated during the time step
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_roof_stepstart !surface temperature of roof saved at the beginning of the time step [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_wall_stepstart !surface temperature of wall saved at the beginning of the time step [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_surf_stepstart !surface temperature saved at the beginning of the time step [degC]

      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: QS_roof ! heat storage flux for roof component [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: QN_roof ! net all-wave radiation of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qe_roof ! latent heat flux of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qh_roof ! sensible heat flux of roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qh_resist_roof ! resist-based sensible heat flux of roof surface [W m-2]

      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: QS_wall ! heat storage flux for wall component [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: QN_wall ! net all-wave radiation of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qe_wall ! latent heat flux of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qh_wall ! sensible heat flux of wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qh_resist_wall ! resistance based sensible heat flux of wall surface [W m-2]

      REAL(KIND(1D0)), DIMENSION(nsurf) :: qs_surf = 0.0D0 ! aggregated heat storage of of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: QN_surf = 0.0D0 ! net all-wave radiation of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qe0_surf = 0.0D0 ! latent heat flux from PM of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qe_surf = 0.0D0 ! latent heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qh_surf = 0.0D0 ! sensinle heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: qh_resist_surf = 0.0D0 ! resistance based sensible heat flux of individual surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(NSURF) :: tsurf_ind = 0.0D0 !snow-free surface temperature [degC]

      REAL(KIND(1D0)) :: QH_LUMPS = 0.0D0 !turbulent sensible heat flux from LUMPS model [W m-2]
      REAL(KIND(1D0)) :: QE_LUMPS = 0.0D0 !turbulent latent heat flux by LUMPS model [W m-2]

      REAL(KIND(1D0)) :: kclear = 0.0D0 !clear sky incoming shortwave radiation [W m-2]
      REAL(KIND(1D0)) :: kup = 0.0D0 !outgoing shortwave radiation [W m-2]
      REAL(KIND(1D0)) :: ldown = 0.0D0 !incoming longtwave radiation [W m-2]
      REAL(KIND(1D0)) :: lup = 0.0D0 !outgoing longwave radiation [W m-2]

      REAL(KIND(1D0)) :: qe = 0.0D0 !turbuent latent heat flux [W m-2]
      REAL(KIND(1D0)) :: qf = 0.0D0 !anthropogenic heat flux [W m-2]
      REAL(KIND(1D0)) :: QF_SAHP = 0.0D0 !total anthropogeic heat flux when EmissionMethod is not 0 [W m-2]
      REAL(KIND(1D0)) :: qh = 0.0D0 !turbulent sensible heat flux [W m-2]
      REAL(KIND(1D0)) :: qh_residual = 0.0D0 ! residual based sensible heat flux [W m-2]
      REAL(KIND(1D0)) :: qh_resist = 0.0D0 !resistance bnased sensible heat flux [W m-2]

      REAL(KIND(1D0)) :: qn = 0.0D0 !net all-wave radiation [W m-2]
      REAL(KIND(1D0)) :: qn_snowfree = 0.0D0 !net all-wave radiation on snow-free surface [W m-2]
      REAL(KIND(1D0)) :: qs = 0.0D0 !heat storage flux [W m-2]

      REAL(KIND(1D0)) :: TSfc_C = 0.0D0 ! surface temperature [degC]
      REAL(KIND(1D0)) :: tsurf = 0.0D0 !surface temperatue [degC]
      REAL(KIND(1D0)) :: QH_Init = 0.0D0 !initialised sensible heat flux [W m-2]

      REAL(KIND(1D0)), DIMENSION(15) :: roof_in_sw_spc ! incoming shortwave radiation on roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(15) :: roof_in_lw_spc ! incoming longwave radiation on roof surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(15) :: wall_in_sw_spc ! incoming shortwave radiation on wall surface [W m-2]
      REAL(KIND(1D0)), DIMENSION(15) :: wall_in_lw_spc ! incoming longwave radiation on wall surface [W m-2]

      ! flag for iteration safety - YES
      ! all variables are intensive and thus can be used for iteration safety
      LOGICAL :: iter_safe = .TRUE.
   CONTAINS
      PROCEDURE :: ALLOCATE => allocHeatState_c
      PROCEDURE :: DEALLOCATE => deallocHeatState_c
   END TYPE HEAT_STATE

CONTAINS

   SUBROUTINE allocHeatState_c(self, num_surf, num_layer, num_depth)
      IMPLICIT NONE
      CLASS(HEAT_STATE), INTENT(inout) :: self
      INTEGER, INTENT(in) :: num_surf, num_layer, num_depth

      !
      ! CALL allocate_heat_state(self, num_surf, num_layer, num_depth)

      CALL self%DEALLOCATE()
      ALLOCATE (self%temp_roof(num_layer, num_depth))
      ALLOCATE (self%temp_wall(num_layer, num_depth))
      ALLOCATE (self%temp_surf(num_surf, num_depth))
      ALLOCATE (self%temp_surf_dyohm(num_surf, num_depth))

      ALLOCATE (self%tsfc_roof(num_layer))
      ALLOCATE (self%tsfc_wall(num_layer))
      ALLOCATE (self%tsfc_surf(num_surf))
      ALLOCATE (self%tsfc_surf_dyohm(num_surf))

      ALLOCATE (self%tsfc_roof_stepstart(num_layer))
      ALLOCATE (self%tsfc_wall_stepstart(num_layer))
      ALLOCATE (self%tsfc_surf_stepstart(num_surf))

      ALLOCATE (self%QS_roof(num_layer))
      ALLOCATE (self%QN_roof(num_layer))
      ALLOCATE (self%qe_roof(num_layer))
      ALLOCATE (self%qh_roof(num_layer))
      ALLOCATE (self%qh_resist_roof(num_layer))
      ALLOCATE (self%QS_wall(num_layer))
      ALLOCATE (self%QN_wall(num_layer))
      ALLOCATE (self%qe_wall(num_layer))
      ALLOCATE (self%qh_wall(num_layer))
      ALLOCATE (self%qh_resist_wall(num_layer))

   END SUBROUTINE allocHeatState_c

   SUBROUTINE deallocHeatState_c(self)
      IMPLICIT NONE
      CLASS(HEAT_STATE), INTENT(inout) :: self
      !
      ! CALL dealloc_heat_state(self)
      IF (ALLOCATED(self%temp_roof)) DEALLOCATE (self%temp_roof)
      IF (ALLOCATED(self%temp_wall)) DEALLOCATE (self%temp_wall)
      IF (ALLOCATED(self%tsfc_roof)) DEALLOCATE (self%tsfc_roof)
      IF (ALLOCATED(self%tsfc_wall)) DEALLOCATE (self%tsfc_wall)
      IF (ALLOCATED(self%tsfc_surf)) DEALLOCATE (self%tsfc_surf)
      IF (ALLOCATED(self%tsfc_surf_dyohm)) DEALLOCATE (self%tsfc_surf_dyohm)
      IF (ALLOCATED(self%tsfc_roof_stepstart)) DEALLOCATE (self%tsfc_roof_stepstart)
      IF (ALLOCATED(self%tsfc_wall_stepstart)) DEALLOCATE (self%tsfc_wall_stepstart)
      IF (ALLOCATED(self%tsfc_surf_stepstart)) DEALLOCATE (self%tsfc_surf_stepstart)
      IF (ALLOCATED(self%temp_surf)) DEALLOCATE (self%temp_surf)
      IF (ALLOCATED(self%temp_surf_dyOHM)) DEALLOCATE (self%temp_surf_dyOHM)
      IF (ALLOCATED(self%QS_roof)) DEALLOCATE (self%QS_roof)
      IF (ALLOCATED(self%QN_roof)) DEALLOCATE (self%QN_roof)
      IF (ALLOCATED(self%qe_roof)) DEALLOCATE (self%qe_roof)
      IF (ALLOCATED(self%qh_roof)) DEALLOCATE (self%qh_roof)
      IF (ALLOCATED(self%qh_resist_roof)) DEALLOCATE (self%qh_resist_roof)
      IF (ALLOCATED(self%QS_wall)) DEALLOCATE (self%QS_wall)
      IF (ALLOCATED(self%QN_wall)) DEALLOCATE (self%QN_wall)
      IF (ALLOCATED(self%qe_wall)) DEALLOCATE (self%qe_wall)
      IF (ALLOCATED(self%qh_wall)) DEALLOCATE (self%qh_wall)
      IF (ALLOCATED(self%qh_resist_wall)) DEALLOCATE (self%qh_resist_wall)

   END SUBROUTINE deallocHeatState_c

end module module_type_heat
