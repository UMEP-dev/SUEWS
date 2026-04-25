module module_type_ehc

   implicit none

   TYPE, PUBLIC :: EHC_PRM
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: soil_storecap_roof ! Capacity of soil store for roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: soil_storecap_wall ! Capacity of soil store for wall [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: state_limit_roof ! Limit for state_id of roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: state_limit_wall ! Limit for state_id of wall [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: wet_thresh_roof ! wetness threshold  of roof [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: wet_thresh_wall ! wetness threshold  of wall [mm]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tin_roof ! indoor temperature for roof [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tin_wall ! indoor temperature for wall [degC]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tin_surf ! deep bottom temperature for each surface [degC]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: k_roof ! thermal conductivity of roof [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: k_wall ! thermal conductivity of wall [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: k_surf ! thermal conductivity of v [W m-1 K]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: cp_roof ! Volumetric Heat capacity of roof [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: cp_wall ! Volumetric Heat capacity of wall [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: cp_surf ! Volumetric Heat capacity of each surface [J m-3 K-1]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dz_roof ! thickness of each layer in roof [m]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dz_wall ! thickness of each layer in wall [m]
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dz_surf ! thickness of each layer in surface [m]
   CONTAINS
      PROCEDURE :: ALLOCATE => allocate_ehc_prm_c
      PROCEDURE :: DEALLOCATE => deallocate_ehc_prm_c
   END TYPE EHC_PRM

CONTAINS

   SUBROUTINE allocate_ehc_prm_c(self, nlayer, ndepth)
      CLASS(EHC_PRM), INTENT(INOUT) :: self
      INTEGER, INTENT(IN) :: nlayer, ndepth

      ! CALL allocate_ehc_prm(self, nlayer, ndepth)
      CALL self%DEALLOCATE()
      ALLOCATE (self%soil_storecap_roof(nlayer))
      ALLOCATE (self%soil_storecap_wall(nlayer))
      ALLOCATE (self%state_limit_roof(nlayer))
      ALLOCATE (self%state_limit_wall(nlayer))
      ALLOCATE (self%wet_thresh_roof(nlayer))
      ALLOCATE (self%wet_thresh_wall(nlayer))
      ALLOCATE (self%tin_roof(nlayer))
      ALLOCATE (self%tin_wall(nlayer))
      ALLOCATE (self%tin_surf(nlayer))
      ALLOCATE (self%k_roof(nlayer, ndepth))
      ALLOCATE (self%k_wall(nlayer, ndepth))
      ALLOCATE (self%k_surf(nlayer, ndepth))
      ALLOCATE (self%cp_roof(nlayer, ndepth))
      ALLOCATE (self%cp_wall(nlayer, ndepth))
      ALLOCATE (self%cp_surf(nlayer, ndepth))
      ALLOCATE (self%dz_roof(nlayer, ndepth))
      ALLOCATE (self%dz_wall(nlayer, ndepth))
      ALLOCATE (self%dz_surf(nlayer, ndepth))

   END SUBROUTINE allocate_ehc_prm_c

   SUBROUTINE deallocate_ehc_prm_c(self)
      CLASS(EHC_PRM), INTENT(INOUT) :: self

      ! CALL deallocate_ehc_prm(self)
      IF (ALLOCATED(self%soil_storecap_roof)) DEALLOCATE (self%soil_storecap_roof)
      IF (ALLOCATED(self%soil_storecap_wall)) DEALLOCATE (self%soil_storecap_wall)
      IF (ALLOCATED(self%state_limit_roof)) DEALLOCATE (self%state_limit_roof)
      IF (ALLOCATED(self%state_limit_wall)) DEALLOCATE (self%state_limit_wall)
      IF (ALLOCATED(self%wet_thresh_roof)) DEALLOCATE (self%wet_thresh_roof)
      IF (ALLOCATED(self%wet_thresh_wall)) DEALLOCATE (self%wet_thresh_wall)
      IF (ALLOCATED(self%tin_roof)) DEALLOCATE (self%tin_roof)
      IF (ALLOCATED(self%tin_wall)) DEALLOCATE (self%tin_wall)
      IF (ALLOCATED(self%tin_surf)) DEALLOCATE (self%tin_surf)
      IF (ALLOCATED(self%k_roof)) DEALLOCATE (self%k_roof)
      IF (ALLOCATED(self%k_wall)) DEALLOCATE (self%k_wall)
      IF (ALLOCATED(self%k_surf)) DEALLOCATE (self%k_surf)
      IF (ALLOCATED(self%cp_roof)) DEALLOCATE (self%cp_roof)
      IF (ALLOCATED(self%cp_wall)) DEALLOCATE (self%cp_wall)
      IF (ALLOCATED(self%cp_surf)) DEALLOCATE (self%cp_surf)
      IF (ALLOCATED(self%dz_roof)) DEALLOCATE (self%dz_roof)
      IF (ALLOCATED(self%dz_wall)) DEALLOCATE (self%dz_wall)
      IF (ALLOCATED(self%dz_surf)) DEALLOCATE (self%dz_surf)

   END SUBROUTINE deallocate_ehc_prm_c

end module module_type_ehc
