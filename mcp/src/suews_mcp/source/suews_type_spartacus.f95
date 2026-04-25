module module_type_spartacus

    USE module_ctrl_const_allocate, ONLY: &
       nspec

    ! use module_ctrl_type

    implicit none

    TYPE, PUBLIC :: SPARTACUS_PRM
       REAL(KIND(1D0)) :: air_ext_lw = 0.0D0 ! Longwave air extinction coefficient [m-1]
       REAL(KIND(1D0)) :: air_ext_sw = 0.0D0 ! Shortwave air extinction coefficient [m-1]
       REAL(KIND(1D0)) :: air_ssa_lw = 0.0D0 ! Longwave air single scattering albedo [-]
       REAL(KIND(1D0)) :: air_ssa_sw = 0.0D0 ! Shortwave air single scattering albedo [-]
       REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: height ! Height of each layer [m]
       REAL(KIND(1D0)) :: ground_albedo_dir_mult_fact = 0.0D0 ! Ground albedo direct multiplication factor [-]
       INTEGER :: n_stream_lw_urban ! LW streams per hemisphere [-]
       INTEGER :: n_stream_sw_urban ! Shortwave diffuse streams per hemisphere [-]
       INTEGER :: n_vegetation_region_urban ! Number of regions used to describe vegetation [-]
       REAL(KIND(1D0)) :: sw_dn_direct_frac = 0.0D0 ! Fraction of direct shortwave radiation [-]
       REAL(KIND(1D0)) :: use_sw_direct_albedo = 0.0D0 ! Flag for using direct shortwave albedo [-]
       REAL(KIND(1D0)) :: veg_contact_fraction_const = 0.0D0 ! Vegetation contact fraction constant [-]
       REAL(KIND(1D0)) :: veg_fsd_const = 0.0D0 ! Vegetation fractional standard deviation constant [-]
       REAL(KIND(1D0)) :: veg_ssa_lw = 0.0D0 ! Longwave vegetation single scattering albedo [-]
       REAL(KIND(1D0)) :: veg_ssa_sw = 0.0D0 ! Shortwave vegetation single scattering albedo [-]
    CONTAINS
       PROCEDURE :: ALLOCATE => allocate_spartacus_prm
       PROCEDURE :: DEALLOCATE => deallocate_spartacus_prm
    END TYPE SPARTACUS_PRM

    TYPE, PUBLIC :: SPARTACUS_LAYER_PRM
       REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: building_frac ! Building fraction [-]
       REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: building_scale ! Diameter of buildings [m]
       REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: veg_frac ! Vegetation fraction [-]
       REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: veg_scale ! Scale of tree crowns [m]
       REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: alb_roof ! Albedo of roof [-]
       REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: emis_roof ! Emissivity of roof [-]
       REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: alb_wall ! Albedo of wall [-]
       REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: emis_wall ! Emissivity of wall [-]
       REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: roof_albedo_dir_mult_fact ! Ratio of direct/diffuse roof albedo [-]
       REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: wall_specular_frac ! Fraction of wall reflection that is specular [-]
    CONTAINS
       PROCEDURE :: ALLOCATE => allocate_spartacus_layer_prm_c
       PROCEDURE :: DEALLOCATE => deallocate_spartacus_layer_prm_c
    END TYPE SPARTACUS_LAYER_PRM

CONTAINS

    SUBROUTINE allocate_spartacus_layer_prm_c(self, nlayer)
       IMPLICIT NONE
       CLASS(SPARTACUS_LAYER_PRM), INTENT(inout) :: self
       INTEGER, INTENT(in) :: nlayer

       CALL self%DEALLOCATE()
       ALLOCATE (self%building_frac(nlayer))
       ALLOCATE (self%building_scale(nlayer))
       ALLOCATE (self%veg_frac(nlayer))
       ALLOCATE (self%veg_scale(nlayer))
       ALLOCATE (self%alb_roof(nlayer))
       ALLOCATE (self%emis_roof(nlayer))
       ALLOCATE (self%alb_wall(nlayer))
       ALLOCATE (self%emis_wall(nlayer))
       ALLOCATE (self%roof_albedo_dir_mult_fact(nspec, nlayer))
       ALLOCATE (self%wall_specular_frac(nspec, nlayer))

    END SUBROUTINE allocate_spartacus_layer_prm_c

    SUBROUTINE deallocate_spartacus_layer_prm_c(self)
       IMPLICIT NONE
       CLASS(SPARTACUS_LAYER_PRM), INTENT(inout) :: self

       IF (ALLOCATED(self%building_frac)) DEALLOCATE (self%building_frac)
       IF (ALLOCATED(self%building_scale)) DEALLOCATE (self%building_scale)
       IF (ALLOCATED(self%veg_frac)) DEALLOCATE (self%veg_frac)
       IF (ALLOCATED(self%veg_scale)) DEALLOCATE (self%veg_scale)
       IF (ALLOCATED(self%alb_roof)) DEALLOCATE (self%alb_roof)
       IF (ALLOCATED(self%emis_roof)) DEALLOCATE (self%emis_roof)
       IF (ALLOCATED(self%alb_wall)) DEALLOCATE (self%alb_wall)
       IF (ALLOCATED(self%emis_wall)) DEALLOCATE (self%emis_wall)
       IF (ALLOCATED(self%roof_albedo_dir_mult_fact)) DEALLOCATE (self%roof_albedo_dir_mult_fact)
       IF (ALLOCATED(self%wall_specular_frac)) DEALLOCATE (self%wall_specular_frac)

    END SUBROUTINE deallocate_spartacus_layer_prm_c

    SUBROUTINE allocate_spartacus_prm(self, nlayer)
       IMPLICIT NONE
       CLASS(SPARTACUS_PRM), INTENT(inout) :: self
       INTEGER, INTENT(in) :: nlayer

       CALL self%DEALLOCATE()
       ! Height array has nlayer+1 levels (ground + layer tops)
       ALLOCATE (self%height(nlayer + 1))
       self%height = 0.0D0

    END SUBROUTINE allocate_spartacus_prm

    SUBROUTINE deallocate_spartacus_prm(self)
       IMPLICIT NONE
       CLASS(SPARTACUS_PRM), INTENT(inout) :: self

       IF (ALLOCATED(self%height)) DEALLOCATE (self%height)

    END SUBROUTINE deallocate_spartacus_prm

end module module_type_spartacus
