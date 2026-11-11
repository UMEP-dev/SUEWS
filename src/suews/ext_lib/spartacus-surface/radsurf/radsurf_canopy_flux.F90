! radsurf_canopy_flux.f90 - Derived type for fluxes within the vegetated/urban canopy
!
! (C) Copyright 2020- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
!

module radsurf_canopy_flux

  use parkind1, only : jpim, jprb

  implicit none

  !---------------------------------------------------------------------
  ! Derived type storing spectral fluxes within the canopy in either
  ! the shortwave or longwave.  All quantities are in W m-2,
  ! which is radiative power per unit horizontal area of the entire
  ! domain. Net fluxes are in-minus-out of the facet.
  type canopy_flux_type

    ! Flux components are dn/in for fluxes into a surface, and net for
    ! fluxes into minus out of a surface, where "top" is the top of
    ! the canopy.  Down fluxes include both the diffuse and direct
    ! components, whereas "dir" indicates only the direct component.
    real(kind=jprb), allocatable :: ground_dn(:,:)     ! (nspec,ncol)
    real(kind=jprb), allocatable :: ground_dn_dir(:,:) ! (nspec,ncol)
    real(kind=jprb), allocatable :: ground_net(:,:)    ! (nspec,ncol)
    real(kind=jprb), allocatable :: top_dn(:,:)        ! (nspec,ncol)
    real(kind=jprb), allocatable :: top_dn_dir(:,:)    ! (nspec,ncol)
    real(kind=jprb), allocatable :: top_net(:,:)       ! (nspec,ncol)
    real(kind=jprb), allocatable :: roof_in(:,:)       ! (nspec,ntotlay)
    real(kind=jprb), allocatable :: roof_net(:,:)      ! (nspec,ntotlay)
    real(kind=jprb), allocatable :: wall_in(:,:)       ! (nspec,ntotlay)
    real(kind=jprb), allocatable :: wall_net(:,:)      ! (nspec,ntotlay)

    ! Diffuse flux into a vertical surface at ground level, needed for
    ! calculating the mean radiant temperature felt by a human. Note
    ! that this quantity is still scaled by the fraction of the lowest
    ! layer that is not building.
    real(kind=jprb), allocatable :: ground_vertical_diff(:,:) ! (nspec,ncol)

    ! Absorption by the clear-air region, the vegetation in the
    ! vegetated region, and the air in the vegetated region
    real(kind=jprb), allocatable :: clear_air_abs(:,:) ! (nspec,ntotlay)
    real(kind=jprb), allocatable :: veg_abs(:,:)       ! (nspec,ntotlay)
    real(kind=jprb), allocatable :: veg_air_abs(:,:)   ! (nspec,ntotlay)

    ! Optional: upward and downward fluxes at top and base of layers,
    ! summing the clear+vegetated regions. 
    real(kind=jprb), allocatable :: flux_dn_layer_top(:,:)     ! (nspec,ntotlay)
    real(kind=jprb), allocatable :: flux_dn_dir_layer_top(:,:) ! (nspec,ntotlay)
    real(kind=jprb), allocatable :: flux_up_layer_top(:,:)     ! (nspec,ntotlay)
    real(kind=jprb), allocatable :: flux_dn_layer_base(:,:)    ! (nspec,ntotlay)
    real(kind=jprb), allocatable :: flux_dn_dir_layer_base(:,:)! (nspec,ntotlay)
    real(kind=jprb), allocatable :: flux_up_layer_base(:,:)    ! (nspec,ntotlay)

    ! Number of spectral intervals, number of columns total number of
    ! layers across all columns
    integer(kind=jpim) :: nspec, ncol, ntotlay

  contains

    procedure :: allocate   => allocate_canopy_flux
    procedure :: deallocate => deallocate_canopy_flux
    procedure :: scale      => scale_canopy_flux
    procedure :: zero       => zero_canopy_flux
    procedure :: zero_all   => zero_all_canopy_flux
    procedure :: sum        => sum_canopy_flux
    procedure :: check      => check_canopy_flux

  end type canopy_flux_type

contains

  !---------------------------------------------------------------------
  subroutine allocate_canopy_flux(this, config, ncol, ntotlay, nspec, use_direct, &
       &  do_save_flux_profile)

    use radsurf_config, only : config_type

    class(canopy_flux_type), intent(inout) :: this
    type(config_type),       intent(in)    :: config
    integer(kind=jpim),      intent(in)    :: ncol, ntotlay, nspec
    logical, optional,       intent(in)    :: use_direct, do_save_flux_profile

    logical :: use_direct_local, do_save_flux_profile_local

    if (present(use_direct)) then
      use_direct_local = use_direct
    else
      use_direct_local = .true.
    end if

    if (present(do_save_flux_profile)) then
      do_save_flux_profile_local = do_save_flux_profile
    else
      do_save_flux_profile_local = .true.
    end if

    call this%deallocate()

    allocate(this%ground_dn(nspec,ncol))
    allocate(this%ground_net(nspec,ncol))
    allocate(this%ground_vertical_diff(nspec,ncol))
    allocate(this%top_dn(nspec,ncol))
    allocate(this%top_net(nspec,ncol))
    if (use_direct_local) then
      allocate(this%ground_dn_dir(nspec,ncol))
      allocate(this%top_dn_dir(nspec,ncol))
    end if
    if (config%do_urban) then
      allocate(this%roof_in(nspec,ntotlay))
      allocate(this%roof_net(nspec,ntotlay))
      allocate(this%wall_in(nspec,ntotlay))
      allocate(this%wall_net(nspec,ntotlay))
    end if
    allocate(this%clear_air_abs(nspec,ntotlay))
    if (config%do_vegetation) then
      allocate(this%veg_abs(nspec,ntotlay))
      allocate(this%veg_air_abs(nspec,ntotlay))
    end if
    if (do_save_flux_profile_local) then
      allocate(this%flux_dn_layer_top(nspec,ntotlay))
      allocate(this%flux_dn_dir_layer_top(nspec,ntotlay))
      allocate(this%flux_up_layer_top(nspec,ntotlay))
      allocate(this%flux_dn_layer_base(nspec,ntotlay))
      allocate(this%flux_dn_dir_layer_base(nspec,ntotlay))
      allocate(this%flux_up_layer_base(nspec,ntotlay))
    end if

    this%nspec   = nspec
    this%ncol    = ncol
    this%ntotlay = ntotlay

  end subroutine allocate_canopy_flux

  !---------------------------------------------------------------------
  subroutine deallocate_canopy_flux(this)

    class(canopy_flux_type), intent(inout) :: this

    if (allocated(this%ground_dn))        deallocate(this%ground_dn)
    if (allocated(this%ground_dn_dir))    deallocate(this%ground_dn_dir)
    if (allocated(this%ground_net))       deallocate(this%ground_net)
    if (allocated(this%ground_vertical_diff))deallocate(this%ground_vertical_diff)
    if (allocated(this%top_dn))           deallocate(this%top_dn)
    if (allocated(this%top_dn_dir))       deallocate(this%top_dn_dir)
    if (allocated(this%top_net))          deallocate(this%top_net)
    if (allocated(this%roof_in))          deallocate(this%roof_in)
    if (allocated(this%roof_net))         deallocate(this%roof_net)
    if (allocated(this%wall_in))          deallocate(this%wall_in)
    if (allocated(this%wall_net))         deallocate(this%wall_net)
    if (allocated(this%clear_air_abs))    deallocate(this%clear_air_abs)
    if (allocated(this%veg_abs))          deallocate(this%veg_abs)
    if (allocated(this%veg_air_abs))      deallocate(this%veg_air_abs)
    if (allocated(this%flux_dn_layer_top))      deallocate(this%flux_dn_layer_top)
    if (allocated(this%flux_dn_dir_layer_top))  deallocate(this%flux_dn_dir_layer_top)
    if (allocated(this%flux_up_layer_top))      deallocate(this%flux_up_layer_top)
    if (allocated(this%flux_dn_layer_base))     deallocate(this%flux_dn_layer_base)
    if (allocated(this%flux_dn_dir_layer_base)) deallocate(this%flux_dn_dir_layer_base)
    if (allocated(this%flux_up_layer_base))     deallocate(this%flux_up_layer_base)

  end subroutine deallocate_canopy_flux
  
  !---------------------------------------------------------------------
  ! Typically the canopy_flux object initially contains normalized
  ! fluxes, e.g. for a top-of-canopy downwelling flux of unity.  Once
  ! the top-of-canopy downwelling flux is known, the canopy fluxes can
  ! be scaled.
  subroutine scale_canopy_flux(this, nlay, factor)

    use radiation_io, only : nulerr, radiation_abort

    class(canopy_flux_type), intent(inout) :: this
    integer(kind=jpim),      intent(in)    :: nlay(:)     ! (ncol)
    real(kind=jprb),         intent(in)    :: factor(:,:) ! (nspec,ncol)

    integer(kind=jpim) :: ncol, istartcol, jcol

    ! Index from layer to column
    integer(kind=jpim) :: indcol(this%ntotlay)

    if (ubound(factor,1) /= this%nspec) then
      write(nulerr,'(a)') '*** Error: spectral resolution mismatch when scaling canopy fluxes'
      print *, ubound(factor,1), this%nspec
      call radiation_abort()
      return
    end if

    ncol = this%ncol

    ! Assign indices from layer to column
    istartcol = 1
    do jcol = 1,ncol
      if (nlay(jcol) > 0) then
        indcol(istartcol:istartcol-1+nlay(jcol)) = jcol
        istartcol = istartcol + nlay(jcol)
      end if
    end do
    
    this%ground_dn        = factor * this%ground_dn
    this%ground_net       = factor * this%ground_net
    this%ground_vertical_diff= factor * this%ground_vertical_diff
    this%top_dn           = factor * this%top_dn
    this%top_net          = factor * this%top_net
    if (allocated(this%ground_dn_dir)) then
      this%ground_dn_dir = factor * this%ground_dn_dir
      this%top_dn_dir = factor * this%top_dn_dir
    end if
    if (allocated(this%roof_in)) then
      this%roof_in  = factor(:,indcol) * this%roof_in
      this%roof_net = factor(:,indcol) * this%roof_net
      this%wall_in  = factor(:,indcol) * this%wall_in
      this%wall_net = factor(:,indcol) * this%wall_net
    end if
    if (allocated(this%clear_air_abs)) then
      this%clear_air_abs = factor(:,indcol) * this%clear_air_abs
    end if
    if (allocated(this%veg_abs)) then
      this%veg_abs     = factor(:,indcol) * this%veg_abs
      this%veg_air_abs = factor(:,indcol) * this%veg_air_abs
    end if
    if (allocated(this%flux_dn_layer_top)) then
      this%flux_dn_layer_top      = factor(:,indcol) * this%flux_dn_layer_top
      this%flux_dn_dir_layer_top  = factor(:,indcol) * this%flux_dn_dir_layer_top
      this%flux_up_layer_top      = factor(:,indcol) * this%flux_up_layer_top
      this%flux_dn_layer_base     = factor(:,indcol) * this%flux_dn_layer_base
      this%flux_dn_dir_layer_base = factor(:,indcol) * this%flux_dn_dir_layer_base
      this%flux_up_layer_base     = factor(:,indcol) * this%flux_up_layer_base
    end if

  end subroutine scale_canopy_flux

  !---------------------------------------------------------------------
  ! Set the fluxes to zero for a particular column
  subroutine zero_canopy_flux(this, icol, ilay1, ilay2)
   
    class(canopy_flux_type),      intent(inout) :: this
    integer(kind=jpim),           intent(in)    :: icol
    integer(kind=jpim), optional, intent(in)    :: ilay1, ilay2

    this%ground_dn(:,icol)        = 0.0_jprb
    this%ground_net(:,icol)       = 0.0_jprb
    this%ground_vertical_diff(:,icol)= 0.0_jprb
    this%top_dn(:,icol)           = 0.0_jprb
    this%top_net(:,icol)          = 0.0_jprb
    if (allocated(this%ground_dn_dir)) then
      this%ground_dn_dir(:,icol) = 0.0_jprb
      this%top_dn_dir(:,icol)    = 0.0_jprb
    end if
    if (present(ilay1) .and. present(ilay2)) then
      if (ilay2 >= ilay1) then
        if (allocated(this%roof_in)) then
          this%roof_in(:,ilay1:ilay2)  = 0.0_jprb
          this%roof_net(:,ilay1:ilay2) = 0.0_jprb
          this%wall_in(:,ilay1:ilay2)  = 0.0_jprb
          this%wall_net(:,ilay1:ilay2) = 0.0_jprb
        end if
        if (allocated(this%clear_air_abs)) then
          this%clear_air_abs(:,ilay1:ilay2) = 0.0_jprb
        end if
        if (allocated(this%veg_abs)) then
          this%veg_abs(:,ilay1:ilay2)     = 0.0_jprb
          this%veg_air_abs(:,ilay1:ilay2) = 0.0_jprb
        end if
        if (allocated(this%flux_dn_layer_top)) then
          this%flux_dn_layer_top(:,ilay1:ilay2)      = 0.0_jprb
          this%flux_dn_dir_layer_top(:,ilay1:ilay2)  = 0.0_jprb
          this%flux_up_layer_top(:,ilay1:ilay2)      = 0.0_jprb
          this%flux_dn_layer_base(:,ilay1:ilay2)     = 0.0_jprb
          this%flux_dn_dir_layer_base(:,ilay1:ilay2) = 0.0_jprb
          this%flux_up_layer_base(:,ilay1:ilay2)     = 0.0_jprb
        end if
      end if
    end if

  end subroutine zero_canopy_flux


  !---------------------------------------------------------------------
  ! Set the fluxes to zero for all columns
  subroutine zero_all_canopy_flux(this)
   
    class(canopy_flux_type),      intent(inout) :: this

    this%ground_dn        = 0.0_jprb
    this%ground_net       = 0.0_jprb
    this%ground_vertical_diff= 0.0_jprb
    this%top_dn           = 0.0_jprb
    this%top_net          = 0.0_jprb
    if (allocated(this%ground_dn_dir)) then
      this%ground_dn_dir = 0.0_jprb
      this%top_dn_dir    = 0.0_jprb
    end if
    if (allocated(this%roof_in)) then
      this%roof_in  = 0.0_jprb
      this%roof_net = 0.0_jprb
      this%wall_in  = 0.0_jprb
      this%wall_net = 0.0_jprb
    end if
    if (allocated(this%clear_air_abs)) then
      this%clear_air_abs = 0.0_jprb
    end if
    if (allocated(this%veg_abs)) then
      this%veg_abs     = 0.0_jprb
      this%veg_air_abs = 0.0_jprb
    end if
    if (allocated(this%flux_dn_layer_top)) then
      this%flux_dn_layer_top      = 0.0_jprb
      this%flux_dn_dir_layer_top  = 0.0_jprb
      this%flux_up_layer_top      = 0.0_jprb
      this%flux_dn_layer_base     = 0.0_jprb
      this%flux_dn_dir_layer_base = 0.0_jprb
      this%flux_up_layer_base     = 0.0_jprb
    end if

  end subroutine zero_all_canopy_flux

  
  !---------------------------------------------------------------------
  ! this = flux1 + flux2
  subroutine sum_canopy_flux(this, flux1, flux2)

    use radiation_io, only : radiation_abort, nulerr

    class(canopy_flux_type), intent(inout) :: this
    type(canopy_flux_type),  intent(in)    :: flux1, flux2

    logical :: use_direct

    use_direct = allocated(flux1%ground_dn_dir)

    if (.not. allocated(this%ground_dn)) then
      write(nulerr,'(a)') 'Attempt to sum canopy fluxes to an unallocated canopy flux object'
      call radiation_abort()
      return
    end if

    this%ground_dn  = flux1%ground_dn + flux2%ground_dn
    this%ground_net = flux1%ground_net + flux2%ground_net
    this%ground_vertical_diff = flux1%ground_vertical_diff + flux2%ground_vertical_diff
    this%top_dn     = flux1%top_dn + flux2%top_dn
    this%top_net    = flux1%top_net + flux2%top_net
    if (use_direct) then
      this%ground_dn_dir = flux1%ground_dn_dir + flux2%ground_dn_dir
      this%top_dn_dir    = flux1%top_dn_dir + flux2%top_dn_dir
    end if
    if (allocated(this%roof_in)) then
      this%roof_in = flux1%roof_in + flux2%roof_in
      this%roof_net = flux1%roof_net + flux2%roof_net
      this%wall_in = flux1%wall_in + flux2%wall_in
      this%wall_net = flux1%wall_net + flux2%wall_net
    end if
    this%clear_air_abs = flux1%clear_air_abs + flux2%clear_air_abs
    if (allocated(this%veg_abs)) then
      this%veg_abs = flux1%veg_abs + flux2%veg_abs
      this%veg_air_abs = flux1%veg_air_abs + flux2%veg_air_abs
    end if
    if (allocated(this%flux_dn_layer_top)) then
      this%flux_dn_layer_top      = flux1%flux_dn_layer_top + flux2%flux_dn_layer_top
      this%flux_dn_dir_layer_top  = flux1%flux_dn_dir_layer_top + flux2%flux_dn_dir_layer_top
      this%flux_up_layer_top      = flux1%flux_up_layer_top + flux2%flux_up_layer_top
      this%flux_dn_layer_base     = flux1%flux_dn_layer_base + flux2%flux_dn_layer_base
      this%flux_dn_dir_layer_base = flux1%flux_dn_dir_layer_base + flux2%flux_dn_dir_layer_base
      this%flux_up_layer_base     = flux1%flux_up_layer_base + flux2%flux_up_layer_base
    end if

  end subroutine sum_canopy_flux


  !---------------------------------------------------------------------
  ! Check for conservation of energy
  subroutine check_canopy_flux(this, canopy_props, istartcol, iendcol, iverbose)

    use radiation_io,               only : nulout
    use radsurf_canopy_properties,  only : ITileFlat,  ITileForest, &
         &                                 ITileUrban, ITileVegetatedUrban, &
         &                                 canopy_properties_type
    
    class(canopy_flux_type),      intent(inout) :: this
    type(canopy_properties_type), intent(in)    :: canopy_props
    integer(kind=jpim), optional, intent(in)    :: istartcol, iendcol
    integer(kind=jpim), optional, intent(in)    :: iverbose

    integer(kind=jpim) :: i_verbose_local, icol1, icol2, ilay1, ilay2

    integer(kind=jpim) :: jcol

    real(kind=jprb) :: ground_net, top_net, wall_net, roof_net, &
         &             clear_air_net, veg_net, veg_air_net, residual
    
    if (present(iverbose)) then
      i_verbose_local = iverbose
    else
      i_verbose_local = 3
    end if

    if (present(istartcol)) then
      icol1 = istartcol
    else
      icol1 = 1
    end if
    if (present(iendcol)) then
      icol2 = iendcol
    else
      icol2 = this%ncol
    end if

    if (i_verbose_local >= 3) then
      write(nulout,'(a)') 'Column  Ground      Air     Wall     Roof      Veg  Air-veg      Top   Residual'
    end if
    
    do jcol = icol1,icol2
      ilay1 = canopy_props%istartlay(jcol)
      ilay2 = canopy_props%istartlay(jcol) + canopy_props%nlay(jcol) - 1
      ground_net = sum(this%ground_net(:,jcol))
      top_net    = sum(this%top_net(:,jcol))
      if (canopy_props%i_representation(jcol) /= ITileFlat) then
        clear_air_net = sum(this%clear_air_abs(:,ilay1:ilay2))
      else
        clear_air_net = 0.0_jprb
      end if
      if (canopy_props%i_representation(jcol) == ITileUrban &
           &  .or. canopy_props%i_representation(jcol) == ITileVegetatedUrban) then
        roof_net = sum(this%roof_net(:,ilay1:ilay2))
        wall_net = sum(this%wall_net(:,ilay1:ilay2))
      else
        roof_net = 0.0_jprb
        wall_net = 0.0_jprb
      end if
      if (canopy_props%i_representation(jcol) == ITileForest &
           &  .or. canopy_props%i_representation(jcol) == ITileVegetatedUrban) then
        veg_net     = sum(this%veg_abs(:,ilay1:ilay2))
        veg_air_net = sum(this%veg_air_abs(:,ilay1:ilay2))
      else
        veg_net     = 0.0_jprb
        veg_air_net = 0.0_jprb
      end if
      residual = ground_net + clear_air_net + wall_net + roof_net &
           &   + veg_net + veg_air_net - top_net
      if (i_verbose_local >= 3) then
        write(nulout,'(i5,7f9.3,e11.3)') jcol, ground_net, clear_air_net, &
             &  wall_net, roof_net, veg_net, veg_air_net, top_net, residual
      end if
    end do

  end subroutine check_canopy_flux
  
end module radsurf_canopy_flux
