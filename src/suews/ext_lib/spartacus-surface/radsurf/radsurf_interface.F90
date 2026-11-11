! radsurf_interface.F90 - Perform surface radiative transfer calculation
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

module radsurf_interface

contains

  subroutine radsurf(config, canopy_props, &
       &             sw_spectral_props, lw_spectral_props, &
       &             bc_out, &
       &             istartcol, iendcol, &
       &             sw_norm_dir, sw_norm_diff, &
       &             lw_internal, lw_norm)

    use parkind1,                   only : jpim, jprb
    use yomhook,                    only : lhook, dr_hook
    use radiation_io,               only : nulout, radiation_error
    use radsurf_config,             only : config_type
    use radsurf_canopy_properties,  only : ITileFlat,  ITileForest, &
         &                                 ITileUrban, ITileVegetatedUrban, &
         &                                 canopy_properties_type
    use radsurf_sw_spectral_properties, only : sw_spectral_properties_type
    use radsurf_lw_spectral_properties, only : lw_spectral_properties_type
    use radsurf_boundary_conds_out, only : boundary_conds_out_type
    use radsurf_canopy_flux,        only : canopy_flux_type
    use radsurf_forest_sw,          only : spartacus_forest_sw
    use radsurf_forest_lw,          only : spartacus_forest_lw
    use radsurf_urban_sw,           only : spartacus_urban_sw
    use radsurf_urban_lw,           only : spartacus_urban_lw
    
    implicit none

    type(config_type),             intent(in)  :: config
    type(canopy_properties_type),  intent(in)  :: canopy_props
    type(lw_spectral_properties_type), intent(in), target :: lw_spectral_props
    type(sw_spectral_properties_type), intent(in), target :: sw_spectral_props
    type(boundary_conds_out_type), intent(inout) :: bc_out
    type(canopy_flux_type),        intent(inout), optional &
         &  :: sw_norm_dir, &  ! SW fluxes normalized by top-of-canopy direct
         &     sw_norm_diff, & ! SW fluxes normalized by top-of-canopy diffuse
         &     lw_internal, &  ! LW fluxes from internal emission
         &     lw_norm         ! LW fluxes normalized by top-of-canopy down

    integer(kind=jpim), optional, intent(in) :: istartcol, iendcol

    ! Actual start and end columns
    integer(kind=jpim) :: icol1, icol2

    ! Start and end layers for current column
    integer(kind=jpim) :: ilay1, ilay2

    ! Loop index for column
    integer(kind=jpim) :: jcol

    ! Representation for current column (flat, vegetated, urban etc)
    integer(kind=jpim) :: irep

    ! Is the current column layered?
    logical :: do_canopy

    ! Direct ground shortwave albedo, in case not provided by user
    real(kind=jprb), pointer :: ground_sw_albedo_dir(:,:) ! (nswinterval,ncol)

    real(jprb) :: hook_handle

    if (lhook) call dr_hook('radiation_interface:radsurf',0,hook_handle)

    ! Find which range of columns to process
    if (present(istartcol)) then
      icol1 = istartcol
    else
      icol1 = 1
    end if

    if (present(iendcol)) then
      icol2 = iendcol
    else
      icol2 = canopy_props%ncol
    end if

    if (config%use_sw_direct_albedo) then
      ground_sw_albedo_dir => sw_spectral_props%ground_albedo_dir
    else
      ground_sw_albedo_dir => sw_spectral_props%ground_albedo
    end if

    ! Loop through columns calculating radiative transfer on each
    do jcol = icol1,icol2

      irep = canopy_props%i_representation(jcol)
      
      do_canopy = (.not. irep == ITileFlat)

      if (do_canopy) then
        ilay1 = canopy_props%istartlay(jcol)
        ilay2 = ilay1 + canopy_props%nlay(jcol)-1
      else
        ! If this is ever used then should be detected with Fortran
        ! bounds checking on
        ilay1 = 0
        ilay2 = 0
      end if

      select case (irep)
      case (ITileFlat)
        ! Flat tiles simply involve copying the ground facet
        ! properties to the outward boundary conditions, and copying
        ! the incoming fluxes down to the ground facet
        if (config%iverbose >= 4) then
          write(nulout,'(i5,a)') jcol, ': Flat'
        end if

        if (config%do_sw) then
          ! Copy shortwave albedo
          bc_out%sw_albedo(:,jcol)     = sw_spectral_props%ground_albedo(:,jcol)
          bc_out%sw_albedo_dir(:,jcol) = ground_sw_albedo_dir(:,jcol)
          ! Rate of change of ground and top-of-canopy fluxes with
          ! respect to direct flux at top-of-canopy
          sw_norm_dir%ground_dn_dir(:,jcol) = 1.0_jprb
          sw_norm_dir%ground_dn(:,jcol)  = 1.0_jprb
          sw_norm_dir%ground_net(:,jcol) = 1.0_jprb - ground_sw_albedo_dir(:,jcol)
          sw_norm_dir%ground_vertical_diff(:,jcol) = 0.5_jprb * ground_sw_albedo_dir(:,jcol)
          sw_norm_dir%top_dn_dir(:,jcol) = 1.0_jprb
          sw_norm_dir%top_dn(:,jcol)  = 1.0_jprb
          sw_norm_dir%top_net(:,jcol) = 1.0_jprb - ground_sw_albedo_dir(:,jcol)
          ! Rate of change of ground and top-of-canopy fluxes with
          ! respect to diffuse downward flux at top-of-canopy
          sw_norm_diff%ground_dn_dir(:,jcol) = 0.0_jprb
          sw_norm_diff%ground_dn(:,jcol)  = 1.0_jprb
          sw_norm_diff%ground_net(:,jcol) = 1.0_jprb - sw_spectral_props%ground_albedo(:,jcol)
          sw_norm_diff%ground_vertical_diff(:,jcol) &
               &  = 0.5_jprb * (1.0_jprb + sw_spectral_props%ground_albedo(:,jcol))
          sw_norm_diff%top_dn_dir(:,jcol) = 0.0_jprb
          sw_norm_diff%top_dn(:,jcol)  = 1.0_jprb
          sw_norm_diff%top_net(:,jcol) = 1.0_jprb - sw_spectral_props%ground_albedo(:,jcol)
        end if

        if (config%do_lw) then
          ! Copy longwave albedo and upward emission
          bc_out%lw_emissivity(:,jcol)   = lw_spectral_props%ground_emissivity(:,jcol)
          bc_out%lw_emission(:,jcol)     = lw_spectral_props%ground_emission(:,jcol)
          ! Longwave fluxes due to surface emission
          lw_internal%ground_dn(:,jcol)  = 0.0_jprb
          lw_internal%ground_net(:,jcol) = -lw_spectral_props%ground_emission(:,jcol)
          lw_internal%ground_vertical_diff(:,jcol) = 0.5_jprb * lw_spectral_props%ground_emission(:,jcol)
          lw_internal%top_dn(:,jcol)  = 0.0_jprb
          lw_internal%top_net(:,jcol) = -lw_spectral_props%ground_emission(:,jcol)
          ! Rate of change of ground and top-of-canopy fluxes with
          ! respect to diffuse downward flux at top-of-canopy
          lw_norm%ground_dn(:,jcol) = 1.0_jprb
          lw_norm%ground_net(:,jcol) = lw_spectral_props%ground_emissivity(:,jcol)
          lw_norm%ground_vertical_diff(:,jcol) &
               &  = 0.5_jprb * (2.0_jprb - lw_spectral_props%ground_emissivity(:,jcol))
          lw_norm%top_dn(:,jcol) = 1.0_jprb
          lw_norm%top_net(:,jcol) = lw_spectral_props%ground_emissivity(:,jcol)
        end if

      case (ITileForest)
        if (config%iverbose >= 4) then
          write(nulout,'(i5,a,i0,a,i0,a,i0,a)') jcol, ': Forest,            ', &
               &  canopy_props%nlay(jcol), ' layers, ', &
               &  config%lg_sw_forest%nstream, ' diffuse streams per hemisphere, ', &
               &  config%n_vegetation_region_forest+1, ' regions'
        end if
        if (config%do_sw) then
          if (canopy_props%cos_sza(jcol) > 0.0_jprb) then
            call spartacus_forest_sw(config, config%nswinternal, &
                 &  config%lg_sw_forest%nstream, config%n_vegetation_region_forest+1, &
                 &  canopy_props%nlay(jcol), jcol, ilay1, ilay2, &
                 &  config%lg_sw_forest, canopy_props%cos_sza(jcol), &
                 &  canopy_props, sw_spectral_props, &
                 &  sw_spectral_props%ground_albedo(:,jcol), &
                 &              ground_sw_albedo_dir(:,jcol), &
                 &  bc_out%sw_albedo(:,jcol), bc_out%sw_albedo_dir(:,jcol), &
                 &  sw_norm_dir, sw_norm_diff)
          else
            call sw_norm_dir%zero(jcol,ilay1,ilay2)
            call sw_norm_diff%zero(jcol,ilay1,ilay2)
          end if
        end if
        if (config%do_lw) then
          call spartacus_forest_lw(config, config%nlwinternal, &
               &  config%lg_lw_forest%nstream, config%n_vegetation_region_forest+1, &
               &  canopy_props%nlay(jcol), jcol, ilay1, ilay2, &
               &  config%lg_lw_forest, canopy_props, lw_spectral_props, &
               &  bc_out%lw_emissivity(:,jcol), bc_out%lw_emission(:,jcol), &
               &  lw_internal, lw_norm)
        end if
        
      case (ITileUrban)
        if (config%iverbose >= 4) then
          write(nulout,'(i5,a,i0,a,i0,a)') jcol, ': Unvegetated urban, ', &
               &  canopy_props%nlay(jcol), ' layers, ', &
               &  config%lg_sw_urban%nstream, ' diffuse streams per hemisphere, 1 region'
        end if

        if (config%do_sw) then
          call sw_norm_dir%zero(jcol,ilay1,ilay2)
          call sw_norm_diff%zero(jcol,ilay1,ilay2)
          if (canopy_props%cos_sza(jcol) > 0.0_jprb) then
            call spartacus_urban_sw(config, config%nswinternal, &
                 &  config%lg_sw_urban%nstream, 1, &
                 &  canopy_props%nlay(jcol), jcol, ilay1, ilay2, &
                 &  config%lg_sw_urban, canopy_props%cos_sza(jcol), &
                 &  canopy_props, sw_spectral_props, &
                 &  sw_spectral_props%ground_albedo(:,jcol), &
                 &              ground_sw_albedo_dir(:,jcol), &
                 &  bc_out%sw_albedo(:,jcol), bc_out%sw_albedo_dir(:,jcol), &
                 &  sw_norm_dir, sw_norm_diff)
            if (radiation_error()) return
          end if
        end if
        if (config%do_lw) then
          call lw_internal%zero(jcol,ilay1,ilay2)
          call lw_norm%zero(jcol,ilay1,ilay2)
          call spartacus_urban_lw(config, config%nlwinternal, &
               &  config%lg_lw_urban%nstream, 1, &
               &  canopy_props%nlay(jcol), jcol, ilay1, ilay2, &
               &  config%lg_lw_urban, canopy_props, lw_spectral_props, &
               &  bc_out%lw_emissivity(:,jcol), bc_out%lw_emission(:,jcol), &
               &  lw_internal, lw_norm)
          if (radiation_error()) return
        end if
        
      case (ITileVegetatedUrban)
        if (config%iverbose >= 4) then
          write(nulout,'(i5,a,i0,a,i0,a,i0,a)') jcol, ': Vegetated urban,   ', &
               &  canopy_props%nlay(jcol), ' layers, ', &
               &  config%lg_sw_urban%nstream, ' diffuse streams per hemisphere, ', &
               &  config%n_vegetation_region_urban+1, ' regions'
        end if
        if (config%do_sw) then
          if (canopy_props%cos_sza(jcol) > 0.0_jprb) then
            call spartacus_urban_sw(config, config%nswinternal, &
                 &  config%lg_sw_urban%nstream, config%n_vegetation_region_urban+1, &
                 &  canopy_props%nlay(jcol), jcol, ilay1, ilay2, &
                 &  config%lg_sw_urban, canopy_props%cos_sza(jcol), &
                 &  canopy_props, sw_spectral_props, &
                 &  sw_spectral_props%ground_albedo(:,jcol), &
                 &                    ground_sw_albedo_dir(:,jcol), &
                 &  bc_out%sw_albedo(:,jcol), bc_out%sw_albedo_dir(:,jcol), &
                 &  sw_norm_dir, sw_norm_diff)
            if (radiation_error()) return
          else
            call sw_norm_dir%zero(jcol,ilay1,ilay2)
            call sw_norm_diff%zero(jcol,ilay1,ilay2)
          end if
        end if
        if (config%do_lw) then
          call spartacus_urban_lw(config, config%nlwinternal, &
               &  config%lg_lw_urban%nstream, config%n_vegetation_region_urban+1, &
               &  canopy_props%nlay(jcol), jcol, ilay1, ilay2, &
               &  config%lg_lw_urban, canopy_props, lw_spectral_props, &
               &  bc_out%lw_emissivity(:,jcol), bc_out%lw_emission(:,jcol), &
               &  lw_internal, lw_norm)
          if (radiation_error()) return
        end if

      end select

    end do

    if (lhook) call dr_hook('radiation_interface:radsurf',0,hook_handle)

  end subroutine radsurf

end module radsurf_interface

