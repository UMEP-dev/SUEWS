! radsurf_urban_lw.F90 - SPARTACUS longwave solver for urban areas
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

module radsurf_urban_lw

contains

  ! ------------------------------------------------------------------
  ! This routine implements the SPARTACUS longwave radiative transfer
  ! algorithm for computing the propagation of thermal-infrared
  ! radiation in an urban canopy with or without vegetation.
  ! 
  ! Sections:
  !   1. Declare variables and arrays
  !   2. Prepare general variables and arrays
  !   3. First loop over layers
  !     3a. Prepare the properties of the current layer
  !     3b. Compute Gamma matrices
  !     3c. Compute reflection/transmission matrices for this layer
  !   4. Albedo/emission of scene at each layer interface
  !   5. Compute normalized flux profile
  !
  subroutine spartacus_urban_lw(config, &
       &  nlw, ns, nreg, nlay, icol, ilay1, ilay2, &
       &  lg, canopy_props, lw_spectral_props, &
       &  top_emissivity, top_emission, &
       &  lw_internal, lw_norm)
    
    use parkind1,                   only : jpim, jprb
    use yomhook,                    only : lhook, dr_hook
    use radiation_io,               only : radiation_abort
    use radsurf_config,             only : config_type
    use radtool_legendre_gauss,     only : legendre_gauss_type
    use radsurf_canopy_properties,  only : canopy_properties_type
    use radsurf_lw_spectral_properties,  only : lw_spectral_properties_type
    use radsurf_canopy_flux,        only : canopy_flux_type
    use radtool_calc_matrices_lw_eig,only: calc_matrices_lw_eig
    use radiation_constants,        only : Pi
    use radtool_matrix,             only : identity_minus_mat_x_mat, &
         &  mat_x_mat, singlemat_x_vec, mat_x_vec, rect_mat_x_vec, &
         &  solve_mat, rect_mat_x_mat, rect_expandedmat_x_mat, &
         &  rect_mat_x_expandedmat, rect_expandedmat_x_vec, solve_vec, &
         &  solve_rect_mat, rect_mat_x_singlemat, rect_singlemat_x_vec
    use radsurf_overlap,            only : calc_overlap_matrices_urban

!#define PRINT_ARRAYS 1

#ifdef PRINT_ARRAYS
    use print_matrix_mod
#endif
    
    implicit none

    ! --------------------------------------------------------
    ! Section 1: Declare variables and arrays
    ! --------------------------------------------------------

    ! Inputs

    ! Algorithm configuration
    type(config_type),             intent(in)  :: config
    ! Number of spectral intervals, number of layers
    integer(kind=jpim),            intent(in)  :: nlw, nlay
    ! Index of current column and first and last layer of the current column
    integer(kind=jpim),            intent(in)  :: icol, ilay1, ilay2
    ! Number of regions, number of diffuse streams in each hemisphere
    integer(kind=jpim),            intent(in)  :: nreg, ns
    ! Legendre-Gauss coefficients
    type(legendre_gauss_type),     intent(in)  :: lg
    ! Geometric and other spectrally independent properties of the canopy
    type(canopy_properties_type),  intent(in)  :: canopy_props
    ! Spectral properties of the air, vegetation and urban facets
    type(lw_spectral_properties_type),  intent(in)  :: lw_spectral_props

    ! Outputs

    ! Top-of-canopy spectral emissivity and emission (W m-2)
    real(kind=jprb), dimension(nlw),intent(out):: top_emissivity, &
         &                                        top_emission
    ! Flux outputs
    type(canopy_flux_type), intent(inout), optional &
         &  :: lw_internal, & ! LW fluxes from internal emission
         &     lw_norm        ! LW fluxes normalized by top-of-canopy down

    ! Local variables and arrays

    ! Transmittance and reflectance of a layer
    real(kind=jprb), dimension(nlw,nreg*ns,nreg*ns,nlay) &
         &  :: trans, ref

    ! Radiation emerging up from the top or down from the base of a
    ! particular layer (in a given region and stream) due to emission
    ! within the layer
    real(kind=jprb), dimension(nlw,nreg*ns,nlay) :: source_lay

    ! Area fraction of each region in each layer, plus a pseudo-layer
    ! at the end representing the free atmosphere above
    real(kind=jprb), dimension(nreg,nlay+1) :: frac
    
    ! Components of the Gamma matrix
    real(kind=jprb), dimension(nlw,nreg*ns,nreg*ns) :: gamma1, gamma2

    ! Emission rate per unit height, "b" in Eq. 32 (W m-3)
    real(kind=jprb), dimension(nlw,nreg*ns) :: emiss_rate

    ! Emission rate per unit height for walls, air and vegetation (the
    ! last two in regions) considering all emission directions (W m-3)
    real(kind=jprb) :: emiss_wall(nlw,nlay), emiss_reg(nlw,nreg,nlay), &
         &             emiss_air(nlw,2:nreg,nlay), emiss_veg(nlw,2:nreg,nlay)

    ! Geometric factor needed in computing the emiss_* arrays above
    real(kind=jprb) :: emiss_factor
    
    ! Normalized vegetation perimeter length (perimeter length divided
    ! by domain area), m-1.  If nreg=2 then there is a clear-sky and a
    ! vegetation region, and norm_perim(1) is the normalized length
    ! between the two regions, while norm_perim(2) is unused.  If
    ! nreg=3 then region 1 is clear-sky, region 2 is low optical depth
    ! vegetation and region 3 is high optical depth
    ! vegetation. norm_perim(1) is the normalized length between
    ! regions 1 and 2, norm_perim(2) is that between regions 2 and 3,
    ! and norm_perim(3) is that between regions 3 and 1.
    real(kind=jprb) :: norm_perim(nreg)

    ! Normalized perimeter between each region and the wall, m-1.
    real(kind=jprb) :: norm_perim_wall(nreg)

    ! Normalized perimeter length between air or wall and any
    ! vegetation, m-1
    real(kind=jprb) :: norm_perim_air_veg, norm_perim_wall_veg

    ! Rate of exchange between regions, excluding the tangent term,
    ! where the dimensions are in the sense of
    ! f_exchange(region_to,region_from)
    real(kind=jprb) :: f_exchange(nreg,nreg)

    ! Rate of interception of radiation in each region with wall,
    ! excluding tangent term
    real(kind=jprb) :: f_wall(nreg,nlay)

    ! Fraction of energy intercepting a wall that is absorbed and
    ! extinguished
    real(kind=jprb) :: wall_abs(nlw), wall_ext(nlw), wall_factor(nlw)

    ! Spectral emission from the wall or the air/leaves in a region
    real(kind=jprb) :: wall_emiss(nlw), volume_emiss(nlw)
    
    ! Extinction (m-1), single scattering albedo and Planck function
    ! of each region
    real(kind=jprb) :: ext_reg(nlw,nreg), ssa_reg(nlw,nreg), planck_reg(nlw,nreg)

    ! Optical depth scaling of vegetation optical depth to represent
    ! inhomogeneity
    real(kind=jprb) :: od_scaling(2:nreg,nlay)

    ! Albedo matrix just above and below each interface. The "above"
    ! albedo includes surface (interface=1) and top-of-canopy
    ! (interface=nlay+1) so has one element more than the number of
    ! layers. The "below" albedo uses the same indexing of interfaces,
    ! but the surface is never used so this dimension starts at 2. The
    ! "below" albedo includes an extra region corresponding to exposed
    ! roofs.
    real(kind=jprb) :: a_above(nlw,nreg*ns,nreg*ns,nlay+1)
    real(kind=jprb) :: a_below(nlw,(nreg+1)*ns,(nreg+1)*ns,2:nlay+1)

    ! Upward radiation just above and below a layer interface due
    ! purely to emission below that interface. The dimensioning is as
    ! for a_above and a_below but with one fewer dimension.
    real(kind=jprb) :: source_above(nlw,nreg*ns,nlay+1)
    real(kind=jprb) :: source_below(nlw,(nreg+1)*ns,1:nlay+1)
    
    ! Denominator matrix used in the matrix adding method
    real(kind=jprb) :: denominator(nlw,nreg*ns,nreg*ns,nlay)

    ! Directional overlap matrices expressing how the fluxes in each
    ! region of one layer pass into the layer above (u_overlap) or
    ! below (v_overlap)
    real(kind=jprb), dimension(nreg,nreg+1,nlay+1) :: u_overlap
    real(kind=jprb), dimension(nreg+1,nreg,nlay+1) :: v_overlap

    ! Fluxes just above or just below a layer interface, either due to
    ! canopy sources or normalized by downwelling longwave flux at
    ! canopy top, noting that the fluxes below a layer interface have
    ! an additional "region" at the end corresponding to the exposed
    ! roof
    real(kind=jprb), dimension(nlw,nreg*ns)     :: flux_dn_above, flux_up_above
    real(kind=jprb), dimension(nlw,(nreg+1)*ns) :: flux_dn_below, flux_up_below
    
    ! The following matrices are defined such that the integrated flux
    ! (u_hat+v_hat in Eq. 29) is
    !   int_flux_mat * (u_conv+v_conv) + int_source
    ! where u_conv+v_conv is the convergence of fluxes into the layer,
    ! i.e. the sum of the fluxes entering the layer from the top or
    ! base, minus the fluxes leaving the layer from top or
    ! base. "int_source" is the integrated fluxes (u_hat+v_hat) due to
    ! emission within the layer.
    real(kind=jprb) &
         &  :: int_flux_mat(nlw,nreg*ns,nreg*ns,nlay), &
         &     int_source(nlw,nreg*ns,nlay)

    ! Integrated flux across a layer
    real(kind=jprb) :: int_flux(nlw,nreg*ns)

    ! Fractional area of exposed roof at a particular layer interface
    real(kind=jprb) :: exposed_roof_frac
    
    ! Loop counters
    integer(kind=jpim) :: jlay, jreg, jreg_fr, jreg_to, js, js_fr, js_to

    ! Index to layer inside canopy_flux object
    integer(kind=jpim) :: ilay

    ! Index to the matrix dimension expressing regions "from" and "to"
    ! in combination with a particular stream
    integer(kind=jpim) :: ifr, ito
    
    ! Do we do vegetation for this column?
    logical :: do_vegetation

    real(jprb) :: hook_handle

    if (lhook) call dr_hook('radsurf_urban_lw:spartacus_urban_lw',0,hook_handle)
    
    associate( &
         &  dz                => canopy_props%dz(ilay1:ilay2), &
         &  building_fraction => canopy_props%building_fraction(ilay1:ilay2), &
         &  building_scale    => canopy_props%building_scale(ilay1:ilay2), &
         &  air_ext           => lw_spectral_props%air_ext(:,ilay1:ilay2), &
         &  air_ssa           => lw_spectral_props%air_ssa(:,ilay1:ilay2), &
         &  clear_air_planck  => lw_spectral_props%clear_air_planck(:,ilay1:ilay2), &
         &  ground_emissivity => lw_spectral_props%ground_emissivity(:,icol), &
         &  ground_emission   => lw_spectral_props%ground_emission(:,icol), &
         &  roof_emissivity   => lw_spectral_props%roof_emissivity(:,ilay1:ilay2), &
         &  roof_emission     => lw_spectral_props%roof_emission(:,ilay1:ilay2), &
         &  wall_emissivity   => lw_spectral_props%wall_emissivity(:,ilay1:ilay2), &
         &  wall_emission     => lw_spectral_props%wall_emission(:,ilay1:ilay2))
      
      ! --------------------------------------------------------
      ! Section 2: Prepare general variables and arrays
      ! --------------------------------------------------------
      
      do_vegetation = (nreg > 1)
      if (do_vegetation .and. .not. config%do_vegetation) then
        call radiation_abort('Attempt to perform urban radiative transfer with more than one region when vegetation not enabled')
      end if

      ! Set the area fraction of each region
      frac(1,1:nlay)  = 1.0_jprb - building_fraction
      frac(1,nlay+1)  = 1.0_jprb ! Free atmosphere
      if (do_vegetation) then
        frac(1,1:nlay) = max(0.0_jprb, frac(1,1:nlay) - canopy_props%veg_fraction(ilay1:ilay2))
        frac(2:,1:nlay) = spread(max(0.0_jprb, 1.0_jprb - building_fraction &
             &  - frac(1,1:nlay)),1,nreg-1) / real(nreg-1,jprb)
        frac(2:,nlay+1) = 0.0_jprb
      end if

#ifdef PRINT_ARRAYS
      call print_vector('veg_fraction',canopy_props%veg_fraction)
      call print_vector('building_fraction',building_fraction)
      call print_vector('veg_scale', canopy_props%veg_scale);
      call print_matrix('frac', frac);
#endif

      ! Compute overlap matrices
      call calc_overlap_matrices_urban(nlay,nreg,frac,u_overlap,v_overlap, &
           &  config%min_vegetation_fraction);

#ifdef PRINT_ARRAYS
      call print_array3('u_overlap',u_overlap)
      call print_array3('v_overlap',v_overlap)
#endif      

      ! --------------------------------------------------------
      ! Section 3: First loop over layers
      ! --------------------------------------------------------
      ! Loop up through the canopy computing the Gamma matrices, and
      ! from those the transmission and reflection matrices
      do jlay = 1,nlay

        ! --------------------------------------------------------
        ! Section 3a: Prepare the properties of the current layer
        ! --------------------------------------------------------
        ! Compute the extinction coefficient and single-scattering
        ! albedo of each region
        ext_reg(:,1)    = air_ext(:,jlay)
        ssa_reg(:,1)    = air_ssa(:,jlay)
        planck_reg(:,1) = clear_air_planck(:,jlay)
        if (nreg == 2) then
          associate ( veg_ext        => canopy_props%veg_ext(ilay1:ilay2), &
            &         veg_ssa        => lw_spectral_props%veg_ssa(:,ilay1:ilay2), &
            &         veg_planck     => lw_spectral_props%veg_planck(:,ilay1:ilay2), &
            &         veg_air_planck => lw_spectral_props%veg_air_planck(:,ilay1:ilay2) )
          ext_reg(:,2) = air_ext(:,jlay) + veg_ext(jlay)
          ssa_reg(:,2) = (ext_reg(:,1)*ssa_reg(:,1) + veg_ext(jlay)*veg_ssa(:,jlay)) &
               &       / max(ext_reg(:,2), 1.0e-8_jprb)
          planck_reg(:,2) = (ext_reg(:,1)*(1.0_jprb-ssa_reg(:,1))*veg_air_planck(:,jlay) &
               &   + veg_ext(jlay)*(1.0_jprb-veg_ssa(:,jlay))*veg_planck(:,jlay)) &
               &       / max(ext_reg(:,2)*(1.0_jprb-ssa_reg(:,2)), 1.0e-8_jprb)
          od_scaling(2,jlay) = 1.0_jprb
          end associate
        else if (nreg == 3) then
          ! Approximate method to approximate a Gamma distribution
          associate(veg_fsd => canopy_props%veg_fsd(ilay1:ilay2))
            od_scaling(2,jlay) = exp(-veg_fsd(jlay)*(1.0_jprb + 0.5_jprb*veg_fsd(jlay) &
                 &                            *(1.0_jprb + 0.5_jprb*veg_fsd(jlay))))
          end associate
          
          associate ( veg_ext        => canopy_props%veg_ext(ilay1:ilay2), &
            &         veg_ssa        => lw_spectral_props%veg_ssa(:,ilay1:ilay2), &
            &         veg_planck     => lw_spectral_props%veg_planck(:,ilay1:ilay2), &
            &         veg_air_planck => lw_spectral_props%veg_air_planck(:,ilay1:ilay2))
          od_scaling(3,jlay) = 2.0_jprb - od_scaling(2,jlay)
          ext_reg(:,2) = air_ext(:,jlay) + od_scaling(2,jlay)*veg_ext(jlay)
          ext_reg(:,3) = air_ext(:,jlay) + od_scaling(3,jlay)*veg_ext(jlay)
          ssa_reg(:,2) = (ext_reg(:,1)*ssa_reg(:,1) &
               &          + od_scaling(2,jlay)*veg_ext(jlay)*veg_ssa(:,jlay)) &
               &       / max(ext_reg(:,2), 1.0e-8_jprb)
          ssa_reg(:,3) = (ext_reg(:,1)*ssa_reg(:,1) &
               &          + od_scaling(3,jlay)*veg_ext(jlay)*veg_ssa(:,jlay)) &
               &       / max(ext_reg(:,3), 1.0e-8_jprb)
          planck_reg(:,2) = (ext_reg(:,1)*(1.0_jprb-ssa_reg(:,1))*veg_air_planck(:,jlay) &
               &   + od_scaling(2,jlay) * veg_ext(jlay)*(1.0_jprb-veg_ssa(:,jlay))*veg_planck(:,jlay)) &
               &       / max(ext_reg(:,2)*(1.0_jprb-ssa_reg(:,2)), 1.0e-8_jprb)
          planck_reg(:,3) = (ext_reg(:,1)*(1.0_jprb-ssa_reg(:,1))*veg_air_planck(:,jlay) &
               &   + od_scaling(3,jlay) * veg_ext(jlay)*(1.0_jprb-veg_ssa(:,jlay))*veg_planck(:,jlay)) &
               &       / max(ext_reg(:,3)*(1.0_jprb-ssa_reg(:,3)), 1.0e-8_jprb)
          end associate
          
        end if

        norm_perim = 0.0_jprb
        if (nreg > 1) then
          associate (veg_fraction => canopy_props%veg_fraction(ilay1:ilay2)) 
          if (veg_fraction(jlay) > config%min_vegetation_fraction) then
            ! Compute the normalized vegetation perimeter length
            associate (veg_scale => canopy_props%veg_scale(ilay1:ilay2))
            if (config%use_symmetric_vegetation_scale_urban) then
              norm_perim(1) = 4.0_jprb * veg_fraction(jlay) &
                   &  * max(0.0_jprb, 1.0_jprb - veg_fraction(jlay) - building_fraction(jlay)) &
                   &  / veg_scale(jlay)
            else
              norm_perim(1) = 4.0_jprb * veg_fraction(jlay) / veg_scale(jlay)
            end if
            end associate

            norm_perim_air_veg = norm_perim(1)

            if (nreg > 2) then
              ! Share the clear-air/vegetation perimeter between the two
              ! vegetated regions
              norm_perim(nreg) = config%vegetation_isolation_factor_urban * norm_perim(1)
              norm_perim(1) = (1.0_jprb - config%vegetation_isolation_factor_urban) &
                   &          * norm_perim(1) 
              ! We assume that the horizontal scale of the vegetation
              ! inhomogeneities is the same as the scale of the tree
              ! crowns themselves. Therefore, to compute the interface
              ! between the two vegetated regions, we use the same
              ! formula as before but with the fraction associated
              ! with one of the two vegetated regions, which is half
              ! the total vegetation fraction.
              associate (veg_scale => canopy_props%veg_scale(ilay1:ilay2))
              if (config%use_symmetric_vegetation_scale_urban) then
                norm_perim(2) = (1.0_jprb - config%vegetation_isolation_factor_urban) &
                     &  * 4.0_jprb * (0.5_jprb*veg_fraction(jlay)) &
                     &  * (1.0_jprb - (0.5_jprb*veg_fraction(jlay))) &
                     &  / veg_scale(jlay)
              else
                !            norm_perim(2) = (1.0_jprb - config%vegetation_isolation_factor_urban) &
                !                 &  * 4.0_jprb * (0.5_jprb*veg_fraction(jlay)) / veg_scale(jlay)
                ! Lollipop model - see Hogan, Quaife and Braghiere (2018) explaining sqrt(2)
                norm_perim(2) = (1.0_jprb - config%vegetation_isolation_factor_urban) &
                     &  * 4.0_jprb * veg_fraction(jlay) / (sqrt(2.0_jprb)*veg_scale(jlay))
              end if
              end associate
            else
              ! Only one vegetated region so the other column of
              ! norm_perim is unused
              norm_perim(2:) = 0.0_jprb
            end if
          end if
          end associate
        end if
        
        ! Compute the normalized length of the interface between each
        ! region and a building wall
        norm_perim_wall = 0.0_jprb
        if (building_fraction(jlay) > config%min_building_fraction) then
          norm_perim_wall(1) = 4.0_jprb * building_fraction(jlay) / building_scale(jlay)
          if (nreg > 1) then
            associate ( veg_contact_fraction  => canopy_props%veg_contact_fraction(ilay1:ilay2) )
            if (veg_contact_fraction(jlay) > 0.0_jprb) then
              ! Compute normalized length of interface between wall
              ! and any vegetation
              norm_perim_wall_veg = min(norm_perim_air_veg*veg_contact_fraction(jlay), &
                   &                    norm_perim_wall(1))
              if (nreg == 2) then
                norm_perim_wall(2) = norm_perim_wall_veg
                norm_perim(1) = norm_perim(1) - norm_perim_wall_veg
              else
                norm_perim_wall(2) = norm_perim_wall_veg &
                     &  * (1.0_jprb - config%vegetation_isolation_factor_urban)
                norm_perim(1) = norm_perim(1) - norm_perim_wall(2)
                norm_perim_wall(3) = norm_perim_wall_veg &
                     &  * config%vegetation_isolation_factor_urban
                norm_perim(3) = norm_perim(3) - norm_perim_wall(3)
              end if
              ! Reduce length of interface between wall and clear-air
              norm_perim_wall(1) = norm_perim_wall(1) - norm_perim_wall_veg
            else if (frac(1,jlay) <= config%min_vegetation_fraction) then
              ! There is no clear region (region 1), so all walls must
              ! be in contact with vegetation (region 2 and possibly
              ! 3)
              if (nreg == 2) then
                norm_perim_wall(2) = norm_perim_wall(1)
              else
                norm_perim_wall(2) = norm_perim_wall(1) &
                     &  * (1.0_jprb - config%vegetation_isolation_factor_urban)
                norm_perim_wall(3) = norm_perim_wall(1) &
                     &  * config%vegetation_isolation_factor_urban
              end if
              norm_perim_wall(1) = 0.0_jprb
            end if
            end associate
          end if
        end if

        ! Compute the rates of exchange between regions, excluding the
        ! tangent term
        f_exchange = 0.0_jprb
        do jreg = 1,nreg-1
          if (frac(jreg,jlay) <= config%min_vegetation_fraction &
               &  .or. frac(jreg+1,jlay) <= config%min_vegetation_fraction) then
            f_exchange(jreg+1,jreg) = 0.0_jprb
            f_exchange(jreg,jreg+1) = 0.0_jprb
          else
            f_exchange(jreg+1,jreg) = norm_perim(jreg) / (Pi * frac(jreg,jlay))
            f_exchange(jreg,jreg+1) = norm_perim(jreg) / (Pi * frac(jreg+1,jlay))
          end if
        end do
        if (nreg > 2 .and. norm_perim(nreg) > 0.0_jprb) then
          if (frac(3,jlay) <= config%min_vegetation_fraction &
               &  .or. frac(1,jlay) <= config%min_vegetation_fraction) then
            f_exchange(1,3) = 0.0_jprb
            f_exchange(3,1) = 0.0_jprb
          else
            f_exchange(1,3) = norm_perim(jreg) / (Pi * frac(3,jlay))
            f_exchange(3,1) = norm_perim(jreg) / (Pi * frac(1,jlay))
          end if
        end if

        ! Compute rate of interception of wall, excluding tangent term
        f_wall(:,jlay) = 0.0_jprb
        do jreg = 1,nreg
          if (frac(jreg,jlay) <= config%min_vegetation_fraction) then
            f_wall(jreg,jlay) = 0.0_jprb
          else
            f_wall(jreg,jlay) = norm_perim_wall(jreg) * lg%vadjustment2 &
                 &            / (Pi * frac(jreg,jlay))
          end if
        end do

        ! Compute fraction of intercepted radiation extinguished
        ! (absorption + scattering) and absorbed by wall; currently
        ! all wall-intercepted radiation is either scattered or
        ! absorbed (1.0), but at some future date we may add a
        ! specular term in which case it would be treated as
        ! unscattered
        wall_ext = 1.0_jprb;
        wall_abs = wall_emissivity(:,jlay)
        wall_factor = 1.0_jprb - wall_emissivity(1,jlay)
        
        ! --------------------------------------------------------
        ! Section 3b: Compute Gamma matrices
        ! --------------------------------------------------------
        ! Compute the Gamma matrix representing exchange of
        ! radiation between regions
        gamma1 = 0.0_jprb
        do jreg_fr = 1,nreg
          do jreg_to = 1,nreg
            if (jreg_fr /= jreg_to) then
              do js = 1,ns
                ifr = js + (jreg_fr-1)*ns
                ito = js + (jreg_to-1)*ns
                gamma1(:,ifr,ifr) = gamma1(:,ifr,ifr) &
                     &  - lg%tan_ang(js) * f_exchange(jreg_to,jreg_fr)
                gamma1(:,ito,ifr) = + lg%tan_ang(js) * f_exchange(jreg_to,jreg_fr)
              end do
            end if
          end do
        end do

        ! Diagonal elements of gamma1 also has loss term due to
        ! extinction through the regions
        do jreg = 1,nreg
          do js = 1,ns
            ifr = js + (jreg-1)*ns
            gamma1(:,ifr,ifr) = gamma1(:,ifr,ifr) - ext_reg(:,jreg)/lg%mu(js) &
                 &                 - lg%tan_ang(js) * f_wall(jreg,jlay) * wall_ext
          end do
        end do

        ! Compute Gamma2, representing the rate of scattering of
        ! radiation from one diffuse stream to another
        gamma2 = 0.0_jprb
        do js_fr = 1,ns
          do js_to = 1,ns
            do jreg = 1,nreg
              ifr = js_fr + (jreg-1)*ns
              ito = js_to + (jreg-1)*ns
              ! Factor of 0.5 is because half of energy goes into
              ! other hemisphere
              gamma2(:,ito,ifr) = 0.5_jprb * (lg%weight(js_to) &
                   &  * ext_reg(:,jreg) * ssa_reg(:,jreg) / lg%mu(js_fr) &
                   &  + lg%vweight(js_to) * lg%tan_ang(js_fr) &
                   &       * f_wall(jreg,jlay) * wall_factor)
            end do
          end do
        end do

        ! Gamma1 also contains gain of diffuse radiation by
        ! scattering, so add Gamma2 to it
        gamma1 = gamma1 + gamma2

        ! Compute emission rates
        emiss_factor = 2.0_jprb * sum(lg%hweight / lg%mu)
        do jreg = 1,nreg
          volume_emiss = frac(jreg,jlay) * (ext_reg(:,jreg) * (1.0_jprb - ssa_reg(:,jreg)) &
               &                            * planck_reg(:,jreg))
          wall_emiss = norm_perim_wall(jreg) * lg%vadjustment &
               &                             * wall_emission(:,jlay)
          do js = 1,ns
            ifr = js + (jreg-1)*ns
            emiss_rate(:,ifr) = (lg%hweight(js) / lg%mu(js)) * volume_emiss &
                 &             + (0.5_jprb * lg%vweight(js)) * wall_emiss
          end do
          emiss_reg(:,jreg,jlay) = emiss_factor * volume_emiss

          if (jreg > 1) then
            associate ( veg_ext        => canopy_props%veg_ext(ilay1:ilay2), &
              &         veg_ssa        => lw_spectral_props%veg_ssa(:,ilay1:ilay2), &
              &         veg_planck     => lw_spectral_props%veg_planck(:,ilay1:ilay2), &
              &         veg_air_planck => lw_spectral_props%veg_air_planck(:,ilay1:ilay2))
            !
            ! Note that in the following we use the extinction and
            ! single-scattering albedo of the clear air region
            emiss_air(:,jreg,jlay) = emiss_factor * frac(jreg,jlay) * ext_reg(:,1) &
                 &              * (1.0_jprb-ssa_reg(:,1)) * veg_air_planck(:,jlay)
            emiss_veg(:,jreg,jlay) = emiss_factor * frac(jreg,jlay) * veg_ext(jlay) &
                 &   * (1.0_jprb-veg_ssa(:,jlay)) * veg_planck(:,jlay) * od_scaling(jreg,jlay)
            end associate
          end if

        end do
        emiss_wall(:,jlay) = (sum(norm_perim_wall) * lg%vadjustment) * wall_emission(:,jlay)
        
        
#ifdef PRINT_ARRAYS
        print *, 'PROPERTIES OF LAYER ', jlay
        call print_vector('ext_reg',ext_reg(1,:))
        call print_vector('ssa_reg',ssa_reg(1,:))
        call print_matrix('f_exchange',f_exchange)
        call print_vector('norm_perim', norm_perim)
        call print_vector('norm_perim_wall', norm_perim_wall)
        call print_matrix('gamma1',gamma1(1,:,:))
        call print_matrix('gamma2',gamma2(1,:,:))
        call print_vector('emiss_rate',emiss_rate(1,:))
        call print_vector('clear_air_planck', clear_air_planck(1,:))
        call print_vector('veg_air_planck', lw_spectral_props%veg_air_planck(:,ilay1:ilay2))
        call print_vector('veg_planck', lw_spectral_props%veg_planck(:,ilay1:ilay2))
#endif        

        ! --------------------------------------------------------
        ! Section 3c: Compute reflection/transmission matrices for this layer
        ! --------------------------------------------------------
        if (do_vegetation) then
          associate (veg_fraction => canopy_props%veg_fraction(ilay1:ilay2))

          if (veg_fraction(jlay) <= config%min_vegetation_fraction) then
            ! Vegetation-free calculation: set all coefficients to zero...
            ref(:,:,:,jlay) = 0.0_jprb
            trans(:,:,:,jlay) = 0.0_jprb
            source_lay(:,:,jlay) = 0.0_jprb
            int_flux_mat(:,:,:,jlay) = 0.0_jprb
            int_source(:,:,jlay) = 0.0_jprb
            ! ...then perform calculations only for the clear-sky region
            call calc_matrices_lw_eig(nlw, ns, dz(jlay), &
                 &  gamma1(:,1:ns,1:ns), gamma2(:,1:ns,1:ns), emiss_rate(:,1:ns), &
                 &  ref(:,1:ns,1:ns,jlay), trans(:,1:ns,1:ns,jlay), source_lay(:,1:ns,jlay), &
                 &  int_flux_mat(:,1:ns,1:ns,jlay), int_source(:,1:ns,jlay))
          else if (frac(1,jlay) <= config%min_vegetation_fraction) then
            ! Vegetation fills the street: set all coefficients to zero...
            ref(:,:,:,jlay) = 0.0_jprb
            trans(:,:,:,jlay) = 0.0_jprb
            source_lay(:,:,jlay) = 0.0_jprb
            int_flux_mat(:,:,:,jlay) = 0.0_jprb
            int_source(:,:,jlay) = 0.0_jprb
            ! ...then perform calculations only for the vegetated regions
            call calc_matrices_lw_eig(nlw, ns*(nreg-1), dz(jlay), &
                 &  gamma1(:,ns+1:nreg*ns,ns+1:nreg*ns), gamma2(:,ns+1:nreg*ns,ns+1:nreg*ns), &
                 &  emiss_rate(:,ns+1:nreg*ns), ref(:,ns+1:nreg*ns,ns+1:nreg*ns,jlay), &
                 &  trans(:,ns+1:nreg*ns,ns+1:nreg*ns,jlay), source_lay(:,ns+1:nreg*ns,jlay), &
                 &  int_flux_mat(:,ns+1:nreg*ns,ns+1:nreg*ns,jlay), int_source(:,ns+1:nreg*ns,jlay))
          else
            call calc_matrices_lw_eig(nlw, nreg*ns, dz(jlay), &
                 &  gamma1, gamma2, emiss_rate, &
                 &  ref(:,:,:,jlay), trans(:,:,:,jlay), source_lay(:,:,jlay), &
                 &  int_flux_mat(:,:,:,jlay), int_source(:,:,jlay))
          end if

          end associate
        else
          ! Vegetation-free calculation: set all coefficients to zero...
          ref(:,:,:,jlay) = 0.0_jprb
          trans(:,:,:,jlay) = 0.0_jprb
          source_lay(:,:,jlay) = 0.0_jprb
          int_flux_mat(:,:,:,jlay) = 0.0_jprb
          int_source(:,:,jlay) = 0.0_jprb
          ! ...then perform calculations only for the clear-sky region
          call calc_matrices_lw_eig(nlw, ns, dz(jlay), &
               &  gamma1(:,1:ns,1:ns), gamma2(:,1:ns,1:ns), emiss_rate(:,1:ns), &
               &  ref(:,1:ns,1:ns,jlay), trans(:,1:ns,1:ns,jlay), source_lay(:,1:ns,jlay), &
               &  int_flux_mat(:,1:ns,1:ns,jlay), int_source(:,1:ns,jlay))
        end if   
      end do ! Loop over layers to compute reflectance/transmittance matrices

      ! --------------------------------------------------------
      ! Section 4: Albedo of scene at each layer interface
      ! --------------------------------------------------------
      ! Set the surface albedo and upward emission
      a_above = 0.0_jprb
      source_above = 0.0_jprb
      do jreg = 1,nreg
        do js_to = 1,ns
          do js_fr = 1,ns
            a_above(:,js_to+(jreg-1)*ns,js_fr+(jreg-1)*ns,1) &
                 &  = (1.0_jprb - ground_emissivity) * lg%hweight(js_to)
          end do
        end do
        do js = 1,ns
          source_above(:,js+(jreg-1)*ns,1) = (lg%hweight(js) * frac(jreg,1)) &
               &                           * ground_emission
        end do
      end do

      ! Loop up through the half levels / interfaces computing albedos
      a_below = 0.0_jprb
      source_below = 0.0_jprb
      do jlay = 1,nlay
        ! Adding method
        denominator(:,:,:,jlay) = identity_minus_mat_x_mat(nlw,nlw,nreg*ns, &
             &  a_above(:,:,:,jlay), ref(:,:,:,jlay))
        ! Recall that a_below has extra rows/columns for the exposed
        ! roof; the following fills only the part related to regions
        ! within layer jlay
        a_below(:,1:nreg*ns,1:nreg*ns,jlay+1) = ref(:,:,:,jlay) &
             &  + mat_x_mat(nlw,nlw,nreg*ns,trans(:,:,:,jlay), &
             &  solve_mat(nlw,nlw,nreg*ns,denominator(:,:,:,jlay), &
             &  mat_x_mat(nlw,nlw,nreg*ns,a_above(:,:,:,jlay), &
             &  trans(:,:,:,jlay))))
        ! Eq. 34
        source_below(:,1:nreg*ns,jlay+1) = source_lay(:,:,jlay) &
             &  + mat_x_vec(nlw,nlw,nreg*ns,trans(:,:,:,jlay), &
             &  solve_vec(nlw,nlw,nreg*ns,denominator(:,:,:,jlay), &
             &  source_above(:,:,jlay) &
             &  + mat_x_vec(nlw,nlw,nreg*ns,a_above(:,:,:,jlay),source_lay(:,:,jlay))))
        ! Add the contribution from the exposed roofs
        if (jlay < nlay) then
          exposed_roof_frac = building_fraction(jlay)-building_fraction(jlay+1)
        else
          exposed_roof_frac = building_fraction(jlay)
        end if
        do js = 1,ns
          a_below(:,nreg*ns+js,nreg*ns+1:(nreg+1)*ns,jlay+1) &
               &  = spread((1.0_jprb - roof_emissivity(:,jlay)) * lg%hweight(js), 2, ns)
          source_below(:,nreg*ns+js,jlay+1) = lg%hweight(js) * roof_emission(:,jlay) &
               &  * exposed_roof_frac
        end do

        ! Overlap: Hogan (2019), equations 22 and 36
        a_above(:,:,:,jlay+1) = rect_expandedmat_x_mat(nlw,nreg,nreg+1,ns,nreg*ns, &
             &  u_overlap(:,:,jlay+1), &
             &  rect_mat_x_expandedmat(nlw,nreg+1,nreg,ns,(nreg+1)*ns, a_below(:,:,:,jlay+1), &
             &                          v_overlap(:,:,jlay+1)))
        source_above(:,:,jlay+1) = rect_expandedmat_x_vec(nlw,nreg,nreg+1,ns, &
             &  u_overlap(:,:,jlay+1), source_below(:,:,jlay+1))
      end do ! Loop over layers for upward pass to compute albedo

#ifdef PRINT_ARRAYS
      call print_array3('a_above', a_above(1,:,:,:))
      call print_matrix('source_above', source_above(1,:,:))
      call print_matrix('source_below', source_below(1,:,:))
      call print_array3('a_below', a_below(1,:,:,:))
      call print_array3('T', trans(1,:,:,:))
      call print_array3('R', ref(1,:,:,:))
      call print_matrix('source_lay', source_lay(1,:,:))
      call print_array3('int_flux_mat', int_flux_mat(1,:,:,:))
      call print_matrix('int_source', int_source(1,:,:))
      call print_matrix('emiss_reg', emiss_reg(1,:,:))
#endif

      ! Store top-of-canopy boundary conditions.  Isotropic emissivity
      ! is weighted average over the ns streams, noting that just
      ! above the "nlay+1" interface we are above the canopy so only
      ! need to consider the clear-sky region (indexed 1:ns).
      top_emissivity = 1.0_jprb - sum(mat_x_vec(nlw,nlw,ns,a_above(:,1:ns,1:ns,nlay+1), &
           &                             spread(lg%hweight,1,nlw)),2)
      ! Top-of-canopy emission is the sum over the streams in the
      ! clear-sky region
      top_emission = sum(source_above(:,1:ns,nlay+1),2)
      
      ! --------------------------------------------------------
      ! Section 5: Compute normalized flux profile
      ! --------------------------------------------------------

      ! Set to the flux components to zero initially
      call lw_internal%zero(icol, ilay1, ilay2)
      call lw_norm%zero( icol, ilay1, ilay2)

      ! First the fluxes associated with emission within the canopy

      ! Initial normalized downward flux is zero
      flux_dn_above = 0.0_jprb
      
      lw_internal%top_dn(:,icol)  = 0.0_jprb
      lw_internal%top_net(:,icol) = -top_emission

      ! Loop down through layers
      do jlay = nlay,1,-1
        ! Find index into output arrays
        ilay = ilay1 + jlay - 1

        ! Translate the downwelling flux component across the
        ! interface at the top of the layer
        flux_dn_below = rect_expandedmat_x_vec(nlw,nreg+1,nreg,ns, & 
             &  v_overlap(:,:,jlay+1), flux_dn_above)
        flux_up_below = mat_x_vec(nlw,nlw,(nreg+1)*ns,a_below(:,:,:,jlay+1),flux_dn_below) &
             &  + source_below(:,:,jlay+1)
        
        ! Store the fluxes into the exposed roof
        lw_internal%roof_in(:,ilay) = sum(flux_dn_below(:,nreg*ns+1:(nreg+1)*ns),2)
        lw_internal%roof_net(:,ilay) = lw_internal%roof_in(:,ilay) &
             &  - sum(flux_up_below(:,nreg*ns+1:(nreg+1)*ns),2)

        ! Compute fluxes at base of layer, removing the roof region
        ! from the "_below" flux arrays
        flux_dn_above = solve_vec(nlw,nlw,nreg*ns,denominator(:,:,:,jlay), &
             &  (mat_x_vec(nlw,nlw,nreg*ns,trans(:,:,:,jlay),flux_dn_below(:,1:nreg*ns)) &
             &   + mat_x_vec(nlw,nlw,nreg*ns,ref(:,:,:,jlay),source_above(:,:,jlay)) &
             &   + source_lay(:,:,jlay)))
        flux_up_above = mat_x_vec(nlw,nlw,nreg*ns,a_above(:,:,:,jlay),flux_dn_above) &
             &  + source_above(:,:,jlay)

        ! Compute integrated flux vectors, recalling that _above means
        ! above the just above the *base* of the layer, and _below
        ! means just below the *top* of the layer
        int_flux = mat_x_vec(nlw,nlw,nreg*ns,int_flux_mat(:,:,:,jlay), &
             &               flux_dn_below(:,1:nreg*ns) + flux_up_above) &
             &   + int_source(:,:,jlay)
       
        ! Absorption by clear-air region - see Eqs. 29 and 30
        lw_internal%clear_air_abs(:,ilay) = lw_internal%clear_air_abs(:,ilay) &
             &  + air_ext(:,jlay)*(1.0_jprb-air_ssa(:,jlay)) &
             &    * sum(int_flux(:,1:ns) * spread(1.0_jprb/lg%mu,nlw,1), 2) &
             &  - emiss_reg(:,1,jlay)*dz(jlay)
        if (do_vegetation) then
          associate ( veg_ext => canopy_props%veg_ext(ilay1:ilay2), &
            &         veg_ssa => lw_spectral_props%veg_ssa(:,ilay1:ilay2) )
          do jreg = 2,nreg
            ! Absorption by clear-air in the vegetated regions
            lw_internal%veg_air_abs(:,ilay) = lw_internal%veg_air_abs(:,ilay) &
                 &  + air_ext(:,jlay)*(1.0_jprb-air_ssa(:,jlay)) & ! Use clear-air properties
                 &    * sum(int_flux(:,(jreg-1)*ns+1:jreg*ns) &
                 &             * spread(1.0_jprb/lg%mu,nlw,1), 2) &
                 &  - emiss_air(:,jreg,jlay)*dz(jlay)
            lw_internal%veg_abs(:,ilay) = lw_internal%veg_abs(:,ilay) &
                 &  + veg_ext(jlay)*(1.0_jprb-veg_ssa(:,jlay)) & ! Use vegetation properties
                 &    * sum(int_flux(:,(jreg-1)*ns+1:jreg*ns) &
                 &             * spread(1.0_jprb/lg%mu,nlw,1), 2) * od_scaling(jreg,jlay) &
                 &  - emiss_veg(:,jreg,jlay)*dz(jlay)
         end do
         end associate
        end if

        ! Inward and net flux into walls
        do jreg = 1,nreg
          lw_internal%wall_in(:,ilay) = lw_internal%wall_in(:,ilay) &
               &  + f_wall(jreg,jlay) &
               &  * sum(int_flux(:,(jreg-1)*ns+1:jreg*ns) &
               &           * spread(lg%tan_ang,1,nlw))
        end do
        lw_internal%wall_net(:,ilay) = lw_internal%wall_in(:,ilay) &
             &  * wall_emissivity(:,jlay) - emiss_wall(:,jlay)*dz(jlay)

        if (allocated(lw_internal%flux_dn_layer_top)) then
          ! Store fluxes at top/base of layer after absorption diagnostics
          ! have been computed, so these optional outputs cannot affect them.
          lw_internal%flux_dn_layer_top(:,ilay) = sum(flux_dn_below(:,1:nreg*ns),2)
          lw_internal%flux_up_layer_top(:,ilay) = sum(flux_up_below(:,1:nreg*ns),2)
          lw_internal%flux_dn_layer_base(:,ilay) = sum(flux_dn_above,2)
          lw_internal%flux_up_layer_base(:,ilay) = sum(flux_up_above,2)
        end if

#ifdef PRINT_ARRAYS
        print *, 'ABSOLUTE FLUXES DUE TO INTERNAL EMISSION AT LAYER ', jlay
        call print_vector('  flux_dn_below ', flux_dn_below(1,:))
        call print_vector('  flux_dn_above ', flux_dn_above(1,:))
        call print_vector('  flux_up_above ', flux_up_above(1,:))
#endif

      end do
      lw_internal%ground_dn(:,icol) = sum(flux_dn_above,2)
      lw_internal%ground_net(:,icol) = lw_internal%ground_dn(:,icol) &
           &  - sum(flux_up_above,2)
      do jreg = 1,nreg
        do js = 1,ns
          ifr = js + (jreg-1)*ns
          ! Project each stream into a horizontal plane
          lw_internal%ground_vertical_diff(:,icol) = lw_internal%ground_vertical_diff(:,icol) &
               &  + (flux_dn_above(:,ifr) +flux_up_above(:,ifr)) * lg%tan_ang(js)/Pi
        end do
      end do

      ! Second the fluxes due to and normalized by the downwelling
      ! flux at canopy top. This is identical to the shortwave case.

      ! Initial normalized diffuse flux sums to 1 in each stream, in
      ! each spectral interval, so use the Legendre-Gauss horizontal
      ! weights
      flux_dn_above           = 0.0_jprb
      flux_dn_above(:,1:ns)   = spread(lg%hweight,nlw,1)

      lw_norm%top_dn(:,icol)  = 1.0_jprb
      lw_norm%top_net(:,icol) = top_emissivity
      
      ! Loop down through layers
      do jlay = nlay,1,-1
        ! Find index into output arrays
        ilay = ilay1 + jlay - 1

        ! Translate the downwelling flux component across the
        ! interface at the top of the layer
        flux_dn_below = rect_expandedmat_x_vec(nlw,nreg+1,nreg,ns, &
             &  v_overlap(:,:,jlay+1), flux_dn_above)
        flux_up_below = mat_x_vec(nlw,nlw,(nreg+1)*ns,a_below(:,:,:,jlay+1),flux_dn_below)

        ! Store the fluxes into the exposed roof
        lw_norm%roof_in(:,ilay) = sum(flux_dn_below(:,nreg*ns+1:(nreg+1)*ns),2)
        lw_norm%roof_net(:,ilay) = lw_norm%roof_in(:,ilay) &
             &  - sum(flux_up_below(:,nreg*ns+1:(nreg+1)*ns),2)

        ! Compute fluxes at base of layer, removing the roof region
        ! from the "_below" flux arrays
        flux_dn_above = solve_vec(nlw,nlw,nreg*ns,denominator(:,:,:,jlay), &
             &  mat_x_vec(nlw,nlw,nreg*ns,trans(:,:,:,jlay),flux_dn_below(:,1:nreg*ns)))
        flux_up_above = mat_x_vec(nlw,nlw,nreg*ns,a_above(:,:,:,jlay), &
             &  flux_dn_above)

        ! Compute integrated flux vectors, recalling that _above means
        ! above the just above the *base* of the layer, and _below
        ! means just below the *top* of the layer
        !int_flux = mat_x_vec(nlw,nlw,nreg*ns,int_flux_mat(:,:,:,jlay), flux_dn_below(:,1:nreg*ns) &
        !     &                   - flux_dn_above - flux_up_below(:,1:nreg*ns) + flux_up_above)
        int_flux = mat_x_vec(nlw,nlw,nreg*ns,int_flux_mat(:,:,:,jlay), &
             &               flux_dn_below(:,1:nreg*ns) + flux_up_above)

        ! Absorption by clear-air region - see Eqs. 29 and 30
        lw_norm%clear_air_abs(:,ilay) = lw_norm%clear_air_abs(:,ilay) &
             &  + air_ext(:,jlay)*(1.0_jprb-air_ssa(:,jlay)) &
             &    * sum(int_flux(:,1:ns) * spread(1.0_jprb/lg%mu,nlw,1), 2)
        if (do_vegetation) then
          associate ( veg_ext => canopy_props%veg_ext(ilay1:ilay2), &
            &         veg_ssa => lw_spectral_props%veg_ssa(:,ilay1:ilay2) )
          do jreg = 2,nreg
            ! Absorption by clear-air in the vegetated regions
            lw_norm%veg_air_abs(:,ilay) = lw_norm%veg_air_abs(:,ilay) &
                 &  + air_ext(:,jlay)*(1.0_jprb-air_ssa(:,jlay)) & ! Use clear-air properties
                 &    * sum(int_flux(:,(jreg-1)*ns+1:jreg*ns) &
                 &             * spread(1.0_jprb/lg%mu,nlw,1), 2)
            lw_norm%veg_abs(:,ilay) = lw_norm%veg_abs(:,ilay) &
                 &  + veg_ext(jlay)*(1.0_jprb-veg_ssa(:,jlay)) & ! Use vegetation properties
                 &    * sum(int_flux(:,(jreg-1)*ns+1:jreg*ns) &
                 &             * spread(1.0_jprb/lg%mu,nlw,1), 2) * od_scaling(jreg,jlay)
          end do
          end associate
        end if

        ! Inward and net flux into walls
        do jreg = 1,nreg
          lw_norm%wall_in(:,ilay) = lw_norm%wall_in(:,ilay) &
               &  + f_wall(jreg,jlay) &
               &  * (sum(int_flux(:,(jreg-1)*ns+1:jreg*ns) &
               &           * spread(lg%tan_ang,1,nlw)))
        end do
        lw_norm%wall_net(:,ilay) = lw_norm%wall_in(:,ilay) &
             &  * wall_emissivity(:,jlay)

        if (allocated(lw_norm%flux_dn_layer_top)) then
          ! Store fluxes at top/base of layer after absorption diagnostics
          ! have been computed, so these optional outputs cannot affect them.
          lw_norm%flux_dn_layer_top(:,ilay) = sum(flux_dn_below(:,1:nreg*ns),2)
          lw_norm%flux_up_layer_top(:,ilay) = sum(flux_up_below(:,1:nreg*ns),2)
          lw_norm%flux_dn_layer_base(:,ilay) = sum(flux_dn_above,2)
          lw_norm%flux_up_layer_base(:,ilay) = sum(flux_up_above,2)
        end if

#ifdef PRINT_ARRAYS
        print *, 'NORMALIZED FLUXES W.R.T. DIFFUSE INCOMING RADIATION AT LAYER ', jlay
        call print_vector('  flux_dn_below ', flux_dn_below(1,:))
        call print_vector('  flux_dn_above ', flux_dn_above(1,:))
        call print_vector('  flux_up_above ', flux_up_above(1,:))
#endif

      end do
      lw_norm%ground_dn(:,icol) = sum(flux_dn_above,2)
      lw_norm%ground_net(:,icol) = lw_norm%ground_dn(:,icol) &
           &  - sum(flux_up_above,2)
      do jreg = 1,nreg
        do js = 1,ns
          ifr = js + (jreg-1)*ns
          ! Project each stream into a horizontal plane
          lw_norm%ground_vertical_diff(:,icol) = lw_norm%ground_vertical_diff(:,icol) &
               &  + (flux_dn_above(:,ifr) +flux_up_above(:,ifr)) * lg%tan_ang(js)/Pi
        end do
      end do

#ifdef PRINT_ARRAYS
      print *, 'ABSOLUTE FLUXES DUE TO INTERNAL EMISSION'
      call print_vector('  clear_air_abs ', lw_internal%clear_air_abs(1,ilay1:ilay2))
      call print_vector('  veg_air_abs ', lw_internal%veg_air_abs(1,ilay1:ilay2))
      call print_vector('  veg_abs ', lw_internal%veg_abs(1,ilay1:ilay2))
      print *, '  ground_dn  = ', lw_internal%ground_dn(1,icol)
      print *, '  ground_net = ', lw_internal%ground_net(1,icol)
      call print_vector('  wall_in', lw_internal%wall_in(1,ilay1:ilay2))
      call print_vector('  wall_net', lw_internal%wall_net(1,ilay1:ilay2))
      print *, 'NORMALIZED FLUXES W.R.T. DIFFUSE INCOMING RADIATION'
      call print_vector('  clear_air_abs ', lw_norm%clear_air_abs(1,ilay1:ilay2))
      call print_vector('  veg_air_abs ', lw_norm%veg_air_abs(1,ilay1:ilay2))
      call print_vector('  veg_abs ', lw_norm%veg_abs(1,ilay1:ilay2))
      print *, '  ground_dn  = ', lw_norm%ground_dn(1,icol)
      print *, '  ground_net = ', lw_norm%ground_net(1,icol)
      call print_vector('  wall_in', lw_norm%wall_in(1,ilay1:ilay2))
      call print_vector('  wall_net', lw_norm%wall_net(1,ilay1:ilay2))
#endif

    end associate

    if (lhook) call dr_hook('radsurf_urban_lw:spartacus_urban_lw',1,hook_handle)

  end subroutine spartacus_urban_lw

end module radsurf_urban_lw
