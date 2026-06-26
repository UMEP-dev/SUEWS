! radsurf_forest_sw.F90 - SPARTACUS shortwave solver for forests
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

module radsurf_forest_sw

contains

  ! ------------------------------------------------------------------
  ! This routine implements the SPARTACUS shortwave radiative transfer
  ! algorithm for computing the propagation of solar radiation in a
  ! forest canopy or other vegetation.
  ! 
  ! Sections:
  !   1. Declare variables and arrays
  !   2. Prepare general variables and arrays
  !   3. First loop over layers
  !     3a. Prepare the properties of the current layer
  !     3b. Compute Gamma matrices
  !     3c. Compute reflection/transmission matrices for this layer
  !   4. Albedo of scene at each layer interface
  !   5. Compute normalized flux profile
  !
  subroutine spartacus_forest_sw(config, &
       &  nsw, ns, nreg, nlay, icol, ilay1, ilay2, &
       &  lg, cos_sza, &
       &  canopy_props, sw_spectral_props, &
       &  ground_albedo_diff, ground_albedo_dir, &
       &  top_albedo_diff, top_albedo_dir, &
       &  sw_norm_dir, sw_norm_diff)
    
    use parkind1,                   only : jpim, jprb
    use yomhook,                    only : lhook, dr_hook
    use radsurf_config,             only : config_type
    use radtool_legendre_gauss,     only : legendre_gauss_type
    use radsurf_canopy_properties,  only : canopy_properties_type
    use radsurf_sw_spectral_properties,only:sw_spectral_properties_type
    use radsurf_canopy_flux,        only : canopy_flux_type
    use radtool_calc_matrices_sw_eig,only: calc_matrices_sw_eig
    use radiation_constants,        only : Pi
    use radtool_matrix,             only : identity_minus_mat_x_mat, &
         &  mat_x_mat, singlemat_x_vec, mat_x_vec, rect_mat_x_vec, &
         &  solve_mat, rect_mat_x_mat, rect_expandedmat_x_mat, &
         &  rect_mat_x_expandedmat, rect_expandedmat_x_vec, solve_vec, &
         &  solve_rect_mat, rect_mat_x_singlemat
    use radsurf_overlap,            only : calc_overlap_matrices

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
    integer(kind=jpim),            intent(in)  :: nsw, nlay
    ! Index of current column and first and last layer of the current column
    integer(kind=jpim),            intent(in)  :: icol, ilay1, ilay2
    ! Number of regions, number of diffuse streams in each hemisphere
    integer(kind=jpim),            intent(in)  :: nreg, ns
    ! Legendre-Gauss coefficients
    type(legendre_gauss_type),     intent(in)  :: lg
    ! Cosine of the solar zenith angle
    real(kind=jprb),               intent(in)  :: cos_sza
    ! Geometric and other spectrally independent properties of the canopy
    type(canopy_properties_type),  intent(in)  :: canopy_props
    ! Spectral properties of the air and vegetation
    type(sw_spectral_properties_type),  intent(in)  :: sw_spectral_props
    ! Spectral albedo of the ground to diffuse and direct radiation
    real(kind=jprb), dimension(nsw),intent(in) :: ground_albedo_diff, &
         &                                        ground_albedo_dir

    ! Outputs

    ! Top-of-canopy spectral albedo to diffuse and direct radiation
    real(kind=jprb), dimension(nsw),intent(out):: top_albedo_diff, &
         &                                        top_albedo_dir
    ! Flux outputs
    type(canopy_flux_type), intent(inout), optional &
         &  :: sw_norm_dir, &  ! SW fluxes normalized by top-of-canopy direct
         &     sw_norm_diff    ! SW fluxes normalized by top-of-canopy diffuse

    ! Local variables and arrays

    ! Transmittance and reflectance of a layer to diffuse radiation
    real(kind=jprb), dimension(nsw,nreg*ns,nreg*ns,nlay) &
         &  :: trans_diff, ref_diff
    ! Reflectance of a layer to direct radiation, and fraction of
    ! direct radiation that is scattered and passes out through the
    ! base of the layer
    real(kind=jprb), dimension(nsw,nreg*ns,nreg,nlay) &
         &   :: ref_dir, trans_dir_diff
    ! Direct (unscattered) transmittance
    real(kind=jprb), dimension(nsw,nreg,nreg,nlay) :: trans_dir_dir

    ! Area fraction of each region in each layer, plus a pseudo-layer
    ! at the end representing the free atmosphere above
    real(kind=jprb), dimension(nreg,nlay+1) :: frac
    
    ! Components of the Gamma matrices
    real(kind=jprb), dimension(nsw,nreg,nreg)       :: gamma0
    real(kind=jprb), dimension(nsw,nreg*ns,nreg*ns) :: gamma1, gamma2
    real(kind=jprb), dimension(nsw,nreg*ns,nreg)    :: gamma3

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

    ! Tangent of solar zenith angle
    real(kind=jprb) :: tan0

    ! Rate of exchange between regions, excluding the tangent term,
    ! where the dimensions are in the sense of
    ! f_exchange(region_to,region_from)
    real(kind=jprb) :: f_exchange(nreg,nreg)

    ! Extinction (m-1) and single scattering albedo of each region
    real(kind=jprb) :: ext_reg(nsw,nreg), ssa_reg(nsw,nreg)

    ! Optical depth scaling of vegetation optical depth to represent
    ! inhomogeneity
    real(kind=jprb) :: od_scaling(2:nreg,nlay)

    ! Diffuse and direct albedo matrices just above and below each
    ! interface. The "above" albedos include surface (interface=1) and
    ! top-of-canopy (interface=nlay+1) so have one element more than
    ! the number of layers. The "below" albedos use the same indexing
    ! of interfaces, but the surface is never used so this dimension
    ! starts at 2.
    real(kind=jprb) :: a_above(nsw,nreg*ns,nreg*ns,nlay+1) ! Diffuse
    real(kind=jprb) :: d_above(nsw,nreg*ns,nreg,nlay+1)    ! Direct
    real(kind=jprb) :: a_below(nsw,nreg*ns,nreg*ns,2:nlay+1)
    real(kind=jprb) :: d_below(nsw,nreg*ns,nreg,2:nlay+1)

    ! Denominator matrix used in the matrix adding method
    real(kind=jprb) :: denominator(nsw,nreg*ns,nreg*ns,nlay)

    ! Directional overlap matrices expressing how the fluxes in each
    ! region of one layer pass into the layer above (u_overlap) or
    ! below (v_overlap)
    real(kind=jprb), dimension(nreg,nreg,nlay+1) :: u_overlap, v_overlap

    ! Fluxes just above or just below a layer interface, normalized by
    ! either the downwelling diffuse or downwelling direct flux at
    ! canopy top
    real(kind=jprb), dimension(nsw,nreg*ns) &
         &  :: flux_dn_diff_above, flux_up_above, &
         &     flux_dn_diff_below, flux_up_below
    real(kind=jprb), dimension(nsw,nreg) &
         &  :: flux_dn_dir_above, flux_dn_dir_below
    ! Reflected direct flux just above a layer interface, used as an
    ! intermediate variable in the final part of the algorithm
    real(kind=jprb), dimension(nsw,nreg*ns) :: flux_reflected_dir
    
    ! The following matrices are defined such that the integrated
    ! diffuse flux (u_hat+v_hat in Eq. 29) is
    !   int_diff * (u_conv+v_conv) + int_dir_diff * s_conv
    ! where u_conv+v_conv is the convergence of diffuse fluxes into
    ! the layer, i.e. the sum of the fluxes entering the layer from
    ! the top or base, minus the fluxes leaving the layer from top or
    ! base, and s_conv is the convergence of direct fluxes into the
    ! layer, i.e. the direct flux at layer top minus the direct flux
    ! at layer base. Likewise, the integrated direct flux is 
    !   int_diff * s_conv
    real(kind=jprb) &
         &  :: int_diff(nsw,nreg*ns,nreg*ns,nlay), &
         &     int_dir(nsw,nreg,nreg,nlay), &
         &     int_dir_diff(nsw,nreg*ns,nreg,nlay)

    ! Integrated diffuse and direct fluxes across a layer
    real(kind=jprb) :: int_flux_diff(nsw,nreg*ns), int_flux_dir(nsw,nreg)

    ! Loop counters
    integer(kind=jpim) :: jlay, jreg, jreg_fr, jreg_to, js, js_fr, js_to

    ! Index to layer inside canopy_flux object
    integer(kind=jpim) :: ilay

    ! Index to the matrix dimension expressing regions "from" and "to"
    ! in combination with a particular stream
    integer(kind=jpim) :: ifr, ito
    
    real(jprb) :: hook_handle

    if (lhook) call dr_hook('radsurf_forest_sw:spartacus_forest_sw',0,hook_handle)

    associate( &
         &  dz           => canopy_props%dz(ilay1:ilay2), &
         &  veg_fraction => canopy_props%veg_fraction(ilay1:ilay2), &
         &  veg_scale    => canopy_props%veg_scale(ilay1:ilay2), &
         &  veg_ext      => canopy_props%veg_ext(ilay1:ilay2), &
         &  veg_fsd      => canopy_props%veg_fsd(ilay1:ilay2), &
         &  air_ext      => sw_spectral_props%air_ext(:,ilay1:ilay2), &
         &  air_ssa      => sw_spectral_props%air_ssa(:,ilay1:ilay2), &
         &  veg_ssa      => sw_spectral_props%veg_ssa(:,ilay1:ilay2))

      ! --------------------------------------------------------
      ! Section 2: Prepare general variables and arrays
      ! --------------------------------------------------------
      
      ! Tangent of solar zenith angle
      tan0 = sqrt(1.0_jprb - cos_sza*cos_sza) / max(cos_sza,1.0e-6_jprb)

      ! Set the area fraction of each region
      frac(1,1:nlay)  = 1.0_jprb - veg_fraction
      frac(2:,1:nlay) = spread(veg_fraction,1,nreg-1) / real(nreg-1,jprb)
      ! Free atmosphere
      frac(1,nlay+1)  = 1.0_jprb
      frac(2:,nlay+1) = 0.0_jprb

      ! Compute overlap matrices
      call calc_overlap_matrices(nlay,nreg,frac,u_overlap,v_overlap, &
           &  config%min_vegetation_fraction);
      
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
        ext_reg(:,1) = air_ext(:,jlay)
        ssa_reg(:,1) = air_ssa(:,jlay)
        if (nreg == 2) then
          ext_reg(:,2) = air_ext(:,jlay) + veg_ext(jlay)
          ssa_reg(:,2) = (ext_reg(:,1)*ssa_reg(:,1) + veg_ext(jlay)*veg_ssa(:,jlay)) &
               &       / max(ext_reg(:,2), 1.0e-8_jprb)
          od_scaling(2,jlay) = 1.0_jprb
        else if (nreg == 3) then
          ! Approximate method to approximate a Gamma distribution
          od_scaling(2,jlay) = exp(-veg_fsd(jlay)*(1.0_jprb + 0.5_jprb*veg_fsd(jlay) &
               &                            *(1.0_jprb + 0.5_jprb*veg_fsd(jlay))))
          od_scaling(3,jlay) = 2.0_jprb - od_scaling(2,jlay)
          ext_reg(:,2) = air_ext(:,jlay) + od_scaling(2,jlay)*veg_ext(jlay)
          ext_reg(:,3) = air_ext(:,jlay) + od_scaling(3,jlay)*veg_ext(jlay)
          ssa_reg(:,2) = (ext_reg(:,1)*ssa_reg(:,1) &
               &          + od_scaling(2,jlay)*veg_ext(jlay)*veg_ssa(:,jlay)) &
               &       / max(ext_reg(:,2), 1.0e-8_jprb)
          ssa_reg(:,3) = (ext_reg(:,1)*ssa_reg(:,1) &
               &          + od_scaling(3,jlay)*veg_ext(jlay)*veg_ssa(:,jlay)) &
               &       / max(ext_reg(:,3), 1.0e-8_jprb)
        end if
        
        ! Compute the normalized vegetation perimeter length
        if (veg_fraction(jlay) > config%min_vegetation_fraction) then
          if (config%use_symmetric_vegetation_scale_forest) then
            norm_perim(1) = 4.0_jprb * veg_fraction(jlay) * (1.0_jprb - veg_fraction(jlay)) &
                 &        / veg_scale(jlay)
          else
            norm_perim(1) = 4.0_jprb * veg_fraction(jlay) / veg_scale(jlay)
          end if

          if (nreg > 2) then
            ! Share the clear-air/vegetation perimeter between the two
            ! vegetated regions
            norm_perim(nreg) = config%vegetation_isolation_factor_forest * norm_perim(1)
            norm_perim(1) = (1.0_jprb - config%vegetation_isolation_factor_forest) &
                 &          * norm_perim(1) 
            ! We assume that the horizontal scale of the vegetation
            ! inhomogeneities is the same as the scale of the tree
            ! crowns themselves. Therefore, to compute the interface
            ! between the two vegetated regions, we use the same
            ! formula as before but with the fraction associated with
            ! one of the two vegetated regions, which is half the
            ! total vegetation fraction.
            if (config%use_symmetric_vegetation_scale_forest) then
              norm_perim(2) = (1.0_jprb - config%vegetation_isolation_factor_forest) &
                   &  * 4.0_jprb * (0.5_jprb*veg_fraction(jlay)) &
                   &  * (1.0_jprb - (0.5_jprb*veg_fraction(jlay))) &
                   &  / veg_scale(jlay)
            else
              !            norm_perim(2) = (1.0_jprb - config%vegetation_isolation_factor_forest) &
              !                 &  * 4.0_jprb * (0.5_jprb*veg_fraction(jlay)) / veg_scale(jlay)
              ! Lollipop model - see Hogan, Quaife and Braghiere (2018) explaining sqrt(2)
              norm_perim(2) = (1.0_jprb - config%vegetation_isolation_factor_forest) &
                   &  * 4.0_jprb * veg_fraction(jlay) / (sqrt(2.0_jprb)*veg_scale(jlay))
            end if
          else
            ! Only one vegetated region so the other column of norm_perim
            ! is unused
            norm_perim(2:) = 0.0_jprb
          end if

        else
          ! No vegetation so no perimeters
          norm_perim = 0.0_jprb
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
        
        ! --------------------------------------------------------
        ! Section 3b: Compute Gamma matrices
        ! --------------------------------------------------------
        ! Compute the Gamma matrices representing exchange of direct
        ! and diffuse radiation between regions (gamma0 and gamma1
        ! respectively)
        gamma0 = 0.0_jprb
        gamma1 = 0.0_jprb
        do jreg_fr = 1,nreg
          do jreg_to = 1,nreg
            if (jreg_fr /= jreg_to) then
              gamma0(:,jreg_fr,jreg_fr) = gamma0(:,jreg_fr,jreg_fr) &
                   &  - tan0 * f_exchange(jreg_to,jreg_fr)
              gamma0(:,jreg_to,jreg_fr) = + tan0 * f_exchange(jreg_to,jreg_fr)
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
        
        ! Diagonal elements of gamma0 and gamma1 also have loss term
        ! due to extinction through the regions
        do jreg = 1,nreg
          gamma0(:,jreg,jreg) = gamma0(:,jreg,jreg) - ext_reg(:,jreg)/cos_sza
          do js = 1,ns
            ifr = js + (jreg-1)*ns
            gamma1(:,ifr,ifr) = gamma1(:,ifr,ifr) - ext_reg(:,jreg)/lg%mu(js)
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
              gamma2(:,ito,ifr) = 0.5_jprb * lg%weight(js_to) &
                   &  * ext_reg(:,jreg) * ssa_reg(:,jreg) / lg%mu(js_fr)
            end do
          end do
        end do

        ! Gamma1 also contains gain of diffuse radiation by
        ! scattering, so add Gamma2 to it
        gamma1 = gamma1 + gamma2

        ! Compute Gamma3, representing scattering from the direct to
        ! the diffuse streams
        gamma3 = 0.0_jprb
        do jreg = 1,nreg
          do js = 1,ns
            ito = js + (jreg-1)*ns
            ! Note that the cosine of the solar zenith angle in Eq. 13
            ! of Hogan (2019) cancels with the one in Eq. 11.
            gamma3(:,ito,jreg) = 0.5_jprb * lg%weight(js) &
                 &             * ext_reg(:,jreg) * ssa_reg(:,jreg)
          end do
        end do

#ifdef PRINT_ARRAYS
        print *, 'PROPERTIES OF LAYER ', jlay
        call print_vector('ext_reg',ext_reg(1,:))
        call print_vector('ssa_reg',ssa_reg(1,:))
        call print_vector('veg_fraction',veg_fraction)
        call print_vector('veg_scale', veg_scale);
        call print_matrix('frac', frac);
        call print_vector('norm_perim', norm_perim)
        call print_matrix('f_exchange',f_exchange)
        call print_vector('tan_ang',lg%tan_ang)
        call print_matrix('gamma0',gamma0(1,:,:))
        call print_matrix('gamma1',gamma1(1,:,:))
        call print_matrix('gamma2',gamma2(1,:,:))
        call print_matrix('gamma3',gamma3(1,:,:))
#endif        

        ! --------------------------------------------------------
        ! Section 3c: Compute reflection/transmission matrices for this layer
        ! --------------------------------------------------------
        if (veg_fraction(jlay) <= config%min_vegetation_fraction) then
          ! Vegetation-free calculation: first set all coefficients to zero...
          ref_diff(:,:,:,jlay) = 0.0_jprb
          trans_diff(:,:,:,jlay) = 0.0_jprb
          ref_dir(:,:,:,jlay) = 0.0_jprb
          trans_dir_diff(:,:,:,jlay) = 0.0_jprb
          trans_dir_dir(:,:,:,jlay) = 0.0_jprb
          int_dir(:,:,:,jlay) = 0.0_jprb
          int_diff(:,:,:,jlay) = 0.0_jprb
          int_dir_diff(:,:,:,jlay) = 0.0_jprb
          ! ...then perform calculations only for the clear-sky region
          call calc_matrices_sw_eig(nsw, ns, 1, dz(jlay), cos_sza, &
               &  gamma0(:,1:1,1:1), gamma1(:,1:ns,1:ns), gamma2(:,1:ns,1:ns), gamma3(:,1:ns,1:1), &
               &  ref_diff(:,1:ns,1:ns,jlay), trans_diff(:,1:ns,1:ns,jlay), &
               &  ref_dir(:,1:ns,1:1,jlay), trans_dir_diff(:,1:ns,1:1,jlay), &
               &  trans_dir_dir(:,1:1,1:1,jlay), &
               &  int_dir(:,1:1,1:1,jlay), int_diff(:,1:ns,1:ns,jlay), &
               &  int_dir_diff(:,1:ns,1:1,jlay))
        else if (frac(1,jlay) <= config%min_vegetation_fraction) then
          ! Vegetation fills the domain: set all coefficients to zero...
          ref_diff(:,:,:,jlay) = 0.0_jprb
          trans_diff(:,:,:,jlay) = 0.0_jprb
          ref_dir(:,:,:,jlay) = 0.0_jprb
          trans_dir_diff(:,:,:,jlay) = 0.0_jprb
          trans_dir_dir(:,:,:,jlay) = 0.0_jprb
          int_dir(:,:,:,jlay) = 0.0_jprb
          int_diff(:,:,:,jlay) = 0.0_jprb
          int_dir_diff(:,:,:,jlay) = 0.0_jprb
          ! ...then perform calculations only for the vegetated regions
          call calc_matrices_sw_eig(nsw, ns*(nreg-1), nreg-1, dz(jlay), cos_sza, &
               &  gamma0(:,2:nreg,2:nreg), gamma1(:,ns+1:nreg*ns,ns+1:nreg*ns), &
               &  gamma2(:,ns+1:nreg*ns,ns+1:nreg*ns), gamma3(:,ns+1:nreg*ns,2:nreg), &
               &  ref_diff(:,ns+1:nreg*ns,ns+1:nreg*ns,jlay), &
               &  trans_diff(:,ns+1:nreg*ns,ns+1:nreg*ns,jlay), &
               &  ref_dir(:,ns+1:nreg*ns,2:nreg,jlay), &
               &  trans_dir_diff(:,ns+1:nreg*ns,2:nreg,jlay), &
               &  trans_dir_dir(:,2:nreg,2:nreg,jlay), &
               &  int_dir(:,2:nreg,2:nreg,jlay), int_diff(:,ns+1:nreg*ns,ns+1:nreg*ns,jlay), &
               &  int_dir_diff(:,ns+1:nreg*ns,2:nreg,jlay))
        else
          call calc_matrices_sw_eig(nsw, nreg*ns, nreg, dz(jlay), cos_sza, &
               &  gamma0, gamma1, gamma2, gamma3, &
               &  ref_diff(:,:,:,jlay), trans_diff(:,:,:,jlay), &
               &  ref_dir(:,:,:,jlay), trans_dir_diff(:,:,:,jlay), &
               &  trans_dir_dir(:,:,:,jlay), &
               &  int_dir(:,:,:,jlay), int_diff(:,:,:,jlay), &
               &  int_dir_diff(:,:,:,jlay))
        end if

      end do ! Loop over layers to compute reflectance/transmittance matrices

      ! --------------------------------------------------------
      ! Section 4: Albedo of scene at each layer interface
      ! --------------------------------------------------------
      ! Set the surface albedo
      a_above = 0.0_jprb
      d_above = 0.0_jprb
      do jreg = 1,nreg
        do js_to = 1,ns
          d_above(:,js_to+(jreg-1)*ns,jreg,1) &
               &  = cos_sza * ground_albedo_dir * lg%hweight(js_to)
          do js_fr = 1,ns
            a_above(:,js_to+(jreg-1)*ns,js_fr+(jreg-1)*ns,1) &
                 &  = ground_albedo_diff * lg%hweight(js_to)
          end do
        end do
      end do

      ! Loop up through the half levels / interfaces computing albedos
      a_below = 0.0_jprb
      d_below = 0.0_jprb
      do jlay = 1,nlay
        ! Adding method
        denominator(:,:,:,jlay) = identity_minus_mat_x_mat(nsw,nsw,nreg*ns, &
             &  a_above(:,:,:,jlay), ref_diff(:,:,:,jlay))
        a_below(:,:,:,jlay+1) = ref_diff(:,:,:,jlay) &
             &  + mat_x_mat(nsw,nsw,nreg*ns,trans_diff(:,:,:,jlay), &
             &  solve_mat(nsw,nsw,nreg*ns,denominator(:,:,:,jlay), &
             &  mat_x_mat(nsw,nsw,nreg*ns,a_above(:,:,:,jlay), &
             &  trans_diff(:,:,:,jlay))))
        d_below(:,:,:,jlay+1) = ref_dir(:,:,:,jlay) &
             &  + rect_mat_x_mat(nsw,nreg*ns,nreg*ns,nreg,trans_diff(:,:,:,jlay), &
             &  solve_rect_mat(nsw,nreg*ns,nreg,denominator(:,:,:,jlay), &
             &    rect_mat_x_mat(nsw,nreg*ns,nreg,nreg,d_above(:,:,:,jlay), &
             &                       trans_dir_dir(:,:,:,jlay)) &
             &   +rect_mat_x_mat(nsw,nreg*ns,nreg*ns,nreg,a_above(:,:,:,jlay), &
             &                       trans_dir_diff(:,:,:,jlay))))
        ! Overlap: Hogan (2019), equations 22 and 23
        a_above(:,:,:,jlay+1) = rect_expandedmat_x_mat(nsw,nreg,nreg,ns,nreg*ns, &
             &  u_overlap(:,:,jlay+1), &
             &  rect_mat_x_expandedmat(nsw,nreg,nreg,ns,nreg*ns, a_below(:,:,:,jlay+1), &
             &                          v_overlap(:,:,jlay+1)))
        d_above(:,:,:,jlay+1) = rect_expandedmat_x_mat(nsw,nreg,nreg,ns,nreg, &
             &  u_overlap(:,:,jlay+1), &
             &  rect_mat_x_singlemat(nsw,nreg*ns,nreg,nreg,d_below(:,:,:,jlay+1), &
             &                 v_overlap(:,:,jlay+1)))
      end do ! Loop over layers for upward pass to compute albedos

#ifdef PRINT_ARRAYS
      call print_array3('a_above', a_above(1,:,:,:))
      call print_array3('d_above', d_above(1,:,:,:))
      call print_array3('a_below', a_below(1,:,:,:))
      call print_array3('d_below', d_below(1,:,:,:))
      call print_array3('T', trans_diff(1,:,:,:))
      call print_array3('R', ref_diff(1,:,:,:))
      call print_array3('Sup', ref_dir(1,:,:,:))
      call print_array3('Sdn', trans_dir_diff(1,:,:,:))
      call print_array3('Ess', trans_dir_dir(1,:,:,:))
      call print_array3('u_overlap',u_overlap)
      call print_array3('v_overlap',v_overlap)
#endif

      ! Store top-of-canopy boundary conditions
      top_albedo_diff = 0.0_jprb
      ! Diffuse isotropic albedo is weighted average over the ns
      ! streams, noting that just above the "nlay+1" interface we are
      ! above the canopy so only need to consider the clear-sky region
      ! (indexed 1:ns).
      top_albedo_diff = sum(mat_x_vec(nsw,nsw,ns,a_above(:,1:ns,1:ns,nlay+1), &
           &                    spread(lg%hweight,1,nsw)),2)
      top_albedo_dir = sum(d_above(:,1:ns,1,nlay+1),2) / cos_sza

      ! --------------------------------------------------------
      ! Section 5: Compute normalized flux profile
      ! --------------------------------------------------------

      ! Set to the flux components to zero initially
      call sw_norm_diff%zero(icol, ilay1, ilay2)
      call sw_norm_dir%zero( icol, ilay1, ilay2)

      ! First the fluxes normalized by the direct downwelling flux at
      ! canopy top.

      ! Initial normalized direct flux is therefore 1 in each spectral
      ! interval
      flux_dn_dir_above(:,1)  = 1.0_jprb / cos_sza
      flux_dn_dir_above(:,2:) = 0.0_jprb
      flux_dn_diff_above      = 0.0_jprb
      
      sw_norm_dir%top_dn_dir(:,icol) = 1.0_jprb !flux_dn_dir_above(:,1)
      sw_norm_dir%top_dn(:,icol)        = sw_norm_dir%top_dn_dir(:,icol)
      sw_norm_dir%top_net(:,icol)       = sw_norm_dir%top_dn_dir(:,icol) &
           &                            * (1.0_jprb-top_albedo_dir)

      ! Loop down through layers
      do jlay = nlay,1,-1
        ! Translate the downwelling flux component across the
        ! interface at the top of the layer
        flux_dn_dir_below = singlemat_x_vec(nsw,nsw,nreg, &
             &  v_overlap(:,:,jlay+1), flux_dn_dir_above)
        flux_dn_diff_below = rect_expandedmat_x_vec(nsw,nreg,nreg,ns, &
             &  v_overlap(:,:,jlay+1), flux_dn_diff_above)
        flux_up_below = mat_x_vec(nsw,nsw,nreg*ns,a_below(:,:,:,jlay+1),flux_dn_diff_below) &
             &  + rect_mat_x_vec(nsw,nreg*ns,nreg,d_below(:,:,:,jlay+1),flux_dn_dir_below)
        ! Compute fluxes at base of layer
        flux_dn_dir_above = mat_x_vec(nsw,nsw,nreg, &
             &  trans_dir_dir(:,:,:,jlay), flux_dn_dir_below)

        flux_reflected_dir = rect_mat_x_vec(nsw,nreg*ns,nreg, &
             &  d_above(:,:,:,jlay), flux_dn_dir_above)

        flux_dn_diff_above = solve_vec(nsw,nsw,nreg*ns,denominator(:,:,:,jlay), &
             &  mat_x_vec(nsw,nsw,nreg*ns,trans_diff(:,:,:,jlay),flux_dn_diff_below) &
             &  + mat_x_vec(nsw,nsw,nreg*ns,ref_diff(:,:,:,jlay),flux_reflected_dir) &
             &  + rect_mat_x_vec(nsw,nreg*ns,nreg,trans_dir_diff(:,:,:,jlay), &
             &                   flux_dn_dir_below))
        flux_up_above = mat_x_vec(nsw,nsw,nreg*ns,a_above(:,:,:,jlay), &
             &  flux_dn_diff_above) + flux_reflected_dir

        ! Compute integrated flux vectors, recalling that _above means
        ! above the just above the *base* of the layer, and _below
        ! means just below the *top* of the layer
        int_flux_dir = mat_x_vec(nsw,nsw,nreg,int_dir(:,:,:,jlay), &
             &                   flux_dn_dir_below - flux_dn_dir_above)
        int_flux_diff= mat_x_vec(nsw,nsw,nreg*ns,int_diff(:,:,:,jlay), flux_dn_diff_below &
             &                   - flux_dn_diff_above - flux_up_below + flux_up_above) &
             &  + rect_mat_x_vec(nsw,nreg*ns,nreg,int_dir_diff(:,:,:,jlay), &
             &                   flux_dn_dir_below - flux_dn_dir_above)

        ilay = ilay1 + jlay - 1
        ! Absorption by clear-air region - see Eqs. 29 and 30
        sw_norm_dir%clear_air_abs(:,ilay) = sw_norm_dir%clear_air_abs(:,ilay) &
             &  + air_ext(:,jlay)*(1.0_jprb-air_ssa(:,jlay)) &
             &    * (int_flux_dir(:,1) & ! / cos_sza &
             &       + sum(int_flux_diff(:,1:ns) * spread(1.0_jprb/lg%mu,1,nsw), 2))
        do jreg = 2,nreg
          ! Absorption by clear-air in the vegetated regions
          sw_norm_dir%veg_air_abs(:,ilay) = sw_norm_dir%veg_air_abs(:,ilay) &
               &  + air_ext(:,jlay)*(1.0_jprb-air_ssa(:,jlay)) & ! Use clear-air properties
               &    * (int_flux_dir(:,jreg) & ! / cos_sza &
               &       + sum(int_flux_diff(:,(jreg-1)*ns+1:jreg*ns) &
               &             * spread(1.0_jprb/lg%mu,1,nsw), 2))
          sw_norm_dir%veg_abs(:,ilay) = sw_norm_dir%veg_abs(:,ilay) &
               &  + veg_ext(jlay)*(1.0_jprb-veg_ssa(:,jlay)) & ! Use vegetation properties
               &    * (int_flux_dir(:,jreg) & ! / cos_sza &
               &       + sum(int_flux_diff(:,(jreg-1)*ns+1:jreg*ns) &
               &             * spread(1.0_jprb/lg%mu,1,nsw), 2)) * od_scaling(jreg,jlay)
        end do

#ifdef PRINT_ARRAYS
        print *, 'NORMALIZED FLUXES W.R.T. DIRECT INCOMING RADIATION AT LAYER ', jlay
        call print_vector('  flux_dn_dir_below ', flux_dn_dir_below(1,:))
        call print_vector('  flux_dn_diff_below ', flux_dn_diff_below(1,:))
        call print_vector('  flux_dn_dir_above ', flux_dn_dir_above(1,:))
        call print_vector('  flux_dn_diff_above ', flux_dn_diff_above(1,:))
        call print_vector('  flux_up_above ', flux_up_above(1,:))
#endif

      end do
      sw_norm_dir%ground_dn_dir(:,icol) = cos_sza * sum(flux_dn_dir_above,2)
      sw_norm_dir%ground_dn(:,icol) = sw_norm_dir%ground_dn_dir(:,icol) &
           &  + sum(flux_dn_diff_above,2)
      sw_norm_dir%ground_net(:,icol) = sw_norm_dir%ground_dn(:,icol) &
           &  - sum(flux_up_above,2)
      do jreg = 1,nreg
        do js = 1,ns
          ifr = js + (jreg-1)*ns
          ! Project each stream into a horizontal plane
          sw_norm_dir%ground_vertical_diff(:,icol) = sw_norm_dir%ground_vertical_diff(:,icol) &
               &  + (flux_dn_diff_above(:,ifr) +flux_up_above(:,ifr)) * lg%tan_ang(js)/Pi
        end do
      end do

      ! Second the fluxes normalized by the diffuse downwelling flux
      ! at canopy top.

      ! Initial normalized diffuse flux sums to 1 in each stream, in
      ! each spectral interval, so use the Legendre-Gauss horizontal
      ! weights
      flux_dn_dir_above          = 0.0_jprb ! No direct calculation now needed below
      flux_dn_diff_above         = 0.0_jprb
      flux_dn_diff_above(:,1:ns) = spread(lg%hweight,nsw,1)

      sw_norm_diff%top_dn_dir(:,icol) = 0.0_jprb
      sw_norm_diff%top_dn(:,icol)        = 1.0_jprb
      sw_norm_diff%top_net(:,icol)       = 1.0_jprb-top_albedo_diff
      
      ! Loop down through layers
      do jlay = nlay,1,-1
        ! Translate the downwelling flux component across the
        ! interface at the top of the layer
        flux_dn_diff_below = rect_expandedmat_x_vec(nsw,nreg,nreg,ns, &
             &  v_overlap(:,:,jlay+1), flux_dn_diff_above)
        flux_up_below = mat_x_vec(nsw,nsw,nreg*ns,a_below(:,:,:,jlay+1),flux_dn_diff_below) 
        ! Compute fluxes at base of layer
        flux_dn_diff_above = solve_vec(nsw,nsw,nreg*ns,denominator(:,:,:,jlay), &
             &  mat_x_vec(nsw,nsw,nreg*ns,trans_diff(:,:,:,jlay),flux_dn_diff_below))
        flux_up_above = mat_x_vec(nsw,nsw,nreg*ns,a_above(:,:,:,jlay), &
             &  flux_dn_diff_above)

        ! Compute integrated flux vectors, recalling that _above means
        ! above the just above the *base* of the layer, and _below
        ! means just below the *top* of the layer
        int_flux_diff= mat_x_vec(nsw,nsw,nreg*ns,int_diff(:,:,:,jlay), flux_dn_diff_below &
             &                   - flux_dn_diff_above - flux_up_below + flux_up_above)

        ilay = ilay1 + jlay - 1
        ! Absorption by clear-air region - see Eqs. 29 and 30
        sw_norm_diff%clear_air_abs(:,ilay) = sw_norm_diff%clear_air_abs(:,ilay) &
             &  + air_ext(:,jlay)*(1.0_jprb-air_ssa(:,jlay)) &
             &    * sum(int_flux_diff(:,1:ns) * spread(1.0_jprb/lg%mu,1,nsw), 2)
        do jreg = 2,nreg
          ! Absorption by clear-air in the vegetated regions
          sw_norm_diff%veg_air_abs(:,ilay) = sw_norm_diff%veg_air_abs(:,ilay) &
               &  + air_ext(:,jlay)*(1.0_jprb-air_ssa(:,jlay)) & ! Use clear-air properties
               &    * sum(int_flux_diff(:,(jreg-1)*ns+1:jreg*ns) &
               &             * spread(1.0_jprb/lg%mu,1,nsw), 2)
          sw_norm_diff%veg_abs(:,ilay) = sw_norm_diff%veg_abs(:,ilay) &
               &  + veg_ext(jlay)*(1.0_jprb-veg_ssa(:,jlay)) & ! Use vegetation properties
               &    * sum(int_flux_diff(:,(jreg-1)*ns+1:jreg*ns) &
               &             * spread(1.0_jprb/lg%mu,1,nsw), 2) * od_scaling(jreg,jlay)
        end do

#ifdef PRINT_ARRAYS
        print *, 'NORMALIZED FLUXES W.R.T. DIFFUSE INCOMING RADIATION AT LAYER ', jlay
        call print_vector('  flux_dn_diff_below ', flux_dn_diff_below(1,:))
        call print_vector('  flux_dn_diff_above ', flux_dn_diff_above(1,:))
        call print_vector('  flux_up_above ', flux_up_above(1,:))
#endif

      end do
      sw_norm_diff%ground_dn_dir(:,icol) = 0.0_jprb
      sw_norm_diff%ground_dn(:,icol) = sum(flux_dn_diff_above,2)
      sw_norm_diff%ground_net(:,icol) = sw_norm_diff%ground_dn(:,icol) &
           &  - sum(flux_up_above,2)
      do jreg = 1,nreg
        do js = 1,ns
          ifr = js + (jreg-1)*ns
          ! Project each stream into a horizontal plane
          sw_norm_diff%ground_vertical_diff(:,icol) = sw_norm_diff%ground_vertical_diff(:,icol) &
               &  + (flux_dn_diff_above(:,ifr) +flux_up_above(:,ifr)) * lg%tan_ang(js)/Pi
        end do
      end do
    
#ifdef PRINT_ARRAYS
      print *, 'NORMALIZED FLUXES W.R.T. DIRECT INCOMING RADIATION'
      call print_vector('  clear_air_abs ', sw_norm_dir%clear_air_abs(1,:))
      call print_vector('  veg_air_abs ', sw_norm_dir%veg_air_abs(1,:))
      call print_vector('  veg_abs ', sw_norm_dir%veg_abs(1,:))
      call print_vector('  ground_dn', sw_norm_dir%ground_dn(1,:))
      call print_vector('  ground_net', sw_norm_dir%ground_net(1,:))
      call print_vector('  ground_dn_dir', sw_norm_dir%ground_dn_dir(1,:))
      print *, 'NORMALIZED FLUXES W.R.T. DIFFUSE INCOMING RADIATION'
      call print_vector('  clear_air_abs ', sw_norm_diff%clear_air_abs(1,:))
      call print_vector('  veg_air_abs ', sw_norm_diff%veg_air_abs(1,:))
      call print_vector('  veg_abs ', sw_norm_diff%veg_abs(1,:))
      call print_vector('  ground_dn', sw_norm_diff%ground_dn(1,:))
      call print_vector('  ground_net', sw_norm_diff%ground_net(1,:))
      call print_vector('  ground_dn_dir', sw_norm_diff%ground_dn_dir(1,:))
#endif

    end associate

    if (lhook) call dr_hook('radsurf_forest_sw:spartacus_forest_sw',1,hook_handle)

  end subroutine spartacus_forest_sw

end module radsurf_forest_sw
