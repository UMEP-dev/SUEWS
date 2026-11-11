! radsurf_overlap.F90 - Compute overlap matrices for vegetation
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

module radsurf_overlap

contains

  !---------------------------------------------------------------------
  ! Calculate overlap matrix O, defined in Eq. 15 of Shonk et
  ! al. (2010),
  ! https://rmets.onlinelibrary.wiley.com/doi/epdf/10.1002/qj.647,
  ! assuming maximum-random overlap. The regions correspond to (1)
  ! clear and (2+) vegetated with different optical depth, and if
  ! there is more than one vegetated region then they all have the
  ! same fractional cover.
  pure function calc_overlap_matrix_max_ran(nreg, &
       &  f_upper, f_lower) result(overlap_matrix)

    use parkind1, only : jprb

    implicit none
    
    integer, intent(in) :: nreg ! Number of regions, must be 2 or 3

    ! Vegetation fraction in the upper and lower layers
    real(jprb), intent(in) :: f_upper, f_lower

    ! Output overlap matrix
    real(jprb) :: overlap_matrix(nreg,nreg)

    ! Combined vegetation cover of pair of layers
    real(jprb) :: pair_cover

    pair_cover = max(f_upper,f_lower)

    ! Clear in both layers
    overlap_matrix(1,1) = 1.0_jprb - pair_cover
    if (nreg == 2) then
      ! Clear in upper layer, vegetation in lower layer
      overlap_matrix(1,2) = pair_cover - f_upper
      ! Clear in lower layer, vegetation in upper layer
      overlap_matrix(2,1) = pair_cover - f_lower
      ! Vegetation in both layers
      overlap_matrix(2,2) = f_upper + f_lower - pair_cover
    else
      ! The following assumes that the two vegetated regions are of
      ! equal area.
      ! Clear in upper layer, vegetated in lower layer
      overlap_matrix(1,2) = 0.5_jprb * (pair_cover - f_upper)
      overlap_matrix(1,3) = overlap_matrix(1,2)
      ! Clear in lower layer, vegetated in upper layer
      overlap_matrix(2,1) = 0.5_jprb * (pair_cover - f_lower)
      overlap_matrix(3,1) = overlap_matrix(2,1)
      ! Vegetated in both layers
      overlap_matrix(2,2) = 0.5_jprb * (f_upper + f_lower - pair_cover)
      overlap_matrix(3,3) = overlap_matrix(2,2)
      overlap_matrix(2,3) = 0.0_jprb
      overlap_matrix(3,2) = 0.0_jprb
    end if

  end function calc_overlap_matrix_max_ran


  !---------------------------------------------------------------------
  ! Compute the upward and downward overlap matrices u_overlap and
  ! v_overlap, respectively, where u_overlap is defined such that
  ! y=u_overlap*x, where x is a vector of upwelling fluxes in each
  ! region just below an interface, and y is a vector of upwelling
  ! fluxes in each region just above that interface. For nlay model
  ! layers there are nlay+1 interfaces including the ground and
  ! top-of-atmosphere, and so that is one of the dimensions of
  ! u_overlap and v_overlap.
  subroutine calc_overlap_matrices(nlay,nreg, &
       &     region_fracs, u_overlap, v_overlap, &
       &     frac_threshold)

    use parkind1,     only : jprb
    use yomhook,      only : lhook, dr_hook

    implicit none
    
    ! Number of layers and regions
    integer,  intent(in) :: nlay, nreg

    ! Area fraction of each region: region 1 is clear sky, and 2+ are
    ! the vegetated regions (only one or two vegetated regions are
    ! supported)
    real(jprb), intent(in), dimension(1:nreg,nlay)  :: region_fracs

    ! Output overlap matrices, one for each layer interface
    real(jprb), intent(out), dimension(nreg,nreg,nlay+1) &
         &  :: u_overlap, v_overlap

    ! Regions smaller than this are ignored
    real(jprb), intent(in), optional :: frac_threshold

    ! Loop indices for column, layer, region and the regions in the
    ! upper and lower layers for an interface
    integer  :: jlay, jupper, jlower

    ! Overlap matrix (non-directional)
    real(jprb) :: overlap_matrix(nreg,nreg)

    ! Fraction of the gridbox occupied by each region in the upper and
    ! lower layers for an interface
    real(jprb) :: frac_upper(nreg), frac_lower(nreg)

    real(jprb) :: hook_handle

    if (lhook) call dr_hook('radsurf_overlap:calc_overlap_matrices',0,hook_handle)

    ! The ground is treated as one clear-sky region, so the fractions
    ! are assigned as such
    frac_lower(1)      = 1.0_jprb
    frac_lower(2:nreg) = 0.0_jprb

    ! Loop up through the canopy, where jlay indexes each layer
    ! interface starting at 1 for the ground, as well as indexing each
    ! layer starting at 1 for the lowest layer.
    do jlay = 1,nlay+1
      ! Fraction of each region just below the interface
      if (jlay > nlay) then
        ! We are at the top of the canopy: treat as a single clear-sky
        ! region
        frac_upper(1)      = 1.0_jprb
        frac_upper(2:nreg) = 0.0_jprb
      else
        frac_upper = region_fracs(1:nreg,jlay)
      end if
   
      ! Simple scheme assuming the two vegetated regions have the same
      ! fraction
      overlap_matrix = calc_overlap_matrix_max_ran(nreg, &
           &  1.0_jprb - frac_upper(1), 1.0_jprb - frac_lower(1))

      ! Convert to directional overlap matrices
      do jupper = 1,nreg
        do jlower = 1,nreg
          if (frac_lower(jlower) >= frac_threshold) then
            u_overlap(jupper,jlower,jlay) = overlap_matrix(jupper,jlower) &
                 &  / frac_lower(jlower)
          else
            u_overlap(jupper,jlower,jlay) = 0.0_jprb
          end if
          if (frac_upper(jupper) >= frac_threshold) then
            v_overlap(jlower,jupper,jlay) = overlap_matrix(jupper,jlower) &
                 &  / frac_upper(jupper)
          else
            v_overlap(jlower,jupper,jlay) = 0.0_jprb
          end if
        end do
      end do
      frac_lower = frac_upper
        
    end do ! layers

    if (lhook) call dr_hook('radsurf_overlap:calc_overlap_matrices',1,hook_handle)

  end subroutine calc_overlap_matrices
  

  !---------------------------------------------------------------------
  ! Calculate overlap matrix O, but for urban areas where the lower
  ! layer has an additional region representing exposed roof, and the
  ! region fractions may not sum to one
  pure function calc_overlap_matrix_max_ran_urban(nreg, &
       &  frac_upper, frac_lower) result(overlap_matrix)

    use parkind1, only : jprb

    implicit none
    
    integer, intent(in) :: nreg ! Number of regions

    ! Region fractions in the upper and lower layers
    real(jprb), intent(in) :: frac_upper(nreg)
    real(jprb), intent(in) :: frac_lower(nreg+1)

    ! Output overlap matrix, the first index corresponding to the
    ! upper layer and the second to the lower layer
    real(jprb) :: overlap_matrix(nreg,nreg+1)

    ! Combined vegetation cover of pair of layers
    real(jprb) :: pair_cover

    if (nreg == 1) then
      ! No vegetation
      overlap_matrix(1,1:2) = frac_lower(1:2)
    else if (nreg == 2) then
      pair_cover = max(frac_upper(2),frac_lower(2))
      ! Assume minimum overlap of trees and buildings
      if (pair_cover <= frac_lower(1) + frac_lower(2)) then
        ! Trees not overhanging buildings
        overlap_matrix(2,3) = 0.0_jprb
        ! All exposed roof under clear sky
        overlap_matrix(1,3) = frac_lower(3)
        ! Clear-to-clear
        overlap_matrix(1,1) = frac_lower(1) + frac_lower(2) - pair_cover
        ! Clear in upper layer, vegetation in lower layer
        overlap_matrix(1,2) = pair_cover - frac_upper(2)
        ! Clear in lower layer, vegetation in upper layer
        overlap_matrix(2,1) = pair_cover - frac_lower(2)
        ! Vegetation in both layers
        overlap_matrix(2,2) = frac_upper(2) + frac_lower(2) - pair_cover
      else
        ! Trees overhang buildings; due to minimum overlap assumption
        ! of trees and buildings, there is no overlap between clear in
        ! the upper layer and either clear or vegetated in the lower
        ! layer
        overlap_matrix(1,1:2) = 0.0_jprb
        ! Clear and vegetated regions in lower layer are both under
        ! vegetation in upper layer
        overlap_matrix(2,1:2) = frac_lower(1:2)
        ! Roof in lower layer, vegetation in upper layer
        overlap_matrix(2,3)   = frac_upper(2) - frac_lower(1) - frac_lower(2)
        ! Roof in lower layer, clear in lower layer
        overlap_matrix(1,3)   = frac_upper(1)
      end if
    else
      ! Never any cross-overlap between the two vegetated regions
      overlap_matrix(2,3) = 0.0_jprb
      overlap_matrix(3,2) = 0.0_jprb
      pair_cover = max(frac_upper(2)+frac_upper(3),frac_lower(2)+frac_lower(3))
      ! Assume minimum overlap of trees and buildings
      if (pair_cover <= frac_lower(1) + frac_lower(2) + frac_lower(3)) then
        ! Trees not overhanging buildings
        overlap_matrix(2:3,4) = 0.0_jprb
        ! All exposed roof under clear sky
        overlap_matrix(1,4) = frac_lower(4)
        ! Clear-to-clear
        overlap_matrix(1,1) = frac_lower(1) + frac_lower(2) + frac_lower(3) - pair_cover
        if (pair_cover > frac_upper(2)+frac_upper(3)) then
          ! More vegetation in lower layer
          overlap_matrix(2:3,1) = 0.0_jprb
          overlap_matrix(2,2)   = frac_upper(2)
          overlap_matrix(3,3)   = frac_upper(3)
          overlap_matrix(1,2)   = frac_lower(2)-frac_upper(2)
          overlap_matrix(1,3)   = frac_lower(3)-frac_upper(3)
        else
          ! More vegetation in upper layer
          overlap_matrix(1,2:3) = 0.0_jprb
          overlap_matrix(2,2)   = frac_lower(2)
          overlap_matrix(3,3)   = frac_lower(3)
          overlap_matrix(2,1)   = frac_upper(2)-frac_lower(2)
          overlap_matrix(3,1)   = frac_upper(3)-frac_lower(3)
        end if
      else
        ! Trees overhang buildings; due to minimum overlap assumption
        ! of trees and buildings, there is no overlap between clear in
        ! the upper layer and either clear or vegetated in the lower
        ! layer
        overlap_matrix(1,1:3) = 0.0_jprb
        ! Clear and vegetated regions in lower layer are both under
        ! vegetation in upper layer, with maximum overlap of
        ! vegetation heterogeneities
        overlap_matrix(2,2)   = frac_lower(2)
        overlap_matrix(3,3)   = frac_lower(3)
        overlap_matrix(2,1)   = frac_lower(1) * 0.5_jprb
        overlap_matrix(3,1)   = overlap_matrix(1,2)
        ! Roof in lower layer, vegetation in upper layer
        overlap_matrix(2,4)   = (frac_lower(4) - frac_upper(1)) * 0.5_jprb
        overlap_matrix(3,4)   = overlap_matrix(2,4)
        ! Roof in lower layer, clear in lower layer
        overlap_matrix(1,4)   = frac_upper(1)
      end if
    end if

  end function calc_overlap_matrix_max_ran_urban


  !---------------------------------------------------------------------
  ! As calc_overlap_matrices, but for an urban canopy with vertically
  ! varying building fraction, such the exposed roof at each layer
  ! interface is treated as its own region in the lower layer. This
  ! leads to the directional overlap matrices being diagonal.
  subroutine calc_overlap_matrices_urban(nlay,nreg, &
       &     region_fracs, u_overlap, v_overlap, &
       &     frac_threshold)

    use parkind1,     only : jprb
    use radiation_io, only : radiation_abort
    use yomhook,      only : lhook, dr_hook

    implicit none
    
    ! Number of layers and regions
    integer,  intent(in) :: nlay, nreg

    ! Area fraction of each region: region 1 is clear sky, and 2+ are
    ! the vegetated regions (only one or two vegetated regions are
    ! supported). If the fractions do not sum to 1 in a layer, the
    ! remainder is building.
    real(jprb), intent(in), dimension(1:nreg,nlay)  :: region_fracs

    ! Output overlap matrices, one for each layer interface
    real(jprb), intent(out), dimension(nreg,nreg+1,nlay+1) :: u_overlap
    real(jprb), intent(out), dimension(nreg+1,nreg,nlay+1) :: v_overlap

    ! Regions smaller than this are ignored
    real(jprb), intent(in), optional :: frac_threshold

    ! Loop indices for column, layer, region and the regions in the
    ! upper and lower layers for an interface
    integer  :: jlay, jupper, jlower

    ! Overlap matrix (non-directional)
    real(jprb) :: overlap_matrix(nreg,nreg+1)

    ! Fraction of the gridbox occupied by each region in the upper and
    ! lower layers for an interface
    real(jprb) :: frac_upper(nreg), frac_lower(nreg+1)

    real(jprb) :: hook_handle

    if (lhook) call dr_hook('radsurf_overlap:calc_overlap_matrices_urban',0,hook_handle)

    ! The ground is treated as one roof region equal in size to the
    ! non-building regions in the lowest layer, so the fractions are
    ! assigned as such
    frac_lower(1:nreg) = 0.0_jprb
    frac_lower(nreg+1) = sum(region_fracs(:,1))

    ! Loop up through the canopy, where jlay indexes each layer
    ! interface starting at 1 for the ground, as well as indexing each
    ! layer starting at 1 for the lowest layer.
    do jlay = 1,nlay+1
      ! Fraction of each region just below the interface
      if (jlay > nlay) then
        ! We are at the top of the canopy: treat as a single clear-sky
        ! region
        frac_upper(1)      = 1.0_jprb
        frac_upper(2:nreg) = 0.0_jprb
      else
        frac_upper = region_fracs(1:nreg,jlay)
      end if
   
      ! Simple scheme assuming the two vegetated regions have the same
      ! fraction
      overlap_matrix = calc_overlap_matrix_max_ran_urban(nreg, &
           &                               frac_upper, frac_lower)

      ! Convert to directional overlap matrices
      do jupper = 1,nreg
        do jlower = 1,nreg+1
          if (frac_lower(jlower) >= frac_threshold) then
            u_overlap(jupper,jlower,jlay) = overlap_matrix(jupper,jlower) &
                 &  / frac_lower(jlower)
          else
            u_overlap(jupper,jlower,jlay) = 0.0_jprb
          end if
          if (frac_upper(jupper) >= frac_threshold) then
            v_overlap(jlower,jupper,jlay) = overlap_matrix(jupper,jlower) &
                 &  / frac_upper(jupper)
          else
            v_overlap(jlower,jupper,jlay) = 0.0_jprb
          end if
        end do
      end do

      ! Copy fractions to lower layer
      frac_lower(1:nreg) = frac_upper
      ! Add the area of exposed roof
      if (jlay < nlay) then
        frac_lower(nreg+1) = sum(region_fracs(:,jlay+1)) - sum(region_fracs(:,jlay))
        if (frac_lower(nreg+1) < 0.0_jprb) then
          call radiation_abort('Building fraction cannot increase with height')
          return
        end if
      else if (jlay == nlay) then
        frac_lower(nreg+1) = 1.0_jprb - sum(region_fracs(:,jlay))
      end if
        
    end do ! layers

    if (lhook) call dr_hook('radsurf_overlap:calc_overlap_matrices_urban',1,hook_handle)

  end subroutine calc_overlap_matrices_urban
  
end module radsurf_overlap
