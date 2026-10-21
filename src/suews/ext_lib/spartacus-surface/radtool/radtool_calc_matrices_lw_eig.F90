! radtool_calc_matrices_lw_eig.f90 - Compute reflectance matrix and friends for longwave
!
! (C) Copyright 2018- ECMWF.
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
! SUEWS modification (2026, GH#1498): handle transparent spectral bands
! with analytical transmission, integrated flux and emission source terms.
! See ../SUEWS_PATCHES.md for scope and upstream follow-up.
!

module radtool_calc_matrices_lw_eig

  implicit none

contains

  ! This routine implements computes various properties in a single
  ! layer of an urban or vegetation canopy in the longwave: the
  ! reflectance and transmittance matrices, the "source" emerging from
  ! either side of the layer due to emission within the layer, and the
  ! integrated fluxes across the layer, both due to emission within
  ! the layer ("int_flux_source") and external sources, the latter
  ! provided in the form of a matrix "int_flux" that should be
  ! multiplied by radiation entering the layer.  The first dimension
  ! of each array is spectral interval, so multiple spectral intervals
  ! are computed at once.  Equation numbers mentioned in the comments
  ! below are from Hogan (2019, Boundary Layer Meteorology: Flexible
  ! treatment of radiative transfer in complex urban canopies).
  recursive subroutine calc_matrices_lw_eig(nmat, norder, dz, gamma1, gamma2, &
       &  emiss_rate, &
       &  reflectance, transmittance, source, int_flux, int_flux_source)

    use parkind1, only : jprb

    use radtool_matrix, only : mat_x_mat, mat_x_vec, invert, solve_mat, solve_vec

    use radtool_eigen_decomposition, only : eigen_decomposition_real

    implicit none

    
    ! Inputs

    ! Number of input matrices
    integer, intent(in) :: nmat

    ! Size of gamma matrices
    integer, intent(in) :: norder

    ! Layer thickness (m)
    real(kind=jprb), intent(in) :: dz

    ! Exchange matrices; the unique parts of the matrix "Gamma" that
    ! is not explicitly constructed:
    !   Gamma = [-gamma1 -gamma2]
    !           [+gamma2 +gamma1]
    real(kind=jprb), intent(in), dimension(nmat,norder,norder) :: gamma1, gamma2
    
    ! Emission rate per unit height, "b" in Eq. 32
    real(kind=jprb), intent(in), dimension(nmat,norder) :: emiss_rate

    
    ! Outputs
    
    ! Reflectance and transmittance matrices, "R" and "T" in Eq. 49
    real(kind=jprb), intent(out), dimension(nmat,norder,norder) :: &
         &  reflectance, transmittance
    
    ! Radiation emerging from top and bottom of layer due to emission
    ! within, "p" in Eq. 57
    real(kind=jprb), intent(out), dimension(nmat,norder) :: source

    ! When int_flux is matrix-multiplied by the incoming flux at the
    ! top and base of the layer, we obtain the vertically integrated
    ! flux across the layer, equal to part of "u hat + v hat" in
    ! Eq. 59.
    real(kind=jprb), intent(out), optional :: int_flux(nmat,norder,norder)

    ! The vertically integrated flux across the layer due to internal
    ! emission, equal to part of "u hat + v hat" in Eq. 59.
    real(kind=jprb), intent(out), optional :: int_flux_source(nmat,norder)

    
    ! Local variables

    ! Difference between gamma1 and gamma2
    real(kind=jprb) :: gamma_diff(nmat,norder,norder)

    ! The matrix product (gamma1-gamma2)*(gamma1+gamma2)
    real(kind=jprb) :: gamma_product(nmat,norder,norder)

    ! Eigenvalues/eigenvectors of gamma_product
    real(kind=jprb) :: eigenval_prod(nmat,norder), &
         &             eigenvec_prod(nmat,norder,norder)

    ! The two unique submatrices of the eigenvector matrix of the full
    ! Gamma matrix, "G_1" and "G_2" in Eq. 45
    real(kind=jprb), dimension(nmat,norder,norder) :: g1, g2

    ! g1 and g2 multiplied by the diagonal matrix "D" defined after
    ! Eq. 46
    real(kind=jprb), dimension(nmat,norder,norder) :: g1_d, g2_d

    ! The square upper and lower parts of the rectangular matrix "C'"
    ! in Eq. 47
    real(kind=jprb), dimension(nmat,norder,norder) :: cprime_upper, cprime_lower

    ! Positive eigenvalues of the full Gamma matrix
    real(kind=jprb) :: lambda(nmat,norder)

    ! exp(-lambda*dz)
    real(kind=jprb) :: exp_lambda_dz(nmat,norder)

    ! Upper or lower parts of -Gamma^-1*[-b;b], which are the same
    real(kind=jprb), dimension(nmat,norder) :: inv_gamma_b

    ! gamma2*(gamma1^-1)
    real(kind=jprb), dimension(nmat,norder,norder) :: gamma2_inv_gamma1
    
    ! Upper or lower part of "c_b'" in Eq. 56, which are the same
    real(kind=jprb), dimension(nmat,norder) :: cb_prime

    ! g1^-1*inv_gamma_b
    real(kind=jprb), dimension(nmat,norder) :: inv_g1_inv_gamma_b

    ! Temporary vector and matrix
    real(kind=jprb), dimension(nmat,norder) :: tmp_vec
    real(kind=jprb), dimension(nmat,norder,norder) :: tmp_mat

    ! Loop index over matrix elements
    integer :: jo, jspec
    logical :: transparent(nmat)
    real(kind=jprb) :: band_int_flux(1,norder,norder), band_int_source(1,norder)

    do jspec = 1,nmat
      transparent(jspec) = all(gamma1(jspec,:,:) == 0.0_jprb) &
           &         .and. all(gamma2(jspec,:,:) == 0.0_jprb)
    end do
    if (any(transparent)) then
      ! A transparent layer has R=0 and T=I. The eigenvalue formulation
      ! below instead tries to invert a zero matrix (GH#1498). Treat each
      ! spectral interval independently, using the existing solver for
      ! ordinary bands (multi-band optimisation may change the last bits).
      do jspec = 1,nmat
        if (transparent(jspec)) then
          reflectance(jspec,:,:) = 0.0_jprb
          transmittance(jspec,:,:) = 0.0_jprb
          band_int_flux = 0.0_jprb
          do jo = 1,norder
            transmittance(jspec,jo,jo) = 1.0_jprb
            band_int_flux(1,jo,jo) = dz
          end do
          ! Integrate the constant source exactly, including the zero
          ! emission of physically transparent air.
          source(jspec,:) = emiss_rate(jspec,:) * dz
          band_int_source(1,:) = emiss_rate(jspec,:) * dz**2
        else
          call calc_matrices_lw_eig(1, norder, dz, &
               &  gamma1(jspec:jspec,:,:), gamma2(jspec:jspec,:,:), &
               &  emiss_rate(jspec:jspec,:), reflectance(jspec:jspec,:,:), &
               &  transmittance(jspec:jspec,:,:), source(jspec:jspec,:), &
               &  band_int_flux, band_int_source)
        end if
        if (present(int_flux)) int_flux(jspec,:,:) = band_int_flux(1,:,:)
        if (present(int_flux_source)) int_flux_source(jspec,:) = band_int_source(1,:)
      end do
      return
    end if

    
    ! Section 1: Compute the eigenvectors and eigenvalues of the full
    ! Gamma matrix using the clever DISORT method exploiting its
    ! regular structure

    ! Compute the eigenvectors and eigenvalues of "gamma_product"
    gamma_diff = gamma1-gamma2;
    gamma_product = mat_x_mat(nmat,nmat,norder,gamma_diff,gamma1+gamma2)
    call eigen_decomposition_real(norder,nmat,gamma_product, &
         &                        eigenval_prod, eigenvec_prod)

    ! Eigenvalues of the full Gamma matrix are (+/-)sqrt(eigenval_prod)
    lambda = sqrt(max(0.0_jprb,eigenval_prod))
    exp_lambda_dz = exp(-lambda*dz)

    ! Compute the submatrices g1 and g2 of the eigenvector matrix of
    ! the full Gamma matrix
    tmp_mat = -solve_mat(nmat,nmat,norder,gamma_diff,eigenvec_prod)
    tmp_mat = tmp_mat * spread(lambda,2,norder)
    g1 = eigenvec_prod + tmp_mat
    g2 = eigenvec_prod - tmp_mat
    !g1 = -0.5*(eigenvec_prod + tmp_mat)
    !g2 = -0.5*(eigenvec_prod - tmp_mat)

    
    ! Section 2: Compute reflectance and transmittance matrices

    ! Various equations require product of g1,g2 and exp(-lambda*dz)
    g1_d = g1 * spread(exp_lambda_dz,2,norder)
    g2_d = g2 * spread(exp_lambda_dz,2,norder)

    ! Solve Eq. 48 to compute the upper and lower part of "C'", using
    ! the Schur complement to exploit the regular structure of the
    ! matrix on the left hand side of Eq. 48.
    cprime_lower = invert(nmat,nmat,norder,(g1 &
         &  -mat_x_mat(nmat,nmat,norder,g2_d, &
         &             solve_mat(nmat,nmat,norder,g1,g2_d))))
    cprime_upper = -solve_mat(nmat,nmat,norder,g1, &
         &   mat_x_mat(nmat,nmat,norder,g2_d, cprime_lower))

    ! Apply Eq. 49
    reflectance   = mat_x_mat(nmat,nmat,norder,g1_d,cprime_upper) &
         &        + mat_x_mat(nmat,nmat,norder,g2,  cprime_lower)
    transmittance = mat_x_mat(nmat,nmat,norder,g2  ,cprime_upper) &
         &        + mat_x_mat(nmat,nmat,norder,g1_d,cprime_lower)


    ! Section 3: Compute inv_gamma_b, the upper or lower part of the
    ! vector Gamma^-1*[-b;b] on the right hand side of Eq. 56. This is
    ! done using the Schur complement to exploit the regular block
    ! structure of Gamma, with a additional optimization exploiting
    ! the repeated "b".
    gamma2_inv_gamma1 = mat_x_mat(nmat,nmat,norder,gamma2,&
         &                        invert(nmat,nmat,norder,gamma1))
    tmp_mat = gamma1 - mat_x_mat(nmat,nmat,norder,&
         &                       gamma2_inv_gamma1, gamma2)
    ! Subtract identity matrix
    do jo = 1,norder
       gamma2_inv_gamma1(:,jo,jo) = gamma2_inv_gamma1(:,jo,jo) - 1.0_jprb
    end do
    inv_gamma_b = solve_vec(nmat,nmat,norder,tmp_mat, &
         &  mat_x_vec(nmat,nmat,norder,gamma2_inv_gamma1,emiss_rate))

    
    ! Section 4: Compute the "source": radiation emerging from top and
    ! bottom of layer due to emission. 

    ! First compute the upper (equal to lower) part of vector "c_b'"
    ! in Eq. 56, using Schur complement method again to exploit
    ! regularity of matrix on left hand side of Eq. 56 (and 48).
    inv_g1_inv_gamma_b = solve_vec(nmat,nmat,norder,g1,inv_gamma_b)
    tmp_vec = inv_gamma_b - mat_x_vec(nmat,nmat,norder,g2_d,inv_g1_inv_gamma_b)
    cb_prime = -mat_x_vec(nmat,nmat,norder,cprime_lower,tmp_vec)

    ! Then apply Eq. 57
    source = mat_x_vec(nmat,nmat,norder,g1_d+g2,cb_prime) + inv_gamma_b

    if (present(int_flux) .or. present(int_flux_source)) then
       ! Section 5: Compute integrated fluxes

       ! Redefine exp_lambda_dz to contain elements of Z in Eq. 58
       exp_lambda_dz = (1.0_jprb-exp_lambda_dz)/lambda

       ! Redefine g1 and g2, multiplying by Z
       g1 = g1*spread(exp_lambda_dz,2,norder)
       g2 = g2*spread(exp_lambda_dz,2,norder)

       ! Extract "u hat + v hat" in Eq. 59 as two parts
       tmp_mat  = g1+g2
       int_flux = mat_x_mat(nmat,nmat,norder,tmp_mat,(cprime_lower+cprime_upper))
       int_flux_source = 2.0_jprb*(mat_x_vec(nmat,nmat,norder,tmp_mat,cb_prime) &
            &                    + inv_gamma_b*dz)
    end if
    
  end subroutine calc_matrices_lw_eig

end module radtool_calc_matrices_lw_eig
