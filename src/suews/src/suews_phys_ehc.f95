MODULE heatflux
   IMPLICIT NONE
CONTAINS
   SUBROUTINE heatcond1d_vstep(T, Qs, Tsfc, dx, dt, k, rhocp, bc, bctype, debug)
      REAL(KIND(1D0)), INTENT(inout) :: T(:)
      REAL(KIND(1D0)), INTENT(in) :: dx(:), dt, k(:), rhocp(:), bc(2)
      REAL(KIND(1D0)), INTENT(out) :: Qs, Tsfc
      LOGICAL, INTENT(in) :: bctype(2) ! if true, use surrogate flux as boundary condition
      LOGICAL, INTENT(in) :: debug
      INTEGER :: i, n
      REAL(KIND(1D0)), ALLOCATABLE :: w(:), a(:), T1(:), cfl(:)

      REAL(KIND(1D0)) :: cfl_max
      REAL(KIND(1D0)) :: dt_remain
      REAL(KIND(1D0)) :: dt_step
      REAL(KIND(1D0)) :: dt_step_cfl

      REAL(KIND(1D0)), ALLOCATABLE :: T_in(:), T_out(:)
      REAL(KIND(1D0)) :: dt_x ! for recursion
      INTEGER :: n_div ! for recursion
      n = SIZE(T)
      ALLOCATE (w(0:n), a(n), T1(n), cfl(n), T_in(n), T_out(n))

      ! save initial temperatures
      T_in = T
      ! w = interface temperature
      w(1:n) = T
      w(0) = bc(1); w(n) = bc(2)
      !convert from flux to equivalent temperature, not exact
      ! F = k dT/dX => dx*F/k + T(i+1) = T(i)
      IF (bctype(1)) w(0) = bc(1)*0.5*dx(1)/k(1) + w(1)
      IF (bctype(2)) w(n) = bc(2)*0.5*dx(n)/k(n) + w(n)
      ! print *, 'bc(1)=', bc(1), 'bc(2)=',bc(2)
      ! print *, 'w(0)=', w(0), 'w(n)=', w(n)
      ! print *, 'k=', k, 'dx=', dx
      a = k/dx
      DO i = 1, n - 1
         w(i) = (T(i + 1)*a(i + 1) + T(i)*a(i))/(a(i) + a(i + 1))
      END DO

      ! fix convergece issue with recursion
      dt_remain = dt
      dt_step_cfl = 0.05*MINVAL(dx**2/(k/rhocp))
      DO WHILE (dt_remain > 1E-10)
         dt_step = MIN(dt_step_cfl, dt_remain)
         !PRINT *, 'dt_remain: ', dt_remain
         !PRINT *, 'dt_step_cfl: ', dt_step_cfl
         !PRINT *, '***** dt_step: ', dt_step
          !!FO!!
         ! PRINT *, 'w: ', w
         ! PRINT *, 'rhocp: ', rhocp
         ! PRINT *, 'dt: ', dt
         ! PRINT *, 'dx: ', dx
         ! PRINT *, 'a: ', a
         DO i = 1, n
            ! PRINT *, 'i: ', i
            ! PRINT *, 'i: ', (dt/rhocp(i))
            ! PRINT *, 'i: ', (w(i - 1) - 2*T(i) + w(i))
            ! PRINT *, 'i: ', 2*a(i)/dx(i)
            T1(i) = &
               (dt_step/rhocp(i)) &
               *(w(i - 1) - 2*T(i) + w(i)) &
               *a(i)/dx(i) &
               + T(i)
         END DO
         T = T1
         DO i = 1, n - 1
            w(i) = (T(i + 1)*a(i + 1) + T(i)*a(i))/(a(i) + a(i + 1))
         END DO
         dt_remain = dt_remain - dt_step
      END DO
       !!FO!!
      ! PRINT *, 'T1: ', T1
      !for storage the internal distribution of heat should not be important
       !!FO!! k*d(dT/dx)/dx = rhoCp*(dT/dt) => rhoCp*(dT/dt)*dx = dQs => dQs = k*d(dT/dx)
      ! Qs = (w(0) - T(1))*2*a(1) + (w(n) - T(n))*2*a(n)
      ! Qs=sum((T1-T)*rhocp*dx)/dt!

      Tsfc = w(0)
      ! save output temperatures
      T_out = T1
      ! new way for calcualating heat storage
      Qs = SUM( &
           (([bc(1), T_out(1:n - 1)] + T_out)/2. & ! updated temperature
            -([bc(1), T_in(1:n - 1)] + T_in)/2) & ! initial temperature
           *rhocp*dx/dt)
   END SUBROUTINE heatcond1d_vstep
END MODULE heatflux

MODULE EHC_module
   !===============================================================================
   ! revision history:
   ! TS 09 Oct 2017: re-organised ESTM subroutines into a module
   !===============================================================================
   IMPLICIT NONE

CONTAINS
   ! ===============================================================================================
   ! extended ESTM, TS 20 Jan 2022
   ! renamed to EHC (explicit heat conduction) to avoid confusion with ESTM
   ! ESTM_ehc accoutns for
   ! 1. heterogeneous building facets (roofs, walls) at different vertical levels
   ! 2. all standard ground-level surfaces (dectr, evetr, grass, bsoil and water)
   SUBROUTINE EHC( &
      tstep, & !input
      nlayer, &
      QG_surf, qg_roof, qg_wall, &
      tsfc_roof, tin_roof, temp_in_roof, k_roof, cp_roof, dz_roof, sfr_roof, & !input
      tsfc_wall, tin_wall, temp_in_wall, k_wall, cp_wall, dz_wall, sfr_wall, & !input
      tsfc_surf, tin_surf, temp_in_surf, k_surf, cp_surf, dz_surf, sfr_surf, & !input
      temp_out_roof, QS_roof, & !output
      temp_out_wall, QS_wall, & !output
      temp_out_surf, QS_surf, & !output
      QS) !output
      USE allocateArray, ONLY: &
         nsurf, ndepth, &
         PavSurf, BldgSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf
      USE heatflux, ONLY: heatcond1d_vstep

      IMPLICIT NONE
      INTEGER, INTENT(in) :: tstep
      INTEGER, INTENT(in) :: nlayer ! number of vertical levels in urban canopy

      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: QG_surf ! ground heat flux
      ! extended for ESTM_ehc

      ! keys:
      ! tsfc: surface temperature
      ! tin: indoor/deep bottom temperature
      ! temp_in: temperature at inner interfaces
      ! k: thermal conductivity
      ! cp: heat capacity
      ! dz: thickness of each layer
      ! roof/wall/surf: roof/wall/ground surface types

      ! input arrays: roof facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qg_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: temp_in_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_roof
      ! input arrays: wall facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qg_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_wall
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: temp_in_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_wall
      ! input arrays: standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tsfc_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: tin_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: temp_in_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: k_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: cp_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(in) :: dz_surf

      ! output arrays
      ! roof facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: QS_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(out) :: temp_out_roof !interface temperature between depth layers
      ! wall facets
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(out) :: QS_wall
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(out) :: temp_out_wall !interface temperature between depth layers
      ! standard suews surfaces
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(out) :: QS_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(out) :: temp_out_surf !interface temperature between depth layers

      ! grid aggregated results
      REAL(KIND(1D0)), INTENT(out) :: QS

      ! internal use
      ! temporary arrays in calculations
      ! note: nsurf is used as the maximum number of surfaces
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_cal
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tin_cal
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: temp_cal
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: k_cal
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: cp_cal
      REAL(KIND(1D0)), DIMENSION(:, :), ALLOCATABLE :: dz_cal
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: qs_cal

      ! REAL(KIND(1D0)), DIMENSION(ndepth) :: temp_all_cal
      ! surface temperatures at innermost depth layer
      ! REAL(KIND(1D0)) :: temp_IBC
      INTEGER :: i_facet, i_group, nfacet, i_depth

      ! settings for boundary conditions
      REAL(KIND(1D0)), DIMENSION(2) :: bc

      ! use temperature as boundary conditions
      LOGICAL, DIMENSION(2) :: bctype
      LOGICAL :: debug

      ! use finite depth heat conduction solver
      LOGICAL :: use_heatcond1d, use_heatcond1d_water

      ! normalised surface fractions
      REAL(KIND(1D0)), DIMENSION(nlayer) :: sfr_roof_n
      REAL(KIND(1D0)), DIMENSION(nlayer) :: sfr_wall_n

      ! initialise solver flags
      use_heatcond1d = .TRUE.
      use_heatcond1d_water = .FALSE.
      debug = .FALSE.

      ! normalised surface fractions
      ! NB: the sum of sfr_roof (sfr_wall) is NOT unity; so need to be normalised by the sum
      sfr_roof_n = sfr_roof/SUM(sfr_roof)
      sfr_wall_n = sfr_wall/SUM(sfr_wall)
      ! sub-facets of buildings: e.g. walls, roofs, etc.
      DO i_group = 1, 3

         ! allocate arrays
         IF (i_group == 1) THEN
            ! PRINT *, 'group: ', 'roof'
            nfacet = nlayer
         ELSE IF (i_group == 2) THEN
            ! PRINT *, 'group: ', 'wall'
            nfacet = nlayer
         ELSE IF (i_group == 3) THEN
            ! PRINT *, 'group: ', 'surf'
            nfacet = nsurf
         END IF
         ! PRINT *, 'nfacet here: ', nfacet
         ALLOCATE (tsfc_cal(nfacet))
         ALLOCATE (tin_cal(nfacet))
         ALLOCATE (qs_cal(nfacet))
         ALLOCATE (temp_cal(nfacet, ndepth))
         ALLOCATE (k_cal(nfacet, ndepth))
         ALLOCATE (cp_cal(nfacet, ndepth))
         ALLOCATE (dz_cal(nfacet, ndepth))
         ! PRINT *, 'allocation done! '

         ! translate input arrays of facet groups to internal use arrays
         IF (i_group == 1) THEN
            ! PRINT *, 'translation for roof! '
            ! TODO: to update with actual values from input files
            tsfc_cal(1:nfacet) = tsfc_roof(1:nfacet)
            ! PRINT *, 'tsfc_cal for roof! ', tsfc_cal
            tin_cal(1:nfacet) = tin_roof(1:nfacet)
            ! PRINT *, 'tin_cal for roof! ', tin_cal
            temp_cal(1:nfacet, 1:ndepth) = temp_in_roof(1:nfacet, 1:ndepth)
            ! PRINT *, 'temp_cal for roof! ',temp_cal
            k_cal(1:nfacet, 1:ndepth) = k_roof(1:nfacet, 1:ndepth)
            ! PRINT *, 'k_roof for roof! ',k_roof(1,:)
            cp_cal(1:nfacet, 1:ndepth) = cp_roof(1:nfacet, 1:ndepth)
            dz_cal(1:nfacet, 1:ndepth) = dz_roof(1:nfacet, 1:ndepth)
            ! qs_cal(1:nfacet) = qs_roof(1:nfacet)
         ELSE IF (i_group == 2) THEN
            ! PRINT *, 'translation for wall! '
            ! TODO: to update with actual values from input files
            tsfc_cal(1:nfacet) = tsfc_wall(1:nfacet)
            tin_cal(1:nfacet) = tin_wall(1:nfacet)
            temp_cal(1:nfacet, 1:ndepth) = temp_in_wall(1:nfacet, 1:ndepth)
            ! TODO: temporarily set this for testing
            k_cal(1:nfacet, 1:ndepth) = k_wall(1:nfacet, 1:ndepth)
            cp_cal(1:nfacet, 1:ndepth) = cp_wall(1:nfacet, 1:ndepth)
            dz_cal(1:nfacet, 1:ndepth) = dz_wall(1:nfacet, 1:ndepth)
            ! qs_cal(1:nfacet) = qs_wall(1:nfacet)

         ELSE IF (i_group == 3) THEN
            ! PRINT *, 'translation for surf! '
            ! nfacet = nsurf
            tsfc_cal(1:nfacet) = tsfc_surf(1:nfacet)
            tin_cal(1:nfacet) = tin_surf(1:nfacet)
            temp_cal(1:nfacet, 1:ndepth) = temp_in_surf(1:nfacet, 1:ndepth)
            ! TODO: to update with actual values from input files
            k_cal(1:nfacet, 1:ndepth) = k_surf(1:nfacet, 1:ndepth)
            cp_cal(1:nfacet, 1:ndepth) = cp_surf(1:nfacet, 1:ndepth)
            dz_cal(1:nfacet, 1:ndepth) = dz_surf(1:nfacet, 1:ndepth)
            ! k_cal(1:nfacet, 1:ndepth) = 1.2
            ! cp_cal(1:nfacet, 1:ndepth) = 2E6
            ! dz_cal(1:nfacet, 1:ndepth) = 0.1
            ! qs_cal(1:nfacet) = QS_surf(1:nfacet)
         END IF
         ! PRINT *, 'translation done! '
         ! TODO: temporary setting
         ! k_cal(1:nfacet, 1:ndepth) = 1.2
         ! cp_cal(1:nfacet, 1:ndepth) = 2E6
         ! dz_cal(1:nfacet, 1:ndepth) = 0.2

         ! PRINT *, 'nfacet: ', nfacet
         DO i_facet = 1, nfacet
            ! PRINT *, 'i_facet: ', i_facet
            ! ASSOCIATE (v => dz_cal(i_facet, 1:ndepth))
            !    PRINT *, 'dz_cal in ESTM_ehc', v, SIZE(v)
            ! END ASSOCIATE

            ! determine the calculation method
            IF (i_group == 3) THEN
               use_heatcond1d = .TRUE.
               IF (i_facet == BldgSurf) THEN
                  ! building surface needs a different treatment
                  use_heatcond1d = .FALSE.
               ELSE IF (i_facet == WaterSurf) THEN
                  ! water surface needs a different treatment
                  use_heatcond1d = .FALSE.
                  use_heatcond1d_water = .TRUE.
               END IF
            ELSE
               use_heatcond1d = .TRUE.
            END IF

            ! actual heat conduction calculations
            IF (dz_cal(i_facet, 1) /= -999.0 .AND. use_heatcond1d) THEN

               ! surface heat flux
               IF (i_group == 1) THEN
                  bc(1) = qg_roof(i_facet)
               ELSE IF (i_group == 2) THEN
                  bc(1) = qg_wall(i_facet)
               ELSE IF (i_group == 3) THEN
                  bc(1) = QG_surf(i_facet)
               END IF
               ! bctype(1) = .TRUE.
               bc(1) = tsfc_cal(i_facet)
               bctype(1) = .FALSE.

               !TODO: should be a prescribed temperature of the innermost boundary
               bc(2) = tin_cal(i_facet)
               bctype(2) = .FALSE.

               ! IF (i_group == 3 .AND. i_facet == 3) THEN
               ! PRINT *, 'temp_cal before: ', temp_cal(i_facet, :)
               ! PRINT *, 'k_cal: ', k_cal(i_facet, 1:ndepth)
               ! PRINT *, 'cp_cal: ', cp_cal(i_facet, 1:ndepth)
               ! PRINT *, 'dz_cal: ', dz_cal(i_facet, 1:ndepth)
               ! PRINT *, 'bc: ', bc

               ! END IF

               ! 1D heat conduction for finite depth layers

               ! IF ((i_group == 3) .AND. (i_facet == 1)) THEN
               !    debug = .FALSE.
               ! ELSE
               !    debug = .FALSE.
               ! END IF
               ! CALL heatcond1d_ext( &
               CALL heatcond1d_vstep( &
                  temp_cal(i_facet, :), &
                  QS_cal(i_facet), &
                  tsfc_cal(i_facet), &
                  dz_cal(i_facet, 1:ndepth), &
                  tstep*1.D0, &
                  k_cal(i_facet, 1:ndepth), &
                  cp_cal(i_facet, 1:ndepth), &
                  bc, &
                  bctype, debug)

               ! update temperature at all inner interfaces
               ! tin_cal(i_facet, :) = temp_all_cal
               ! IF (i_group == 3 .AND. i_facet == 3) THEN
               ! PRINT *, 'temp_cal after: ', temp_cal(i_facet, :)
               ! PRINT *, 'QS_cal after: ', QS_cal(i_facet)
               ! PRINT *, 'k_cal: ', k_cal(i_facet, 1:ndepth)
               ! PRINT *, 'cp_cal: ', cp_cal(i_facet, 1:ndepth)
               ! PRINT *, 'dz_cal: ', dz_cal(i_facet, 1:ndepth)
               ! PRINT *, 'bc: ', bc
               ! PRINT *, ''

               ! END IF
            END IF

            IF (dz_cal(i_facet, 1) /= -999.0 .AND. use_heatcond1d_water) THEN
               ! temperatures at all interfaces, including the outmost surface
               ! temp_all_cal = temp_cal(i_facet, :)

               ! outermost surface temperature
               ! bc(1) = tsfc_cal(i_facet)
               bc(1) = tsfc_cal(i_facet)
               bctype(1) = .FALSE.

               !TODO: should be a prescribed temperature of the innermost boundary
               bc(2) = tin_cal(i_facet)
               bctype(2) = .FALSE.

               ! 1D heat conduction for finite depth layers
               ! TODO: this should be a water specific heat conduction solver: to implement
               ! CALL heatcond1d_ext( &
               CALL heatcond1d_vstep( &
                  temp_cal(i_facet, :), &
                  QS_cal(i_facet), &
                  tsfc_cal(i_facet), &
                  dz_cal(i_facet, 1:ndepth), &
                  tstep*1.D0, &
                  k_cal(i_facet, 1:ndepth), &
                  cp_cal(i_facet, 1:ndepth), &
                  bc, &
                  bctype, debug)

               ! ! update temperature at all inner interfaces
               ! temp_cal(i_facet, :) = temp_all_cal
            END IF

         END DO ! end of i_facet loop

         ! translate results to back to the output arrays of facet groups
         IF (i_group == 1) THEN
            QS_roof = QS_cal(1:nfacet)
            ! tsfc_roof = tsfc_cal(1:nfacet)
            temp_out_roof = temp_cal(:nfacet, :)
         ELSE IF (i_group == 2) THEN
            QS_wall = QS_cal(1:nfacet)
            ! tsfc_wall = tsfc_cal(1:nfacet)
            temp_out_wall = temp_cal(:nfacet, :)
         ELSE IF (i_group == 3) THEN
            QS_surf = QS_cal(1:nfacet)
            ! tsfc_surf = tsfc_cal(1:nfacet)
            temp_out_surf = temp_cal(:nfacet, :)
         END IF

         ! deallocate memory
         DEALLOCATE (tsfc_cal)
         DEALLOCATE (tin_cal)
         DEALLOCATE (qs_cal)
         DEALLOCATE (temp_cal)
         DEALLOCATE (k_cal)
         DEALLOCATE (cp_cal)
         DEALLOCATE (dz_cal)

      END DO ! end do i_group

      ! aggregated results
      ! building surface
      ! PRINT *, 'QS_roof in ESTM_ehc', DOT_PRODUCT(QS_roof, sfr_roof), 'for', sfr_roof
      ! PRINT *, 'QS_wall in ESTM_ehc', DOT_PRODUCT(QS_wall, sfr_wall), 'for', sfr_wall
      IF (sfr_surf(BldgSurf) < 1.0E-8) THEN
         QS_surf(BldgSurf) = 0.0
      ELSE
         QS_surf(BldgSurf) = (DOT_PRODUCT(QS_roof, sfr_roof) + DOT_PRODUCT(QS_wall, sfr_wall))/sfr_surf(BldgSurf)
      END IF

      DO i_depth = 1, ndepth
         temp_out_surf(BldgSurf, i_depth) = &
            (DOT_PRODUCT(temp_out_roof(:, i_depth), sfr_roof) &
             + DOT_PRODUCT(temp_out_wall(:, i_depth), sfr_wall)) &
            /(SUM(sfr_roof) + SUM(sfr_wall))
      END DO

      ! all standard suews surfaces
      qs = DOT_PRODUCT(QS_surf, sfr_surf)

   END SUBROUTINE EHC

END MODULE EHC_module
