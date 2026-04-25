! EHC heatflux calculations module
MODULE module_phys_ehc_heatflux
   IMPLICIT NONE
CONTAINS
   SUBROUTINE thomas_triMat(lw, diag, up, rhs, n, x)
      IMPLICIT NONE
      INTEGER, INTENT(in) :: n
      REAL(KIND(1D0)), DIMENSION(:), INTENT(in) :: lw, diag, up, rhs
      REAL(KIND(1D0)), DIMENSION(:), INTENT(out) :: x
      REAL(KIND(1D0)), DIMENSION(n - 1) :: c_prime
      REAL(KIND(1D0)), DIMENSION(n) :: d_prime
      INTEGER :: i

      c_prime(1) = up(1)/diag(1)
      d_prime(1) = rhs(1)/diag(1)
      DO i = 2, n - 1
         c_prime(i) = up(i)/(diag(i) - lw(i - 1)*c_prime(i - 1))
         d_prime(i) = (rhs(i) - lw(i - 1)*d_prime(i - 1))/(diag(i) - lw(i - 1)*c_prime(i - 1))
      END DO

      d_prime(n) = (rhs(n) - lw(n - 1)*d_prime(n - 1))/(diag(n) - lw(n - 1)*c_prime(n - 1))

      x(n) = d_prime(n)
      DO i = n - 1, 1, -1
         x(i) = d_prime(i) - c_prime(i)*x(i + 1)
      END DO
   END SUBROUTINE thomas_triMat

   SUBROUTINE heatcond1d_vstep(T, Qs, dx, dt, k, rhocp, bc)
      REAL(KIND(1D0)), INTENT(inout) :: T(:)
      REAL(KIND(1D0)), INTENT(in) :: dx(:), dt, k(:), rhocp(:), bc(2)
      REAL(KIND(1D0)), INTENT(out) :: Qs
      ! REAL(KIND(1D0)), INTENT(out) :: Tsfc
      LOGICAL :: bctype(2) ! if true, use surrogate flux as boundary condition
      ! LOGICAL, INTENT(in) :: debug
      INTEGER :: i, n
      REAL(KIND(1D0)), ALLOCATABLE :: w(:), a(:), T_temp(:), cfl(:)

      ! REAL(KIND(1D0)) :: cfl_max
      REAL(KIND(1D0)) :: dt_remain
      REAL(KIND(1D0)) :: dt_step
      REAL(KIND(1D0)) :: dt_step_cfl

      REAL(KIND(1D0)), ALLOCATABLE :: T_in(:), T_out(:)
      ! REAL(KIND(1D0)) :: dt_x ! for recursion
      ! INTEGER :: n_div ! for recursion
      n = SIZE(T)
      ALLOCATE (w(0:n), a(n), T_temp(n), cfl(n), T_in(n), T_out(n))

      ! save initial temperatures
      T_in = T
      ! w = interface temperature
      w(1:n) = T
      w(0) = bc(1); w(n) = bc(2)
      !convert from flux to equivalent temperature, not exact
      ! F = k dT/dX => dx*F/k + T(i+1) = T(i)
      bctype = .FALSE. ! force to use T as boundary condition, TS 09 Aug 2023
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
            T_temp(i) = &
               (dt_step/rhocp(i)) &
               *(w(i - 1) - 2*T(i) + w(i)) &
               *a(i)/dx(i) &
               + T(i)
         END DO
         T = T_temp
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

      ! Tsfc = w(0)
      ! save output temperatures
      T_out = T_temp
      ! new way for calcualating heat storage
      Qs = SUM( &
           (([bc(1), T_out(1:n - 1)] + T_out)/2. & ! updated temperature
            -([bc(1), T_in(1:n - 1)] + T_in)/2) & ! initial temperature
           *rhocp*dx/dt)
   END SUBROUTINE heatcond1d_vstep

   RECURSIVE SUBROUTINE heatcond1d_CN(T, Qs, dx, dt, k, rhocp, bc)
      REAL(KIND(1D0)), INTENT(inout) :: T(:)
      REAL(KIND(1D0)), INTENT(in) :: dx(:), dt, k(:), rhocp(:), bc(2)
      ! REAL(KIND(1D0)), INTENT(out) :: Qs, Tsfc
      REAL(KIND(1D0)), INTENT(out) :: Qs
      ! LOGICAL, INTENT(in) :: bctype(2) ! if true, use surrogate flux as boundary condition
      REAL(KIND(1D0)) :: alpha, T_lw, T_up
      REAL(KIND(1D0)) :: Qs_acc

      ! LOGICAL, INTENT(in) :: debug
      INTEGER :: i, n
      REAL(KIND(1D0)), ALLOCATABLE :: T_tmp(:), k_itf(:)
      REAL(KIND(1D0)), ALLOCATABLE :: T_in(:), T_out(:)
      REAL(KIND(1D0)), ALLOCATABLE :: vec_lw(:), vec_up(:), vec_diag(:), vec_rhs(:)

      REAL(KIND(1D0)) :: dt_remain
      REAL(KIND(1D0)) :: dt_step
      REAL(KIND(1D0)) :: dt_step_cfl

      n = SIZE(T)

      ALLOCATE (T_tmp(1:n)) ! temporary temperature array
      ALLOCATE (k_itf(1:n - 1)) ! thermal conductivity at interfaces
      ALLOCATE (T_in(1:n)) ! initial temperature array
      ALLOCATE (T_out(1:n)) ! output temperature array

      ALLOCATE (vec_lw(1:n - 1))
      ALLOCATE (vec_up(1:n - 1))
      ALLOCATE (vec_diag(1:n))
      ALLOCATE (vec_rhs(1:n))

      alpha = 0.5
      T_up = bc(1)
      T_lw = bc(2)

      ! save initial temperatures
      T_in = T
      T_tmp = T_in

      ! calculate the depth-averaged thermal conductivity
      DO i = 1, n - 1
         k_itf(i) = (k(i)*k(i + 1)*(dx(i) + dx(i + 1)))/(k(i)*dx(i + 1) + k(i + 1)*dx(i))
      END DO

      dt_remain = dt
      ! dt_step_cfl = 0.002*MINVAL(dx**2/(k/rhocp))
      dt_step_cfl = MINVAL(dx**2/(k/rhocp))
      !PRINT *, 'dt_step_cfl: ', dt_step_cfl

      Qs_acc = 0.0 ! accumulated heat storage
      DO WHILE (dt_remain > 1E-10)
         dt_step = MIN(dt_step_cfl, dt_remain)
         !T_tmp(1) = T_up
         !T_tmp(n) = T_lw
         ! set the tridiagonal matrix for 1D heat conduction solver based on Crank-Nicholson method
         DO i = 1, n
            IF (i == 1) THEN
               vec_up(i) = (1.0 - alpha)*k_itf(i)/(0.5*(dx(i + 1) + dx(i)))
               vec_diag(i) = -(1.0 - alpha)*k(1)/(0.5*(dx(i))) &
                             - (1.0 - alpha)*k_itf(i)/(0.5*(dx(i + 1) + dx(i))) &
                             - rhocp(i)*dx(i)/dt_step
               vec_rhs(i) = -rhocp(i)*dx(i)/dt_step*T_tmp(i) &
                            - alpha*k(1)/(0.5*(dx(i)))*(T_up - T_tmp(i)) &
                            + alpha*k_itf(i)/(0.5*(dx(i + 1) + dx(i)))*(T_tmp(i) - T_tmp(i + 1)) &
                            - (1.0 - alpha)*k(1)/(0.5*(dx(i)))*T_up
            ELSE IF (i == n) THEN
               vec_lw(i - 1) = (1.0 - alpha)*k_itf(i - 1)/(0.5*(dx(i - 1) + dx(i)))
               vec_diag(i) = -(1.0 - alpha)*k_itf(i - 1)/(0.5*(dx(i - 1) + dx(i))) &
                             - (1.0 - alpha)*k(n)/(0.5*(dx(i))) &
                             - rhocp(i)*dx(i)/dt_step
               vec_rhs(i) = -rhocp(i)*dx(i)/dt_step*T_tmp(i) &
                            - alpha*k_itf(i - 1)/(0.5*(dx(i - 1) + dx(i)))*(T_tmp(i - 1) - T_tmp(i)) &
                            + alpha*k(n)/(0.5*(dx(i)))*(T_tmp(i) - T_lw) &
                            - (1.0 - alpha)*k(n)/(0.5*(dx(i)))*T_lw
            ELSE
               vec_lw(i - 1) = (1.0 - alpha)*k_itf(i - 1)/(0.5*(dx(i - 1) + dx(i)))
               vec_up(i) = (1.0 - alpha)*k_itf(i)/(0.5*(dx(i + 1) + dx(i)))
               vec_diag(i) = -(1.0 - alpha)*k_itf(i - 1)/(0.5*(dx(i - 1) + dx(i))) &
                             - (1.0 - alpha)*k_itf(i)/(0.5*(dx(i + 1) + dx(i))) &
                             - rhocp(i)*dx(i)/dt_step
               vec_rhs(i) = -rhocp(i)*dx(i)/dt_step*T_tmp(i) &
                            - alpha*k_itf(i - 1)/(0.5*(dx(i - 1) + dx(i)))*(T_tmp(i - 1) - T_tmp(i)) &
                            + alpha*k_itf(i)/(0.5*(dx(i + 1) + dx(i)))*(T_tmp(i) - T_tmp(i + 1))
            END IF
         END DO

         ! solve the tridiagonal matrix using Thomas algorithm
         CALL thomas_triMat(vec_lw, vec_diag, vec_up, vec_rhs, n, T_tmp)
         dt_remain = dt_remain - dt_step
         Qs_acc = Qs_acc + (T_up - T_tmp(1))*k(1)/(dx(1)*0.5)*dt_step
      END DO

      T_out = T_tmp
      ! Tsfc = T_out(1)
      T = T_out

      ! new way for calcualating heat storage
      ! Qs = SUM( &
      !      (([bc(1), T_out(1:n - 1)] + T_out)/2. & ! updated temperature
      !       -([bc(1), T_in(1:n - 1)] + T_in)/2) & ! initial temperature
      !      *rhocp*dx/dt)
      ! Qs = SUM( &
      !      (T_out - T_in) & ! temperature changes over dt
      !      *rhocp*dx)/dt
      ! ---Here we use the outermost surface temperatures to calculate
      ! ------the heat flux from the surface as the change of Qs for SEB
      ! ------considering there might be fluxes going out from the lower boundary
      Qs = (T_up - T_out(1))*k(1)/(dx(1)*0.5) - (T_out(n) - T_lw)*k(n)/(dx(n)*0.5)
      ! Qs = (T_out(1) - T_out(2)) * k(1) / dx(1)
      ! Qs = Qs_acc / dt
   END SUBROUTINE heatcond1d_CN

   SUBROUTINE heatcond1d_CN_dense(T, Qs, dx, dt, k, rhocp, bc)
      REAL(KIND(1D0)), INTENT(inout) :: T(:)
      REAL(KIND(1D0)), INTENT(in) :: dx(:), dt, k(:), rhocp(:), bc(2)
      REAL(KIND(1D0)), INTENT(out) :: Qs
      ! LOGICAL, INTENT(in) :: bctype(2) ! if true, use surrogate flux as boundary condition
      REAL(KIND(1D0)) :: alpha, T_lw, T_up
      REAL(KIND(1D0)) :: Qs_acc

      ! LOGICAL, INTENT(in) :: debug
      INTEGER :: i, ids_new, n, n_tmp
      INTEGER :: ratio
      REAL(KIND(1D0)), ALLOCATABLE :: T_tmp(:), k_itf(:)
      REAL(KIND(1D0)), ALLOCATABLE :: T_in(:), T_out(:)
      REAL(KIND(1D0)), ALLOCATABLE :: k_tmp(:), rhocp_tmp(:), dx_tmp(:)
      REAL(KIND(1D0)), ALLOCATABLE :: vec_lw(:), vec_up(:), vec_diag(:), vec_rhs(:)

      REAL(KIND(1D0)) :: dt_remain
      REAL(KIND(1D0)) :: dt_step
      REAL(KIND(1D0)) :: dt_step_cfl

      ratio = 3 ! ratio between the number of layers in the used dense and input coarse grids
      n = SIZE(T)
      n_tmp = n*ratio

      ALLOCATE (T_tmp(1:n_tmp)) ! temporary temperature array
      ALLOCATE (T_in(1:n)) ! initial temperature array
      ALLOCATE (k_itf(1:n_tmp - 1)) ! thermal conductivity at interfaces
      ALLOCATE (T_out(1:n)) ! output temperature array
      ALLOCATE (k_tmp(1:n_tmp))
      ALLOCATE (rhocp_tmp(1:n_tmp))
      ALLOCATE (dx_tmp(1:n_tmp))

      ALLOCATE (vec_lw(1:n_tmp - 1))
      ALLOCATE (vec_up(1:n_tmp - 1))
      ALLOCATE (vec_diag(1:n_tmp))
      ALLOCATE (vec_rhs(1:n_tmp))

      alpha = 0.5
      T_up = bc(1)
      T_lw = bc(2)

      ! save initial temperatures
      DO i = 1, n
         T_in(i) = T(i)
         T_tmp((i - 1)*ratio + 1:i*ratio) = T(i)
         k_tmp((i - 1)*ratio + 1:i*ratio) = k(i)
         rhocp_tmp((i - 1)*ratio + 1:i*ratio) = rhocp(i)
         dx_tmp((i - 1)*ratio + 1:i*ratio) = dx(i)/ratio
      END DO

      ! calculate the depth-averaged thermal conductivity
      DO i = 1, n_tmp - 1
         k_itf(i) = (k_tmp(i)*k_tmp(i + 1)*(dx_tmp(i) + dx_tmp(i + 1)))/(k_tmp(i)*dx_tmp(i + 1) + k_tmp(i + 1)*dx_tmp(i))
      END DO

      dt_remain = dt
      dt_step_cfl = 0.01*MINVAL(dx_tmp**2/(k_tmp/rhocp_tmp))
      !PRINT *, 'dt_step_cfl: ', dt_step_cfl

      Qs_acc = 0.0 ! accumulated heat storage
      DO WHILE (dt_remain > 1E-10)
         dt_step = MIN(dt_step_cfl, dt_remain)
         !T_tmp(1) = T_up
         !T_tmp(n) = T_lw
         ! set the tridiagonal matrix for 1D heat conduction solver based on Crank-Nicholson method
         DO i = 1, n_tmp
            IF (i == 1) THEN
               vec_up(i) = (1.0 - alpha)*k_itf(i)/(0.5*(dx_tmp(i + 1) + dx_tmp(i)))
               vec_diag(i) = -(1.0 - alpha)*k_tmp(1)/(0.5*(dx_tmp(i))) &
                             - (1.0 - alpha)*k_itf(i)/(0.5*(dx_tmp(i + 1) + dx_tmp(i))) &
                             - rhocp_tmp(i)*dx_tmp(i)/dt_step
               vec_rhs(i) = -rhocp_tmp(i)*dx_tmp(i)/dt_step*T_tmp(i) &
                            - alpha*k_tmp(1)/(0.5*dx_tmp(i))*(T_up - T_tmp(i)) &
                            + alpha*k_itf(i)/(0.5*(dx_tmp(i + 1) + dx_tmp(i)))*(T_tmp(i) - T_tmp(i + 1)) &
                            - (1.0 - alpha)*k_tmp(1)/(0.5*(dx_tmp(i)))*T_up
            ELSE IF (i == n_tmp) THEN
               vec_lw(i - 1) = (1.0 - alpha)*k_itf(i - 1)/(0.5*(dx_tmp(i - 1) + dx_tmp(i)))
               vec_diag(i) = -(1.0 - alpha)*k_itf(i - 1)/(0.5*(dx_tmp(i - 1) + dx_tmp(i))) &
                             - (1.0 - alpha)*k_tmp(n)/(0.5*(dx_tmp(i))) &
                             - rhocp_tmp(i)*dx_tmp(i)/dt_step
               vec_rhs(i) = -rhocp_tmp(i)*dx_tmp(i)/dt_step*T_tmp(i) &
                            - alpha*k_itf(i - 1)/(0.5*(dx_tmp(i - 1) + dx_tmp(i)))*(T_tmp(i - 1) - T_tmp(i)) &
                            + alpha*k_tmp(n)/(0.5*(dx_tmp(i)))*(T_tmp(i) - T_lw) &
                            - (1.0 - alpha)*k_tmp(n)/(0.5*(dx_tmp(i)))*T_lw
            ELSE
               vec_lw(i - 1) = (1.0 - alpha)*k_itf(i - 1)/(0.5*(dx_tmp(i - 1) + dx_tmp(i)))
               vec_up(i) = (1.0 - alpha)*k_itf(i)/(0.5*(dx_tmp(i + 1) + dx_tmp(i)))
               vec_diag(i) = -(1.0 - alpha)*k_itf(i - 1)/(0.5*(dx_tmp(i - 1) + dx_tmp(i))) &
                             - (1.0 - alpha)*k_itf(i)/(0.5*(dx_tmp(i + 1) + dx_tmp(i))) &
                             - rhocp_tmp(i)*dx_tmp(i)/dt_step
               vec_rhs(i) = -rhocp_tmp(i)*dx_tmp(i)/dt_step*T_tmp(i) &
                            - alpha*k_itf(i - 1)/(0.5*(dx_tmp(i - 1) + dx_tmp(i)))*(T_tmp(i - 1) - T_tmp(i)) &
                            + alpha*k_itf(i)/(0.5*(dx_tmp(i + 1) + dx_tmp(i)))*(T_tmp(i) - T_tmp(i + 1))
            END IF
         END DO

         ! solve the tridiagonal matrix using Thomas algorithm
         CALL thomas_triMat(vec_lw, vec_diag, vec_up, vec_rhs, n, T_tmp)
         dt_remain = dt_remain - dt_step
         Qs_acc = Qs_acc + (T_up - T_tmp(1))*k_tmp(1)/(dx_tmp(1)*0.5)*dt_step
      END DO

      DO i = 1, n
         ids_new = (i - 1)*ratio + (ratio - 1)/2 + 1
         T_out(i) = T_tmp(ids_new)
      END DO
      ! Tsfc = T_out(1)
      T = T_out

      ! new way for calcualating heat storage
      ! Qs = SUM( &
      !      (([bc(1), T_out(1:n - 1)] + T_out)/2. & ! updated temperature
      !       -([bc(1), T_in(1:n - 1)] + T_in)/2) & ! initial temperature
      !      *rhocp*dx/dt)
      Qs = SUM( &
           (T_out - T_in) & ! initial temperature
           *rhocp*dx/dt)
      ! ---Here we use the outermost surface temperatures to calculate
      ! ------the heat flux from the surface as the change of Qs for SEB
      ! ------considering there might be fluxes going out from the lower boundary
      ! Qs = (T_up - T_tmp(1))*k_tmp(1)/(dx_tmp(1)*0.5)
      ! Qs = (T_out(1) - T_out(2)) * k(1) / dx(1)
      ! Qs = Qs_acc / dt
      ! IF (debug) THEN
      !    !PRINT *, "T_up: ", T_up, "T_lw: ", T_lw
      !    !PRINT *, "T_out: ", T_out
      !    !PRINT *, "T_in: ", T_in
      !    !PRINT *, "Qs_last: ", Qs
      !    !PRINT *, "Qs_acc_avg: ", Qs_acc / dt
      ! END IF
   END SUBROUTINE heatcond1d_CN_dense

END MODULE module_phys_ehc_heatflux

! Backward compatibility alias
MODULE heatflux
   USE module_phys_ehc_heatflux
END MODULE heatflux

! Main module following naming standard: matches filename
MODULE module_phys_ehc
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
      tsfc_roof, tin_roof, temp_in_roof, k_roof, cp_roof, dz_roof, sfr_roof, & !input
      tsfc_wall, tin_wall, temp_in_wall, k_wall, cp_wall, dz_wall, sfr_wall, & !input
      tsfc_surf, tin_surf, temp_in_surf, k_surf, cp_surf, dz_surf, sfr_surf, & !input
      temp_out_roof, QS_roof, & !output
      temp_out_wall, QS_wall, & !output
      temp_out_surf, QS_surf, & !output
      QS) !output
      USE module_ctrl_const_allocate, ONLY: &
         nsurf, ndepth, &
         PavSurf, BldgSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf
      USE module_phys_ehc_heatflux, ONLY: heatcond1d_vstep, heatcond1d_CN, heatcond1d_CN_dense

      IMPLICIT NONE
      INTEGER, INTENT(in) :: tstep
      INTEGER, INTENT(in) :: nlayer ! number of vertical levels in urban canopy

      ! REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(in) :: QG_surf ! ground heat flux
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
      ! REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qg_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tsfc_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: tin_roof
      REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: sfr_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: temp_in_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: k_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: cp_roof
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth), INTENT(in) :: dz_roof
      ! input arrays: wall facets
      ! REAL(KIND(1D0)), DIMENSION(nlayer), INTENT(in) :: qg_wall
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
               ! IF (i_group == 1) THEN
               !    bc(1) = qg_roof(i_facet)
               !    debug = .TRUE.
               ! ELSE IF (i_group == 2) THEN
               !    bc(1) = qg_wall(i_facet)
               !    debug = .FALSE.
               ! ELSE IF (i_group == 3) THEN
               !    bc(1) = QG_surf(i_facet)
               !    debug = .FALSE.
               ! END IF
               ! bctype(1) = .TRUE.
               bc(1) = tsfc_cal(i_facet)
               ! bctype(1) = .FALSE.

               !TODO: should be a prescribed temperature of the innermost boundary
               bc(2) = tin_cal(i_facet)
               ! bctype(2) = .FALSE.

               ! IF (i_group == 3 .AND. i_facet == 3) THEN
               !PRINT *, i_facet, ' temp_cal before: ', temp_cal(i_facet, :)
               ! PRINT *, 'k_cal: ', k_cal(i_facet, 1:ndepth)
               ! PRINT *, 'cp_cal: ', cp_cal(i_facet, 1:ndepth)
               ! PRINT *, 'dz_cal: ', dz_cal(i_facet, 1:ndepth)
               !PRINT *, i_facet, ' bc: ', bc

               ! END IF

               IF (i_group == 3 .AND. i_facet == 1) THEN
                  ! PRINT *, i_facet, ' temp_cal after: ', temp_cal(i_facet, :)
                  ! PRINT *, '-----------------'
                  ! PRINT *, 'tsfc_cal before: ', tsfc_cal(i_facet)
                  ! PRINT *, 'QS_cal before: ', QG_surf(i_facet)
                  ! PRINT *, 'temp_cal before: ', temp_cal(i_facet, :)
                  ! PRINT *, 'k_cal: ', k_cal(i_facet, 1:ndepth)
                  ! PRINT *, 'cp_cal: ', cp_cal(i_facet, 1:ndepth)
                  ! PRINT *, 'dz_cal: ', dz_cal(i_facet, 1:ndepth)
                  ! PRINT *, 'bc: ', bc
                  ! PRINT *, ''

               END IF
               ! 1D heat conduction for finite depth layers

               IF ((i_group == 3) .AND. (i_facet == 1)) THEN
                  debug = .TRUE.
               ELSE
                  debug = .FALSE.
               END IF
               ! CALL heatcond1d_ext( &
               CALL heatcond1d_vstep( &
                  ! CALL heatcond1d_CN( &
                  ! CALL heatcond1d_CN_dense( &
                  temp_cal(i_facet, :), &
                  QS_cal(i_facet), &
                  dz_cal(i_facet, 1:ndepth), &
                  tstep*1.D0, &
                  k_cal(i_facet, 1:ndepth), &
                  cp_cal(i_facet, 1:ndepth), &
                  bc)

               ! update temperature at all inner interfaces
               ! tin_cal(i_facet, :) = temp_all_cal
               ! IF (i_group == 3 .AND. i_facet == 3) THEN
               IF ((i_group == 3) .AND. (i_facet == 1)) THEN
                  ! PRINT *, i_facet, ' temp_cal after: ', temp_cal(i_facet, :)
                  ! PRINT *, 'QS_cal after: ', QS_cal(i_facet)
                  ! PRINT *, i_facet, 'tsfc_cal after: ', tsfc_cal(i_facet)

                  ! PRINT *, 'k_cal: ', k_cal(i_facet, 1:ndepth)
                  ! PRINT *, 'cp_cal: ', cp_cal(i_facet, 1:ndepth)
                  ! PRINT *, 'dz_cal: ', dz_cal(i_facet, 1:ndepth)
                  ! PRINT *, 'bc: ', bc
                  ! PRINT *, ''

               END IF
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
               !CALL heatcond1d_vstep( &
               CALL heatcond1d_CN( &
                  ! CALL heatcond1d_CN_dense( &
                  temp_cal(i_facet, :), &
                  QS_cal(i_facet), &
                  dz_cal(i_facet, 1:ndepth), &
                  tstep*1.D0, &
                  k_cal(i_facet, 1:ndepth), &
                  cp_cal(i_facet, 1:ndepth), &
                  bc)

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
      !PRINT *, 'QS_surf in ESTM_ehc', QS_surf

   END SUBROUTINE EHC

END MODULE module_phys_ehc

! Backward compatibility alias
MODULE EHC_module
   USE module_phys_ehc
END MODULE EHC_module
