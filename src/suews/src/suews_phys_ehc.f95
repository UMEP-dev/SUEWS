! EHC heatflux calculations module
MODULE module_phys_ehc_heatflux
   USE, INTRINSIC :: ieee_arithmetic, ONLY: IEEE_IS_FINITE
   USE module_ctrl_error_state, ONLY: set_supy_error
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

   SUBROUTINE heatcond1d_vstep(T, Qs, dx, dt, k, rhocp, bc, q_top, q_bottom)
      REAL(KIND(1D0)), INTENT(inout) :: T(:)
      REAL(KIND(1D0)), INTENT(in) :: dx(:), dt, k(:), rhocp(:), bc(2)
      REAL(KIND(1D0)), INTENT(out) :: Qs
      ! time-mean conductive fluxes over dt: into the slab at the top face and
      ! out of the slab at the bottom face [W m-2]; dU/dt = q_top - q_bottom
      REAL(KIND(1D0)), INTENT(out), OPTIONAL :: q_top, q_bottom
      REAL(KIND(1D0)) :: q_top_acc, q_bottom_acc
      INTEGER :: i, n
      REAL(KIND(1D0)), ALLOCATABLE :: g_itf(:), T_in(:), T_temp(:), dt_limit(:)
      REAL(KIND(1D0)) :: dt_remain
      REAL(KIND(1D0)) :: dt_step
      REAL(KIND(1D0)) :: dt_step_cfl
      REAL(KIND(1D0)) :: g_up, g_down, g_left, g_right
      REAL(KIND(1D0)) :: denom

      n = SIZE(T)
      Qs = 0.0D0
      IF (PRESENT(q_top)) q_top = 0.0D0
      IF (PRESENT(q_bottom)) q_bottom = 0.0D0
      IF (n <= 0 .OR. dt <= 0.0D0) RETURN
      IF (ANY(dx <= 0.0D0) .OR. ANY(k <= 0.0D0) .OR. ANY(rhocp <= 0.0D0)) RETURN

      ALLOCATE (g_itf(MAX(1, n - 1)), T_in(n), T_temp(n), dt_limit(n))
      T_in = T

      g_up = 2.0D0*k(1)/dx(1)
      g_down = 2.0D0*k(n)/dx(n)
      DO i = 1, n - 1
         denom = k(i + 1)*dx(i) + k(i)*dx(i + 1)
         g_itf(i) = 2.0D0*k(i)*k(i + 1)/denom
      END DO

      DO i = 1, n
         IF (i == 1) THEN
            g_left = g_up
         ELSE
            g_left = g_itf(i - 1)
         END IF
         IF (i == n) THEN
            g_right = g_down
         ELSE
            g_right = g_itf(i)
         END IF
         dt_limit(i) = rhocp(i)*dx(i)/(g_left + g_right)
      END DO

      ! Explicit finite-volume update with Dirichlet boundary temperatures.
      ! Qs is the material heat-content tendency, matching DeltaQS storage.
      dt_remain = dt
      dt_step_cfl = 0.45D0*MINVAL(dt_limit)
      q_top_acc = 0.0D0
      q_bottom_acc = 0.0D0
      DO WHILE (dt_remain > 1.0D-10)
         dt_step = MIN(dt_step_cfl, dt_remain)
         q_top_acc = q_top_acc + dt_step*g_up*(bc(1) - T(1))
         q_bottom_acc = q_bottom_acc + dt_step*g_down*(T(n) - bc(2))
         DO i = 1, n
            IF (i == 1) THEN
               g_left = g_up*(bc(1) - T(i))
            ELSE
               g_left = g_itf(i - 1)*(T(i - 1) - T(i))
            END IF
            IF (i == n) THEN
               g_right = g_down*(bc(2) - T(i))
            ELSE
               g_right = g_itf(i)*(T(i + 1) - T(i))
            END IF
            T_temp(i) = T(i) + dt_step*(g_left + g_right)/(rhocp(i)*dx(i))
         END DO
         T = T_temp
         dt_remain = dt_remain - dt_step
      END DO
      Qs = SUM((T - T_in)*rhocp*dx)/dt
      IF (PRESENT(q_top)) q_top = q_top_acc/dt
      IF (PRESENT(q_bottom)) q_bottom = q_bottom_acc/dt
   END SUBROUTINE heatcond1d_vstep

   SUBROUTINE heatcond1d_gstep(T, Qs, heat_cap, g_face, dt, bc, q_top, q_bottom)
      REAL(KIND(1D0)), INTENT(inout) :: T(:)
      REAL(KIND(1D0)), INTENT(in) :: heat_cap(:), g_face(:), dt, bc(2)
      REAL(KIND(1D0)), INTENT(out) :: Qs
      REAL(KIND(1D0)), INTENT(out), OPTIONAL :: q_top, q_bottom
      INTEGER :: i, n
      REAL(KIND(1D0)), ALLOCATABLE :: T_in(:), T_temp(:), dt_limit(:)
      REAL(KIND(1D0)) :: dt_remain
      REAL(KIND(1D0)) :: dt_step
      REAL(KIND(1D0)) :: dt_step_cfl
      REAL(KIND(1D0)) :: flux_left, flux_right
      REAL(KIND(1D0)) :: q_top_acc, q_bottom_acc

      n = SIZE(T)
      Qs = 0.0D0
      IF (PRESENT(q_top)) q_top = 0.0D0
      IF (PRESENT(q_bottom)) q_bottom = 0.0D0
      IF (n <= 0 .OR. dt <= 0.0D0) RETURN
      IF (SIZE(heat_cap) /= n .OR. SIZE(g_face) /= n + 1) RETURN
      IF (ANY(heat_cap <= 0.0D0) .OR. ANY(g_face < 0.0D0)) RETURN

      ALLOCATE (T_in(n), T_temp(n), dt_limit(n))
      T_in = T

      DO i = 1, n
         IF (g_face(i) + g_face(i + 1) <= 0.0D0) RETURN
         dt_limit(i) = heat_cap(i)/(g_face(i) + g_face(i + 1))
      END DO

      ! Explicit finite-volume update for a lumped slab with pre-aggregated
      ! plan-area heat capacities and interface conductances.
      dt_remain = dt
      dt_step_cfl = 0.45D0*MINVAL(dt_limit)
      q_top_acc = 0.0D0
      q_bottom_acc = 0.0D0
      DO WHILE (dt_remain > 1.0D-10)
         dt_step = MIN(dt_step_cfl, dt_remain)
         q_top_acc = q_top_acc + dt_step*g_face(1)*(bc(1) - T(1))
         q_bottom_acc = q_bottom_acc + dt_step*g_face(n + 1)*(T(n) - bc(2))
         DO i = 1, n
            IF (i == 1) THEN
               flux_left = g_face(i)*(bc(1) - T(i))
            ELSE
               flux_left = g_face(i)*(T(i - 1) - T(i))
            END IF
            IF (i == n) THEN
               flux_right = g_face(i + 1)*(bc(2) - T(i))
            ELSE
               flux_right = g_face(i + 1)*(T(i + 1) - T(i))
            END IF
            T_temp(i) = T(i) + dt_step*(flux_left + flux_right)/heat_cap(i)
         END DO
         T = T_temp
         dt_remain = dt_remain - dt_step
      END DO
      Qs = SUM((T - T_in)*heat_cap)/dt
      IF (PRESENT(q_top)) q_top = q_top_acc/dt
      IF (PRESENT(q_bottom)) q_bottom = q_bottom_acc/dt
   END SUBROUTINE heatcond1d_gstep

   SUBROUTINE heatcond1d_seb_implicit( &
      T, heat_cap, g_face, dt, t_bottom, &
      g_top, a_top, b_top, &
      ts, q_top_each, q_top, q_bottom, dudt)
      ! Backward-Euler conduction through one column whose top face is closed
      ! by the linearised surface energy balance of m surface nodes rather than
      ! by a prescribed surface temperature.
      !
      ! Each surface node j has zero heat capacity and satisfies
      !    a_top(j) - b_top(j)*ts(j) - g_top(j)*(ts(j) - T(1)) = 0,
      ! i.e. Q* - QE - rho*cp*(Ts - Ta)/r_a - G = 0 linearised about the
      ! current Tsfc iterate Ts* with dQ*/dTs = -R1 (R1 >= 0):
      !    b_top = rho*cp/r_a + R1,  a_top = Q*(Ts*) - QE + R1*Ts* + rho*cp*Ta/r_a.
      ! No surface node carries QF; indoor heat enters through the lower boundary.
      ! All of g_top, a_top and b_top are per unit area of the column (for a
      ! plan-area slab, already weighted by surface fraction), so m = 1 is a
      ! single facet and m > 1 is several surfaces sharing one slab.
      ! Eliminating the surface nodes gives a Robin condition on layer 1:
      !    q_top = sum_j g_top(j)*b_top(j)/(g_top(j) + b_top(j)) * (a_top(j)/b_top(j) - T(1)),
      ! and the layers are then one tridiagonal solve. All fluxes are end-of-step
      ! (backward Euler), so dU/dt = q_top - q_bottom and the surface balance
      ! closes exactly for the linearised source.
      !
      ! T(n):           layer temperatures, in: start of step, out: end of step
      ! heat_cap(n):    layer heat capacity per unit area [J m-2 K-1]
      ! g_face(n+1):    face conductances [W m-2 K-1]; g_face(1) is unused (the
      !                 top faces are g_top), g_face(n+1) couples to t_bottom
      !                 (0 gives an insulated lower face)
      ! ts(m):          end-of-step surface temperatures
      ! q_top_each(m):  conductive flux from surface node j into layer 1
      ! q_top, q_bottom, dudt: column totals [W m-2]
      IMPLICIT NONE
      REAL(KIND(1D0)), DIMENSION(:), INTENT(INOUT) :: T
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: heat_cap, g_face
      REAL(KIND(1D0)), INTENT(IN) :: dt, t_bottom
      REAL(KIND(1D0)), DIMENSION(:), INTENT(IN) :: g_top, a_top, b_top
      REAL(KIND(1D0)), DIMENSION(:), INTENT(OUT) :: ts, q_top_each
      REAL(KIND(1D0)), INTENT(OUT) :: q_top, q_bottom, dudt

      INTEGER :: n, m, i, j
      REAL(KIND(1D0)), DIMENSION(SIZE(T)) :: T_in, lw, diag, up, rhs, T_out
      REAL(KIND(1D0)) :: g_robin, s_robin, denom

      n = SIZE(T)
      m = SIZE(g_top)
      ! Safe outputs on failure; the caller propagates the fatal error and
      ! retains the incoming layer temperatures.
      ts = -999.0D0
      q_top_each = -999.0D0
      q_top = -999.0D0
      q_bottom = -999.0D0
      dudt = -999.0D0
      IF (n <= 0 .OR. m <= 0 .OR. .NOT. IEEE_IS_FINITE(dt) .OR. dt <= 0.0D0) THEN
         CALL set_supy_error(115, 'heatcond1d_seb_implicit: column sizes and timestep must be positive and finite')
         RETURN
      END IF
      IF (SIZE(heat_cap) /= n .OR. SIZE(g_face) /= n + 1 .OR. SIZE(a_top) /= m .OR. SIZE(b_top) /= m &
          .OR. SIZE(ts) /= m .OR. SIZE(q_top_each) /= m) THEN
         CALL set_supy_error(115, 'heatcond1d_seb_implicit: inconsistent column or surface array sizes')
         RETURN
      END IF
      IF (ANY(.NOT. IEEE_IS_FINITE(heat_cap)) .OR. ANY(heat_cap <= 0.0D0) &
          .OR. ANY(.NOT. IEEE_IS_FINITE(g_face)) .OR. ANY(g_face < 0.0D0) &
          .OR. ANY(.NOT. IEEE_IS_FINITE(g_top)) .OR. ANY(g_top < 0.0D0) &
          .OR. ANY(.NOT. IEEE_IS_FINITE(b_top)) .OR. ANY(b_top < 0.0D0) &
          .OR. ANY(.NOT. IEEE_IS_FINITE(a_top)) .OR. ANY(.NOT. IEEE_IS_FINITE(T)) &
          .OR. .NOT. IEEE_IS_FINITE(t_bottom)) THEN
         CALL set_supy_error(115, 'heatcond1d_seb_implicit: invalid heat capacity, conductance or temperature')
         RETURN
      END IF

      T_in = T
      ! Robin coefficients of the eliminated surface nodes
      g_robin = 0.0D0
      s_robin = 0.0D0
      DO j = 1, m
         denom = g_top(j) + b_top(j)
         IF (denom > 0.0D0) THEN
            g_robin = g_robin + g_top(j)*b_top(j)/denom
            s_robin = s_robin + g_top(j)*a_top(j)/denom
         END IF
      END DO

      lw = 0.0D0
      up = 0.0D0
      DO i = 1, n
         diag(i) = heat_cap(i)/dt + g_face(i + 1)
         rhs(i) = heat_cap(i)/dt*T_in(i)
         IF (i == 1) THEN
            diag(i) = diag(i) + g_robin
            rhs(i) = rhs(i) + s_robin
         ELSE
            diag(i) = diag(i) + g_face(i)
            lw(i - 1) = -g_face(i)
         END IF
         IF (i < n) THEN
            up(i) = -g_face(i + 1)
         ELSE
            rhs(i) = rhs(i) + g_face(n + 1)*t_bottom
         END IF
      END DO

      IF (n == 1) THEN
         T_out(1) = rhs(1)/diag(1)
      ELSE
         CALL thomas_triMat(lw, diag, up, rhs, n, T_out)
      END IF
      T = T_out

      DO j = 1, m
         denom = g_top(j) + b_top(j)
         IF (denom > 0.0D0) THEN
            ts(j) = (a_top(j) + g_top(j)*T(1))/denom
         ELSE
            ts(j) = T(1)
         END IF
         q_top_each(j) = g_top(j)*(ts(j) - T(1))
      END DO
      q_top = SUM(q_top_each)
      q_bottom = g_face(n + 1)*(T(n) - t_bottom)
      dudt = SUM(heat_cap*(T - T_in))/dt
   END SUBROUTINE heatcond1d_seb_implicit

   RECURSIVE SUBROUTINE heatcond1d_CN(T, Qs, dx, dt, k, rhocp, bc, q_bottom)
      REAL(KIND(1D0)), INTENT(inout) :: T(:)
      REAL(KIND(1D0)), INTENT(in) :: dx(:), dt, k(:), rhocp(:), bc(2)
      ! REAL(KIND(1D0)), INTENT(out) :: Qs, Tsfc
      REAL(KIND(1D0)), INTENT(out) :: Qs
      ! diagnostic only: time mean of the end-of-substep conductive flux out
      ! of the lower face [W m-2] (does not affect T or Qs)
      REAL(KIND(1D0)), INTENT(out), OPTIONAL :: q_bottom
      REAL(KIND(1D0)) :: q_bottom_acc
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
      q_bottom_acc = 0.0D0
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
         q_bottom_acc = q_bottom_acc + (T_tmp(n) - T_lw)*k(n)/(dx(n)*0.5D0)*dt_step
      END DO
      IF (PRESENT(q_bottom)) q_bottom = q_bottom_acc/dt

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
      Qs = SUM( &
           (T_out - T_in) &
           *rhocp*dx/dt)
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
         CALL thomas_triMat(vec_lw, vec_diag, vec_up, vec_rhs, n_tmp, T_tmp)
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
   USE, INTRINSIC :: ieee_arithmetic, ONLY: IEEE_IS_FINITE
   USE module_ctrl_const_allocate, ONLY: nsurf
   USE module_ctrl_error_state, ONLY: set_supy_error, supy_error_flag
   IMPLICIT NONE

   ! ---------------------------------------------------------------------------
   ! Implicit surface coupling of EHC (ehc_surface_coupling = implicit).
   ! Passing this argument to EHC selects the implicit solver, so the inputs
   ! cannot be supplied in part. Inputs, per unit facet area: each facet's
   ! surface energy balance linearised about the current Tsfc iterate,
   !    a - b*Tsfc - G = 0   (see heatcond1d_seb_implicit),
   ! and the lower-boundary temperatures of the columns. Outputs: the solved
   ! surface temperatures.
   ! ---------------------------------------------------------------------------
   TYPE, PUBLIC :: dts_ehc_implicit
      REAL(KIND(1D0)), DIMENSION(nsurf) :: a_surf = 0.0D0 ! [W m-2]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: b_surf = 0.0D0 ! [W m-2 K-1]
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: a_roof, b_roof ! (nlayer)
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: a_wall, b_wall ! (nlayer)
      ! lower boundary of each ground-surface column (deep soil) [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: t_deep_surf = 0.0D0
      ! indoor boundary of the roof and wall columns and of the building
      ! surface on the plan-area slab [degC]
      REAL(KIND(1D0)) :: t_indoor = 20.0D0
      ! ISO 6946:2017 Table 7; effective indoor film includes convection and
      ! radiation. Zero selects the former bare half-cell boundary [m2 K W-1].
      REAL(KIND(1D0)) :: r_indoor_roof = 0.10D0
      REAL(KIND(1D0)) :: r_indoor_wall = 0.13D0
      ! solved surface temperatures [degC]
      REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc_surf = 0.0D0
      REAL(KIND(1D0)), DIMENSION(:), ALLOCATABLE :: tsfc_roof, tsfc_wall ! (nlayer)
   END TYPE dts_ehc_implicit

CONTAINS

   SUBROUTINE ehc_init_profile(dz, k, t_outer, t_inner, r_inner, temp)
      ! Cell-centre steady profile: outer face prescribed, indoor film in
      ! series with all material layers. Temperature is linear in cumulative
      ! resistance, not geometric depth. Caller selects fresh implicit starts
      ! only; invalid/absent material columns retain their configured state.
      REAL(KIND(1D0)), INTENT(IN) :: dz(:), k(:), t_outer, t_inner, r_inner
      REAL(KIND(1D0)), INTENT(INOUT) :: temp(:)
      REAL(KIND(1D0)) :: resist(SIZE(dz)), r_total, r_centre
      INTEGER :: i

      IF (SIZE(temp) /= SIZE(dz) .OR. SIZE(k) /= SIZE(dz)) THEN
         CALL set_supy_error(115, 'ehc_init_profile: inconsistent column sizes')
         RETURN
      END IF
      IF (.NOT. IEEE_IS_FINITE(r_inner) .OR. r_inner < 0.0D0) THEN
         CALL set_supy_error(115, 'ehc_init_profile: indoor resistance must be finite and non-negative')
         RETURN
      END IF
      IF (SIZE(dz) == 0) RETURN
      IF (ANY(.NOT. IEEE_IS_FINITE(dz)) .OR. ANY(.NOT. IEEE_IS_FINITE(k))) RETURN
      IF (ANY(dz <= 0.0D0) .OR. ANY(k <= 0.0D0)) RETURN
      resist = dz/k
      r_total = SUM(resist) + r_inner
      r_centre = 0.0D0
      DO i = 1, SIZE(dz)
         temp(i) = t_outer + (t_inner - t_outer)*(r_centre + 0.5D0*resist(i))/r_total
         r_centre = r_centre + resist(i)
      END DO
   END SUBROUTINE ehc_init_profile
   ! ===============================================================================================
   ! extended ESTM, TS 20 Jan 2022
   ! renamed to EHC (explicit heat conduction) to avoid confusion with ESTM
   ! EHC can calculate storage heat flux as either a coarse plan-area slab
   ! for OHM-like benchmarks or as facet-resolved roof/wall/surface conduction.
   SUBROUTINE EHC( &
      tstep, & !input
      nlayer, use_lumped_slab, &
      tsfc_roof, tin_roof, temp_in_roof, k_roof, cp_roof, dz_roof, sfr_roof, & !input
      tsfc_wall, tin_wall, temp_in_wall, k_wall, cp_wall, dz_wall, sfr_wall, & !input
      tsfc_surf, tin_surf, temp_in_surf, k_surf, cp_surf, dz_surf, sfr_surf, & !input
      temp_out_roof, QS_roof, & !output
      temp_out_wall, QS_wall, & !output
      temp_out_surf, temp_fast_surf, temp_slow_surf, QS_surf, & !output
      QS, & !output
      dudt_grid, q_bottom_grid, q_indoor_grid, & !output: diagnostics
      errorState, & ! inout: per-grid warning log (GH#1737)
      implicit_seb) ! optional inout: implicit surface coupling
      USE module_ctrl_const_allocate, ONLY: &
         nsurf, ndepth, &
         PavSurf, BldgSurf, ConifSurf, DecidSurf, GrassSurf, BSoilSurf, WaterSurf
      USE module_ctrl_type, ONLY: error_state
      USE module_phys_ehc_heatflux, ONLY: heatcond1d_vstep, heatcond1d_gstep, heatcond1d_CN, heatcond1d_CN_dense, &
                                          heatcond1d_seb_implicit

      IMPLICIT NONE
      INTEGER, INTENT(in) :: tstep
      INTEGER, INTENT(in) :: nlayer ! number of vertical levels in urban canopy
      LOGICAL, INTENT(in) :: use_lumped_slab
      TYPE(error_state), INTENT(INOUT) :: errorState ! per-grid warning log (GH#1737)

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
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(inout) :: temp_fast_surf ! fast EHC surface thermal state
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(inout) :: temp_slow_surf ! slow EHC surface thermal state

      ! grid aggregated results
      REAL(KIND(1D0)), INTENT(out) :: QS

      ! Diagnostics, plan-area weighted over the grid [W m-2]; -999 where a
      ! route does not provide them:
      !   dudt_grid      heat-content tendency of all conduction columns
      !   q_bottom_grid  conductive flux leaving the columns at their lower
      !                  boundaries (deep soil and indoor), positive downward
      !   q_indoor_grid  conductive flux from the indoor boundary into the
      !                  building columns (roofs and walls, or the building
      !                  surface on the plan-area slab)
      REAL(KIND(1D0)), INTENT(out) :: dudt_grid, q_bottom_grid, q_indoor_grid

      ! Implicit surface coupling: when present, Tsfc is solved here together
      ! with the layer temperatures and returned in implicit_seb%tsfc_*, the
      ! lower boundaries come from implicit_seb, and QS is the conductive flux
      ! into the material at the surface (q_top). Water on the facet route
      ! keeps heatcond1d_CN and the driver's Tsfc update.
      TYPE(dts_ehc_implicit), INTENT(inout), OPTIONAL :: implicit_seb

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
      INTEGER :: i_facet, i_group, nfacet, i_depth, env_status

      ! settings for boundary conditions
      REAL(KIND(1D0)), DIMENSION(2) :: bc

      ! use temperature as boundary conditions
      LOGICAL, DIMENSION(2) :: bctype
      LOGICAL :: debug

      ! use finite depth heat conduction solver
      LOGICAL :: use_heatcond1d, use_heatcond1d_water
      LOGICAL :: use_zero_flux_bottom, use_parallel_surfaces
      CHARACTER(LEN=32) :: env_value
      REAL(KIND(1D0)), DIMENSION(ndepth) :: temp_lumped, cap_lumped
      REAL(KIND(1D0)), DIMENSION(ndepth) :: temp_fast_lumped, temp_slow_lumped
      REAL(KIND(1D0)), DIMENSION(ndepth) :: cap_fast_lumped, cap_slow_lumped
      REAL(KIND(1D0)), DIMENSION(ndepth) :: temp_branch, cap_branch
      REAL(KIND(1D0)), DIMENSION(ndepth + 1) :: g_lumped
      REAL(KIND(1D0)), DIMENSION(ndepth + 1) :: g_fast_lumped, g_slow_lumped
      REAL(KIND(1D0)), DIMENSION(ndepth + 1) :: g_branch
      REAL(KIND(1D0)) :: tsfc_lumped, tin_lumped, qs_lumped, qs_per_surface, bottom_g_scale
      REAL(KIND(1D0)) :: q_top_lumped, q_bottom_lumped
      REAL(KIND(1D0)) :: surface_weight, layer_cap, layer_temp_cap, face_resistance
      REAL(KIND(1D0)) :: layer_fast_temp_cap, layer_slow_temp_cap
      REAL(KIND(1D0)) :: qs_fast_lumped, qs_slow_lumped
      REAL(KIND(1D0)) :: q_top_fast_lumped, q_bottom_fast_lumped
      REAL(KIND(1D0)) :: q_top_slow_lumped, q_bottom_slow_lumped
      REAL(KIND(1D0)) :: dual_fast_weight, dual_slow_weight
      REAL(KIND(1D0)) :: fast_cap_scale, slow_cap_scale, fast_g_scale, slow_g_scale
      REAL(KIND(1D0)) :: lumped_layer_temp_for_alloc
      REAL(KIND(1D0)) :: state_layer_temp, state_top_gradient, state_g_scale
      REAL(KIND(1D0)) :: state_warming_g_boost, state_cooling_g_damp
      REAL(KIND(1D0)) :: state_gradient_scale, state_min_g_scale
      LOGICAL :: valid_lumped
      LOGICAL, DIMENSION(nsurf) :: valid_lumped_surf
      LOGICAL :: use_dual_timescale, use_qs_surface_gradient_alloc, allocation_success
      LOGICAL :: use_state_admittance
      LOGICAL :: use_ehc_experimental_controls
      ! per-facet diagnostics
      REAL(KIND(1D0)) :: q_top_facet, q_bottom_facet, dudt_facet, weight_facet
      ! implicit surface coupling
      LOGICAL :: use_implicit
      INTEGER :: n_top, i_top
      INTEGER, DIMENSION(nsurf) :: top_surface
      REAL(KIND(1D0)), DIMENSION(nsurf) :: g_top_imp, a_top_imp, b_top_imp, ts_imp, q_each_imp
      REAL(KIND(1D0)), DIMENSION(ndepth + 1) :: g_imp
      REAL(KIND(1D0)), DIMENSION(ndepth) :: cap_imp
      REAL(KIND(1D0)) :: q_bottom_imp, dudt_imp, t_bottom_imp, g_bottom_bldg
      REAL(KIND(1D0)), DIMENSION(nsurf) :: tin_surf_use

      dudt_grid = -999.0D0
      q_bottom_grid = -999.0D0
      q_indoor_grid = -999.0D0
      tin_surf_use = tin_surf

      use_implicit = PRESENT(implicit_seb)
      IF (use_implicit) THEN
         IF (.NOT. IEEE_IS_FINITE(implicit_seb%r_indoor_roof) .OR. &
             .NOT. IEEE_IS_FINITE(implicit_seb%r_indoor_wall) .OR. &
             implicit_seb%r_indoor_roof < 0.0D0 .OR. implicit_seb%r_indoor_wall < 0.0D0) THEN
            CALL set_supy_error(115, 'EHC: indoor resistance must be finite and non-negative')
            CALL ehc_fail_outputs()
            RETURN
         END IF
         IF (.NOT. (ALLOCATED(implicit_seb%a_roof) .AND. ALLOCATED(implicit_seb%b_roof) &
                    .AND. ALLOCATED(implicit_seb%a_wall) .AND. ALLOCATED(implicit_seb%b_wall) &
                    .AND. ALLOCATED(implicit_seb%tsfc_roof) .AND. ALLOCATED(implicit_seb%tsfc_wall))) THEN
            CALL set_supy_error(115, 'EHC: implicit coupling arrays for roofs and walls are not allocated')
            CALL ehc_fail_outputs()
            RETURN
         END IF
         IF (MIN(SIZE(implicit_seb%a_roof), SIZE(implicit_seb%b_roof), SIZE(implicit_seb%a_wall), &
                 SIZE(implicit_seb%b_wall), SIZE(implicit_seb%tsfc_roof), SIZE(implicit_seb%tsfc_wall)) < nlayer) THEN
            CALL set_supy_error(115, 'EHC: implicit coupling arrays are shorter than nlayer')
            CALL ehc_fail_outputs()
            RETURN
         END IF
         implicit_seb%tsfc_surf = tsfc_surf
         implicit_seb%tsfc_roof(1:nlayer) = tsfc_roof
         implicit_seb%tsfc_wall(1:nlayer) = tsfc_wall
         ! ground columns end at the deep-soil temperature, the building
         ! surface of the plan-area slab at the indoor temperature
         tin_surf_use = implicit_seb%t_deep_surf
         tin_surf_use(BldgSurf) = implicit_seb%t_indoor
      END IF

      QS = 0.0D0
      QS_surf = 0.0D0
      QS_roof = 0.0D0
      QS_wall = 0.0D0
      temp_out_surf = temp_in_surf
      temp_out_roof = temp_in_roof
      temp_out_wall = temp_in_wall

      use_ehc_experimental_controls = (.NOT. use_implicit) .AND. ehc_experimental_controls_enabled()
      use_zero_flux_bottom = .FALSE.
      use_parallel_surfaces = .FALSE.
      bottom_g_scale = 1.0D0
      use_dual_timescale = .FALSE.
      dual_fast_weight = 0.35D0
      dual_slow_weight = 1.0D0 - dual_fast_weight
      fast_cap_scale = 0.35D0
      slow_cap_scale = 1.25D0
      fast_g_scale = 1.75D0
      slow_g_scale = 0.75D0
      use_state_admittance = .FALSE.
      state_warming_g_boost = 0.50D0
      state_cooling_g_damp = 0.35D0
      state_gradient_scale = 2.0D0
      state_min_g_scale = 0.35D0
      use_qs_surface_gradient_alloc = .FALSE.

      IF (use_ehc_experimental_controls) THEN
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_ZERO_FLUX_BOTTOM", env_value, STATUS=env_status)
         IF (env_status == 0) THEN
            SELECT CASE (TRIM(ADJUSTL(env_value)))
            CASE ("1", "true", "TRUE", "True", "yes", "YES", "Yes")
               use_zero_flux_bottom = .TRUE.
            END SELECT
         END IF
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_PARALLEL_SURFACES", env_value, STATUS=env_status)
         IF (env_status == 0) THEN
            SELECT CASE (TRIM(ADJUSTL(env_value)))
            CASE ("1", "true", "TRUE", "True", "yes", "YES", "Yes")
               use_parallel_surfaces = .TRUE.
            END SELECT
         END IF
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_BOTTOM_G_SCALE", env_value, STATUS=env_status)
         IF (env_status == 0 .AND. LEN_TRIM(env_value) > 0) THEN
            READ (env_value, *, IOSTAT=env_status) bottom_g_scale
            IF (env_status /= 0) bottom_g_scale = 1.0D0
            bottom_g_scale = MAX(0.0D0, bottom_g_scale)
         END IF
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_DUAL_TIMESCALE", env_value, STATUS=env_status)
         IF (env_status == 0) THEN
            SELECT CASE (TRIM(ADJUSTL(env_value)))
            CASE ("1", "true", "TRUE", "True", "yes", "YES", "Yes")
               use_dual_timescale = .TRUE.
            END SELECT
         END IF
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_FAST_WEIGHT", env_value, STATUS=env_status)
         IF (env_status == 0 .AND. LEN_TRIM(env_value) > 0) THEN
            READ (env_value, *, IOSTAT=env_status) dual_fast_weight
            IF (env_status /= 0) dual_fast_weight = 0.35D0
         END IF
         dual_fast_weight = MAX(0.0D0, MIN(1.0D0, dual_fast_weight))
         dual_slow_weight = 1.0D0 - dual_fast_weight
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_FAST_CAP_SCALE", env_value, STATUS=env_status)
         IF (env_status == 0 .AND. LEN_TRIM(env_value) > 0) THEN
            READ (env_value, *, IOSTAT=env_status) fast_cap_scale
            IF (env_status /= 0) fast_cap_scale = 0.35D0
         END IF
         fast_cap_scale = MAX(0.05D0, fast_cap_scale)
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_SLOW_CAP_SCALE", env_value, STATUS=env_status)
         IF (env_status == 0 .AND. LEN_TRIM(env_value) > 0) THEN
            READ (env_value, *, IOSTAT=env_status) slow_cap_scale
            IF (env_status /= 0) slow_cap_scale = 1.25D0
         END IF
         slow_cap_scale = MAX(0.05D0, slow_cap_scale)
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_FAST_G_SCALE", env_value, STATUS=env_status)
         IF (env_status == 0 .AND. LEN_TRIM(env_value) > 0) THEN
            READ (env_value, *, IOSTAT=env_status) fast_g_scale
            IF (env_status /= 0) fast_g_scale = 1.75D0
         END IF
         fast_g_scale = MAX(0.05D0, fast_g_scale)
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_SLOW_G_SCALE", env_value, STATUS=env_status)
         IF (env_status == 0 .AND. LEN_TRIM(env_value) > 0) THEN
            READ (env_value, *, IOSTAT=env_status) slow_g_scale
            IF (env_status /= 0) slow_g_scale = 0.75D0
         END IF
         slow_g_scale = MAX(0.05D0, slow_g_scale)
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_STATE_ADMITTANCE", env_value, STATUS=env_status)
         IF (env_status == 0) THEN
            SELECT CASE (TRIM(ADJUSTL(env_value)))
            CASE ("1", "true", "TRUE", "True", "yes", "YES", "Yes")
               use_state_admittance = .TRUE.
            END SELECT
         END IF
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_WARMING_G_BOOST", env_value, STATUS=env_status)
         IF (env_status == 0 .AND. LEN_TRIM(env_value) > 0) THEN
            READ (env_value, *, IOSTAT=env_status) state_warming_g_boost
            IF (env_status /= 0) state_warming_g_boost = 0.50D0
         END IF
         state_warming_g_boost = MAX(0.0D0, MIN(5.0D0, state_warming_g_boost))
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_COOLING_G_DAMP", env_value, STATUS=env_status)
         IF (env_status == 0 .AND. LEN_TRIM(env_value) > 0) THEN
            READ (env_value, *, IOSTAT=env_status) state_cooling_g_damp
            IF (env_status /= 0) state_cooling_g_damp = 0.35D0
         END IF
         state_cooling_g_damp = MAX(0.0D0, MIN(0.95D0, state_cooling_g_damp))
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_GRADIENT_SCALE", env_value, STATUS=env_status)
         IF (env_status == 0 .AND. LEN_TRIM(env_value) > 0) THEN
            READ (env_value, *, IOSTAT=env_status) state_gradient_scale
            IF (env_status /= 0) state_gradient_scale = 2.0D0
         END IF
         state_gradient_scale = MAX(0.10D0, state_gradient_scale)
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_MIN_G_SCALE", env_value, STATUS=env_status)
         IF (env_status == 0 .AND. LEN_TRIM(env_value) > 0) THEN
            READ (env_value, *, IOSTAT=env_status) state_min_g_scale
            IF (env_status /= 0) state_min_g_scale = 0.35D0
         END IF
         state_min_g_scale = MAX(0.05D0, MIN(1.0D0, state_min_g_scale))
         CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_QS_SURF_ALLOC", env_value, STATUS=env_status)
         IF (env_status == 0) THEN
            SELECT CASE (TRIM(ADJUSTL(env_value)))
            CASE ("conductance_gradient", "CONDUCTANCE_GRADIENT", &
                  "gradient", "GRADIENT", "1", "true", "TRUE", &
                  "True", "yes", "YES", "Yes")
               use_qs_surface_gradient_alloc = .TRUE.
            END SELECT
         END IF
      END IF

      ! Validate all layers of an active implicit column before any division.
      ! A wholly absent column still uses the existing zero-conduction path.
      IF (use_implicit) THEN
         IF (.NOT. use_lumped_slab) THEN
            DO i_facet = 1, nlayer
               CALL check_active_column(dz_roof(i_facet, :), k_roof(i_facet, :), cp_roof(i_facet, :))
               CALL check_active_column(dz_wall(i_facet, :), k_wall(i_facet, :), cp_wall(i_facet, :))
            END DO
         END IF
         DO i_facet = 1, nsurf
            IF (.NOT. use_lumped_slab .AND. i_facet == BldgSurf) CYCLE
            CALL check_active_column(dz_surf(i_facet, :), k_surf(i_facet, :), cp_surf(i_facet, :))
         END DO
         IF (supy_error_flag()) THEN
            CALL ehc_fail_outputs()
            RETURN
         END IF
      END IF

      IF (use_lumped_slab) THEN
         ! Coarse plan-area slab: aggregate only the standard SUEWS surface fractions.
         ! This keeps EHC comparable to OHM before adding facet-resolved complexity.
         surface_weight = 0.0D0
         tsfc_lumped = 0.0D0
         tin_lumped = 0.0D0
         g_lumped = 0.0D0
         cap_lumped = 0.0D0
         temp_lumped = 0.0D0
         valid_lumped = .TRUE.
         valid_lumped_surf = .FALSE.
         DO i_facet = 1, nsurf
            IF (sfr_surf(i_facet) > 0.0D0) THEN
               valid_lumped_surf(i_facet) = .TRUE.
               DO i_depth = 1, ndepth
                  IF (dz_surf(i_facet, i_depth) <= 0.0D0 &
                      .OR. k_surf(i_facet, i_depth) <= 0.0D0 &
                      .OR. cp_surf(i_facet, i_depth) <= 0.0D0) valid_lumped_surf(i_facet) = .FALSE.
               END DO
               IF (valid_lumped_surf(i_facet)) surface_weight = surface_weight + sfr_surf(i_facet)
            END IF
         END DO
         IF (surface_weight <= 1.0D-12) THEN
            CALL errorState%report( &
               message='lumped slab: no valid positive-fraction surface thermal layers; QS remains zero', &
               location='EHC', is_fatal=.FALSE.)
            RETURN
         END IF

         DO i_depth = 1, ndepth
            layer_cap = 0.0D0
            layer_temp_cap = 0.0D0
            layer_fast_temp_cap = 0.0D0
            layer_slow_temp_cap = 0.0D0
            DO i_facet = 1, nsurf
               IF (valid_lumped_surf(i_facet)) THEN
                  layer_cap = layer_cap + sfr_surf(i_facet)*cp_surf(i_facet, i_depth)*dz_surf(i_facet, i_depth)
                  layer_temp_cap = layer_temp_cap + sfr_surf(i_facet)*cp_surf(i_facet, i_depth) &
                                   *dz_surf(i_facet, i_depth)*temp_in_surf(i_facet, i_depth)
                  layer_fast_temp_cap = layer_fast_temp_cap + sfr_surf(i_facet)*cp_surf(i_facet, i_depth) &
                                        *dz_surf(i_facet, i_depth)*temp_fast_surf(i_facet, i_depth)
                  layer_slow_temp_cap = layer_slow_temp_cap + sfr_surf(i_facet)*cp_surf(i_facet, i_depth) &
                                        *dz_surf(i_facet, i_depth)*temp_slow_surf(i_facet, i_depth)
               END IF
            END DO
            IF (layer_cap <= 1.0D-12) THEN
               valid_lumped = .FALSE.
            ELSE
               cap_lumped(i_depth) = layer_cap
               temp_lumped(i_depth) = layer_temp_cap/layer_cap
               temp_fast_lumped(i_depth) = layer_fast_temp_cap/layer_cap
               temp_slow_lumped(i_depth) = layer_slow_temp_cap/layer_cap
            END IF
         END DO
         IF (.NOT. valid_lumped) THEN
            CALL errorState%report( &
               message='lumped slab: invalid aggregated heat capacity; QS remains zero', &
               location='EHC', is_fatal=.FALSE.)
            RETURN
         END IF

         IF (use_implicit) THEN
            ! One slab shared by the valid surfaces; each surface keeps its own
            ! zero-capacity surface node and energy balance, coupled to the top
            ! layer through its own half-layer conductance.
            n_top = 0
            g_imp = 0.0D0
            t_bottom_imp = 0.0D0
            g_bottom_bldg = 0.0D0
            DO i_facet = 1, nsurf
               IF (.NOT. valid_lumped_surf(i_facet)) CYCLE
               n_top = n_top + 1
               top_surface(n_top) = i_facet
               g_top_imp(n_top) = sfr_surf(i_facet)*2.0D0*k_surf(i_facet, 1)/dz_surf(i_facet, 1)
               a_top_imp(n_top) = sfr_surf(i_facet)*implicit_seb%a_surf(i_facet)
               b_top_imp(n_top) = sfr_surf(i_facet)*implicit_seb%b_surf(i_facet)
               DO i_depth = 1, ndepth - 1
                  g_imp(i_depth + 1) = g_imp(i_depth + 1) + sfr_surf(i_facet)/ &
                                       (0.5D0*dz_surf(i_facet, i_depth)/k_surf(i_facet, i_depth) &
                                        + 0.5D0*dz_surf(i_facet, i_depth + 1)/k_surf(i_facet, i_depth + 1))
               END DO
               face_resistance = 0.5D0*dz_surf(i_facet, ndepth)/k_surf(i_facet, ndepth)
               ! The shared slab has a horizontal building surface, no
               ! separate wall column: use the roof film on its indoor branch.
               IF (i_facet == BldgSurf) face_resistance = face_resistance + implicit_seb%r_indoor_roof
               g_imp(ndepth + 1) = g_imp(ndepth + 1) + sfr_surf(i_facet)/face_resistance
               t_bottom_imp = t_bottom_imp + sfr_surf(i_facet)*tin_surf_use(i_facet)/face_resistance
               IF (i_facet == BldgSurf) g_bottom_bldg = sfr_surf(i_facet)/face_resistance
            END DO
            ! the shared lower face sees each surface's boundary through that
            ! surface's own conductance: one effective temperature
            IF (g_imp(ndepth + 1) > 0.0D0) t_bottom_imp = t_bottom_imp/g_imp(ndepth + 1)

            CALL heatcond1d_seb_implicit( &
               temp_lumped, cap_lumped, g_imp, tstep*1.0D0, t_bottom_imp, &
               g_top_imp(1:n_top), a_top_imp(1:n_top), b_top_imp(1:n_top), &
               ts_imp(1:n_top), q_each_imp(1:n_top), qs_lumped, q_bottom_imp, dudt_imp)
            IF (supy_error_flag()) THEN
               CALL ehc_fail_outputs()
               RETURN
            END IF

            QS = qs_lumped
            QS_roof = 0.0D0
            QS_wall = 0.0D0
            DO i_top = 1, n_top
               i_facet = top_surface(i_top)
               QS_surf(i_facet) = q_each_imp(i_top)/sfr_surf(i_facet)
               implicit_seb%tsfc_surf(i_facet) = ts_imp(i_top)
               temp_out_surf(i_facet, :) = temp_lumped
            END DO
            ! backward Euler: every flux is end-of-step, so the shared lower
            ! face splits exactly into each surface's share
            dudt_grid = dudt_imp
            q_bottom_grid = q_bottom_imp
            q_indoor_grid = g_bottom_bldg*(implicit_seb%t_indoor - temp_lumped(ndepth))
            ! The building surface's storage also takes in what its column
            ! receives from the indoor boundary, so the building tile's residual
            ! QH, with that indoor input as its QF, is the aerodynamic flux of
            ! its surface node (see ehc_implicit_qf_surface in the driver).
            IF (sfr_surf(BldgSurf) > 0.0D0) &
               QS_surf(BldgSurf) = QS_surf(BldgSurf) + q_indoor_grid/sfr_surf(BldgSurf)
            QS = qs_lumped + q_indoor_grid
            ! a surface without valid thermal layers stores nothing: close its
            ! energy balance with zero conduction
            DO i_facet = 1, nsurf
               IF (sfr_surf(i_facet) > 0.0D0 .AND. .NOT. valid_lumped_surf(i_facet) &
                   .AND. implicit_seb%b_surf(i_facet) > 0.0D0) &
                  implicit_seb%tsfc_surf(i_facet) = implicit_seb%a_surf(i_facet)/implicit_seb%b_surf(i_facet)
            END DO
            RETURN
         END IF

         IF (use_parallel_surfaces) THEN
            DO i_facet = 1, nsurf
               IF (valid_lumped_surf(i_facet)) THEN
                  temp_branch = temp_in_surf(i_facet, :)
                  cap_branch = cp_surf(i_facet, :)*dz_surf(i_facet, :)
                  g_branch = 0.0D0

                  face_resistance = 0.5D0*dz_surf(i_facet, 1)/k_surf(i_facet, 1)
                  g_branch(1) = 1.0D0/face_resistance
                  IF (.NOT. use_zero_flux_bottom) THEN
                     face_resistance = 0.5D0*dz_surf(i_facet, ndepth)/k_surf(i_facet, ndepth)
                     g_branch(ndepth + 1) = bottom_g_scale/face_resistance
                  END IF
                  DO i_depth = 1, ndepth - 1
                     face_resistance = 0.5D0*dz_surf(i_facet, i_depth)/k_surf(i_facet, i_depth) &
                                       + 0.5D0*dz_surf(i_facet, i_depth + 1)/k_surf(i_facet, i_depth + 1)
                     g_branch(i_depth + 1) = 1.0D0/face_resistance
                  END DO

                  IF (ANY(g_branch(1:ndepth) <= 1.0D-12)) THEN
                     CALL errorState%report( &
                        message='parallel slab: invalid internal conductance; surface QS remains zero', &
                        location='EHC', is_fatal=.FALSE.)
                     CYCLE
                  END IF
                  IF ((.NOT. use_zero_flux_bottom) .AND. g_branch(ndepth + 1) <= 1.0D-12) THEN
                     CALL errorState%report( &
                        message='parallel slab: invalid lower-boundary conductance; surface QS remains zero', &
                        location='EHC', is_fatal=.FALSE.)
                     CYCLE
                  END IF

                  bc(1) = tsfc_surf(i_facet)
                  bc(2) = tin_surf(i_facet)
                  CALL heatcond1d_gstep(temp_branch, qs_lumped, cap_branch, g_branch, tstep*1.0D0, bc, &
                                        q_top_lumped, q_bottom_lumped)
                  IF (use_zero_flux_bottom) THEN
                     QS_surf(i_facet) = qs_lumped
                  ELSE
                     QS_surf(i_facet) = q_top_lumped
                  END IF
                  temp_out_surf(i_facet, :) = temp_branch
               END IF
            END DO
            QS = DOT_PRODUCT(QS_surf, sfr_surf)
            RETURN
         END IF

         DO i_facet = 1, nsurf
            IF (valid_lumped_surf(i_facet)) THEN
               face_resistance = 0.5D0*dz_surf(i_facet, 1)/k_surf(i_facet, 1)
               g_lumped(1) = g_lumped(1) + sfr_surf(i_facet)/face_resistance
               tsfc_lumped = tsfc_lumped + sfr_surf(i_facet)*tsfc_surf(i_facet)/face_resistance

               IF (.NOT. use_zero_flux_bottom) THEN
                  face_resistance = 0.5D0*dz_surf(i_facet, ndepth)/k_surf(i_facet, ndepth)
                  g_lumped(ndepth + 1) = g_lumped(ndepth + 1) + bottom_g_scale*sfr_surf(i_facet)/face_resistance
                  tin_lumped = tin_lumped + bottom_g_scale*sfr_surf(i_facet)*tin_surf(i_facet)/face_resistance
               END IF

               DO i_depth = 1, ndepth - 1
                  face_resistance = 0.5D0*dz_surf(i_facet, i_depth)/k_surf(i_facet, i_depth) &
                                    + 0.5D0*dz_surf(i_facet, i_depth + 1)/k_surf(i_facet, i_depth + 1)
                  g_lumped(i_depth + 1) = g_lumped(i_depth + 1) + sfr_surf(i_facet)/face_resistance
               END DO
            END IF
         END DO
         IF (ANY(g_lumped(1:ndepth) <= 1.0D-12)) THEN
            CALL errorState%report( &
               message='lumped slab: invalid aggregated internal conductance; QS remains zero', &
               location='EHC', is_fatal=.FALSE.)
            RETURN
         END IF
         IF ((.NOT. use_zero_flux_bottom) .AND. g_lumped(ndepth + 1) <= 1.0D-12) THEN
            CALL errorState%report( &
               message='lumped slab: invalid aggregated lower-boundary conductance; QS remains zero', &
               location='EHC', is_fatal=.FALSE.)
            RETURN
         END IF

         tsfc_lumped = tsfc_lumped/g_lumped(1)
         IF (.NOT. use_zero_flux_bottom) THEN
            tin_lumped = tin_lumped/g_lumped(ndepth + 1)
         END IF

         IF (use_state_admittance) THEN
            IF (use_dual_timescale) THEN
               state_layer_temp = dual_fast_weight*temp_fast_lumped(1) + dual_slow_weight*temp_slow_lumped(1)
            ELSE
               state_layer_temp = temp_lumped(1)
            END IF
            state_top_gradient = tsfc_lumped - state_layer_temp
            IF (state_top_gradient >= 0.0D0) THEN
               state_g_scale = 1.0D0 + state_warming_g_boost*TANH(state_top_gradient/state_gradient_scale)
            ELSE
               state_g_scale = 1.0D0 - state_cooling_g_damp*TANH((-state_top_gradient)/state_gradient_scale)
               state_g_scale = MAX(state_min_g_scale, state_g_scale)
            END IF
            g_lumped(1) = g_lumped(1)*state_g_scale
         END IF

            bc(1) = tsfc_lumped
            bc(2) = tin_lumped
            IF (use_dual_timescale) THEN
               cap_fast_lumped = cap_lumped*fast_cap_scale
               cap_slow_lumped = cap_lumped*slow_cap_scale
               g_fast_lumped = g_lumped*fast_g_scale
               g_slow_lumped = g_lumped*slow_g_scale
               lumped_layer_temp_for_alloc = dual_fast_weight*temp_fast_lumped(1) + dual_slow_weight*temp_slow_lumped(1)

               CALL heatcond1d_gstep(temp_fast_lumped, qs_fast_lumped, cap_fast_lumped, g_fast_lumped, tstep*1.0D0, bc, &
                                     q_top_fast_lumped, q_bottom_fast_lumped)
            CALL heatcond1d_gstep(temp_slow_lumped, qs_slow_lumped, cap_slow_lumped, g_slow_lumped, tstep*1.0D0, bc, &
                                  q_top_slow_lumped, q_bottom_slow_lumped)

               IF (use_zero_flux_bottom) THEN
                  QS = dual_fast_weight*qs_fast_lumped + dual_slow_weight*qs_slow_lumped
               ELSE
                  QS = dual_fast_weight*q_top_fast_lumped + dual_slow_weight*q_top_slow_lumped
               END IF
               QS_roof = 0.0D0
               QS_wall = 0.0D0
               IF (use_qs_surface_gradient_alloc) THEN
                  CALL allocate_lumped_qs_by_surface_gradient( &
                     QS, tsfc_surf, sfr_surf, dz_surf, k_surf, lumped_layer_temp_for_alloc, &
                     valid_lumped_surf, QS_surf, allocation_success)
               ELSE
                  allocation_success = .FALSE.
               END IF
               IF (.NOT. allocation_success) qs_per_surface = QS/surface_weight
               DO i_facet = 1, nsurf
                  IF (valid_lumped_surf(i_facet)) THEN
                     IF (.NOT. allocation_success) QS_surf(i_facet) = qs_per_surface
                     temp_fast_surf(i_facet, :) = temp_fast_lumped
                     temp_slow_surf(i_facet, :) = temp_slow_lumped
                     temp_out_surf(i_facet, :) = dual_fast_weight*temp_fast_lumped + dual_slow_weight*temp_slow_lumped
               END IF
               END DO
               RETURN
            END IF

            lumped_layer_temp_for_alloc = temp_lumped(1)
            CALL heatcond1d_gstep(temp_lumped, qs_lumped, cap_lumped, g_lumped, tstep*1.0D0, bc, &
                                  q_top_lumped, q_bottom_lumped)

         ! With a finite lower boundary, SUEWS needs the outdoor-surface storage
         ! flux; the material heat-content tendency also includes lower exchange.
         IF (use_zero_flux_bottom) THEN
            QS = qs_lumped
         ELSE
            QS = q_top_lumped
         END IF
         ! the explicit slab has no separate indoor boundary (the building
         ! surface shares the lumped lower face), so q_indoor_grid stays -999
         dudt_grid = qs_lumped
         q_bottom_grid = q_bottom_lumped
            QS_roof = 0.0D0
            QS_wall = 0.0D0
            IF (use_qs_surface_gradient_alloc) THEN
               CALL allocate_lumped_qs_by_surface_gradient( &
                  QS, tsfc_surf, sfr_surf, dz_surf, k_surf, lumped_layer_temp_for_alloc, &
                  valid_lumped_surf, QS_surf, allocation_success)
            ELSE
               allocation_success = .FALSE.
            END IF
            IF (.NOT. allocation_success) qs_per_surface = QS/surface_weight
            DO i_facet = 1, nsurf
               IF (valid_lumped_surf(i_facet)) THEN
                  IF (.NOT. allocation_success) QS_surf(i_facet) = qs_per_surface
                  temp_out_surf(i_facet, :) = temp_lumped
               END IF
            END DO
         RETURN
      END IF

      ! initialise solver flags
      use_heatcond1d = .TRUE.
      use_heatcond1d_water = .FALSE.
      debug = .FALSE.
      dudt_grid = 0.0D0
      q_bottom_grid = 0.0D0
      q_indoor_grid = 0.0D0

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
            qs_cal = 0.0D0
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
            IF (use_implicit) tin_cal(1:nfacet) = implicit_seb%t_indoor
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
            IF (use_implicit) tin_cal(1:nfacet) = implicit_seb%t_indoor
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
            tin_cal(1:nfacet) = tin_surf_use(1:nfacet)
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

            ! plan-area weight of this facet in the grid aggregates
            IF (i_group == 1) THEN
               weight_facet = sfr_roof(i_facet)
            ELSE IF (i_group == 2) THEN
               weight_facet = sfr_wall(i_facet)
            ELSE
               weight_facet = sfr_surf(i_facet)
            END IF

            ! implicit surface coupling: one solver for every conducting facet
            ! except water, which keeps heatcond1d_CN below (its Tsfc is
            ! updated by the driver); the building tile is the roof/wall
            ! aggregate
            IF (use_implicit .AND. i_group == 3 .AND. i_facet == WaterSurf) THEN
               IF (column_absent(dz_cal(i_facet, :), cp_cal(i_facet, :))) THEN
                  IF (implicit_seb%b_surf(i_facet) > 0.0D0) &
                     tsfc_cal(i_facet) = implicit_seb%a_surf(i_facet)/implicit_seb%b_surf(i_facet)
                  CYCLE
               END IF
            END IF
            IF (use_implicit .AND. .NOT. (i_group == 3 .AND. i_facet == WaterSurf)) THEN
               IF (.NOT. (i_group == 3 .AND. i_facet == BldgSurf)) THEN
                  IF (.NOT. column_absent(dz_cal(i_facet, :), cp_cal(i_facet, :))) THEN
                     cap_imp = cp_cal(i_facet, :)*dz_cal(i_facet, :)
                     g_imp(1) = 2.0D0*k_cal(i_facet, 1)/dz_cal(i_facet, 1)
                     DO i_depth = 1, ndepth - 1
                        g_imp(i_depth + 1) = 2.0D0*k_cal(i_facet, i_depth)*k_cal(i_facet, i_depth + 1) &
                                             /(k_cal(i_facet, i_depth + 1)*dz_cal(i_facet, i_depth) &
                                               + k_cal(i_facet, i_depth)*dz_cal(i_facet, i_depth + 1))
                     END DO
                     ! Keep the existing ground and zero-film arithmetic;
                     ! only indoor facets acquire a series surface resistance.
                     g_imp(ndepth + 1) = 2.0D0*k_cal(i_facet, ndepth)/dz_cal(i_facet, ndepth)
                     face_resistance = 0.5D0*dz_cal(i_facet, ndepth)/k_cal(i_facet, ndepth)
                     IF (i_group == 1 .AND. implicit_seb%r_indoor_roof > 0.0D0) &
                        g_imp(ndepth + 1) = 1.0D0/(implicit_seb%r_indoor_roof + face_resistance)
                     IF (i_group == 2 .AND. implicit_seb%r_indoor_wall > 0.0D0) &
                        g_imp(ndepth + 1) = 1.0D0/(implicit_seb%r_indoor_wall + face_resistance)
                     IF (i_group == 1) THEN
                        a_top_imp(1) = implicit_seb%a_roof(i_facet)
                        b_top_imp(1) = implicit_seb%b_roof(i_facet)
                     ELSE IF (i_group == 2) THEN
                        a_top_imp(1) = implicit_seb%a_wall(i_facet)
                        b_top_imp(1) = implicit_seb%b_wall(i_facet)
                     ELSE
                        a_top_imp(1) = implicit_seb%a_surf(i_facet)
                        b_top_imp(1) = implicit_seb%b_surf(i_facet)
                     END IF
                     CALL heatcond1d_seb_implicit( &
                        temp_cal(i_facet, :), cap_imp, g_imp, tstep*1.0D0, tin_cal(i_facet), &
                        g_imp(1:1), a_top_imp(1:1), b_top_imp(1:1), &
                        ts_imp(1:1), q_each_imp(1:1), QS_cal(i_facet), q_bottom_imp, dudt_imp)
                     IF (supy_error_flag()) THEN
                        CALL ehc_fail_outputs()
                        RETURN
                     END IF
                     tsfc_cal(i_facet) = ts_imp(1)
                     dudt_grid = dudt_grid + weight_facet*dudt_imp
                     q_bottom_grid = q_bottom_grid + weight_facet*q_bottom_imp
                     IF (i_group /= 3) q_indoor_grid = q_indoor_grid - weight_facet*q_bottom_imp
                  ELSE
                     ! no valid thermal layers: QS = 0 and the surface balance
                     ! closes with zero conduction
                     IF (i_group == 1) THEN
                        IF (implicit_seb%b_roof(i_facet) > 0.0D0) &
                           tsfc_cal(i_facet) = implicit_seb%a_roof(i_facet)/implicit_seb%b_roof(i_facet)
                     ELSE IF (i_group == 2) THEN
                        IF (implicit_seb%b_wall(i_facet) > 0.0D0) &
                           tsfc_cal(i_facet) = implicit_seb%a_wall(i_facet)/implicit_seb%b_wall(i_facet)
                     ELSE
                        IF (implicit_seb%b_surf(i_facet) > 0.0D0) &
                           tsfc_cal(i_facet) = implicit_seb%a_surf(i_facet)/implicit_seb%b_surf(i_facet)
                     END IF
                  END IF
               END IF
               CYCLE
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
                  bc, q_top_facet, q_bottom_facet)
               ! Legacy facet QS is the heat-content tendency dU/dt.
               dudt_facet = QS_cal(i_facet)
               dudt_grid = dudt_grid + weight_facet*dudt_facet
               q_bottom_grid = q_bottom_grid + weight_facet*q_bottom_facet
               IF (i_group /= 3) q_indoor_grid = q_indoor_grid - weight_facet*q_bottom_facet

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
                  bc, q_bottom_facet)
               ! water QS is the heat-content tendency here in both couplings
               dudt_grid = dudt_grid + weight_facet*QS_cal(i_facet)
               q_bottom_grid = q_bottom_grid + weight_facet*q_bottom_facet

               ! ! update temperature at all inner interfaces
               ! temp_cal(i_facet, :) = temp_all_cal
            END IF

         END DO ! end of i_facet loop

         ! translate results to back to the output arrays of facet groups
         IF (i_group == 1) THEN
            QS_roof = QS_cal(1:nfacet)
            IF (use_implicit) implicit_seb%tsfc_roof(1:nfacet) = tsfc_cal(1:nfacet)
            temp_out_roof = temp_cal(:nfacet, :)
         ELSE IF (i_group == 2) THEN
            QS_wall = QS_cal(1:nfacet)
            IF (use_implicit) implicit_seb%tsfc_wall(1:nfacet) = tsfc_cal(1:nfacet)
            temp_out_wall = temp_cal(:nfacet, :)
         ELSE IF (i_group == 3) THEN
            QS_surf = QS_cal(1:nfacet)
            IF (use_implicit) implicit_seb%tsfc_surf = tsfc_cal(1:nfacet)
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
         ! implicit: roof and wall QS are each facet's surface flux (q_top);
         ! the building tile also stores what its envelope receives from the
         ! indoor boundary, so its QS is the envelope's dU/dt
         IF (use_implicit) QS_surf(BldgSurf) = QS_surf(BldgSurf) + q_indoor_grid/sfr_surf(BldgSurf)
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

   CONTAINS

      LOGICAL FUNCTION column_absent(dz, cp)
         REAL(KIND(1D0)), INTENT(IN) :: dz(:), cp(:)
         ! Preserve the existing whole-column sentinels: missing thicknesses
         ! or zero heat capacity throughout. One bad layer is not an absence.
         column_absent = ALL(dz == -999.0D0) .OR. ALL(cp == 0.0D0)
      END FUNCTION column_absent

      SUBROUTINE check_active_column(dz, k, cp)
         REAL(KIND(1D0)), INTENT(IN) :: dz(:), k(:), cp(:)
         IF (ANY(.NOT. IEEE_IS_FINITE(dz)) .OR. ANY(.NOT. IEEE_IS_FINITE(k)) &
             .OR. ANY(.NOT. IEEE_IS_FINITE(cp))) THEN
            CALL set_supy_error(115, 'EHC: thermal layer properties must be finite')
            RETURN
         END IF
         IF (column_absent(dz, cp)) RETURN
         IF (ANY(dz <= 0.0D0) .OR. ANY(k <= 0.0D0) .OR. ANY(cp <= 0.0D0)) &
            CALL set_supy_error(115, 'EHC: active thermal layers must have positive thickness, conductivity and heat capacity')
      END SUBROUTINE check_active_column

      SUBROUTINE ehc_fail_outputs()
         ! safe outputs before returning on a fatal error
         QS = -999.0D0
         QS_surf = -999.0D0
         QS_roof = -999.0D0
         QS_wall = -999.0D0
         dudt_grid = -999.0D0
         q_bottom_grid = -999.0D0
         q_indoor_grid = -999.0D0
         IF (PRESENT(implicit_seb)) THEN
            implicit_seb%tsfc_surf = tsfc_surf
            IF (ALLOCATED(implicit_seb%tsfc_roof)) implicit_seb%tsfc_roof = tsfc_roof
            IF (ALLOCATED(implicit_seb%tsfc_wall)) implicit_seb%tsfc_wall = tsfc_wall
         END IF
         temp_out_surf = temp_in_surf
         temp_out_roof = temp_in_roof
         temp_out_wall = temp_in_wall
      END SUBROUTINE ehc_fail_outputs

   END SUBROUTINE EHC

   SUBROUTINE allocate_lumped_qs_by_surface_gradient( &
      qs_total, tsfc_surf, sfr_surf, dz_surf, k_surf, temp_ref, valid_surf, qs_surf, success)
      USE module_ctrl_const_allocate, ONLY: nsurf, ndepth

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(IN) :: qs_total
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: tsfc_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(IN) :: sfr_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(IN) :: dz_surf
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth), INTENT(IN) :: k_surf
      REAL(KIND(1D0)), INTENT(IN) :: temp_ref
      LOGICAL, DIMENSION(nsurf), INTENT(IN) :: valid_surf
      REAL(KIND(1D0)), DIMENSION(nsurf), INTENT(OUT) :: qs_surf
      LOGICAL, INTENT(OUT) :: success

      INTEGER :: i_surf
      REAL(KIND(1D0)), DIMENSION(nsurf) :: raw_qs_surf
      REAL(KIND(1D0)), DIMENSION(nsurf) :: top_conductance_surf
      REAL(KIND(1D0)) :: face_resistance, raw_grid_qs, conductance_grid, scale_factor

      success = .FALSE.
      qs_surf = 0.0D0
      raw_qs_surf = 0.0D0
      top_conductance_surf = 0.0D0

      DO i_surf = 1, nsurf
         IF (valid_surf(i_surf) .AND. sfr_surf(i_surf) > 0.0D0 &
             .AND. dz_surf(i_surf, 1) > 0.0D0 .AND. k_surf(i_surf, 1) > 0.0D0) THEN
            face_resistance = 0.5D0*dz_surf(i_surf, 1)/k_surf(i_surf, 1)
            IF (face_resistance > 1.0D-12) THEN
               top_conductance_surf(i_surf) = 1.0D0/face_resistance
               raw_qs_surf(i_surf) = top_conductance_surf(i_surf)*(tsfc_surf(i_surf) - temp_ref)
            END IF
         END IF
      END DO

      raw_grid_qs = DOT_PRODUCT(raw_qs_surf, sfr_surf)
      IF (ABS(raw_grid_qs) <= 1.0D-8) THEN
         conductance_grid = DOT_PRODUCT(top_conductance_surf, sfr_surf)
         IF (conductance_grid <= 1.0D-12) RETURN
         raw_qs_surf = top_conductance_surf
         raw_grid_qs = conductance_grid
      END IF

      scale_factor = qs_total/raw_grid_qs
      qs_surf = raw_qs_surf*scale_factor
      success = .TRUE.
   END SUBROUTINE allocate_lumped_qs_by_surface_gradient

   LOGICAL FUNCTION ehc_experimental_controls_enabled()
      IMPLICIT NONE
      CHARACTER(LEN=32) :: env_value
      INTEGER :: env_status

      ehc_experimental_controls_enabled = .FALSE.
      CALL GET_ENVIRONMENT_VARIABLE("SUEWS_EHC_EXPERIMENTAL_CONTROLS", env_value, STATUS=env_status)
      IF (env_status /= 0) RETURN

      SELECT CASE (TRIM(ADJUSTL(env_value)))
      CASE ("1", "true", "TRUE", "True", "yes", "YES", "Yes", &
            "on", "ON", "On")
         ehc_experimental_controls_enabled = .TRUE.
      END SELECT
   END FUNCTION ehc_experimental_controls_enabled

END MODULE module_phys_ehc

! Backward compatibility alias
MODULE EHC_module
   USE module_phys_ehc
END MODULE EHC_module
