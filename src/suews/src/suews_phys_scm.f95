!========================================================================================
! SUEWS single-column model (SCM) atmospheric column
!
! Native Fortran port of the validated Python reference implementation in
! scm/suews_scm/column.py + scm/suews_scm/coupling.py (kept in lock-step;
! the Python package carries the benchmark suite: GABLS1 vs the LES
! ensemble of Beare et al. 2006, convective growth vs Tennekes 1973).
!
! Physics:
! - unstable: first-order non-local K-profile within the diagnosed
!   boundary layer (Troen & Mahrt 1986) with the counter-gradient term
!   and turbulent Prandtl number of Holtslag & Boville (1993);
! - stable/neutral and free atmosphere: local Blackadar mixing length
!   with a sharp Richardson-number cut-off (Cuxart et al. 2006 showed
!   long tails over-deepen the SBL; the long-tail form is selectable);
! - boundary-layer height: bulk Richardson number with the
!   Vogelezang & Holtslag (1996) 100*ustar^2 shear term and a
!   Troen-Mahrt convective thermal excess;
! - numerics: backward-Euler implicit vertical diffusion in conservative
!   flux form (column integrals conserved exactly up to the surface flux).
!
! Coupled mode only: the surface boundary condition is the kinematic flux
! pair (wth, wq) plus friction velocity supplied by SUEWS each substep.
! Wind is relaxed towards a log profile anchored at the observed speed
! (a 1-D column cannot generate the synoptic pressure gradient), and the
! column may be ventilated by a background (rural companion) atmosphere
! on the advective time scale tau = city_length / U.
!
! Author: ported from the suews-scm Python reference, 2026.
!========================================================================================
MODULE module_phys_scm

   USE module_ctrl_error_state, ONLY: set_supy_error

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: dts_scm_column, dts_scm_background, dts_scm_params
   PUBLIC :: scm_params_from_flat, scm_column_init, scm_column_step
   PUBLIC :: scm_sample_zmeas, scm_wind_nudge, scm_ventilate
   PUBLIC :: scm_background_on_grid, scm_q_from_rh
   PUBLIC :: scm_column_set_profiles, scm_rho_air, scm_kinematic_fluxes
   PUBLIC :: SCM_PARAMS_LEN, SCM_DIAG_NCOL

   ! ---- physical constants (match scm/suews_scm/constants.py exactly) ----
   REAL(KIND(1D0)), PARAMETER :: GRAV = 9.81D0 ! [m s-2]
   REAL(KIND(1D0)), PARAMETER :: R_D = 287.05D0 ! [J kg-1 K-1]
   REAL(KIND(1D0)), PARAMETER :: CP_AIR = 1005.0D0 ! [J kg-1 K-1]
   REAL(KIND(1D0)), PARAMETER :: LV = 2.5D6 ! [J kg-1]
   REAL(KIND(1D0)), PARAMETER :: P0 = 1.0D5 ! [Pa]
   REAL(KIND(1D0)), PARAMETER :: KAPPA_P = R_D/CP_AIR ! Poisson constant [-]
   REAL(KIND(1D0)), PARAMETER :: VONK = 0.4D0 ! von Karman [-]
   REAL(KIND(1D0)), PARAMETER :: EPS_W = 0.622D0 ! Mw/Md [-]

   INTEGER, PARAMETER :: SCM_PARAMS_LEN = 23 ! length of the flat parameter vector
   INTEGER, PARAMETER :: SCM_DIAG_NCOL = 7 ! per-step diagnostic columns

   ! flat parameter vector layout (1-based; keep in step with
   ! scm/suews_scm/native.py and suews_bridge/src/lib.rs):
   !  1 dz0 [m]            2 ztop [m]          3 stretch [-]
   !  4 h_init [m]         5 gamma_theta [K m-1] 6 gamma_q [kg kg-1 m-1]
   !  7 z_ft_nudge [m]     8 tau_ft [s]        9 tau_wind [s]
   ! 10 radiative_cooling [K day-1, positive = cooling]
   ! 11 city_length [m]   12 tau_adv_min [s]  13 substeps [-]
   ! 14 z0m_wind [m]      15 lambda_mix [m]   16 ric_stable [-]
   ! 17 ric_h [-]         18 k_background [m2 s-1]
   ! 19 cg_a [-]          20 excess_b [-]     21 wstar_fac [-]
   ! 22 stable_fn [0 = sharp cut-off, 1 = long tail]
   ! 23 use_background [0/1]

   TYPE :: dts_scm_params
      REAL(KIND(1D0)) :: dz0 = 20.0D0 ! [m]
      REAL(KIND(1D0)) :: ztop = 3000.0D0 ! [m]
      REAL(KIND(1D0)) :: stretch = 1.06D0 ! [-]
      REAL(KIND(1D0)) :: h_init = 300.0D0 ! [m]
      REAL(KIND(1D0)) :: gamma_theta = 0.006D0 ! [K m-1]
      REAL(KIND(1D0)) :: gamma_q = -5.0D-7 ! [kg kg-1 m-1]
      REAL(KIND(1D0)) :: z_ft_nudge = 1500.0D0 ! [m]
      REAL(KIND(1D0)) :: tau_ft = 86400.0D0 ! [s]
      REAL(KIND(1D0)) :: tau_wind = 1800.0D0 ! [s]
      REAL(KIND(1D0)) :: radiative_cooling = 2.0D0 ! [K day-1]
      REAL(KIND(1D0)) :: city_length = 15000.0D0 ! [m]
      REAL(KIND(1D0)) :: tau_adv_min = 900.0D0 ! [s]
      INTEGER :: substeps = 5 ! [-]
      REAL(KIND(1D0)) :: z0m_wind = 1.0D0 ! [m]
      REAL(KIND(1D0)) :: lambda_mix = 40.0D0 ! [m]
      REAL(KIND(1D0)) :: ric_stable = 0.2D0 ! [-]
      REAL(KIND(1D0)) :: ric_h = 0.25D0 ! [-]
      REAL(KIND(1D0)) :: k_background = 1.0D-4 ! [m2 s-1]
      REAL(KIND(1D0)) :: cg_a = 7.2D0 ! [-]
      REAL(KIND(1D0)) :: excess_b = 8.5D0 ! [-]
      REAL(KIND(1D0)) :: wstar_fac = 0.6D0 ! [-]
      INTEGER :: stable_fn = 0 ! 0 sharp, 1 long tail
      LOGICAL :: use_background = .FALSE.
   END TYPE dts_scm_params

   TYPE :: dts_scm_column
      INTEGER :: n = 0 ! number of layers
      REAL(KIND(1D0)), ALLOCATABLE :: z(:) ! cell-centre heights [m]
      REAL(KIND(1D0)), ALLOCATABLE :: zi(:) ! interface heights [m], n+1
      REAL(KIND(1D0)), ALLOCATABLE :: dz(:) ! layer thicknesses [m]
      REAL(KIND(1D0)), ALLOCATABLE :: dzc(:) ! centre-to-centre distances [m], n-1
      REAL(KIND(1D0)), ALLOCATABLE :: theta(:) ! potential temperature [K]
      REAL(KIND(1D0)), ALLOCATABLE :: q(:) ! specific humidity [kg kg-1]
      REAL(KIND(1D0)), ALLOCATABLE :: u(:) ! zonal wind [m s-1]
      REAL(KIND(1D0)), ALLOCATABLE :: v(:) ! meridional wind [m s-1]
      REAL(KIND(1D0)), ALLOCATABLE :: p_levels(:) ! hydrostatic pressure [Pa]
      REAL(KIND(1D0)), ALLOCATABLE :: exner_lev(:) ! Exner function [-]
      REAL(KIND(1D0)), ALLOCATABLE :: theta_ref(:) ! free-troposphere nudge target [K]
      REAL(KIND(1D0)), ALLOCATABLE :: q_ref(:) ! free-troposphere nudge target [kg kg-1]
      REAL(KIND(1D0)) :: theta_tendency = 0.0D0 ! prescribed tendency [K s-1]
      REAL(KIND(1D0)) :: h_last = 0.0D0 ! last diagnosed BL height [m]
   END TYPE dts_scm_column

   TYPE :: dts_scm_background
      INTEGER :: nt = 0 ! number of time snapshots
      INTEGER :: nz = 0 ! number of background levels
      REAL(KIND(1D0)), ALLOCATABLE :: t_sec(:) ! snapshot times since run start [s]
      REAL(KIND(1D0)), ALLOCATABLE :: z(:) ! background heights [m]
      REAL(KIND(1D0)), ALLOCATABLE :: theta(:, :) ! (nt, nz) [K]
      REAL(KIND(1D0)), ALLOCATABLE :: q(:, :) ! (nt, nz) [kg kg-1]
   END TYPE dts_scm_background

CONTAINS

   !=====================================================================
   ! parameter unpacking
   !=====================================================================
   SUBROUTINE scm_params_from_flat(flat, n_flat, prm, ok)
      REAL(KIND(1D0)), INTENT(IN) :: flat(*)
      INTEGER, INTENT(IN) :: n_flat
      TYPE(dts_scm_params), INTENT(OUT) :: prm
      LOGICAL, INTENT(OUT) :: ok

      ok = .FALSE.
      IF (n_flat < SCM_PARAMS_LEN) RETURN
      prm%dz0 = flat(1)
      prm%ztop = flat(2)
      prm%stretch = flat(3)
      prm%h_init = flat(4)
      prm%gamma_theta = flat(5)
      prm%gamma_q = flat(6)
      prm%z_ft_nudge = flat(7)
      prm%tau_ft = flat(8)
      prm%tau_wind = flat(9)
      prm%radiative_cooling = flat(10)
      prm%city_length = flat(11)
      prm%tau_adv_min = flat(12)
      prm%substeps = MAX(INT(flat(13)), 1)
      prm%z0m_wind = flat(14)
      prm%lambda_mix = flat(15)
      prm%ric_stable = flat(16)
      prm%ric_h = flat(17)
      prm%k_background = flat(18)
      prm%cg_a = flat(19)
      prm%excess_b = flat(20)
      prm%wstar_fac = flat(21)
      prm%stable_fn = INT(flat(22))
      prm%use_background = (flat(23) > 0.5D0)
      ok = .TRUE.
   END SUBROUTINE scm_params_from_flat

   !=====================================================================
   ! thermodynamic helpers (match scm/suews_scm/thermo.py exactly)
   !=====================================================================
   PURE ELEMENTAL FUNCTION scm_exner(p) RESULT(ex)
      REAL(KIND(1D0)), INTENT(IN) :: p ! [Pa]
      REAL(KIND(1D0)) :: ex
      ex = (p/P0)**KAPPA_P
   END FUNCTION scm_exner

   PURE ELEMENTAL FUNCTION scm_esat_pa(t_k) RESULT(es)
      ! Magnus formula, Alduchov & Eskridge (1996) coefficients
      REAL(KIND(1D0)), INTENT(IN) :: t_k ! [K]
      REAL(KIND(1D0)) :: es ! [Pa]
      REAL(KIND(1D0)) :: tc
      tc = t_k - 273.15D0
      es = 610.94D0*EXP(17.625D0*tc/(tc + 243.04D0))
   END FUNCTION scm_esat_pa

   PURE FUNCTION scm_q_from_rh(rh_pct, t_k, p_pa) RESULT(q)
      REAL(KIND(1D0)), INTENT(IN) :: rh_pct ! [%]
      REAL(KIND(1D0)), INTENT(IN) :: t_k ! [K]
      REAL(KIND(1D0)), INTENT(IN) :: p_pa ! [Pa]
      REAL(KIND(1D0)) :: q ! [kg kg-1]
      REAL(KIND(1D0)) :: e
      e = rh_pct/100.0D0*scm_esat_pa(t_k)
      q = EPS_W*e/(p_pa - (1.0D0 - EPS_W)*e)
   END FUNCTION scm_q_from_rh

   PURE FUNCTION scm_rh_from_q(q, t_k, p_pa) RESULT(rh)
      REAL(KIND(1D0)), INTENT(IN) :: q ! [kg kg-1]
      REAL(KIND(1D0)), INTENT(IN) :: t_k ! [K]
      REAL(KIND(1D0)), INTENT(IN) :: p_pa ! [Pa]
      REAL(KIND(1D0)) :: rh ! [%]
      REAL(KIND(1D0)) :: e
      e = q*p_pa/(EPS_W + (1.0D0 - EPS_W)*q)
      rh = 100.0D0*e/scm_esat_pa(t_k)
   END FUNCTION scm_rh_from_q

   PURE FUNCTION scm_rho_air(t_k, p_pa, q) RESULT(rho)
      REAL(KIND(1D0)), INTENT(IN) :: t_k ! [K]
      REAL(KIND(1D0)), INTENT(IN) :: p_pa ! [Pa]
      REAL(KIND(1D0)), INTENT(IN) :: q ! [kg kg-1]
      REAL(KIND(1D0)) :: rho ! [kg m-3]
      rho = p_pa/(R_D*(t_k*(1.0D0 + 0.61D0*q)))
   END FUNCTION scm_rho_air

   !=====================================================================
   ! linear interpolation with numpy.interp clamp semantics
   !=====================================================================
   PURE FUNCTION scm_interp1(x, xp, fp, n) RESULT(f)
      REAL(KIND(1D0)), INTENT(IN) :: x
      INTEGER, INTENT(IN) :: n
      REAL(KIND(1D0)), INTENT(IN) :: xp(n), fp(n)
      REAL(KIND(1D0)) :: f
      INTEGER :: k
      IF (x <= xp(1)) THEN
         f = fp(1)
      ELSE IF (x >= xp(n)) THEN
         f = fp(n)
      ELSE
         DO k = 1, n - 1
            IF (x <= xp(k + 1)) THEN
               f = fp(k) + (fp(k + 1) - fp(k))*(x - xp(k))/(xp(k + 1) - xp(k))
               RETURN
            END IF
         END DO
         f = fp(n)
      END IF
   END FUNCTION scm_interp1

   !=====================================================================
   ! Thomas algorithm (port of scm/suews_scm/tridiag.py)
   !=====================================================================
   SUBROUTINE scm_solve_tridiag(lower, diag, upper, rhs, x, n)
      INTEGER, INTENT(IN) :: n
      REAL(KIND(1D0)), INTENT(IN) :: lower(n), diag(n), upper(n), rhs(n)
      REAL(KIND(1D0)), INTENT(OUT) :: x(n)
      REAL(KIND(1D0)) :: b(n), d(n), w
      INTEGER :: i
      b = diag
      d = rhs
      DO i = 2, n
         w = lower(i)/b(i - 1)
         b(i) = b(i) - w*upper(i - 1)
         d(i) = d(i) - w*d(i - 1)
      END DO
      x(n) = d(n)/b(n)
      DO i = n - 1, 1, -1
         x(i) = (d(i) - upper(i)*x(i + 1))/b(i)
      END DO
   END SUBROUTINE scm_solve_tridiag

   !=====================================================================
   ! column initialisation (port of CoupledSCM._init_column)
   !=====================================================================
   SUBROUTINE scm_column_init(col, prm, tair_c, pres_hpa)
      TYPE(dts_scm_column), INTENT(OUT) :: col
      TYPE(dts_scm_params), INTENT(IN) :: prm
      REAL(KIND(1D0)), INTENT(IN) :: tair_c ! first-record air temperature [degC]
      REAL(KIND(1D0)), INTENT(IN) :: pres_hpa ! first-record pressure [hPa]

      REAL(KIND(1D0)) :: p_sfc, t_air, h_scale
      REAL(KIND(1D0)) :: zi_val, dzk
      INTEGER :: n, k

      ! --- stretched grid; replicate the Python accumulation exactly ---
      n = 0
      zi_val = 0.0D0
      dzk = prm%dz0
      DO WHILE (zi_val < prm%ztop)
         zi_val = zi_val + dzk
         dzk = dzk*prm%stretch
         n = n + 1
      END DO
      col%n = n
      ALLOCATE (col%zi(n + 1), col%z(n), col%dz(n), col%dzc(n - 1))
      ALLOCATE (col%theta(n), col%q(n), col%u(n), col%v(n))
      ALLOCATE (col%p_levels(n), col%exner_lev(n))
      ALLOCATE (col%theta_ref(n), col%q_ref(n))

      col%zi(1) = 0.0D0
      dzk = prm%dz0
      DO k = 1, n
         col%zi(k + 1) = col%zi(k) + dzk
         dzk = dzk*prm%stretch
      END DO
      DO k = 1, n
         col%dz(k) = col%zi(k + 1) - col%zi(k)
         col%z(k) = 0.5D0*(col%zi(k) + col%zi(k + 1))
      END DO
      DO k = 1, n - 1
         col%dzc(k) = col%z(k + 1) - col%z(k)
      END DO

      ! --- pressure levels (constant scale height per run) ---
      p_sfc = pres_hpa*100.0D0
      t_air = tair_c + 273.15D0

      h_scale = R_D*t_air/GRAV
      DO k = 1, n
         col%p_levels(k) = p_sfc*EXP(-col%z(k)/h_scale)
         col%exner_lev(k) = scm_exner(col%p_levels(k))
      END DO

      ! profiles are anchored at the measurement height by
      ! scm_column_set_profiles, called by the driver after this
      col%theta = 0.0D0
      col%q = 0.0D0
      col%u = 0.0D0
      col%v = 0.0D0
      col%theta_tendency = -prm%radiative_cooling/86400.0D0
      col%h_last = col%z(1)
   END SUBROUTINE scm_column_init

   !=====================================================================
   ! initial profiles anchored at the measurement height
   !=====================================================================
   SUBROUTINE scm_column_set_profiles(col, prm, z_meas, tair_c, rh_pct, u_ms, pres_hpa)
      TYPE(dts_scm_column), INTENT(INOUT) :: col
      TYPE(dts_scm_params), INTENT(IN) :: prm
      REAL(KIND(1D0)), INTENT(IN) :: z_meas ! [m]
      REAL(KIND(1D0)), INTENT(IN) :: tair_c, rh_pct, u_ms, pres_hpa

      REAL(KIND(1D0)) :: p_sfc, t_air, u_obs, h_scale, p_zm, theta_m, q_m
      INTEGER :: k

      p_sfc = pres_hpa*100.0D0
      t_air = tair_c + 273.15D0
      u_obs = MAX(u_ms, 0.5D0)
      h_scale = R_D*t_air/GRAV
      p_zm = p_sfc*EXP(-z_meas/h_scale)
      theta_m = t_air/scm_exner(p_zm)
      q_m = scm_q_from_rh(rh_pct, t_air, p_zm)

      DO k = 1, col%n
         col%theta(k) = theta_m + prm%gamma_theta*MAX(col%z(k) - prm%h_init, 0.0D0)
         col%q(k) = MAX(q_m + prm%gamma_q*MAX(col%z(k) - prm%h_init, 0.0D0), 1.0D-5)
         col%u(k) = u_obs*scm_wind_shape(col%z(k), z_meas, prm%z0m_wind)
         col%v(k) = 0.0D0
      END DO
      col%theta_ref = col%theta
      col%q_ref = col%q
   END SUBROUTINE scm_column_set_profiles

   !=====================================================================
   ! log-profile shape anchored at the measurement height
   !=====================================================================
   PURE FUNCTION scm_wind_shape(z, z_meas, z0) RESULT(f)
      REAL(KIND(1D0)), INTENT(IN) :: z, z_meas, z0
      REAL(KIND(1D0)) :: f
      f = LOG((MIN(z, 300.0D0) + z0)/z0)/LOG((z_meas + z0)/z0)
   END FUNCTION scm_wind_shape

   !=====================================================================
   ! one column step with prescribed surface fluxes
   ! (port of ColumnModel.step, prescribed-flux branch, f = 0)
   !=====================================================================
   SUBROUTINE scm_column_step(col, prm, dt, wth, wq, ustar_in, h_out)
      TYPE(dts_scm_column), INTENT(INOUT) :: col
      TYPE(dts_scm_params), INTENT(IN) :: prm
      REAL(KIND(1D0)), INTENT(IN) :: dt ! [s]
      REAL(KIND(1D0)), INTENT(IN) :: wth ! kinematic heat flux [K m s-1]
      REAL(KIND(1D0)), INTENT(IN) :: wq ! kinematic moisture flux [kg kg-1 m s-1]
      REAL(KIND(1D0)), INTENT(IN) :: ustar_in ! friction velocity [m s-1]
      REAL(KIND(1D0)), INTENT(OUT) :: h_out ! diagnosed BL height [m]

      INTEGER :: n, k
      REAL(KIND(1D0)) :: ustar, wind1, wthv, h, wstar, w_m, gamma_th, gamma_q, drag
      REAL(KIND(1D0)), DIMENSION(col%n) :: thv, src_th, src_q, src_zero
      REAL(KIND(1D0)), DIMENSION(col%n + 1) :: k_m, k_h
      REAL(KIND(1D0)), DIMENSION(col%n - 1) :: k_m_loc, k_h_loc

      n = col%n
      thv = col%theta*(1.0D0 + 0.61D0*col%q)
      wind1 = MAX(HYPOT(col%u(1), col%v(1)), 0.1D0)
      ustar = MAX(ustar_in, 0.01D0)
      wthv = wth*(1.0D0 + 0.61D0*col%q(1)) + 0.61D0*col%theta(1)*wq

      h = scm_diagnose_h(col, prm, thv, ustar, wthv)

      k_m = 0.0D0
      k_h = 0.0D0
      CALL scm_local_k(col, prm, thv, k_m_loc, k_h_loc)
      wstar = 0.0D0
      gamma_th = 0.0D0
      gamma_q = 0.0D0
      IF (wthv > 1.0D-6) THEN
         CALL scm_nonlocal_blend(col, prm, thv, h, ustar, wthv, &
                                 k_m_loc, k_h_loc, k_m, k_h, wstar, w_m)
         gamma_th = prm%cg_a*wstar*wth/(w_m**2*h)
         gamma_q = prm%cg_a*wstar*wq/(w_m**2*h)
      ELSE
         DO k = 1, n - 1
            k_m(k + 1) = k_m_loc(k)
            k_h(k + 1) = k_h_loc(k)
         END DO
      END IF
      DO k = 2, n
         k_m(k) = k_m(k) + prm%k_background
         k_h(k) = k_h(k) + prm%k_background
      END DO

      CALL scm_counter_gradient_src(col, k_h, h, gamma_th, src_th)
      CALL scm_counter_gradient_src(col, k_h, h, gamma_q, src_q)
      src_th = src_th + col%theta_tendency
      DO k = 1, n
         IF (col%z(k) >= prm%z_ft_nudge) THEN
            src_th(k) = src_th(k) + (col%theta_ref(k) - col%theta(k))/prm%tau_ft
            src_q(k) = src_q(k) + (col%q_ref(k) - col%q(k))/prm%tau_ft
         END IF
      END DO

      CALL scm_diffuse(col, col%theta, k_h, dt, wth, 0.0D0, src_th)
      CALL scm_diffuse(col, col%q, k_h, dt, wq, 0.0D0, src_q)
      DO k = 1, n
         col%q(k) = MAX(col%q(k), 0.0D0)
      END DO

      drag = ustar**2/wind1
      src_zero = 0.0D0
      CALL scm_diffuse(col, col%u, k_m, dt, 0.0D0, drag, src_zero)
      CALL scm_diffuse(col, col%v, k_m, dt, 0.0D0, drag, src_zero)

      ! fail loudly rather than propagate NaNs into SUEWS forcing
      IF (col%theta(1) /= col%theta(1)) THEN
         CALL set_supy_error(106, 'scm_column_step: column temperature became NaN')
         h_out = -999.0D0
         RETURN
      END IF

      col%h_last = h
      h_out = h
   END SUBROUTINE scm_column_step

   !=====================================================================
   ! boundary-layer height by the bulk Richardson number method
   !=====================================================================
   FUNCTION scm_diagnose_h(col, prm, thv, ustar, wthv) RESULT(h)
      TYPE(dts_scm_column), INTENT(IN) :: col
      TYPE(dts_scm_params), INTENT(IN) :: prm
      REAL(KIND(1D0)), INTENT(IN) :: thv(col%n)
      REAL(KIND(1D0)), INTENT(IN) :: ustar, wthv
      REAL(KIND(1D0)) :: h

      REAL(KIND(1D0)) :: du2(col%n), thv_ref, wstar, w_m

      du2 = (col%u - col%u(1))**2 + (col%v - col%v(1))**2 + 100.0D0*ustar**2

      h = scm_rib_scan(col, prm, thv, du2, thv(1))
      IF (wthv > 1.0D-6) THEN
         wstar = (GRAV/thv(1)*wthv*MAX(h, 10.0D0))**(1.0D0/3.0D0)
         w_m = (ustar**3 + prm%wstar_fac*wstar**3)**(1.0D0/3.0D0)
         thv_ref = thv(1) + prm%excess_b*wthv/MAX(w_m, 0.01D0)
         h = scm_rib_scan(col, prm, thv, du2, thv_ref)
      END IF
      h = MAX(h, col%z(1))
   END FUNCTION scm_diagnose_h

   FUNCTION scm_rib_scan(col, prm, thv, du2, thv_ref) RESULT(h)
      TYPE(dts_scm_column), INTENT(IN) :: col
      TYPE(dts_scm_params), INTENT(IN) :: prm
      REAL(KIND(1D0)), INTENT(IN) :: thv(col%n), du2(col%n), thv_ref
      REAL(KIND(1D0)) :: h
      REAL(KIND(1D0)) :: rib(col%n), w
      INTEGER :: k, k_cross

      rib = (GRAV/thv_ref)*(thv - thv_ref)*col%z/MAX(du2, 0.01D0)
      k_cross = 0
      DO k = 1, col%n
         IF (rib(k) >= prm%ric_h) THEN
            k_cross = k
            EXIT
         END IF
      END DO
      IF (k_cross == 0) THEN
         h = col%z(col%n)
      ELSE IF (k_cross == 1) THEN
         h = col%z(1)
      ELSE
         w = (prm%ric_h - rib(k_cross - 1))/MAX(rib(k_cross) - rib(k_cross - 1), 1.0D-12)
         h = col%z(k_cross - 1) + w*(col%z(k_cross) - col%z(k_cross - 1))
      END IF
   END FUNCTION scm_rib_scan

   !=====================================================================
   ! local mixing-length diffusivities at internal interfaces
   !=====================================================================
   SUBROUTINE scm_local_k(col, prm, thv, k_m_loc, k_h_loc)
      TYPE(dts_scm_column), INTENT(IN) :: col
      TYPE(dts_scm_params), INTENT(IN) :: prm
      REAL(KIND(1D0)), INTENT(IN) :: thv(col%n)
      REAL(KIND(1D0)), INTENT(OUT) :: k_m_loc(col%n - 1), k_h_loc(col%n - 1)

      REAL(KIND(1D0)) :: zi_k, dthv, du, dv, shear2, thv_i, n2, ri, lmix, f_stab, f_unst, f, kk
      INTEGER :: k

      DO k = 1, col%n - 1
         zi_k = col%zi(k + 1)
         dthv = (thv(k + 1) - thv(k))/col%dzc(k)
         du = (col%u(k + 1) - col%u(k))/col%dzc(k)
         dv = (col%v(k + 1) - col%v(k))/col%dzc(k)
         shear2 = du**2 + dv**2 + 1.0D-8
         thv_i = 0.5D0*(thv(k) + thv(k + 1))
         n2 = GRAV/thv_i*dthv
         ri = n2/shear2

         lmix = 1.0D0/(1.0D0/(VONK*zi_k) + 1.0D0/prm%lambda_mix)
         IF (prm%stable_fn == 0) THEN
            f_stab = MAX(1.0D0 - ri/prm%ric_stable, 0.0D0)**2
         ELSE
            f_stab = 1.0D0/(1.0D0 + 10.0D0*ri)
         END IF
         f_unst = SQRT(MAX(1.0D0 - 16.0D0*ri, 1.0D0))
         IF (ri >= 0.0D0) THEN
            f = f_stab
         ELSE
            f = f_unst
         END IF
         kk = lmix**2*SQRT(shear2)*f
         k_m_loc(k) = kk
         k_h_loc(k) = kk
      END DO
   END SUBROUTINE scm_local_k

   !=====================================================================
   ! Troen-Mahrt non-local K-profile, blended with the local values
   !=====================================================================
   SUBROUTINE scm_nonlocal_blend(col, prm, thv, h, ustar, wthv, &
                                 k_m_loc, k_h_loc, k_m, k_h, wstar, w_m)
      TYPE(dts_scm_column), INTENT(IN) :: col
      TYPE(dts_scm_params), INTENT(IN) :: prm
      REAL(KIND(1D0)), INTENT(IN) :: thv(col%n), h, ustar, wthv
      REAL(KIND(1D0)), INTENT(IN) :: k_m_loc(col%n - 1), k_h_loc(col%n - 1)
      REAL(KIND(1D0)), INTENT(INOUT) :: k_m(col%n + 1), k_h(col%n + 1)
      REAL(KIND(1D0)), INTENT(OUT) :: wstar, w_m

      REAL(KIND(1D0)) :: obukhov, zeta, phi_m, phi_h, pr, zi_k, shape, k_nl
      INTEGER :: k

      wstar = (GRAV/thv(1)*wthv*h)**(1.0D0/3.0D0)
      w_m = (ustar**3 + prm%wstar_fac*wstar**3)**(1.0D0/3.0D0)

      obukhov = -(ustar**3)*thv(1)/(VONK*GRAV*MAX(wthv, 1.0D-9))
      zeta = 0.1D0*h/obukhov
      phi_m = (1.0D0 - 16.0D0*zeta)**(-0.25D0)
      phi_h = (1.0D0 - 16.0D0*zeta)**(-0.5D0)
      pr = phi_h/phi_m + prm%cg_a*VONK*0.1D0*wstar/MAX(w_m, 1.0D-6)

      DO k = 1, col%n - 1
         zi_k = col%zi(k + 1)
         shape = MIN(MAX(zi_k/h, 0.0D0), 1.0D0)
         k_nl = VONK*w_m*zi_k*(1.0D0 - shape)**2
         IF (zi_k >= h) k_nl = 0.0D0
         k_m(k + 1) = MAX(k_nl, k_m_loc(k))
         k_h(k + 1) = MAX(k_nl/pr, k_h_loc(k))
      END DO
   END SUBROUTINE scm_nonlocal_blend

   !=====================================================================
   ! explicit flux-form tendency from the counter-gradient term
   !=====================================================================
   SUBROUTINE scm_counter_gradient_src(col, k_h, h, gamma, src)
      TYPE(dts_scm_column), INTENT(IN) :: col
      REAL(KIND(1D0)), INTENT(IN) :: k_h(col%n + 1), h, gamma
      REAL(KIND(1D0)), INTENT(OUT) :: src(col%n)

      REAL(KIND(1D0)) :: flux(col%n + 1)
      INTEGER :: k

      flux = 0.0D0
      DO k = 2, col%n
         IF (col%zi(k) > 0.0D0 .AND. col%zi(k) < h) THEN
            flux(k) = k_h(k)*gamma
         END IF
      END DO
      DO k = 1, col%n
         src(k) = -(flux(k + 1) - flux(k))/col%dz(k)
      END DO
   END SUBROUTINE scm_counter_gradient_src

   !=====================================================================
   ! backward-Euler implicit diffusion in conservative flux form
   ! (port of ColumnModel._assemble_diffusion)
   !=====================================================================
   SUBROUTINE scm_diffuse(col, phi, k_iface, dt, flux_sfc, drag, src)
      TYPE(dts_scm_column), INTENT(IN) :: col
      REAL(KIND(1D0)), INTENT(INOUT) :: phi(col%n)
      REAL(KIND(1D0)), INTENT(IN) :: k_iface(col%n + 1)
      REAL(KIND(1D0)), INTENT(IN) :: dt, flux_sfc, drag
      REAL(KIND(1D0)), INTENT(IN) :: src(col%n)

      INTEGER :: n, k
      REAL(KIND(1D0)), DIMENSION(col%n) :: lower, diag, upper, rhs, x
      REAL(KIND(1D0)) :: cu, cl

      n = col%n
      lower = 0.0D0
      upper = 0.0D0
      diag = 1.0D0
      rhs = phi

      DO k = 1, n - 1
         cu = dt*k_iface(k + 1)/(col%dz(k)*col%dzc(k))
         cl = dt*k_iface(k + 1)/(col%dz(k + 1)*col%dzc(k))
         upper(k) = -cu
         lower(k + 1) = -cl
         diag(k) = diag(k) + cu
         diag(k + 1) = diag(k + 1) + cl
      END DO

      rhs(1) = rhs(1) + dt*flux_sfc/col%dz(1)
      diag(1) = diag(1) + dt*drag/col%dz(1)
      rhs = rhs + dt*src

      CALL scm_solve_tridiag(lower, diag, upper, rhs, x, n)
      phi = x
   END SUBROUTINE scm_diffuse

   !=====================================================================
   ! wind nudging towards a log profile anchored at the observed speed
   !=====================================================================
   SUBROUTINE scm_wind_nudge(col, prm, dt, u_obs, z_meas)
      TYPE(dts_scm_column), INTENT(INOUT) :: col
      TYPE(dts_scm_params), INTENT(IN) :: prm
      REAL(KIND(1D0)), INTENT(IN) :: dt, u_obs, z_meas
      REAL(KIND(1D0)) :: fac, u_target
      INTEGER :: k
      fac = dt/prm%tau_wind
      DO k = 1, col%n
         u_target = u_obs*scm_wind_shape(col%z(k), z_meas, prm%z0m_wind)
         col%u(k) = col%u(k) + fac*(u_target - col%u(k))
         col%v(k) = col%v(k) + fac*(0.0D0 - col%v(k))
      END DO
   END SUBROUTINE scm_wind_nudge

   !=====================================================================
   ! background atmosphere: profiles on the column grid at a given time
   !=====================================================================
   SUBROUTINE scm_background_on_grid(bg, col, t_sec, theta_bg, q_bg)
      TYPE(dts_scm_background), INTENT(IN) :: bg
      TYPE(dts_scm_column), INTENT(IN) :: col
      REAL(KIND(1D0)), INTENT(IN) :: t_sec ! time since run start [s]
      REAL(KIND(1D0)), INTENT(OUT) :: theta_bg(col%n), q_bg(col%n)

      INTEGER :: it, k
      ! latest snapshot at or before t_sec (clamped to the first one)
      it = 1
      DO k = 2, bg%nt
         IF (bg%t_sec(k) <= t_sec) THEN
            it = k
         ELSE
            EXIT
         END IF
      END DO
      DO k = 1, col%n
         theta_bg(k) = scm_interp1(col%z(k), bg%z, bg%theta(it, :), bg%nz)
         q_bg(k) = scm_interp1(col%z(k), bg%z, bg%q(it, :), bg%nz)
      END DO
   END SUBROUTINE scm_background_on_grid

   !=====================================================================
   ! advective ventilation towards the background air, tau = L / U
   !=====================================================================
   SUBROUTINE scm_ventilate(col, prm, dt, theta_bg, q_bg, h_bl, tau_out)
      TYPE(dts_scm_column), INTENT(INOUT) :: col
      TYPE(dts_scm_params), INTENT(IN) :: prm
      REAL(KIND(1D0)), INTENT(IN) :: dt, h_bl
      REAL(KIND(1D0)), INTENT(IN) :: theta_bg(col%n), q_bg(col%n)
      REAL(KIND(1D0)), INTENT(OUT) :: tau_out ! [s]

      REAL(KIND(1D0)) :: z_cap, u_sum, u_mean, fac
      INTEGER :: k, n_below

      z_cap = MAX(1.2D0*h_bl, 800.0D0)
      u_sum = 0.0D0
      n_below = 0
      DO k = 1, col%n
         IF (col%z(k) <= z_cap) THEN
            u_sum = u_sum + HYPOT(col%u(k), col%v(k))
            n_below = n_below + 1
         END IF
      END DO
      u_mean = u_sum/MAX(n_below, 1)
      tau_out = MAX(prm%city_length/MAX(u_mean, 0.5D0), prm%tau_adv_min)
      fac = dt/tau_out
      col%theta = col%theta + fac*(theta_bg - col%theta)
      col%q = col%q + fac*(q_bg - col%q)
   END SUBROUTINE scm_ventilate

   !=====================================================================
   ! sample the column at the measurement height
   !=====================================================================
   SUBROUTINE scm_sample_zmeas(col, z_meas, tair_c, rh_pct, wind, q_zm, p_zm)
      TYPE(dts_scm_column), INTENT(IN) :: col
      REAL(KIND(1D0)), INTENT(IN) :: z_meas ! [m]
      REAL(KIND(1D0)), INTENT(OUT) :: tair_c ! [degC]
      REAL(KIND(1D0)), INTENT(OUT) :: rh_pct ! [%]
      REAL(KIND(1D0)), INTENT(OUT) :: wind ! [m s-1]
      REAL(KIND(1D0)), INTENT(OUT) :: q_zm ! [kg kg-1]
      REAL(KIND(1D0)), INTENT(OUT) :: p_zm ! [Pa]

      REAL(KIND(1D0)) :: theta_zm, u_zm, v_zm, t_air

      theta_zm = scm_interp1(z_meas, col%z, col%theta, col%n)
      q_zm = scm_interp1(z_meas, col%z, col%q, col%n)
      u_zm = scm_interp1(z_meas, col%z, col%u, col%n)
      v_zm = scm_interp1(z_meas, col%z, col%v, col%n)
      p_zm = scm_interp1(z_meas, col%z, col%p_levels, col%n)

      t_air = theta_zm*scm_exner(p_zm)
      tair_c = t_air - 273.15D0
      rh_pct = MIN(MAX(scm_rh_from_q(q_zm, t_air, p_zm), 2.0D0), 100.0D0)
      wind = MAX(HYPOT(u_zm, v_zm), 0.5D0)
   END SUBROUTINE scm_sample_zmeas

   !=====================================================================
   ! convert SUEWS energy fluxes to kinematic fluxes for the column
   ! (port of the conversion in CoupledSCM.run)
   !=====================================================================
   SUBROUTINE scm_kinematic_fluxes(qh_wm2, qe_wm2, tair_k, p_pa, q_kgkg, wth, wq)
      REAL(KIND(1D0)), INTENT(IN) :: qh_wm2 ! sensible heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: qe_wm2 ! latent heat flux [W m-2]
      REAL(KIND(1D0)), INTENT(IN) :: tair_k ! air temperature at z_meas [K]
      REAL(KIND(1D0)), INTENT(IN) :: p_pa ! pressure at z_meas [Pa]
      REAL(KIND(1D0)), INTENT(IN) :: q_kgkg ! specific humidity at z_meas [kg kg-1]
      REAL(KIND(1D0)), INTENT(OUT) :: wth ! [K m s-1]
      REAL(KIND(1D0)), INTENT(OUT) :: wq ! [kg kg-1 m s-1]
      REAL(KIND(1D0)) :: rho
      rho = scm_rho_air(tair_k, p_pa, q_kgkg)
      wth = qh_wm2/(rho*CP_AIR)
      wq = qe_wm2/(rho*LV)
   END SUBROUTINE scm_kinematic_fluxes

END MODULE module_phys_scm
