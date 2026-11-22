! Probe for heatcond1d_seb_implicit: the implicit EHC conduction column whose
! top face is closed by a linearised surface energy balance.
! Reads one scenario from standard input and prints "key value" lines; the
! assertions live in test/physics/test_ehc_implicit_solver.py.
!
! Scenarios (first input line), parameters on the second line:
!   steady     n                 layered slab, Robin top, fixed bottom, to steady state
!   insulated  n nstep           insulated bottom, time-varying surface forcing
!   sine       n depth dt nper mode h
!              homogeneous slab under a diurnal sinusoid; mode 0 prescribes the
!              surface temperature (b -> large), mode 1 forces air temperature
!              through the conductance h; errors against the semi-infinite
!              analytical solution over the last period
!   water      (no parameters) one EHC facet-route step with only water
!              covered, explicit and implicit coupling, and heatcond1d_CN
!              called directly on the same column
PROGRAM ehc_implicit_probe
   USE module_phys_ehc_heatflux, ONLY: heatcond1d_seb_implicit, heatcond1d_CN
   IMPLICIT NONE
   CHARACTER(LEN=16) :: scenario

   READ (*, *) scenario
   SELECT CASE (TRIM(scenario))
   CASE ('steady')
      CALL run_steady()
   CASE ('insulated')
      CALL run_insulated()
   CASE ('sine')
      CALL run_sine()
   CASE ('profile')
      CALL run_profile()
   CASE ('indoor')
      CALL run_indoor()
   CASE ('invalid')
      CALL run_invalid()
   CASE ('water')
      CALL run_water()
   CASE DEFAULT
      PRINT '(A)', 'error unknown-scenario'
   END SELECT

CONTAINS

   SUBROUTINE run_invalid()
      USE, INTRINSIC :: ieee_arithmetic, ONLY: IEEE_VALUE, IEEE_QUIET_NAN, IEEE_POSITIVE_INF
      USE module_ctrl_error_state, ONLY: reset_supy_error, get_supy_error
      INTEGER :: mode, code
      CHARACTER(LEN=512) :: message
      REAL(KIND(1D0)) :: t(2), cap(2), g(3), gt(1), a(1), b(1), dt, tb, ts(1), qe(1), qt, qb, du
      READ (*, *) mode
      CALL reset_supy_error()
      t = 10.0D0
      cap = 1.0D5
      g = 2.0D0
      gt = 2.0D0
      a = 300.0D0
      b = 10.0D0
      dt = 300.0D0
      tb = 10.0D0
      SELECT CASE (mode)
      CASE (1)
         cap(2) = 0.0D0
      CASE (2)
         g(2) = -1.0D0
      CASE (3)
         gt = IEEE_VALUE(0.0D0, IEEE_QUIET_NAN)
      CASE (4)
         b = -1.0D0
      CASE (5)
         a = IEEE_VALUE(0.0D0, IEEE_QUIET_NAN)
      CASE (6)
         dt = IEEE_VALUE(0.0D0, IEEE_POSITIVE_INF)
      CASE (7)
         tb = IEEE_VALUE(0.0D0, IEEE_QUIET_NAN)
      END SELECT
      IF (mode == 8) THEN
         CALL heatcond1d_seb_implicit(t, cap(:1), g, dt, tb, gt, a, b, ts, qe, qt, qb, du)
      ELSE
         CALL heatcond1d_seb_implicit(t, cap, g, dt, tb, gt, a, b, ts, qe, qt, qb, du)
      END IF
      CALL get_supy_error(code, message)
      PRINT '(A,I0)', 'code ', code
      PRINT '(A,ES24.16)', 'temperature_change ', MAXVAL(ABS(t - 10.0D0))
      PRINT '(A,ES24.16)', 'unsafe_outputs ', MAXVAL(ABS([ts, qe, qt, qb, du] + 999.0D0))
   END SUBROUTINE run_invalid

   SUBROUTINE layered(n, dz, k, rc)
      ! a deliberately non-uniform layered slab
      INTEGER, INTENT(IN) :: n
      REAL(KIND(1D0)), DIMENSION(n), INTENT(OUT) :: dz, k, rc
      INTEGER :: i
      DO i = 1, n
         dz(i) = 0.02D0*(1.0D0 + 0.5D0*MOD(i, 3))
         k(i) = 0.8D0 + 0.3D0*MOD(i, 4)
         rc(i) = 1.5D6 + 2.0D5*MOD(i, 5)
      END DO
   END SUBROUTINE layered

   SUBROUTINE faces(n, dz, k, g)
      INTEGER, INTENT(IN) :: n
      REAL(KIND(1D0)), DIMENSION(n), INTENT(IN) :: dz, k
      REAL(KIND(1D0)), DIMENSION(n + 1), INTENT(OUT) :: g
      INTEGER :: i
      g(1) = 2.0D0*k(1)/dz(1)
      DO i = 1, n - 1
         g(i + 1) = 2.0D0*k(i)*k(i + 1)/(k(i + 1)*dz(i) + k(i)*dz(i + 1))
      END DO
      g(n + 1) = 2.0D0*k(n)/dz(n)
   END SUBROUTINE faces

   SUBROUTINE run_steady()
      INTEGER :: n, istep
      REAL(KIND(1D0)), ALLOCATABLE :: dz(:), k(:), rc(:), g(:), T(:)
      REAL(KIND(1D0)) :: ts(1), qe(1), q_top, q_bot, dudt, h, t_eq, t_b, r_total, q_exact
      READ (*, *) n
      ALLOCATE (dz(n), k(n), rc(n), g(n + 1), T(n))
      CALL layered(n, dz, k, rc)
      CALL faces(n, dz, k, g)
      h = 20.0D0
      t_eq = 30.0D0
      t_b = 5.0D0
      T = 12.0D0
      DO istep = 1, 4000
         CALL heatcond1d_seb_implicit(T, rc*dz, g, 3600.0D0, t_b, g(1:1), [h*t_eq], [h], &
                                      ts, qe, q_top, q_bot, dudt)
      END DO
      r_total = 1.0D0/h + SUM(1.0D0/g)
      q_exact = (t_eq - t_b)/r_total
      PRINT '(A,ES24.16)', 'q_top ', q_top
      PRINT '(A,ES24.16)', 'q_bottom ', q_bot
      PRINT '(A,ES24.16)', 'dudt ', dudt
      PRINT '(A,ES24.16)', 'q_exact ', q_exact
      PRINT '(A,ES24.16)', 'ts ', ts(1)
      PRINT '(A,ES24.16)', 'ts_exact ', t_eq - q_exact/h
   END SUBROUTINE run_steady

   SUBROUTINE run_insulated()
      INTEGER :: n, nstep, istep
      REAL(KIND(1D0)), ALLOCATABLE :: dz(:), k(:), rc(:), g(:), T(:), T0(:)
      REAL(KIND(1D0)) :: ts(2), qe(2), q_top, q_bot, dudt, a(2), b(2), gt(2), dt, time_s
      REAL(KIND(1D0)) :: max_budget, max_bottom, max_seb, u_change, q_sum
      READ (*, *) n, nstep
      ALLOCATE (dz(n), k(n), rc(n), g(n + 1), T(n), T0(n))
      CALL layered(n, dz, k, rc)
      CALL faces(n, dz, k, g)
      g(n + 1) = 0.0D0
      T = 10.0D0
      T0 = T
      dt = 300.0D0
      ! two surface nodes sharing the column, as on the plan-area slab
      gt = [0.6D0, 0.4D0]*g(1)
      max_budget = 0.0D0
      max_bottom = 0.0D0
      max_seb = 0.0D0
      q_sum = 0.0D0
      DO istep = 1, nstep
         time_s = istep*dt
         b = [0.6D0*(15.0D0 + 4.0D0), 0.4D0*(25.0D0 + 5.0D0)]
         a = [0.6D0*(400.0D0*MAX(0.0D0, SIN(7.27D-5*time_s)) + 15.0D0*(10.0D0 + 5.0D0*SIN(7.27D-5*time_s)) + 4.0D0*12.0D0), &
              0.4D0*(150.0D0*MAX(0.0D0, SIN(7.27D-5*time_s)) + 25.0D0*(10.0D0 + 5.0D0*SIN(7.27D-5*time_s)) + 5.0D0*12.0D0)]
         CALL heatcond1d_seb_implicit(T, rc*dz, g, dt, 0.0D0, gt, a, b, ts, qe, q_top, q_bot, dudt)
         max_budget = MAX(max_budget, ABS(q_top - q_bot - dudt))
         max_bottom = MAX(max_bottom, ABS(q_bot))
         max_seb = MAX(max_seb, MAXVAL(ABS(a - b*ts - qe)))
         q_sum = q_sum + q_top*dt
      END DO
      u_change = SUM(rc*dz*(T - T0))
      PRINT '(A,ES24.16)', 'max_abs_qtop_minus_dudt ', max_budget
      PRINT '(A,ES24.16)', 'max_abs_qbottom ', max_bottom
      PRINT '(A,ES24.16)', 'max_abs_seb_residual ', max_seb
      PRINT '(A,ES24.16)', 'energy_in ', q_sum
      PRINT '(A,ES24.16)', 'energy_stored ', u_change
   END SUBROUTINE run_insulated

   SUBROUTINE run_sine()
      INTEGER :: n, nper, mode, istep, nstep, nlast
      REAL(KIND(1D0)) :: depth, dt, h, kc, rcc, omega, amp, d, time_s, b_big
      REAL(KIND(1D0)), ALLOCATABLE :: dz(:), k(:), rc(:), g(:), T(:)
      REAL(KIND(1D0)) :: ts(1), qe(1), q_top, q_bot, dudt, a(1), b(1)
      COMPLEX(KIND(1D0)) :: y_adm, ts_hat, g_hat, ta_hat
      REAL(KIND(1D0)) :: g_ref, ts_ref, err_g2, err_ts2, ref_g2, ref_ts2
      REAL(KIND(1D0)), PARAMETER :: pi = 3.14159265358979323846D0

      READ (*, *) n, depth, dt, nper, mode, h
      kc = 1.2D0
      rcc = 2.0D6
      omega = 2.0D0*pi/86400.0D0
      amp = 10.0D0
      d = SQRT(2.0D0*kc/rcc/omega)
      ALLOCATE (dz(n), k(n), rc(n), g(n + 1), T(n))
      dz = depth/n
      k = kc
      rc = rcc
      CALL faces(n, dz, k, g)
      b_big = 1.0D12

      ! semi-infinite solution: G = Y*Ts with admittance Y = k(1+i)/d;
      ! forcing amp*sin(omega t) = Re(-i amp exp(i omega t))
      y_adm = CMPLX(kc/d, kc/d, KIND(1D0))
      ta_hat = CMPLX(0.0D0, -amp, KIND(1D0))
      IF (mode == 0) THEN
         ts_hat = ta_hat
      ELSE
         ts_hat = h*ta_hat/(h + y_adm)
      END IF
      g_hat = y_adm*ts_hat
      ! start on the periodic solution at the cell centres, so no start-up
      ! transient (which decays only as t**-0.5) pollutes the error
      DO istep = 1, n
         T(istep) = REAL(ts_hat*EXP(-CMPLX(1.0D0, 1.0D0, KIND(1D0))*(istep - 0.5D0)*dz(istep)/d))
      END DO

      nstep = NINT(nper*86400.0D0/dt)
      nlast = NINT(86400.0D0/dt)
      err_g2 = 0.0D0
      err_ts2 = 0.0D0
      ref_g2 = 0.0D0
      ref_ts2 = 0.0D0
      DO istep = 1, nstep
         time_s = istep*dt
         IF (mode == 0) THEN
            b(1) = b_big
            a(1) = b_big*amp*SIN(omega*time_s)
         ELSE
            b(1) = h
            a(1) = h*amp*SIN(omega*time_s)
         END IF
         CALL heatcond1d_seb_implicit(T, rc*dz, g, dt, 0.0D0, g(1:1), a, b, ts, qe, q_top, q_bot, dudt)
         IF (istep > nstep - nlast) THEN
            g_ref = REAL(g_hat*EXP(CMPLX(0.0D0, omega*time_s, KIND(1D0))))
            ts_ref = REAL(ts_hat*EXP(CMPLX(0.0D0, omega*time_s, KIND(1D0))))
            err_g2 = err_g2 + (q_top - g_ref)**2
            err_ts2 = err_ts2 + (ts(1) - ts_ref)**2
            ref_g2 = ref_g2 + g_ref**2
            ref_ts2 = ref_ts2 + ts_ref**2
         END IF
      END DO
      PRINT '(A,ES24.16)', 'damping_depth ', d
      PRINT '(A,ES24.16)', 'rel_rms_err_flux ', SQRT(err_g2/ref_g2)
      PRINT '(A,ES24.16)', 'rel_rms_err_ts ', SQRT(err_ts2/ref_ts2)
      PRINT '(A,ES24.16)', 'flux_amplitude ', ABS(g_hat)
   END SUBROUTINE run_sine

   SUBROUTINE run_profile()
      USE module_phys_ehc, ONLY: ehc_init_profile
      REAL(KIND(1D0)) :: temp(3), rsi, outer, inner
      READ (*, *) rsi, outer, inner
      temp = -99.0D0
      CALL ehc_init_profile([0.02D0, 0.10D0, 0.04D0], [0.2D0, 0.5D0, 2.0D0], outer, inner, rsi, temp)
      PRINT '(A,ES24.16)', 't1 ', temp(1)
      PRINT '(A,ES24.16)', 't2 ', temp(2)
      PRINT '(A,ES24.16)', 't3 ', temp(3)
   END SUBROUTINE run_profile

   SUBROUTINE run_indoor()
      ! Five identical layers, steady heat flow from 20 degC indoors to
      ! 0 degC outdoor air with h_out=10. Exercise the actual EHC assembly,
      ! including its plan-area building branch, not only the column solver.
      USE module_phys_ehc, ONLY: EHC, dts_ehc_implicit
      USE module_ctrl_type, ONLY: error_state
      USE module_ctrl_const_allocate, ONLY: nsurf, ndepth, BldgSurf
      REAL(KIND(1D0)) :: tr(1, ndepth), tw(1, ndepth), kr(1, ndepth), cr(1, ndepth), dzr(1, ndepth)
      REAL(KIND(1D0)) :: tor(1, ndepth), tow(1, ndepth), qsr(1), qsw(1)
      REAL(KIND(1D0)) :: ts(nsurf, ndepth), ks(nsurf, ndepth), cs(nsurf, ndepth), dzs(nsurf, ndepth)
      REAL(KIND(1D0)) :: tos(nsurf, ndepth), tfs(nsurf, ndepth), tss(nsurf, ndepth), qss(nsurf), sfr(nsurf)
      REAL(KIND(1D0)) :: qs, dudt, qb, qi, rr, rw, qr, qw, area_r, area_w
      INTEGER :: i, mode
      TYPE(dts_ehc_implicit) :: seb
      TYPE(error_state) :: err
      READ (*, *) mode, rr, rw
      kr = 1.0D0
      cr = 2.0D6
      dzr = 0.10D0
      ks = 1.0D0
      cs = 2.0D6
      dzs = 0.10D0
      qr = 20.0D0/(0.10D0 + ndepth*0.10D0 + rr)
      qw = 20.0D0/(0.10D0 + ndepth*0.10D0 + rw)
      DO i = 1, ndepth
         tr(1, i) = qr*(0.10D0 + (i - 0.5D0)*0.10D0)
         tw(1, i) = qw*(0.10D0 + (i - 0.5D0)*0.10D0)
         ts(:, i) = tr(1, i)
      END DO
      sfr = 0.0D0
      area_r = 0.4D0
      area_w = 0.6D0
      IF (mode == 1) THEN
         sfr(BldgSurf) = 1.0D0
         area_r = 0.0D0
         area_w = 0.0D0
      END IF
      ALLOCATE (seb%a_roof(1), seb%b_roof(1), seb%a_wall(1), seb%b_wall(1), &
                seb%tsfc_roof(1), seb%tsfc_wall(1))
      seb%a_roof = 0.0D0
      seb%a_wall = 0.0D0
      seb%a_surf = 0.0D0
      seb%b_roof = 10.0D0
      seb%b_wall = 10.0D0
      seb%b_surf = 10.0D0
      seb%t_indoor = 20.0D0
      seb%t_deep_surf = 20.0D0
      seb%r_indoor_roof = rr
      seb%r_indoor_wall = rw
      tfs = ts
      tss = ts
      CALL EHC(300, 1, mode == 1, &
               [qr/10.0D0], [20.0D0], tr, kr, cr, dzr, [area_r], &
               [qw/10.0D0], [20.0D0], tw, kr, cr, dzr, [area_w], &
               SPREAD(qr/10.0D0, 1, nsurf), SPREAD(20.0D0, 1, nsurf), ts, ks, cs, dzs, sfr, &
               tor, qsr, tow, qsw, tos, tfs, tss, qss, qs, dudt, qb, qi, err, seb)
      PRINT '(A,ES24.16)', 'q_indoor ', qi
      PRINT '(A,ES24.16)', 'q_bottom ', qb
      PRINT '(A,ES24.16)', 'dudt ', dudt
      IF (mode == 1) THEN
         PRINT '(A,ES24.16)', 'temp_change ', MAXVAL(ABS(tos(BldgSurf, :) - ts(BldgSurf, :)))
      ELSE
         PRINT '(A,ES24.16)', 'temp_change ', MAX(MAXVAL(ABS(tor - tr)), MAXVAL(ABS(tow - tw)))
         PRINT '(A,ES24.16)', 'qs_roof ', qsr(1)
         PRINT '(A,ES24.16)', 'qs_wall ', qsw(1)
      END IF
   END SUBROUTINE run_indoor

   SUBROUTINE run_water()
      ! Water keeps heatcond1d_CN in the implicit coupling: EHC must return
      ! the same QS and layer temperatures for water as the explicit route
      ! and as heatcond1d_CN itself, and leave the water Tsfc for the driver.
      USE module_phys_ehc, ONLY: EHC, dts_ehc_implicit
      USE module_ctrl_type, ONLY: error_state
      USE module_ctrl_const_allocate, ONLY: nsurf, ndepth, WaterSurf, PavSurf
      INTEGER, PARAMETER :: nlayer = 1
      REAL(KIND(1D0)), DIMENSION(nlayer) :: tsfc_r, tin_r, sfr_r, tsfc_w, tin_w, sfr_w, qs_r, qs_w
      REAL(KIND(1D0)), DIMENSION(nlayer, ndepth) :: temp_r, k_r, cp_r, dz_r, tout_r, tout_w
      REAL(KIND(1D0)), DIMENSION(nsurf) :: tsfc_s, tin_s, sfr_s, qs_s_exp, qs_s_imp
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth) :: temp_s, k_s, cp_s, dz_s, tout_s_exp, tout_s_imp
      REAL(KIND(1D0)), DIMENSION(nsurf, ndepth) :: tfast_s, tslow_s
      REAL(KIND(1D0)), DIMENSION(ndepth) :: t_cn
      REAL(KIND(1D0)) :: qs, dudt, qbot, qin, qs_cn, qbot_cn
      TYPE(error_state) :: err
      TYPE(dts_ehc_implicit) :: seb
      INTEGER :: i

      DO i = 1, ndepth
         temp_s(:, i) = 15.0D0 - 1.0D0*i
         temp_r(:, i) = 15.0D0 - 1.0D0*i
      END DO
      dz_s = 0.1D0
      k_s = 0.6D0
      cp_s = 4.18D6
      dz_r = 0.05D0
      k_r = 1.0D0
      cp_r = 1.5D6
      tsfc_s = 18.0D0
      tin_s = 9.0D0
      sfr_s = 0.0D0
      sfr_s(WaterSurf) = 0.6D0
      sfr_s(PavSurf) = 0.4D0
      tsfc_r = 18.0D0
      tin_r = 20.0D0
      sfr_r = 0.0D0
      tsfc_w = 18.0D0
      tin_w = 20.0D0
      sfr_w = 0.0D0

      tfast_s = temp_s
      tslow_s = temp_s
      CALL EHC(300, nlayer, .FALSE., &
               tsfc_r, tin_r, temp_r, k_r, cp_r, dz_r, sfr_r, &
               tsfc_w, tin_w, temp_r, k_r, cp_r, dz_r, sfr_w, &
               tsfc_s, tin_s, temp_s, k_s, cp_s, dz_s, sfr_s, &
               tout_r, qs_r, tout_w, qs_w, tout_s_exp, tfast_s, tslow_s, qs_s_exp, qs, &
               dudt, qbot, qin, err)

      ALLOCATE (seb%a_roof(nlayer), seb%b_roof(nlayer), seb%a_wall(nlayer), seb%b_wall(nlayer), &
                seb%tsfc_roof(nlayer), seb%tsfc_wall(nlayer))
      seb%a_surf = 400.0D0 + 20.0D0*10.0D0
      seb%b_surf = 20.0D0
      seb%a_roof = 0.0D0
      seb%b_roof = 20.0D0
      seb%a_wall = 0.0D0
      seb%b_wall = 20.0D0
      seb%t_deep_surf = tin_s
      seb%t_indoor = 20.0D0
      tfast_s = temp_s
      tslow_s = temp_s
      CALL EHC(300, nlayer, .FALSE., &
               tsfc_r, tin_r, temp_r, k_r, cp_r, dz_r, sfr_r, &
               tsfc_w, tin_w, temp_r, k_r, cp_r, dz_r, sfr_w, &
               tsfc_s, tin_s, temp_s, k_s, cp_s, dz_s, sfr_s, &
               tout_r, qs_r, tout_w, qs_w, tout_s_imp, tfast_s, tslow_s, qs_s_imp, qs, &
               dudt, qbot, qin, err, seb)

      t_cn = temp_s(WaterSurf, :)
      CALL heatcond1d_CN(t_cn, qs_cn, dz_s(WaterSurf, :), 300.0D0, k_s(WaterSurf, :), cp_s(WaterSurf, :), &
                         [tsfc_s(WaterSurf), tin_s(WaterSurf)], qbot_cn)

      PRINT '(A,ES24.16)', 'water_qs_explicit ', qs_s_exp(WaterSurf)
      PRINT '(A,ES24.16)', 'water_qs_implicit ', qs_s_imp(WaterSurf)
      PRINT '(A,ES24.16)', 'water_qs_cn ', qs_cn
      PRINT '(A,ES24.16)', 'water_temp_maxdiff_explicit_implicit ', &
         MAXVAL(ABS(tout_s_exp(WaterSurf, :) - tout_s_imp(WaterSurf, :)))
      PRINT '(A,ES24.16)', 'water_temp_maxdiff_cn ', MAXVAL(ABS(tout_s_imp(WaterSurf, :) - t_cn))
      PRINT '(A,ES24.16)', 'water_tsfc_in ', tsfc_s(WaterSurf)
      PRINT '(A,ES24.16)', 'water_tsfc_implicit ', seb%tsfc_surf(WaterSurf)
      PRINT '(A,ES24.16)', 'paved_tsfc_implicit ', seb%tsfc_surf(PavSurf)
      PRINT '(A,ES24.16)', 'paved_qs_implicit ', qs_s_imp(PavSurf)
   END SUBROUTINE run_water

END PROGRAM ehc_implicit_probe
