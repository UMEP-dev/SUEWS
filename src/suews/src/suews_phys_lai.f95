MODULE module_phys_lai
   USE, INTRINSIC :: ieee_arithmetic, ONLY: ieee_is_finite
   USE module_ctrl_const_allocate, ONLY: nvegsurf
   USE module_ctrl_error_state, ONLY: set_supy_error, supy_error_flag

   IMPLICIT NONE

CONTAINS

   SUBROUTINE update_gddlai( &
      id, lai_calc_yes, & !input
      lat, lai_obs, &
      t_min_id_prev, t_max_id_prev, &
      base_t_gdd, base_t_sdd, &
      gdd_base, sdd_base, &
      lai_min, lai_max, lai_power, &
      lai_id_prev, &
      gdd_id, sdd_id, & !inout
      lai_id_next, & !output
      sfr_veg, & !optional input
      run_veg) !optional input

      IMPLICIT NONE

      !------------------------------------------------------------------------------
      ! Calculation of LAI from daily growing and senescence degree-day increments
      ! within fixed hemisphere-dependent growth and senescence seasons.
      !------------------------------------------------------------------------------

      INTEGER, INTENT(IN) :: id
      INTEGER, INTENT(IN) :: lai_calc_yes

      REAL(KIND(1D0)), INTENT(IN) :: lat
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: lai_obs
      REAL(KIND(1D0)), INTENT(IN) :: t_min_id_prev
      REAL(KIND(1D0)), INTENT(IN) :: t_max_id_prev

      ! --- Vegetation phenology ---------------------------------------------------------------------
      ! Parameters provided for each vegetation surface (EveTr, DecTr, Grass)
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: base_t_gdd ! Base temperature for growing degree days [degC]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: base_t_sdd ! Base temperature for senescence degree days [degC]

      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: gdd_base ! Growing degree days needed to initiate LAI growth [K d]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: sdd_base ! Senescence degree days needed to initiate LAI decline [K d]

      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: lai_min ! Min LAI [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: lai_max ! Max LAI [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(4, nvegsurf), INTENT(IN) :: lai_power ! Coeffs for LAI equation: 1,2 - leaf growth; 3,4 - leaf off

      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: gdd_id ! Growing degree days [K d]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: sdd_id ! Senescence degree days [K d]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: lai_id_prev ! LAI of previous day [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(OUT) :: lai_id_next ! Updated LAI for each veg surface [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN), OPTIONAL :: sfr_veg ! Surface fractions of veg surfaces [-]
      LOGICAL, DIMENSION(nvegsurf), INTENT(IN), OPTIONAL :: run_veg ! Explicit per-surface phenology execution mask [-]

      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: gdd_id_prev ! GDD of previous day [K d]
      REAL(KIND(1D0)), DIMENSION(nvegsurf) :: sdd_id_prev ! SDD of previous day [K d]

      REAL(KIND(1D0)) :: delta_gdd ! Daily change in GDD [K d]
      REAL(KIND(1D0)) :: delta_sdd ! Daily change in SDD [K d]

      INTEGER :: growth_state ! State of growth (0: dormant, 1: growing)
      INTEGER :: senescence_state ! State of senescence (0: not senescing, 1: senescing)
      INTEGER :: iv
      LOGICAL :: valid_observed_lai
      LOGICAL, DIMENSION(nvegsurf) :: active_veg
      LOGICAL, DIMENSION(nvegsurf) :: run_veg_local

      ! Hemisphere-specific fixed-DOY reset days for LAI phenology
      INTEGER :: summer_reset_day ! DOY when growth season ends and senescence season starts
      INTEGER :: winter_reset_day ! DOY when senescence season ends and growth season starts

      ! Where observed LAI is supplied, validate and assign it without reading
      ! inactive GDD/SDD phenology parameters (base_t_gdd, base_t_sdd,
      ! gdd_base, sdd_base, lai_min, lai_max, lai_power) (#2363).
      IF (lai_calc_yes == 0) THEN
         CALL observed_lai( &
            lai_obs=lai_obs, &
            lai_id_next=lai_id_next, &
            valid=valid_observed_lai &
         )
         RETURN
      END IF

      active_veg = .TRUE.
      IF (PRESENT(sfr_veg)) active_veg = (sfr_veg > 0.0D0)
      run_veg_local = .TRUE.
      IF (PRESENT(run_veg)) run_veg_local = run_veg

      CALL get_reset_days( &
         lat=lat, &
         summer_reset_day=summer_reset_day, &
         winter_reset_day=winter_reset_day &
      )

      ! Snapshot incoming accumulators for each vegetation surface
      gdd_id_prev = gdd_id
      sdd_id_prev = sdd_id

      ! Growth and senescence season flags depend only on DOY and hemisphere
      CALL update_growth_state( &
         id=id, &
         lat=lat, &
         summer_reset_day=summer_reset_day, &
         winter_reset_day=winter_reset_day, &
         growth_state=growth_state, &
         senescence_state=senescence_state &
      )

      ! Loop through vegetation types (iv)
      DO iv = 1, nvegsurf
         ! Skip absent vegetation surfaces whose phenology parameters are
         ! omitted/sentinel/non-finite (#2363), while preserving diagnostic
         ! DailyState evolution when an absent surface carries valid params.
         IF (.NOT. run_veg_local(iv)) THEN
            lai_id_next(iv) = lai_id_prev(iv)
            CYCLE
         END IF
         IF (.NOT. active_veg(iv)) THEN
            IF (.NOT. has_valid_absent_lai_params( &
                base_t_gdd(iv), base_t_sdd(iv), gdd_base(iv), sdd_base(iv), &
                lai_min(iv), lai_max(iv), lai_power(:, iv), lai_id_prev(iv))) THEN
               lai_id_next(iv) = lai_id_prev(iv)
               CYCLE
            END IF
         END IF

         ! Reset accumulators BEFORE computing today's degree-day increment so
         ! that boundary days (winter_reset_day / summer_reset_day) start the
         ! new season cleanly from zero without dropping today's increment or
         ! leaving stored accumulators inconsistent with delta_gdd/delta_sdd.
         CALL reset_degree_days( &
            id=id, &
            summer_reset_day=summer_reset_day, &
            winter_reset_day=winter_reset_day, &
            sdd=sdd_id_prev(iv), &
            gdd=gdd_id_prev(iv) &
         )

         ! Compute daily increment and updated cumulative GDD/SDD
         CALL calc_degree_days( &
            growth_state=growth_state, &
            senescence_state=senescence_state, &
            tmin_prev=t_min_id_prev, &
            tmax_prev=t_max_id_prev, &
            base_t_gdd=base_t_gdd(iv), &
            base_t_sdd=base_t_sdd(iv), &
            gdd_id_prev=gdd_id_prev(iv), &
            sdd_id_prev=sdd_id_prev(iv), &
            delta_gdd=delta_gdd, &
            delta_sdd=delta_sdd, &
            gdd_id=gdd_id(iv), &
            sdd_id=sdd_id(iv) &
         )

         IF (lai_calc_yes /= 0) THEN
            CALL calculate_lai( &
               growth_state=growth_state, &
               senescence_state=senescence_state, &
               delta_gdd=delta_gdd, &
               delta_sdd=delta_sdd, &
               gdd_id=gdd_id(iv), &
               sdd_id=sdd_id(iv), &
               gdd_base=gdd_base(iv), &
               sdd_base=sdd_base(iv), &
               lai_power=lai_power(:, iv), &
               lai_max=lai_max(iv), &
               lai_min=lai_min(iv), &
               lai_id_prev=lai_id_prev(iv), &
               lai_id_next=lai_id_next(iv) &
            )
            IF (supy_error_flag()) THEN
               lai_id_next = -999.0D0
               RETURN
            END IF
         END IF
      END DO ! End of loop over veg surfaces

   END SUBROUTINE update_gddlai


   PURE LOGICAL FUNCTION is_missing_sentinel(val) RESULT(is_missing)

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(IN) :: val

      ! Check finiteness before comparing so quiet NaNs never raise IEEE_INVALID.
      IF (.NOT. ieee_is_finite(val)) THEN
         is_missing = .TRUE.
         RETURN
      END IF
      is_missing = (ABS(val + 999.0D0) <= 1.0D-6)
   END FUNCTION is_missing_sentinel


   PURE LOGICAL FUNCTION has_valid_absent_lai_params( &
      base_t_gdd, base_t_sdd, gdd_base, sdd_base, &
      lai_min, lai_max, lai_power, lai_id_prev) RESULT(is_valid)

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(IN) :: base_t_gdd
      REAL(KIND(1D0)), INTENT(IN) :: base_t_sdd
      REAL(KIND(1D0)), INTENT(IN) :: gdd_base
      REAL(KIND(1D0)), INTENT(IN) :: sdd_base
      REAL(KIND(1D0)), INTENT(IN) :: lai_min
      REAL(KIND(1D0)), INTENT(IN) :: lai_max
      REAL(KIND(1D0)), DIMENSION(4), INTENT(IN) :: lai_power
      REAL(KIND(1D0)), INTENT(IN) :: lai_id_prev

      INTEGER :: k

      ! Check finiteness and recognised -999 missing sentinels on EVERY consumed
      ! coefficient BEFORE any ordering comparison so quiet NaNs never raise
      ! IEEE_INVALID, finite -999 sentinels (e.g. lai_power=-999) never reach
      ! calculate_lai, and legitimate negative thresholds (e.g. sdd_base=-1000)
      ! are preserved (#2363).
      is_valid = .FALSE.
      IF (is_missing_sentinel(base_t_gdd)) RETURN
      IF (is_missing_sentinel(base_t_sdd)) RETURN
      IF (is_missing_sentinel(gdd_base)) RETURN
      IF (is_missing_sentinel(sdd_base)) RETURN
      IF (is_missing_sentinel(lai_min)) RETURN
      IF (is_missing_sentinel(lai_max)) RETURN
      IF (is_missing_sentinel(lai_id_prev)) RETURN
      DO k = 1, 4
         IF (is_missing_sentinel(lai_power(k))) RETURN
      END DO

      IF (lai_id_prev < 0.0D0) RETURN
      IF (lai_min < 0.0D0 .OR. lai_max < 0.0D0) RETURN
      IF (lai_id_prev == 0.0D0 .AND. .NOT. (lai_min == 0.0D0 .AND. lai_max == 0.0D0)) THEN
         IF (lai_power(1) < 0.0D0 .OR. lai_power(3) < 0.0D0) RETURN
      END IF

      is_valid = .TRUE.
   END FUNCTION has_valid_absent_lai_params


   SUBROUTINE observed_lai( &
      lai_obs, lai_id_next, valid)

      IMPLICIT NONE

      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: lai_obs
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(OUT) :: lai_id_next
      LOGICAL, INTENT(OUT) :: valid

      valid = .FALSE.

      IF (ANY(.NOT. ieee_is_finite(lai_obs)) .OR. ANY(lai_obs < 0.0D0)) THEN
         ! Invalid lai_obs (NaN, +/-Inf, or negative) slipped past pre-flight;
         ! raise an error before mutating phenology state and assign a safe sentinel.
         lai_id_next = -999.0D0
         CALL set_supy_error( &
            105, &
            'update_GDDLAI: laimethod=0 requires finite non-missing lai_* or lai >= 0 at every timestep')
         RETURN
      END IF

      ! Observed-LAI override: when lai_calc_yes == 0, every timestep's forcing
      ! value must be a finite, non-negative observation (lai_obs >= 0).
      !
      ! A genuine zero observation (e.g. complete winter dieback) is valid and
      ! passes through unchanged. Missing/NaN/Inf values and strictly negative
      ! values -- including the -999 missing sentinel -- are rejected; choosing
      ! this path commits the user to providing an observation for every
      ! timestep. Observed LAI is intentionally not clipped to lai_min/lai_max,
      ! because those bounds describe the internal GDD/SDD phenology path.
      !
      ! The Python pre-flight validator (supy._check.check_forcing) enforces
      ! this contract before a run starts; the guard above is a defensive
      ! backstop for callers that bypass preflight. Reports via
      ! module_ctrl_error_state so SuPy can surface a clean exception --
      ! never WRITE(*,...) + STOP, which kills the embedding Python process.
      lai_id_next = lai_obs
      valid = .TRUE.

   END SUBROUTINE observed_lai


   SUBROUTINE get_reset_days(lat, summer_reset_day, winter_reset_day)

      ! Fixed-DOY GDD/SDD reset days for the site's hemisphere: DOY 15 and DOY 196
      ! in both ordinary and leap years (mid-January and mid-July in the northern
      ! hemisphere, swapped in the southern hemisphere). Equator (lat == 0) uses
      ! the northern-hemisphere dates.

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(IN) :: lat
      INTEGER, INTENT(OUT) :: summer_reset_day
      INTEGER, INTENT(OUT) :: winter_reset_day

      IF (lat >= 0.0D0) THEN
         summer_reset_day = 196
         winter_reset_day = 15
      ELSE
         summer_reset_day = 15
         winter_reset_day = 196
      END IF

   END SUBROUTINE get_reset_days


   SUBROUTINE update_growth_state( &
      id, lat, summer_reset_day, winter_reset_day, & ! input
      growth_state, senescence_state) ! output

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: id
      INTEGER, INTENT(IN) :: summer_reset_day
      INTEGER, INTENT(IN) :: winter_reset_day
      REAL(KIND(1D0)), INTENT(IN) :: lat

      INTEGER, INTENT(OUT) :: growth_state
      INTEGER, INTENT(OUT) :: senescence_state

      ! Northern and southern hemispheres use complementary interval tests
      ! because senescence wraps across the year boundary in NH while growth
      ! wraps across the year boundary in SH.
      IF (lat >= 0.0D0) THEN
         IF ((id >= summer_reset_day) .OR. (id < winter_reset_day)) THEN
            senescence_state = 1
            growth_state = 0
         ELSE
            growth_state = 1
            senescence_state = 0
         END IF
      ELSE
         IF ((id >= summer_reset_day) .AND. (id < winter_reset_day)) THEN
            senescence_state = 1
            growth_state = 0
         ELSE
            growth_state = 1
            senescence_state = 0
         END IF
      END IF

   END SUBROUTINE update_growth_state


   SUBROUTINE calc_degree_days( &
      growth_state, senescence_state, &
      tmin_prev, tmax_prev, base_t_gdd, base_t_sdd, &
      gdd_id_prev, sdd_id_prev, &
      delta_gdd, delta_sdd, &
      gdd_id, sdd_id)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: growth_state
      INTEGER, INTENT(IN) :: senescence_state

      REAL(KIND(1D0)), INTENT(IN) :: tmin_prev
      REAL(KIND(1D0)), INTENT(IN) :: tmax_prev
      REAL(KIND(1D0)), INTENT(IN) :: base_t_gdd
      REAL(KIND(1D0)), INTENT(IN) :: base_t_sdd
      REAL(KIND(1D0)), INTENT(IN) :: gdd_id_prev
      REAL(KIND(1D0)), INTENT(IN) :: sdd_id_prev

      REAL(KIND(1D0)), INTENT(OUT) :: delta_gdd
      REAL(KIND(1D0)), INTENT(OUT) :: delta_sdd

      REAL(KIND(1D0)), INTENT(OUT) :: gdd_id
      REAL(KIND(1D0)), INTENT(OUT) :: sdd_id

      ! Calculate daily GDD increment during the growth season
      IF (growth_state == 0) THEN
         delta_gdd = 0.0D0
      ELSE
         delta_gdd = calc_delta_degree_days( &
            tmin_prev, tmax_prev, base_t_gdd)
      END IF

      ! Calculate daily SDD increment during the senescence season
      IF (senescence_state == 0) THEN
         delta_sdd = 0.0D0
      ELSE
         delta_sdd = calc_delta_degree_days( &
            tmin_prev, tmax_prev, base_t_sdd)
      END IF

      ! delta_sdd cannot be positive; delta_gdd cannot be negative
      IF (delta_sdd > 0.0D0) delta_sdd = 0.0D0
      IF (delta_gdd < 0.0D0) delta_gdd = 0.0D0

      ! Update cumulative growing and senescence degree days
      gdd_id = gdd_id_prev + delta_gdd
      sdd_id = sdd_id_prev + delta_sdd

   END SUBROUTINE calc_degree_days


   FUNCTION calc_delta_degree_days(tmin_prev, tmax_prev, base_t) RESULT(delta_dd)

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(IN) :: tmin_prev
      REAL(KIND(1D0)), INTENT(IN) :: tmax_prev
      REAL(KIND(1D0)), INTENT(IN) :: base_t

      REAL(KIND(1D0)) :: delta_dd

      delta_dd = (tmin_prev + tmax_prev) / 2.0D0 - base_t

   END FUNCTION calc_delta_degree_days


   SUBROUTINE calculate_lai( &
      growth_state, senescence_state, &
      delta_gdd, delta_sdd, gdd_id, sdd_id, &
      gdd_base, sdd_base, lai_power, &
      lai_id_prev, lai_max, lai_min, lai_id_next)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: growth_state
      INTEGER, INTENT(IN) :: senescence_state

      REAL(KIND(1D0)), INTENT(IN) :: delta_gdd
      REAL(KIND(1D0)), INTENT(IN) :: delta_sdd
      REAL(KIND(1D0)), INTENT(IN) :: gdd_id
      REAL(KIND(1D0)), INTENT(IN) :: sdd_id
      REAL(KIND(1D0)), INTENT(IN) :: gdd_base
      REAL(KIND(1D0)), INTENT(IN) :: sdd_base

      REAL(KIND(1D0)), DIMENSION(4), INTENT(IN) :: lai_power

      REAL(KIND(1D0)), INTENT(IN) :: lai_id_prev
      REAL(KIND(1D0)), INTENT(IN) :: lai_max
      REAL(KIND(1D0)), INTENT(IN) :: lai_min

      REAL(KIND(1D0)), INTENT(OUT) :: lai_id_next

      IF (.NOT. ieee_is_finite(lai_id_prev) .OR. lai_id_prev < 0.0D0) THEN
         lai_id_next = -999.0D0
         CALL set_supy_error( &
            117, &
            'calculate_lai: lai_id_prev must be finite and non-negative')
         RETURN
      END IF

      IF (.NOT. ieee_is_finite(lai_min) .OR. .NOT. ieee_is_finite(lai_max)) THEN
         lai_id_next = -999.0D0
         CALL set_supy_error( &
            117, &
            'calculate_lai: lai_min and lai_max must be finite')
         RETURN
      END IF

      IF (.NOT. ieee_is_finite(delta_gdd) .OR. .NOT. ieee_is_finite(delta_sdd) .OR. &
          .NOT. ieee_is_finite(gdd_id) .OR. .NOT. ieee_is_finite(sdd_id) .OR. &
          .NOT. ieee_is_finite(gdd_base) .OR. .NOT. ieee_is_finite(sdd_base)) THEN
         lai_id_next = -999.0D0
         CALL set_supy_error( &
            117, &
            'calculate_lai: degree-day state, threshold, or increment is non-finite')
         RETURN
      END IF

      ! Fixed-zero absent-canopy envelope (explicitly permitted for sfr == 0):
      ! canopy is pinned at zero regardless of lai_power exponents.
      IF (lai_min == 0.0D0 .AND. lai_max == 0.0D0 .AND. lai_id_prev == 0.0D0) THEN
         lai_id_next = 0.0D0
         RETURN
      END IF

      ! If in growing half of year, GDD has reached gdd_base, and today has positive GDD
      IF ((gdd_id >= gdd_base) .AND. (growth_state == 1) .AND. (delta_gdd > 0.0D0)) THEN
         IF (lai_id_prev <= 0.0D0 .AND. lai_power(1) < 0.0D0) THEN
            lai_id_next = -999.0D0
            CALL set_supy_error( &
               117, &
               'calculate_lai: lai_id_prev <= 0 with negative growth exponent lai_power(1) is singular; use positive lai_id/lai_min')
            RETURN
         END IF
         lai_id_next = calculate_gdd( &
            lai_id_prev=lai_id_prev, &
            lai_power=lai_power, &
            gdd=delta_gdd &
         )

      ! If in senescing half of year, SDD has reached sdd_base, and today has negative SDD
      ELSE IF ((sdd_id <= sdd_base) .AND. (senescence_state == 1) .AND. (delta_sdd < 0.0D0)) THEN
         IF (lai_id_prev <= 0.0D0 .AND. lai_power(3) < 0.0D0) THEN
            lai_id_next = -999.0D0
            CALL set_supy_error( &
               117, &
               'calculate_lai: lai_id_prev <= 0 with negative senescence exponent lai_power(3) is singular; use positive lai_id/lai_min')
            RETURN
         END IF
         lai_id_next = calculate_sdd_type0( &
            lai_id_prev=lai_id_prev, &
            lai_power=lai_power, &
            sdd=delta_sdd &
         )

      ELSE
         ! Otherwise GDD and SDD do not change LAI
         lai_id_next = lai_id_prev
      END IF

      IF (.NOT. ieee_is_finite(lai_id_next)) THEN
         lai_id_next = -999.0D0
         CALL set_supy_error( &
            117, &
            'calculate_lai: computed LAI is non-finite (NaN or Inf)')
         RETURN
      END IF

      ! Keep internally computed phenology within the configured canopy envelope
      CALL limit_lai( &
         lai_id_next=lai_id_next, &
         lai_max=lai_max, &
         lai_min=lai_min &
      )

   END SUBROUTINE calculate_lai


   SUBROUTINE reset_degree_days( &
      id, &
      summer_reset_day, winter_reset_day, &
      sdd, gdd)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: id
      INTEGER, INTENT(IN) :: summer_reset_day
      INTEGER, INTENT(IN) :: winter_reset_day

      REAL(KIND(1D0)), INTENT(INOUT) :: gdd
      REAL(KIND(1D0)), INTENT(INOUT) :: sdd

      IF (id == summer_reset_day .OR. id == winter_reset_day) THEN
         sdd = 0.0D0
         gdd = 0.0D0
      END IF

   END SUBROUTINE reset_degree_days


   FUNCTION calculate_gdd(lai_id_prev, lai_power, gdd) RESULT(lai_id_next)

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(IN) :: lai_id_prev
      REAL(KIND(1D0)), DIMENSION(4), INTENT(IN) :: lai_power
      REAL(KIND(1D0)), INTENT(IN) :: gdd
      REAL(KIND(1D0)) :: lai_id_next

      ! Short-circuit non-positive daily GDD increments to avoid evaluating 0**p * 0
      IF (gdd <= 0.0D0) THEN
         lai_id_next = lai_id_prev
         RETURN
      END IF

      IF (lai_id_prev == 0.0D0) THEN
         IF (lai_power(1) > 0.0D0) THEN
            ! Absorbing zero state: 0**p == 0 for p > 0
            lai_id_next = 0.0D0
         ELSE IF (lai_power(1) == 0.0D0) THEN
            ! Linear growth limit: lai**0 == 1
            lai_id_next = gdd * lai_power(2)
         ELSE
            ! Guarded in calculate_lai; keep expression well-defined if called directly
            lai_id_next = lai_id_prev
         END IF
         RETURN
      END IF

      lai_id_next = lai_id_prev + (lai_id_prev**lai_power(1) * gdd * lai_power(2))

   END FUNCTION calculate_gdd


   FUNCTION calculate_sdd_type0(lai_id_prev, lai_power, sdd) RESULT(lai_id_next)

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(IN) :: lai_id_prev
      REAL(KIND(1D0)), DIMENSION(4), INTENT(IN) :: lai_power
      REAL(KIND(1D0)), INTENT(IN) :: sdd
      REAL(KIND(1D0)) :: lai_id_next

      ! Short-circuit non-negative daily SDD increments to avoid evaluating 0**p * 0
      IF (sdd >= 0.0D0) THEN
         lai_id_next = lai_id_prev
         RETURN
      END IF

      IF (lai_id_prev == 0.0D0) THEN
         IF (lai_power(3) > 0.0D0) THEN
            ! Absorbing zero state: 0**p == 0 for p > 0
            lai_id_next = 0.0D0
         ELSE IF (lai_power(3) == 0.0D0) THEN
            ! Linear senescence limit: lai**0 == 1
            lai_id_next = sdd * lai_power(4)
         ELSE
            ! Guarded in calculate_lai; keep expression well-defined if called directly
            lai_id_next = lai_id_prev
         END IF
         RETURN
      END IF

      lai_id_next = lai_id_prev + (lai_id_prev**lai_power(3) * sdd * lai_power(4))

   END FUNCTION calculate_sdd_type0


   SUBROUTINE limit_lai(lai_id_next, lai_max, lai_min)

      ! Keep internally computed phenology within the configured canopy envelope,
      ! rejecting non-finite values explicitly rather than letting NaN pass
      ! through or silently clamping +/-Inf.

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(INOUT) :: lai_id_next
      REAL(KIND(1D0)), INTENT(IN) :: lai_max
      REAL(KIND(1D0)), INTENT(IN) :: lai_min

      IF (.NOT. ieee_is_finite(lai_id_next) .OR. &
          .NOT. ieee_is_finite(lai_min) .OR. &
          .NOT. ieee_is_finite(lai_max)) THEN
         lai_id_next = -999.0D0
         CALL set_supy_error( &
            117, &
            'limit_lai: non-finite LAI or LAI bounds encountered')
         RETURN
      END IF

      IF (lai_id_next > lai_max) THEN
         lai_id_next = lai_max
      ELSE IF (lai_id_next < lai_min) THEN
         lai_id_next = lai_min
      END IF

   END SUBROUTINE limit_lai

END MODULE module_phys_lai
