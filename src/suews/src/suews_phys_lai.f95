MODULE module_phys_lai
   USE, INTRINSIC :: ieee_arithmetic, ONLY: ieee_is_nan
   USE module_ctrl_const_allocate, ONLY: nvegsurf
   USE module_ctrl_error_state, ONLY: set_supy_error, supy_error_flag

   IMPLICIT NONE

   ! Enumeration parameters for the LAI type
   INTEGER, PARAMETER :: LAI_ORIGINAL = 0
   INTEGER, PARAMETER :: LAI_NEW = 1
   !FA INTEGER, PARAMETER :: LAI_INVERTED = 2

   ! Enumeration parameters for senescence conditions
   INTEGER, PARAMETER :: SEN_DAYLENGTH = 1
   INTEGER, PARAMETER :: SEN_SDD = 2

CONTAINS

!FA changing from full to base in here
   SUBROUTINE update_gddlai( &
      id, lai_calc_yes, & !input
      lat, lai_obs, &
      t_min_id_prev, t_max_id_prev, len_day_id_prev, &
      theta_r, smd, & ! 28/07/26 MP: added SOIL moisture limits to LAI growth
      base_t_gdd, base_t_sdd, &
      gdd_base, sdd_base, &
      lai_min, lai_max, lai_power, lai_type, &
      lai_id_prev, &
      gdd_id, sdd_id, & !input
      stress_state, stress_days, &
      lai_id_next) !output

      IMPLICIT NONE

      !------------------------------------------------------------------------------
      ! Main subroutine for calculation of LAI, using growing degree days, senescence degree days,
      ! and stress induced by soil moisture deficit.
      ! This was revised and checked on 16 Feb 2014 by LJ
      !------------------------------------------------------------------------------

      INTEGER, INTENT(IN) :: id
      INTEGER, INTENT(IN) :: lai_calc_yes

      REAL(KIND(1D0)), INTENT(IN) :: lat
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: lai_obs
      REAL(KIND(1D0)), INTENT(IN) :: t_min_id_prev
      REAL(KIND(1D0)), INTENT(IN) :: t_max_id_prev
      REAL(KIND(1D0)), INTENT(IN) :: len_day_id_prev

      REAL(KIND(1D0)), INTENT(IN) :: theta_r ! Residual volumetric soil moisture (m3 / m3)
      REAL(KIND(1D0)), INTENT(IN) :: smd ! soil moisture deficit (mm)

      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: stress_state
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: stress_days ! Number of consecutive days vegetation in a stressed state

      ! --- Vegetation phenology ---------------------------------------------------------------------
      ! Parameters provided in input information for each vegetation surface (SUEWS_Veg.txt)
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: base_t_gdd !Base temperature for growing degree days [degC]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: base_t_sdd !Base temperature for senescence degree days [degC]
      !FA update to using new base parameters rather than full for gdd and sdd
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: gdd_base !Growing degree days needed to initiate leaf growth [degC]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: sdd_base !Senescence degree days needed to initiate leaf off [degC]
      !FA
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: lai_min !Min LAI [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: lai_max !Max LAI [m2 m-2]
      REAL(KIND(1D0)), DIMENSION(4, nvegsurf), INTENT(IN) :: lai_power !Coeffs for LAI equation: 1,2 - leaf growth; 3,4 - leaf off

      INTEGER, DIMENSION(nvegsurf), INTENT(IN) :: lai_type !LAI equation to use: original (0) or new (1)

      REAL(KIND(1D0)), DIMENSION(3), INTENT(INOUT) :: gdd_id !Growing Degree Days (see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(3), INTENT(INOUT) :: sdd_id !Senescence Degree Days (see SUEWS_DailyState.f95)
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(INOUT) :: lai_id_prev ! LAI of previous day
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(OUT) :: lai_id_next !LAI for each veg surface [m2 m-2]

      REAL(KIND(1D0)) :: delta_gdd !Switches and checks for GDD
      REAL(KIND(1D0)) :: delta_sdd !Switches and checks for GDD

      REAL(KIND(1D0)), DIMENSION(3) :: gdd_id_prev ! GDD of previous day
      REAL(KIND(1D0)), DIMENSION(3) :: sdd_id_prev ! SDD of previous day

      INTEGER, DIMENSION(nvegsurf) :: growth_state = 0 !State of growth (0: dormant, 1: growing, 2: senescing)
      INTEGER, DIMENSION(nvegsurf) :: senescence_state = 0 !State of senescence (0: not senescing, 1: senescing)

      INTEGER :: iv

      ! Hemisphere specific parameters --------------------------------------------
      ! INTEGER :: sdd_reset_day
      INTEGER :: summer_day ! day of year on which gdd reset to zero
      INTEGER :: winter_day ! day of year on which sdd reset to zero 
      INTEGER :: senescence_mode ! flag deciding whether day length should be used to initate senescence portion of year 
      LOGICAL :: southern_hemisphere

      REAL(KIND(1D0)) :: soilmoisture_stress_limit = 0.05
      REAL(KIND(1D0)) :: LAI_loss_rate_perc = 0.005D0   ! percentage per day⁻¹
      REAL(KIND(1D0)) :: wilting_point

      LOGICAL :: stressed
      LOGICAL :: valid_observed_lai

      !FA TODO: formula needs looking at (shodl wilting point be larger than theta_r?)
      wilting_point = theta_r - (theta_r*soilmoisture_stress_limit)

      ! translate values of previous day to local variables
      GDD_id_prev = GDD_id
      SDD_id_prev = SDD_id

      IF (lai_calc_yes == 0) THEN
         CALL observed_lai( &
            lai_obs=lai_obs, &
            lai_id_next=lai_id_next, &
            valid=valid_observed_lai &
            )
         IF (.NOT. valid_observed_lai) RETURN
      END IF

      !FA TODO: formula needs looking at - not comparing like-for-like units here?
      stressed = (smd > wilting_point)

      ! Determine N/S hemisphere parameters
      ! TODO: Move outside timestep loop as timestep independent
      IF (lat >= 0) THEN
         ! sdd_reset_day = 140
         ! summer_day = 170
         ! winter_day = 170
         summer_day = 182
         winter_day = 1
         southern_hemisphere = .FALSE.
         senescence_mode = SEN_DAYLENGTH
      ELSE !! N.B. not identical to N hemisphere - return to later
         ! sdd_reset_day = 300
         summer_day = 1
         winter_day = 182
         southern_hemisphere = .TRUE.
         senescence_mode = SEN_SDD
      END IF

      ! Loop through vegetation types (iv)
      DO iv = 1, NVegSurf

         stress_state(iv) = determine_stress_state_surf( &
                            stressed=stressed, &
                            stress_days=stress_days(iv) &
                            )

         CALL lai_stress_response_surf( &
            lai_id_prev=lai_id_prev(iv), &
            laimin=lai_min(iv), &
            laimax=lai_max(iv), &
            LAI_loss_rate_perc=LAI_loss_rate_perc, &
            stress_state=stress_state(iv), &
            stress_days=stress_days(iv) &
            )
         
         CALL calc_degree_days( &
            growth_state=growth_state(iv), &
            senescence_state=senescence_state(iv), &
            tmin_prev=t_min_id_prev, &
            tmax_prev=t_max_id_prev, &
            base_t_gdd=base_t_gdd(iv), &
            base_t_sdd=base_t_sdd(iv), &
            gdd_id_prev=gdd_id_prev(iv), &
            sdd_id_prev=sdd_id_prev(iv), &
            lai_id_prev=lai_id_prev(iv), &
            lai_max=lai_max(iv), &
            lai_min=lai_min(iv), &
            delta_gdd=delta_gdd, &
            delta_sdd=delta_sdd, &
            gdd_id=gdd_id(iv), &
            sdd_id=sdd_id(iv) &
            )

         ! Now calculate LAI itself
         CALL reset_degree_days( &
            id=id, &
            summer_reset_day=summer_day, &
            winter_reset_day=winter_day, &
            gdd_base=gdd_base(iv), &
            sdd_base=sdd_base(iv), &
            growth_state=growth_state(iv), &
            senescence_state=senescence_state(iv), &
            sdd=sdd_id(iv), &
            gdd=gdd_id(iv) &
            )

         IF (lai_calc_yes /= 0) THEN
            CALL calculate_lai( &
               senescence_mode=senescence_mode, &
               len_day_id_prev=len_day_id_prev, &
               delta_gdd=delta_gdd, &
               delta_sdd=delta_sdd, &
               gdd_id=gdd_id(iv), &
               sdd_id=sdd_id(iv), &
               gdd_base=gdd_base(iv), &
               sdd_base=sdd_base(iv), &
               lai_type=lai_type(iv), &
               lai_power=lai_power(:, iv), &
               lai_max=lai_max(iv), &
               lai_min=lai_min(iv), &
               lai_id_prev=lai_id_prev(iv), &
               lai_id_next=lai_id_next(iv), &
               stress_days=stress_days(iv) &
               )
         END IF

      END DO !End of loop over veg surfaces

   END SUBROUTINE update_gddlai

!------------------------------------------------------------------------------


   SUBROUTINE observed_lai( &
      lai_obs, lai_id_next, valid)

      IMPLICIT NONE

      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(IN) :: lai_obs
      REAL(KIND(1D0)), DIMENSION(nvegsurf), INTENT(OUT) :: lai_id_next
      logical, INTENT(OUT) :: valid

      valid = .FALSE.

      IF (ANY(ieee_is_nan(lai_obs)) .or. ANY(lai_obs < 0.0D0)) THEN
         ! Invalid lai_obs slipped past pre-flight; raise an error before
         ! mutating phenology state and assign a safe sentinel to the output.
         lai_id_next = -999.0D0
         CALL set_supy_error( &
            105, &
            'update_GDDLAI: laimethod=0 requires non-missing lai_* or lai >= 0 at every timestep')
         RETURN
      END IF

      ! Observed-LAI override: when lai_calc_yes == 0, every timestep's forcing
      ! value must be a non-missing, non-negative observation (lai_obs >= 0).

      ! A genuine zero observation (e.g. complete winter dieback) is valid and
      ! passes through unchanged. Missing/NaN values and strictly negative
      ! values - including the -999 missing sentinel - are rejected; choosing
      ! this path commits the user to providing an observation for every
      ! timestep. Observed LAI is intentionally not clipped to lai_min/lai_max,
      ! because those bounds describe the internal GDD/SDD phenology path.

      ! The Python pre-flight validator (supy._check.check_forcing) enforces
      ! this contract before a run starts; the guard below is a defensive
      ! backstop for callers that bypass preflight. Reports via
      ! module_ctrl_error_state so SuPy can surface a clean exception —
      ! never WRITE(*,...) + STOP, which kills the embedding Python process.

      ! Copy the effective observed LAI for EveTr, DecTr and Grass into the
      ! daily state without applying the GDD/SDD envelope.
      lai_id_next = lai_obs
      valid = .TRUE.

   END SUBROUTINE observed_lai

!------------------------------------------------------------------------------

   SUBROUTINE calc_degree_days( &
      growth_state, senescence_state, &
      tmin_prev, tmax_prev, base_t_gdd, base_t_sdd, &
      gdd_id_prev, sdd_id_prev, &
      lai_id_prev, lai_max, lai_min, &
      delta_gdd, delta_sdd, &
      gdd_id, sdd_id)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: growth_state
      INTEGER, INTENT(IN) :: senescence_state

      REAL(KIND(1D0)), INTENT(IN)  :: tmin_prev
      REAL(KIND(1D0)), INTENT(IN)  :: tmax_prev
      REAL(KIND(1D0)), INTENT(IN)  :: base_t_gdd
      REAL(KIND(1D0)), INTENT(IN)  :: base_t_sdd
      REAL(KIND(1D0)), INTENT(IN)  :: gdd_id_prev
      REAL(KIND(1D0)), INTENT(IN)  :: sdd_id_prev

      REAL(KIND(1D0)), INTENT(IN)  :: lai_id_prev
      REAL(KIND(1D0)), INTENT(IN)  :: lai_max
      REAL(KIND(1D0)), INTENT(IN)  :: lai_min

      REAL(KIND(1D0)), INTENT(OUT) :: delta_gdd
      REAL(KIND(1D0)), INTENT(OUT) :: delta_sdd

      REAL(KIND(1D0)), INTENT(OUT) :: gdd_id
      REAL(KIND(1D0)), INTENT(OUT) :: sdd_id

      ! Calculate GDD and SDD
      IF ((lai_id_prev >= lai_max) .or. (growth_state == 0)) THEN
         delta_gdd = 0.0D0
      ELSE
         delta_gdd = calc_delta_degree_days( &
                     tmin_prev, tmax_prev, base_t_gdd)
      END IF

      IF ((lai_id_prev <= lai_min) .or. (senescence_state == 0)) THEN
         delta_sdd = 0.0D0
      ELSE
         delta_sdd = calc_delta_degree_days( &
                     tmin_prev, tmax_prev, base_t_sdd)
      END IF

      ! SDD cannot be positive
      IF (delta_sdd > 0) delta_sdd = 0

      ! GDD cannot be negative
      IF (delta_gdd < 0) THEN
         delta_gdd = 0
      END IF

      !FA if LAI type is inverted, flip the response of LAI to SDD/GDD
      ! comment out for now as need to decide whether to adjust summer/winter resetting
      ! IF (lai_type(iv) == LAI_INVERTED) then
      !    delta_gdd = -delta_gdd
      !    delta_sdd = -delta_sdd
      ! end IF

      ! Calculate cumulative growing and senescence degree days
      gdd_id = gdd_id_prev + delta_gdd
      sdd_id = sdd_id_prev + delta_sdd

   END SUBROUTINE calc_degree_days

!------------------------------------------------------------------------------

   FUNCTION calc_delta_degree_days( &
      Tmin_prev, Tmax_prev, base_t) RESULT(delta_dd)

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(IN) :: Tmin_prev
      REAL(KIND(1D0)), INTENT(IN) :: Tmax_prev
      REAL(KIND(1D0)), INTENT(IN) :: base_t

      REAL(KIND(1D0)) :: delta_dd

      delta_dd = (Tmin_prev + Tmax_prev)/2 - base_t

   END FUNCTION calc_delta_degree_days

!------------------------------------------------------------------------------

   SUBROUTINE calculate_lai( &
      senescence_mode, &
      delta_gdd, delta_sdd, gdd_id, sdd_id, &
      gdd_base, sdd_base, lai_power, lai_type, &
      len_day_id_prev, lai_id_prev, lai_max, lai_min, lai_id_next, &
      stress_days)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: senescence_mode

      REAL(KIND(1D0)), INTENT(IN) :: delta_gdd
      REAL(KIND(1D0)), INTENT(IN) :: delta_sdd
      REAL(KIND(1D0)), INTENT(IN) :: gdd_id
      REAL(KIND(1D0)), INTENT(IN) :: sdd_id
      REAL(KIND(1D0)), INTENT(IN) :: gdd_base
      REAL(KIND(1D0)), INTENT(IN) :: sdd_base

      REAL(KIND(1D0)), DIMENSION(4), INTENT(IN) :: lai_power

      INTEGER, INTENT(IN) :: lai_type

      REAL(KIND(1D0)), INTENT(IN) :: len_day_id_prev
      REAL(KIND(1D0)), INTENT(IN) :: lai_id_prev
      REAL(KIND(1D0)), INTENT(IN) :: lai_max
      REAL(KIND(1D0)), INTENT(IN) :: lai_min

      REAL(KIND(1D0)), INTENT(OUT) :: lai_id_next

      REAL(KIND(1D0)), INTENT(OUT) :: stress_days

      LOGICAL :: start_senescence

      lai_id_next = lai_id_prev

      IF (gdd_id >= gdd_base) THEN !Leaves can still grow
         lai_id_next = calculate_gdd( &
                        lai_id_prev=lai_id_prev, &
                        lai_power=lai_power, &
                        gdd=delta_gdd &
                        )

      ELSE IF (lai_type <= LAI_ORIGINAL) THEN !Original LAI type
         start_senescence = check_start_senescence( &
                              senescence_mode=senescence_mode, &
                              len_day_id_prev=len_day_id_prev, &
                              sdd_id=sdd_id, &
                              sdd_base=sdd_base &
                              )
         IF (start_senescence) THEN !Start senescence
            lai_id_next = calculate_sdd_type0( &
                           lai_id_prev=lai_id_prev, &
                           lai_power=lai_power, &
                           sdd=delta_sdd &
                           )
         END IF

      ELSE
         !! Use day length to start senescence at high latitudes (controlled in senescence_mode)
         start_senescence = check_start_senescence( &
                              senescence_mode=senescence_mode, &
                              len_day_id_prev=len_day_id_prev, &
                              sdd_id=sdd_id, &
                              sdd_base=sdd_base &
                              )

         IF (start_senescence) THEN !Start senescence
            lai_id_next = calculate_sdd_type1( &
                           lai_id_prev=lai_id_prev, &
                           lai_power=lai_power, &
                           sdd=delta_sdd &
                           )
         END IF

      END IF

      ! Keep internally computed phenology within the configured canopy envelope.
      CALL limit_lai( &
         lai_id_next=lai_id_next, &
         stress_days=stress_days, &
         lai_max=lai_max, &
         lai_min=lai_min &
         )

   END SUBROUTINE calculate_lai

!------------------------------------------------------------------------------

   SUBROUTINE reset_degree_days( &
      id, &
      sdd_base, gdd_base, &
      summer_reset_day, winter_reset_day, &
      growth_state, senescence_state, &
      sdd, gdd)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: id
      INTEGER, INTENT(IN) :: summer_reset_day
      INTEGER, INTENT(IN) :: winter_reset_day

      REAL(KIND(1D0)), INTENT(IN) :: gdd_base
      REAL(KIND(1D0)), INTENT(IN) :: sdd_base

      INTEGER, INTENT(INOUT) :: growth_state
      INTEGER, INTENT(INOUT) :: senescence_state

      REAL(KIND(1D0)), INTENT(INOUT) :: gdd
      REAL(KIND(1D0)), INTENT(INOUT) :: sdd

      IF (gdd >= gdd_base) THEN
         growth_state = 2
      END IF
      IF (sdd <= sdd_base) THEN
         senescence_state = 2
      END IF

      IF (id == summer_reset_day) THEN
         sdd = 0.0D0
         gdd = 0.0D0
         senescence_state = 1
         growth_state = 0
      ELSE IF (id == winter_reset_day) THEN
         gdd = 0.0D0
         sdd = 0.0D0
         growth_state = 1
         senescence_state = 0
      END IF

   END SUBROUTINE reset_degree_days

!------------------------------------------------------------------------------

   FUNCTION check_start_senescence( &
      senescence_mode, len_day_id_prev, sdd_id, sdd_base) RESULT(start_senescence)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: senescence_mode

      REAL(KIND(1D0)), INTENT(IN) :: len_day_id_prev
      REAL(KIND(1D0)), INTENT(IN) :: sdd_id
      REAL(KIND(1D0)), INTENT(IN) :: sdd_base

      LOGICAL :: start_senescence

      SELECT case (senescence_mode)

      CASE (SEN_DAYLENGTH)
         start_senescence = ((len_day_id_prev <= 12) .and. (sdd_id <= sdd_base))

      CASE (SEN_SDD)
         start_senescence = (sdd_id <= sdd_base)

      CASE DEFAULT
         ! Invalid option falls back to SEN_SDD. No error yet registered.
         ! default currently not possible as function calls hard-coded
         start_senescence = (sdd_id <= sdd_base)

      END SELECT

   END FUNCTION check_start_senescence

!------------------------------------------------------------------------------

   FUNCTION calculate_gdd( &
      lai_id_prev, lai_power, gdd) RESULT(lai_id_next)

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(IN) :: lai_id_prev
      REAL(KIND(1D0)), DIMENSION(4), INTENT(IN) :: lai_power
      REAL(KIND(1D0)), INTENT(IN) :: gdd
      REAL(KIND(1D0)) :: lai_id_next

      lai_id_next = lai_id_prev + (lai_id_prev**lai_power(1)*gdd*lai_power(2))

   END FUNCTION calculate_gdd

!------------------------------------------------------------------------------

   FUNCTION calculate_sdd_type0( &
      lai_id_prev, lai_power, sdd) RESULT(lai_id_next)

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(IN) :: lai_id_prev
      REAL(KIND(1D0)), DIMENSION(4), INTENT(IN) :: lai_power
      REAL(KIND(1D0)), INTENT(IN) :: sdd
      REAL(KIND(1D0)) :: lai_id_next

      lai_id_next = lai_id_prev + (lai_id_prev**lai_power(3)*sdd*lai_power(4))

   END FUNCTION calculate_sdd_type0

!------------------------------------------------------------------------------

   FUNCTION calculate_sdd_type1( &
      lai_id_prev, lai_power, sdd) RESULT(lai_id_next)

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(IN) :: lai_id_prev
      REAL(KIND(1D0)), DIMENSION(4), INTENT(IN) :: lai_power
      REAL(KIND(1D0)), INTENT(IN) :: sdd
      REAL(KIND(1D0)) :: lai_id_next

      lai_id_next = lai_id_prev + (lai_id_prev*lai_power(3)*(1 - sdd)*lai_power(4))

   END FUNCTION calculate_sdd_type1

!------------------------------------------------------------------------------

   FUNCTION determine_stress_state_surf( &
      stressed, stress_days &
      ) result(stress_state)

      IMPLICIT NONE

      logical, INTENT(IN) :: stressed ! Flag indicating if soil under stress
      REAL(KIND(1D0)), INTENT(IN) :: stress_days
      REAL(KIND(1D0)) :: stress_state

      IF (stressed) THEN
         stress_state = 1 ! Stressed, losing LAI
      ELSE
         IF (stress_days > 0) THEN
            stress_state = 2 ! No longer stressed, recovering LAI
         ELSE
            stress_state = 0 ! No stress or required recovery
         END IF
      END IF

   END FUNCTION determine_stress_state_surf

!------------------------------------------------------------------------------

   SUBROUTINE lai_stress_response_surf( &
      laimin, laimax, lai_loss_rate_perc, &
      lai_id_prev, stress_state, stress_days &
      )

      IMPLICIT NONE

      INTEGER :: delta_stress_days

      REAL(KIND(1D0)), INTENT(IN) :: laimin
      REAL(KIND(1D0)), INTENT(IN) :: laimax
      REAL(KIND(1D0)), INTENT(IN) :: lai_loss_rate_perc

      REAL(KIND(1D0)), INTENT(INOUT) :: lai_id_prev
      REAL(KIND(1D0)), INTENT(INOUT) :: stress_state
      REAL(KIND(1D0)), INTENT(INOUT) :: stress_days

      IF (stress_state == 0) RETURN

      IF (stress_state == 1) THEN
         lai_id_prev = MAX(laimin, lai_id_prev*(1 - lai_loss_rate_perc))
         delta_stress_days = 1
      ELSE IF (stress_state == 2) THEN
         lai_id_prev = MIN(laimax, lai_id_prev*(1 + lai_loss_rate_perc))
         delta_stress_days = -1
      END IF

      stress_days = MAX(0.0, stress_days + delta_stress_days)

   END SUBROUTINE lai_stress_response_surf

!------------------------------------------------------------------------------

   SUBROUTINE limit_lai( &
      lai_id_next, stress_days, lai_max, lai_min)

      ! Keep internally computed phenology within the configured canopy envelope.

      IMPLICIT NONE

      REAL(KIND(1D0)), INTENT(INOUT) :: lai_id_next
      REAL(KIND(1D0)), INTENT(INOUT) :: stress_days
      REAL(KIND(1D0)), INTENT(IN) :: lai_max
      REAL(KIND(1D0)), INTENT(IN) :: lai_min

      IF (lai_id_next > lai_max) THEN
         lai_id_next = lai_max
         stress_days = 0
      ELSE IF (lai_id_next < lai_min) THEN
         lai_id_next = lai_min
      END IF

   END SUBROUTINE limit_lai


END MODULE module_phys_lai
