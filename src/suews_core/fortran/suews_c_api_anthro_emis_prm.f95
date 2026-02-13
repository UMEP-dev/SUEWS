! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for anthroEMIS_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_anthro_emis_prm
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_ANTHRO_EMIS_PRM_LEN = 228_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_ANTHRO_EMIS_PRM_SCHEMA_VERSION = 1_c_int

   TYPE :: anthro_heat_prm_shadow
      REAL(c_double) :: qf0_beu_working = 0.0_c_double
      REAL(c_double) :: qf0_beu_holiday = 0.0_c_double
      REAL(c_double) :: qf_a_working = 0.0_c_double
      REAL(c_double) :: qf_a_holiday = 0.0_c_double
      REAL(c_double) :: qf_b_working = 0.0_c_double
      REAL(c_double) :: qf_b_holiday = 0.0_c_double
      REAL(c_double) :: qf_c_working = 0.0_c_double
      REAL(c_double) :: qf_c_holiday = 0.0_c_double
      REAL(c_double) :: baset_cooling_working = 0.0_c_double
      REAL(c_double) :: baset_cooling_holiday = 0.0_c_double
      REAL(c_double) :: baset_heating_working = 0.0_c_double
      REAL(c_double) :: baset_heating_holiday = 0.0_c_double
      REAL(c_double) :: ah_min_working = 0.0_c_double
      REAL(c_double) :: ah_min_holiday = 0.0_c_double
      REAL(c_double) :: ah_slope_cooling_working = 0.0_c_double
      REAL(c_double) :: ah_slope_cooling_holiday = 0.0_c_double
      REAL(c_double) :: ah_slope_heating_working = 0.0_c_double
      REAL(c_double) :: ah_slope_heating_holiday = 0.0_c_double
      REAL(c_double), DIMENSION(24) :: ahprof_24hr_working = 0.0_c_double
      REAL(c_double), DIMENSION(24) :: ahprof_24hr_holiday = 0.0_c_double
      REAL(c_double) :: popdensdaytime_working = 0.0_c_double
      REAL(c_double) :: popdensdaytime_holiday = 0.0_c_double
      REAL(c_double) :: popdensnighttime = 0.0_c_double
      REAL(c_double), DIMENSION(24) :: popprof_24hr_working = 0.0_c_double
      REAL(c_double), DIMENSION(24) :: popprof_24hr_holiday = 0.0_c_double
   END TYPE anthro_heat_prm_shadow

   TYPE :: anthro_emis_prm_shadow
      INTEGER(c_int) :: startdls = 0_c_int
      INTEGER(c_int) :: enddls = 0_c_int
      TYPE(anthro_heat_prm_shadow) :: anthroheat
      REAL(c_double) :: ef_umolco2perj = 0.0_c_double
      REAL(c_double) :: enef_v_jkm = 0.0_c_double
      REAL(c_double) :: frfossilfuel_heat = 0.0_c_double
      REAL(c_double) :: frfossilfuel_nonheat = 0.0_c_double
      REAL(c_double), DIMENSION(2) :: fcef_v_kgkm = 0.0_c_double
      REAL(c_double), DIMENSION(24) :: humactivity_24hr_working = 0.0_c_double
      REAL(c_double), DIMENSION(24) :: humactivity_24hr_holiday = 0.0_c_double
      REAL(c_double) :: maxfcmetab = 0.0_c_double
      REAL(c_double) :: maxqfmetab = 0.0_c_double
      REAL(c_double) :: minfcmetab = 0.0_c_double
      REAL(c_double) :: minqfmetab = 0.0_c_double
      REAL(c_double) :: trafficrate_working = 0.0_c_double
      REAL(c_double) :: trafficrate_holiday = 0.0_c_double
      REAL(c_double) :: trafficunits = 0.0_c_double
      REAL(c_double), DIMENSION(24) :: traffprof_24hr_working = 0.0_c_double
      REAL(c_double), DIMENSION(24) :: traffprof_24hr_holiday = 0.0_c_double
   END TYPE anthro_emis_prm_shadow

   PUBLIC :: suews_anthro_emis_prm_len
   PUBLIC :: suews_anthro_emis_prm_schema_version
   PUBLIC :: suews_anthro_emis_prm_default
   PUBLIC :: suews_anthro_emis_prm_error_message

CONTAINS

   SUBROUTINE suews_anthro_emis_prm_len(n_flat, err) BIND(C, name='suews_anthro_emis_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_ANTHRO_EMIS_PRM_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_anthro_emis_prm_len


   SUBROUTINE suews_anthro_emis_prm_schema_version(schema_version, err) BIND(C, name='suews_anthro_emis_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_ANTHRO_EMIS_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_anthro_emis_prm_schema_version


   SUBROUTINE suews_anthro_emis_prm_default(flat, n_flat, err) BIND(C, name='suews_anthro_emis_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(anthro_emis_prm_shadow) :: state

      CALL anthro_emis_prm_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_anthro_emis_prm_default


   SUBROUTINE anthro_emis_prm_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(anthro_emis_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      INTEGER(c_int) :: idx
      INTEGER :: i

      IF (n_flat < SUEWS_CAPI_ANTHRO_EMIS_PRM_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1
      flat(idx) = REAL(state%startdls, c_double); idx = idx + 1
      flat(idx) = REAL(state%enddls, c_double); idx = idx + 1

      flat(idx) = state%anthroheat%qf0_beu_working; idx = idx + 1
      flat(idx) = state%anthroheat%qf0_beu_holiday; idx = idx + 1
      flat(idx) = state%anthroheat%qf_a_working; idx = idx + 1
      flat(idx) = state%anthroheat%qf_a_holiday; idx = idx + 1
      flat(idx) = state%anthroheat%qf_b_working; idx = idx + 1
      flat(idx) = state%anthroheat%qf_b_holiday; idx = idx + 1
      flat(idx) = state%anthroheat%qf_c_working; idx = idx + 1
      flat(idx) = state%anthroheat%qf_c_holiday; idx = idx + 1
      flat(idx) = state%anthroheat%baset_cooling_working; idx = idx + 1
      flat(idx) = state%anthroheat%baset_cooling_holiday; idx = idx + 1
      flat(idx) = state%anthroheat%baset_heating_working; idx = idx + 1
      flat(idx) = state%anthroheat%baset_heating_holiday; idx = idx + 1
      flat(idx) = state%anthroheat%ah_min_working; idx = idx + 1
      flat(idx) = state%anthroheat%ah_min_holiday; idx = idx + 1
      flat(idx) = state%anthroheat%ah_slope_cooling_working; idx = idx + 1
      flat(idx) = state%anthroheat%ah_slope_cooling_holiday; idx = idx + 1
      flat(idx) = state%anthroheat%ah_slope_heating_working; idx = idx + 1
      flat(idx) = state%anthroheat%ah_slope_heating_holiday; idx = idx + 1

      DO i = 1, 24
         flat(idx) = state%anthroheat%ahprof_24hr_working(i)
         idx = idx + 1
      END DO

      DO i = 1, 24
         flat(idx) = state%anthroheat%ahprof_24hr_holiday(i)
         idx = idx + 1
      END DO

      flat(idx) = state%anthroheat%popdensdaytime_working; idx = idx + 1
      flat(idx) = state%anthroheat%popdensdaytime_holiday; idx = idx + 1
      flat(idx) = state%anthroheat%popdensnighttime; idx = idx + 1

      DO i = 1, 24
         flat(idx) = state%anthroheat%popprof_24hr_working(i)
         idx = idx + 1
      END DO

      DO i = 1, 24
         flat(idx) = state%anthroheat%popprof_24hr_holiday(i)
         idx = idx + 1
      END DO

      flat(idx) = state%ef_umolco2perj; idx = idx + 1
      flat(idx) = state%enef_v_jkm; idx = idx + 1
      flat(idx) = state%frfossilfuel_heat; idx = idx + 1
      flat(idx) = state%frfossilfuel_nonheat; idx = idx + 1

      DO i = 1, 2
         flat(idx) = state%fcef_v_kgkm(i)
         idx = idx + 1
      END DO

      DO i = 1, 24
         flat(idx) = state%humactivity_24hr_working(i)
         idx = idx + 1
      END DO

      DO i = 1, 24
         flat(idx) = state%humactivity_24hr_holiday(i)
         idx = idx + 1
      END DO

      flat(idx) = state%maxfcmetab; idx = idx + 1
      flat(idx) = state%maxqfmetab; idx = idx + 1
      flat(idx) = state%minfcmetab; idx = idx + 1
      flat(idx) = state%minqfmetab; idx = idx + 1
      flat(idx) = state%trafficrate_working; idx = idx + 1
      flat(idx) = state%trafficrate_holiday; idx = idx + 1
      flat(idx) = state%trafficunits; idx = idx + 1

      DO i = 1, 24
         flat(idx) = state%traffprof_24hr_working(i)
         idx = idx + 1
      END DO

      DO i = 1, 24
         flat(idx) = state%traffprof_24hr_holiday(i)
         idx = idx + 1
      END DO

      err = SUEWS_CAPI_OK

   END SUBROUTINE anthro_emis_prm_pack


   SUBROUTINE suews_anthro_emis_prm_error_message(code, buffer, buffer_len) BIND(C, name='suews_anthro_emis_prm_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_anthro_emis_prm_error_message

END MODULE module_c_api_anthro_emis_prm

MODULE c_api_anthro_emis_prm_module
   USE module_c_api_anthro_emis_prm
END MODULE c_api_anthro_emis_prm_module
