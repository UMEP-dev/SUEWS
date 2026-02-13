! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for atm_state.
! -----------------------------------------------------------------------------
MODULE module_c_api_atm
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text
   USE module_ctrl_const_allocate, ONLY: nsurf

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_ATM_STATE_LEN = 39_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_ATM_STATE_SCHEMA_VERSION = 1_c_int

   TYPE :: atm_state_shadow
      REAL(c_double) :: fcld = 0.0_c_double
      REAL(c_double) :: avcp = 0.0_c_double
      REAL(c_double) :: dens_dry = 0.0_c_double
      REAL(c_double) :: avdens = 0.0_c_double
      REAL(c_double) :: dq = 0.0_c_double
      REAL(c_double) :: ea_hpa = 0.0_c_double
      REAL(c_double) :: es_hpa = 0.0_c_double
      REAL(c_double) :: lv_j_kg = 0.0_c_double
      REAL(c_double) :: lvs_j_kg = 0.0_c_double
      REAL(c_double) :: tlv = 0.0_c_double
      REAL(c_double) :: psyc_hpa = 0.0_c_double
      REAL(c_double) :: psycice_hpa = 0.0_c_double
      REAL(c_double) :: s_pa = 0.0_c_double
      REAL(c_double) :: s_hpa = 0.0_c_double
      REAL(c_double) :: sice_hpa = 0.0_c_double
      REAL(c_double) :: vpd_hpa = 0.0_c_double
      REAL(c_double) :: vpd_pa = 0.0_c_double
      REAL(c_double) :: u10_ms = 0.0_c_double
      REAL(c_double) :: u_hbh = 0.0_c_double
      REAL(c_double) :: t2_c = 0.0_c_double
      REAL(c_double) :: t_half_bldg_c = 0.0_c_double
      REAL(c_double) :: q2_gkg = 0.0_c_double
      REAL(c_double) :: rh2 = 0.0_c_double
      REAL(c_double) :: l_mod = 0.0_c_double
      REAL(c_double) :: zl = 0.0_c_double
      REAL(c_double) :: ra_h = 0.0_c_double
      REAL(c_double) :: rs = 0.0_c_double
      REAL(c_double) :: ustar = 0.0_c_double
      REAL(c_double) :: tstar = 0.0_c_double
      REAL(c_double) :: rb = 0.0_c_double
      REAL(c_double) :: tair_av = 0.0_c_double
      REAL(c_double), DIMENSION(nsurf) :: rss_surf = 0.0_c_double
      LOGICAL :: iter_safe = .TRUE.
   END TYPE atm_state_shadow

   PUBLIC :: suews_atm_state_len
   PUBLIC :: suews_atm_state_schema_version
   PUBLIC :: suews_atm_state_default
   PUBLIC :: suews_atm_error_message

CONTAINS

   SUBROUTINE suews_atm_state_len(n_flat, err) BIND(C, name='suews_atm_state_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_ATM_STATE_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_atm_state_len


   SUBROUTINE suews_atm_state_schema_version(schema_version, err) BIND(C, name='suews_atm_state_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_ATM_STATE_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_atm_state_schema_version


   SUBROUTINE suews_atm_state_default(flat, n_flat, err) BIND(C, name='suews_atm_state_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(atm_state_shadow) :: state

      CALL atm_state_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_atm_state_default


   SUBROUTINE atm_state_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(atm_state_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      INTEGER :: idx
      INTEGER :: i

      IF (n_flat < SUEWS_CAPI_ATM_STATE_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1
      flat(idx) = state%fcld; idx = idx + 1
      flat(idx) = state%avcp; idx = idx + 1
      flat(idx) = state%dens_dry; idx = idx + 1
      flat(idx) = state%avdens; idx = idx + 1
      flat(idx) = state%dq; idx = idx + 1
      flat(idx) = state%ea_hpa; idx = idx + 1
      flat(idx) = state%es_hpa; idx = idx + 1
      flat(idx) = state%lv_j_kg; idx = idx + 1
      flat(idx) = state%lvs_j_kg; idx = idx + 1
      flat(idx) = state%tlv; idx = idx + 1
      flat(idx) = state%psyc_hpa; idx = idx + 1
      flat(idx) = state%psycice_hpa; idx = idx + 1
      flat(idx) = state%s_pa; idx = idx + 1
      flat(idx) = state%s_hpa; idx = idx + 1
      flat(idx) = state%sice_hpa; idx = idx + 1
      flat(idx) = state%vpd_hpa; idx = idx + 1
      flat(idx) = state%vpd_pa; idx = idx + 1
      flat(idx) = state%u10_ms; idx = idx + 1
      flat(idx) = state%u_hbh; idx = idx + 1
      flat(idx) = state%t2_c; idx = idx + 1
      flat(idx) = state%t_half_bldg_c; idx = idx + 1
      flat(idx) = state%q2_gkg; idx = idx + 1
      flat(idx) = state%rh2; idx = idx + 1
      flat(idx) = state%l_mod; idx = idx + 1
      flat(idx) = state%zl; idx = idx + 1
      flat(idx) = state%ra_h; idx = idx + 1
      flat(idx) = state%rs; idx = idx + 1
      flat(idx) = state%ustar; idx = idx + 1
      flat(idx) = state%tstar; idx = idx + 1
      flat(idx) = state%rb; idx = idx + 1
      flat(idx) = state%tair_av; idx = idx + 1

      DO i = 1, nsurf
         flat(idx) = state%rss_surf(i)
         idx = idx + 1
      END DO

      flat(idx) = MERGE(1.0_c_double, 0.0_c_double, state%iter_safe)

      err = SUEWS_CAPI_OK

   END SUBROUTINE atm_state_pack


   SUBROUTINE suews_atm_error_message(code, buffer, buffer_len) BIND(C, name='suews_atm_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_atm_error_message

END MODULE module_c_api_atm

MODULE c_api_atm_module
   USE module_c_api_atm
END MODULE c_api_atm_module
