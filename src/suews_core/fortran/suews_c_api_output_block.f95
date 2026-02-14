! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for output_block.
! -----------------------------------------------------------------------------
MODULE module_c_api_output_block
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text
   USE module_ctrl_const_allocate, ONLY: &
      ncolumnsDataOutSUEWS, ncolumnsDataOutSnow, ncolumnsDataOutESTM, ncolumnsDataOutEHC, &
      ncolumnsDataOutRSL, ncolumnsDataOutBEERS, ncolumnsDataOutDebug, ncolumnsDataOutSPARTACUS, &
      ncolumnsDataOutDailyState, ncolumnsDataOutSTEBBS, ncolumnsDataOutNHood

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_BLOCK_FIELD_COUNT = 11_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_BLOCK_BASE_LEN = 0_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_OUTPUT_BLOCK_SCHEMA_VERSION = 1_c_int

   PUBLIC :: suews_output_block_len
   PUBLIC :: suews_output_block_schema_version
   PUBLIC :: suews_output_block_columns
   PUBLIC :: suews_output_block_default
   PUBLIC :: suews_output_block_error_message

CONTAINS

   SUBROUTINE suews_output_block_len(n_flat, err) BIND(C, name='suews_output_block_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_OUTPUT_BLOCK_BASE_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_output_block_len


   SUBROUTINE suews_output_block_schema_version(schema_version, err) BIND(C, name='suews_output_block_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_OUTPUT_BLOCK_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_output_block_schema_version


   SUBROUTINE suews_output_block_columns(cols, n_cols, err) BIND(C, name='suews_output_block_columns')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: cols(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_cols
      INTEGER(c_int), INTENT(out) :: err

      IF (n_cols < SUEWS_CAPI_OUTPUT_BLOCK_FIELD_COUNT) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      cols(1) = INT(ncolumnsDataOutSUEWS, c_int)
      cols(2) = INT(ncolumnsDataOutSnow, c_int)
      cols(3) = INT(ncolumnsDataOutESTM, c_int)
      cols(4) = INT(ncolumnsDataOutEHC, c_int)
      cols(5) = INT(ncolumnsDataOutRSL, c_int)
      cols(6) = INT(ncolumnsDataOutBEERS, c_int)
      cols(7) = INT(ncolumnsDataOutDebug, c_int)
      cols(8) = INT(ncolumnsDataOutSPARTACUS, c_int)
      cols(9) = INT(ncolumnsDataOutDailyState, c_int)
      cols(10) = INT(ncolumnsDataOutSTEBBS, c_int)
      cols(11) = INT(ncolumnsDataOutNHood, c_int)

      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_output_block_columns


   SUBROUTINE suews_output_block_default(flat, n_flat, err) BIND(C, name='suews_output_block_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      ! output_block defaults to unallocated arrays; there is no flat payload.
      ! Keep this entry point for parity with other bridge adapters.
      IF (n_flat < 0_c_int) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_output_block_default


   SUBROUTINE suews_output_block_error_message(code, buffer, buffer_len) BIND(C, name='suews_output_block_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_output_block_error_message

END MODULE module_c_api_output_block

MODULE c_api_output_block_module
   USE module_c_api_output_block
END MODULE c_api_output_block_module
