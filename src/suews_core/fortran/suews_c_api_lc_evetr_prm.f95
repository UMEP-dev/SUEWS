! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for LC_EVETR_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_lc_evetr_prm
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_LC_EVETR_PRM_LEN = 57_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_LC_EVETR_PRM_SCHEMA_VERSION = 1_c_int

   TYPE :: ohm_coef_lc_shadow
      REAL(c_double) :: summer_dry = 0.0_c_double
      REAL(c_double) :: summer_wet = 0.0_c_double
      REAL(c_double) :: winter_dry = 0.0_c_double
      REAL(c_double) :: winter_wet = 0.0_c_double
   END TYPE ohm_coef_lc_shadow

   TYPE :: ohm_prm_shadow
      REAL(c_double) :: chanohm = 0.0_c_double
      REAL(c_double) :: cpanohm = 0.0_c_double
      REAL(c_double) :: kkanohm = 0.0_c_double
      REAL(c_double) :: ohm_threshsw = 0.0_c_double
      REAL(c_double) :: ohm_threshwd = 0.0_c_double
      TYPE(ohm_coef_lc_shadow), DIMENSION(3) :: ohm_coef_lc
   END TYPE ohm_prm_shadow

   TYPE :: soil_prm_shadow
      REAL(c_double) :: soildepth = 0.0_c_double
      REAL(c_double) :: soilstorecap = 0.0_c_double
      REAL(c_double) :: sathydraulicconduct = 0.0_c_double
   END TYPE soil_prm_shadow

   TYPE :: bioco2_prm_shadow
      REAL(c_double) :: beta_bioco2 = 0.0_c_double
      REAL(c_double) :: beta_enh_bioco2 = 0.0_c_double
      REAL(c_double) :: alpha_bioco2 = 0.0_c_double
      REAL(c_double) :: alpha_enh_bioco2 = 0.0_c_double
      REAL(c_double) :: resp_a = 0.0_c_double
      REAL(c_double) :: resp_b = 0.0_c_double
      REAL(c_double) :: theta_bioco2 = 0.0_c_double
      REAL(c_double) :: min_res_bioco2 = 0.0_c_double
   END TYPE bioco2_prm_shadow

   TYPE :: lai_prm_shadow
      REAL(c_double) :: baset = 0.0_c_double
      REAL(c_double) :: gddfull = 0.0_c_double
      REAL(c_double) :: basete = 0.0_c_double
      REAL(c_double) :: sddfull = 0.0_c_double
      REAL(c_double) :: laimin = 0.0_c_double
      REAL(c_double) :: laimax = 0.0_c_double
      REAL(c_double), DIMENSION(4) :: laipower = 0.0_c_double
      INTEGER(c_int) :: laitype = 0_c_int
   END TYPE lai_prm_shadow

   TYPE :: water_dist_prm_shadow
      REAL(c_double) :: to_paved = 0.0_c_double
      REAL(c_double) :: to_bldg = 0.0_c_double
      REAL(c_double) :: to_evetr = 0.0_c_double
      REAL(c_double) :: to_dectr = 0.0_c_double
      REAL(c_double) :: to_grass = 0.0_c_double
      REAL(c_double) :: to_bsoil = 0.0_c_double
      REAL(c_double) :: to_water = 0.0_c_double
      REAL(c_double) :: to_soilstore = 0.0_c_double
   END TYPE water_dist_prm_shadow

   TYPE :: lc_evetr_prm_shadow
      REAL(c_double) :: sfr = 0.0_c_double
      REAL(c_double) :: emis = 0.0_c_double
      REAL(c_double) :: faievetree = 0.0_c_double
      REAL(c_double) :: evetreeh = 0.0_c_double
      REAL(c_double) :: alb_min = 0.0_c_double
      REAL(c_double) :: alb_max = 0.0_c_double
      TYPE(ohm_prm_shadow) :: ohm
      TYPE(soil_prm_shadow) :: soil
      REAL(c_double) :: statelimit = 0.0_c_double
      REAL(c_double) :: irrfracevetr = 0.0_c_double
      REAL(c_double) :: wetthresh = 0.0_c_double
      TYPE(bioco2_prm_shadow) :: bioco2
      REAL(c_double) :: maxconductance = 0.0_c_double
      TYPE(lai_prm_shadow) :: lai
      TYPE(water_dist_prm_shadow) :: waterdist
   END TYPE lc_evetr_prm_shadow

   PUBLIC :: suews_lc_evetr_prm_len
   PUBLIC :: suews_lc_evetr_prm_schema_version
   PUBLIC :: suews_lc_evetr_prm_default
   PUBLIC :: suews_lc_evetr_prm_error_message

CONTAINS

   SUBROUTINE suews_lc_evetr_prm_len(n_flat, err) BIND(C, name='suews_lc_evetr_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_LC_EVETR_PRM_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_lc_evetr_prm_len


   SUBROUTINE suews_lc_evetr_prm_schema_version(schema_version, err) BIND(C, name='suews_lc_evetr_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_LC_EVETR_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_lc_evetr_prm_schema_version


   SUBROUTINE suews_lc_evetr_prm_default(flat, n_flat, err) BIND(C, name='suews_lc_evetr_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(lc_evetr_prm_shadow) :: state

      CALL lc_evetr_prm_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_lc_evetr_prm_default


   SUBROUTINE lc_evetr_prm_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(lc_evetr_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      INTEGER(c_int) :: idx
      INTEGER :: i

      IF (n_flat < SUEWS_CAPI_LC_EVETR_PRM_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1
      flat(idx) = state%sfr; idx = idx + 1
      flat(idx) = state%emis; idx = idx + 1
      flat(idx) = state%faievetree; idx = idx + 1
      flat(idx) = state%evetreeh; idx = idx + 1
      flat(idx) = state%alb_min; idx = idx + 1
      flat(idx) = state%alb_max; idx = idx + 1

      flat(idx) = state%ohm%chanohm; idx = idx + 1
      flat(idx) = state%ohm%cpanohm; idx = idx + 1
      flat(idx) = state%ohm%kkanohm; idx = idx + 1
      flat(idx) = state%ohm%ohm_threshsw; idx = idx + 1
      flat(idx) = state%ohm%ohm_threshwd; idx = idx + 1
      DO i = 1, 3
         flat(idx) = state%ohm%ohm_coef_lc(i)%summer_dry; idx = idx + 1
         flat(idx) = state%ohm%ohm_coef_lc(i)%summer_wet; idx = idx + 1
         flat(idx) = state%ohm%ohm_coef_lc(i)%winter_dry; idx = idx + 1
         flat(idx) = state%ohm%ohm_coef_lc(i)%winter_wet; idx = idx + 1
      END DO

      flat(idx) = state%soil%soildepth; idx = idx + 1
      flat(idx) = state%soil%soilstorecap; idx = idx + 1
      flat(idx) = state%soil%sathydraulicconduct; idx = idx + 1

      flat(idx) = state%statelimit; idx = idx + 1
      flat(idx) = state%irrfracevetr; idx = idx + 1
      flat(idx) = state%wetthresh; idx = idx + 1

      flat(idx) = state%bioco2%beta_bioco2; idx = idx + 1
      flat(idx) = state%bioco2%beta_enh_bioco2; idx = idx + 1
      flat(idx) = state%bioco2%alpha_bioco2; idx = idx + 1
      flat(idx) = state%bioco2%alpha_enh_bioco2; idx = idx + 1
      flat(idx) = state%bioco2%resp_a; idx = idx + 1
      flat(idx) = state%bioco2%resp_b; idx = idx + 1
      flat(idx) = state%bioco2%theta_bioco2; idx = idx + 1
      flat(idx) = state%bioco2%min_res_bioco2; idx = idx + 1

      flat(idx) = state%maxconductance; idx = idx + 1

      flat(idx) = state%lai%baset; idx = idx + 1
      flat(idx) = state%lai%gddfull; idx = idx + 1
      flat(idx) = state%lai%basete; idx = idx + 1
      flat(idx) = state%lai%sddfull; idx = idx + 1
      flat(idx) = state%lai%laimin; idx = idx + 1
      flat(idx) = state%lai%laimax; idx = idx + 1
      DO i = 1, 4
         flat(idx) = state%lai%laipower(i); idx = idx + 1
      END DO
      flat(idx) = REAL(state%lai%laitype, c_double); idx = idx + 1

      flat(idx) = state%waterdist%to_paved; idx = idx + 1
      flat(idx) = state%waterdist%to_bldg; idx = idx + 1
      flat(idx) = state%waterdist%to_evetr; idx = idx + 1
      flat(idx) = state%waterdist%to_dectr; idx = idx + 1
      flat(idx) = state%waterdist%to_grass; idx = idx + 1
      flat(idx) = state%waterdist%to_bsoil; idx = idx + 1
      flat(idx) = state%waterdist%to_water; idx = idx + 1
      flat(idx) = state%waterdist%to_soilstore; idx = idx + 1

      err = SUEWS_CAPI_OK

   END SUBROUTINE lc_evetr_prm_pack


   SUBROUTINE suews_lc_evetr_prm_error_message(code, buffer, buffer_len) BIND(C, name='suews_lc_evetr_prm_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_lc_evetr_prm_error_message

END MODULE module_c_api_lc_evetr_prm

MODULE c_api_lc_evetr_prm_module
   USE module_c_api_lc_evetr_prm
END MODULE c_api_lc_evetr_prm_module
