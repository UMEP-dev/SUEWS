! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for BUILDING_ARCHETYPE_PRM.
! -----------------------------------------------------------------------------
MODULE module_c_api_building_archetype_prm
   USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_double, c_char
   USE module_c_api_common, ONLY: &
      SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
      copy_to_c_buffer, suews_capi_error_text

   IMPLICIT NONE

   PRIVATE

   PUBLIC :: SUEWS_CAPI_OK
   PUBLIC :: SUEWS_CAPI_BAD_BUFFER
   PUBLIC :: SUEWS_CAPI_BAD_STATE

   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS = 144_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS = 2_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_LEN = 639_c_int
   INTEGER(c_int), PARAMETER, PUBLIC :: SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_SCHEMA_VERSION = 1_c_int

   TYPE :: building_archetype_prm_shadow
      REAL(c_double) :: buildingcount = 0.0_c_double
      REAL(c_double) :: occupants = 0.0_c_double
      REAL(c_double) :: hhs0 = 0.0_c_double
      REAL(c_double) :: age_0_4 = 0.0_c_double
      REAL(c_double) :: age_5_11 = 0.0_c_double
      REAL(c_double) :: age_12_18 = 0.0_c_double
      REAL(c_double) :: age_19_64 = 0.0_c_double
      REAL(c_double) :: age_65plus = 0.0_c_double
      REAL(c_double) :: stebbs_height = 0.0_c_double
      REAL(c_double) :: footprintarea = 0.0_c_double
      REAL(c_double) :: wallexternalarea = 0.0_c_double
      REAL(c_double) :: ratiointernalvolume = 0.0_c_double
      REAL(c_double) :: wwr = 0.0_c_double
      REAL(c_double) :: wallthickness = 0.0_c_double
      REAL(c_double) :: walleffectiveconductivity = 0.0_c_double
      REAL(c_double) :: walldensity = 0.0_c_double
      REAL(c_double) :: wallcp = 0.0_c_double
      REAL(c_double) :: wallextthickness = 0.0_c_double
      REAL(c_double) :: wallexteffectiveconductivity = 0.0_c_double
      REAL(c_double) :: wallextdensity = 0.0_c_double
      REAL(c_double) :: wallextcp = 0.0_c_double
      REAL(c_double) :: walloutercapfrac = 0.0_c_double
      REAL(c_double) :: wallexternalemissivity = 0.0_c_double
      REAL(c_double) :: wallinternalemissivity = 0.0_c_double
      REAL(c_double) :: walltransmissivity = 0.0_c_double
      REAL(c_double) :: wallabsorbtivity = 0.0_c_double
      REAL(c_double) :: wallreflectivity = 0.0_c_double
      REAL(c_double) :: roofthickness = 0.0_c_double
      REAL(c_double) :: roofeffectiveconductivity = 0.0_c_double
      REAL(c_double) :: roofdensity = 0.0_c_double
      REAL(c_double) :: roofcp = 0.0_c_double
      REAL(c_double) :: roofextthickness = 0.0_c_double
      REAL(c_double) :: roofexteffectiveconductivity = 0.0_c_double
      REAL(c_double) :: roofextdensity = 0.0_c_double
      REAL(c_double) :: roofextcp = 0.0_c_double
      REAL(c_double) :: roofoutercapfrac = 0.0_c_double
      REAL(c_double) :: roofexternalemissivity = 0.0_c_double
      REAL(c_double) :: roofinternalemissivity = 0.0_c_double
      REAL(c_double) :: rooftransmissivity = 0.0_c_double
      REAL(c_double) :: roofabsorbtivity = 0.0_c_double
      REAL(c_double) :: roofreflectivity = 0.0_c_double
      REAL(c_double) :: floorthickness = 0.0_c_double
      REAL(c_double) :: groundflooreffectiveconductivity = 0.0_c_double
      REAL(c_double) :: groundfloordensity = 0.0_c_double
      REAL(c_double) :: groundfloorcp = 0.0_c_double
      REAL(c_double) :: windowthickness = 0.0_c_double
      REAL(c_double) :: windoweffectiveconductivity = 0.0_c_double
      REAL(c_double) :: windowdensity = 0.0_c_double
      REAL(c_double) :: windowcp = 0.0_c_double
      REAL(c_double) :: windowexternalemissivity = 0.0_c_double
      REAL(c_double) :: windowinternalemissivity = 0.0_c_double
      REAL(c_double) :: windowtransmissivity = 0.0_c_double
      REAL(c_double) :: windowabsorbtivity = 0.0_c_double
      REAL(c_double) :: windowreflectivity = 0.0_c_double
      REAL(c_double) :: internalmassdensity = 0.0_c_double
      REAL(c_double) :: internalmasscp = 0.0_c_double
      REAL(c_double) :: internalmassemissivity = 0.0_c_double
      REAL(c_double) :: maxheatingpower = 0.0_c_double
      REAL(c_double) :: watertankwatervolume = 0.0_c_double
      REAL(c_double) :: maximumhotwaterheatingpower = 0.0_c_double
      REAL(c_double) :: heatingsetpointtemperature = 0.0_c_double
      REAL(c_double) :: coolingsetpointtemperature = 0.0_c_double
      REAL(c_double), DIMENSION(0:143, 2) :: metabolismprofile = 0.0_c_double
      REAL(c_double), DIMENSION(0:143, 2) :: applianceprofile = 0.0_c_double
      LOGICAL :: iter_safe = .TRUE.
   END TYPE building_archetype_prm_shadow

   PUBLIC :: suews_building_archetype_prm_len
   PUBLIC :: suews_building_archetype_prm_schema_version
   PUBLIC :: suews_building_archetype_prm_default
   PUBLIC :: suews_building_archetype_prm_error_message

CONTAINS

   SUBROUTINE suews_building_archetype_prm_len(n_flat, err) BIND(C, name='suews_building_archetype_prm_len')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      n_flat = SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_LEN
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_building_archetype_prm_len


   SUBROUTINE suews_building_archetype_prm_schema_version(schema_version, err) BIND(C, name='suews_building_archetype_prm_schema_version')
      IMPLICIT NONE

      INTEGER(c_int), INTENT(out) :: schema_version
      INTEGER(c_int), INTENT(out) :: err

      schema_version = SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_SCHEMA_VERSION
      err = SUEWS_CAPI_OK

   END SUBROUTINE suews_building_archetype_prm_schema_version


   SUBROUTINE suews_building_archetype_prm_default(flat, n_flat, err) BIND(C, name='suews_building_archetype_prm_default')
      IMPLICIT NONE

      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), VALUE, INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      TYPE(building_archetype_prm_shadow) :: state

      CALL building_archetype_prm_pack(state, flat, n_flat, err)

   END SUBROUTINE suews_building_archetype_prm_default


   SUBROUTINE building_archetype_prm_pack(state, flat, n_flat, err)
      IMPLICIT NONE

      TYPE(building_archetype_prm_shadow), INTENT(in) :: state
      REAL(c_double), INTENT(out) :: flat(*)
      INTEGER(c_int), INTENT(in) :: n_flat
      INTEGER(c_int), INTENT(out) :: err

      INTEGER(c_int) :: idx
      INTEGER(c_int) :: i
      INTEGER(c_int) :: j

      IF (n_flat < SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_LEN) THEN
         err = SUEWS_CAPI_BAD_BUFFER
         RETURN
      END IF

      idx = 1_c_int

      flat(idx) = state%buildingcount; idx = idx + 1_c_int
      flat(idx) = state%occupants; idx = idx + 1_c_int
      flat(idx) = state%hhs0; idx = idx + 1_c_int
      flat(idx) = state%age_0_4; idx = idx + 1_c_int
      flat(idx) = state%age_5_11; idx = idx + 1_c_int
      flat(idx) = state%age_12_18; idx = idx + 1_c_int
      flat(idx) = state%age_19_64; idx = idx + 1_c_int
      flat(idx) = state%age_65plus; idx = idx + 1_c_int
      flat(idx) = state%stebbs_height; idx = idx + 1_c_int
      flat(idx) = state%footprintarea; idx = idx + 1_c_int
      flat(idx) = state%wallexternalarea; idx = idx + 1_c_int
      flat(idx) = state%ratiointernalvolume; idx = idx + 1_c_int
      flat(idx) = state%wwr; idx = idx + 1_c_int
      flat(idx) = state%wallthickness; idx = idx + 1_c_int
      flat(idx) = state%walleffectiveconductivity; idx = idx + 1_c_int
      flat(idx) = state%walldensity; idx = idx + 1_c_int
      flat(idx) = state%wallcp; idx = idx + 1_c_int
      flat(idx) = state%wallextthickness; idx = idx + 1_c_int
      flat(idx) = state%wallexteffectiveconductivity; idx = idx + 1_c_int
      flat(idx) = state%wallextdensity; idx = idx + 1_c_int
      flat(idx) = state%wallextcp; idx = idx + 1_c_int
      flat(idx) = state%walloutercapfrac; idx = idx + 1_c_int
      flat(idx) = state%wallexternalemissivity; idx = idx + 1_c_int
      flat(idx) = state%wallinternalemissivity; idx = idx + 1_c_int
      flat(idx) = state%walltransmissivity; idx = idx + 1_c_int
      flat(idx) = state%wallabsorbtivity; idx = idx + 1_c_int
      flat(idx) = state%wallreflectivity; idx = idx + 1_c_int
      flat(idx) = state%roofthickness; idx = idx + 1_c_int
      flat(idx) = state%roofeffectiveconductivity; idx = idx + 1_c_int
      flat(idx) = state%roofdensity; idx = idx + 1_c_int
      flat(idx) = state%roofcp; idx = idx + 1_c_int
      flat(idx) = state%roofextthickness; idx = idx + 1_c_int
      flat(idx) = state%roofexteffectiveconductivity; idx = idx + 1_c_int
      flat(idx) = state%roofextdensity; idx = idx + 1_c_int
      flat(idx) = state%roofextcp; idx = idx + 1_c_int
      flat(idx) = state%roofoutercapfrac; idx = idx + 1_c_int
      flat(idx) = state%roofexternalemissivity; idx = idx + 1_c_int
      flat(idx) = state%roofinternalemissivity; idx = idx + 1_c_int
      flat(idx) = state%rooftransmissivity; idx = idx + 1_c_int
      flat(idx) = state%roofabsorbtivity; idx = idx + 1_c_int
      flat(idx) = state%roofreflectivity; idx = idx + 1_c_int
      flat(idx) = state%floorthickness; idx = idx + 1_c_int
      flat(idx) = state%groundflooreffectiveconductivity; idx = idx + 1_c_int
      flat(idx) = state%groundfloordensity; idx = idx + 1_c_int
      flat(idx) = state%groundfloorcp; idx = idx + 1_c_int
      flat(idx) = state%windowthickness; idx = idx + 1_c_int
      flat(idx) = state%windoweffectiveconductivity; idx = idx + 1_c_int
      flat(idx) = state%windowdensity; idx = idx + 1_c_int
      flat(idx) = state%windowcp; idx = idx + 1_c_int
      flat(idx) = state%windowexternalemissivity; idx = idx + 1_c_int
      flat(idx) = state%windowinternalemissivity; idx = idx + 1_c_int
      flat(idx) = state%windowtransmissivity; idx = idx + 1_c_int
      flat(idx) = state%windowabsorbtivity; idx = idx + 1_c_int
      flat(idx) = state%windowreflectivity; idx = idx + 1_c_int
      flat(idx) = state%internalmassdensity; idx = idx + 1_c_int
      flat(idx) = state%internalmasscp; idx = idx + 1_c_int
      flat(idx) = state%internalmassemissivity; idx = idx + 1_c_int
      flat(idx) = state%maxheatingpower; idx = idx + 1_c_int
      flat(idx) = state%watertankwatervolume; idx = idx + 1_c_int
      flat(idx) = state%maximumhotwaterheatingpower; idx = idx + 1_c_int
      flat(idx) = state%heatingsetpointtemperature; idx = idx + 1_c_int
      flat(idx) = state%coolingsetpointtemperature; idx = idx + 1_c_int

      DO j = 1_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS
         DO i = 0_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS - 1_c_int
            flat(idx) = state%metabolismprofile(i, j)
            idx = idx + 1_c_int
         END DO
      END DO

      DO j = 1_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_GROUPS
         DO i = 0_c_int, SUEWS_CAPI_BUILDING_ARCHETYPE_PRM_PROFILE_STEPS - 1_c_int
            flat(idx) = state%applianceprofile(i, j)
            idx = idx + 1_c_int
         END DO
      END DO

      flat(idx) = MERGE(1.0_c_double, 0.0_c_double, state%iter_safe)

      err = SUEWS_CAPI_OK

   END SUBROUTINE building_archetype_prm_pack


   SUBROUTINE suews_building_archetype_prm_error_message(code, buffer, buffer_len) BIND(C, name='suews_building_archetype_prm_error_message')
      IMPLICIT NONE

      INTEGER(c_int), VALUE, INTENT(in) :: code
      CHARACTER(c_char), INTENT(out) :: buffer(*)
      INTEGER(c_int), VALUE, INTENT(in) :: buffer_len

      CHARACTER(LEN=128) :: msg

      CALL suews_capi_error_text(code, msg)
      CALL copy_to_c_buffer(msg, buffer, buffer_len)

   END SUBROUTINE suews_building_archetype_prm_error_message

END MODULE module_c_api_building_archetype_prm

MODULE c_api_building_archetype_prm_module
   USE module_c_api_building_archetype_prm
END MODULE c_api_building_archetype_prm_module
