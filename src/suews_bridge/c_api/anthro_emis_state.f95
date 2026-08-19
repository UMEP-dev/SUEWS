! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade for anthroEmis_STATE.
! -----------------------------------------------------------------------------
module module_c_api_anthro_emis_state
use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
use module_c_api_common, only: &
   SUEWS_CAPI_OK, SUEWS_CAPI_BAD_BUFFER, SUEWS_CAPI_BAD_STATE, &
   copy_to_c_buffer, suews_capi_error_text
use module_type_anthro, only: anthroEmis_STATE

implicit none

private

public :: SUEWS_CAPI_OK
public :: SUEWS_CAPI_BAD_BUFFER
public :: SUEWS_CAPI_BAD_STATE

integer(c_int), parameter, public :: SUEWS_CAPI_ANTHROEMIS_STATE_LEN = 22_c_int
integer(c_int), parameter, public :: SUEWS_CAPI_ANTHROEMIS_STATE_SCHEMA_VERSION = 1_c_int

type :: anthroemis_state_shadow
   real(c_double), dimension(12) :: hdd_id = 0.0_c_double
   real(c_double) :: fc = 0.0_c_double
   real(c_double) :: fc_anthro = 0.0_c_double
   real(c_double) :: fc_biogen = 0.0_c_double
   real(c_double) :: fc_build = 0.0_c_double
   real(c_double) :: fc_metab = 0.0_c_double
   real(c_double) :: fc_photo = 0.0_c_double
   real(c_double) :: fc_point = 0.0_c_double
   real(c_double) :: fc_respi = 0.0_c_double
   real(c_double) :: fc_traff = 0.0_c_double
   logical :: iter_safe = .false.
end type anthroemis_state_shadow

public :: suews_anthroemis_state_len
public :: suews_anthroemis_state_schema_version
public :: suews_anthroemis_state_default
public :: suews_anthroemis_state_error_message
public :: anthroemis_state_unpack

contains

subroutine suews_anthroemis_state_len(n_flat, err) bind(C, name='suews_anthroemis_state_len')
   implicit none

   integer(c_int), intent(out) :: n_flat
   integer(c_int), intent(out) :: err

   n_flat = SUEWS_CAPI_ANTHROEMIS_STATE_LEN
   err = SUEWS_CAPI_OK

end subroutine suews_anthroemis_state_len

subroutine suews_anthroemis_state_schema_version(schema_version, err) bind(C, name='suews_anthroemis_state_schema_version')
   implicit none

   integer(c_int), intent(out) :: schema_version
   integer(c_int), intent(out) :: err

   schema_version = SUEWS_CAPI_ANTHROEMIS_STATE_SCHEMA_VERSION
   err = SUEWS_CAPI_OK

end subroutine suews_anthroemis_state_schema_version

subroutine suews_anthroemis_state_default(flat, n_flat, err) bind(C, name='suews_anthroemis_state_default')
   implicit none

   real(c_double), intent(out) :: flat(*)
   integer(c_int), value, intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   type(anthroemis_state_shadow) :: state

   call anthroemis_state_pack(state, flat, n_flat, err)

end subroutine suews_anthroemis_state_default

subroutine anthroemis_state_pack(state, flat, n_flat, err)
   implicit none

   type(anthroemis_state_shadow), intent(in) :: state
   real(c_double), intent(out) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   integer(c_int), intent(out) :: err

   integer :: i
   integer :: idx

   if (n_flat<SUEWS_CAPI_ANTHROEMIS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1
   do i = 1, 12
      flat(idx) = state%hdd_id(i)
      idx = idx + 1
   end do
   flat(idx) = state%fc; idx = idx + 1
   flat(idx) = state%fc_anthro; idx = idx + 1
   flat(idx) = state%fc_biogen; idx = idx + 1
   flat(idx) = state%fc_build; idx = idx + 1
   flat(idx) = state%fc_metab; idx = idx + 1
   flat(idx) = state%fc_photo; idx = idx + 1
   flat(idx) = state%fc_point; idx = idx + 1
   flat(idx) = state%fc_respi; idx = idx + 1
   flat(idx) = state%fc_traff; idx = idx + 1
   flat(idx) = merge(1.0_c_double, 0.0_c_double, state%iter_safe)

   err = SUEWS_CAPI_OK

end subroutine anthroemis_state_pack

subroutine anthroemis_state_unpack(flat, n_flat, state, err)
   implicit none

   real(c_double), intent(in) :: flat(*)
   integer(c_int), intent(in) :: n_flat
   type(anthroEmis_STATE), intent(out) :: state
   integer(c_int), intent(out) :: err

   integer(c_int) :: idx
   integer(c_int) :: i

   if (n_flat<SUEWS_CAPI_ANTHROEMIS_STATE_LEN) then
      err = SUEWS_CAPI_BAD_BUFFER
      return
   end if

   idx = 1_c_int
   do i = 1_c_int, 12_c_int
      state%HDD_id(i) = flat(idx)
      idx = idx + 1_c_int
   end do
   state%Fc = flat(idx); idx = idx + 1_c_int
   state%Fc_anthro = flat(idx); idx = idx + 1_c_int
   state%Fc_biogen = flat(idx); idx = idx + 1_c_int
   state%Fc_build = flat(idx); idx = idx + 1_c_int
   state%Fc_metab = flat(idx); idx = idx + 1_c_int
   state%Fc_photo = flat(idx); idx = idx + 1_c_int
   state%Fc_point = flat(idx); idx = idx + 1_c_int
   state%Fc_respi = flat(idx); idx = idx + 1_c_int
   state%Fc_traff = flat(idx); idx = idx + 1_c_int
   state%iter_safe = flat(idx)>=0.5_c_double

   err = SUEWS_CAPI_OK

end subroutine anthroemis_state_unpack

subroutine suews_anthroemis_state_error_message(code, buffer, buffer_len) bind(C, name='suews_anthroemis_state_error_message')
   implicit none

   integer(c_int), value, intent(in) :: code
   character(c_char), intent(out) :: buffer(*)
   integer(c_int), value, intent(in) :: buffer_len

   character(LEN=128) :: msg

   call suews_capi_error_text(code, msg)
   call copy_to_c_buffer(msg, buffer, buffer_len)

end subroutine suews_anthroemis_state_error_message

end module module_c_api_anthro_emis_state

module c_api_anthro_emis_state_module
use module_c_api_anthro_emis_state
end module c_api_anthro_emis_state_module
