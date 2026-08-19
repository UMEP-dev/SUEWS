! -----------------------------------------------------------------------------
! SUEWS Rust bridge C API facade: sun position calculation.
!
! Thin wrapper around NARP_cal_SunPosition from module_phys_narp.
! Requires physics feature (libsuewsphys.a) for linking.
! -----------------------------------------------------------------------------
module module_c_api_sunposition
  use, intrinsic :: iso_c_binding, only: c_int, c_double
  use module_c_api_common, only: SUEWS_CAPI_OK
  use module_phys_narp, only: NARP_cal_SunPosition
  implicit none
  private
  public :: suews_sunposition_calc
contains
  subroutine suews_sunposition_calc(year, idectime, utc, lat, lon, alt, &
                                     azimuth, zenith, err) &
    bind(C, name='suews_sunposition_calc')
    real(c_double), value, intent(in) :: year, idectime, utc, lat, lon, alt
    real(c_double), intent(out) :: azimuth, zenith
    integer(c_int), intent(out) :: err
    call NARP_cal_SunPosition(year, idectime, utc, lat, lon, alt, azimuth, zenith)
    err = SUEWS_CAPI_OK
  end subroutine
end module
