! radiation_io.F90 - Provides logging and abort functionality
!
! (C) Copyright 2015- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!
! Author:  Robin Hogan
! Email:   r.j.hogan@ecmwf.int
!
!  This file provides file units used for logging (nulout and nulerr)
!  and for reading data files (nulrad), as well as an abort routine
!  that should do clean-up appropriate for the environment in which
!  the radiation code is embedded.
!
!  This version is for the offline package, and should be rewritten as
!  appropriate if the radiation code is to be embedded in a larger
!  model.

module radiation_io

  use parkind1, only : jpim
  use iso_c_binding, only : c_int, c_char

  implicit none
  
  ! In the IFS, nulout is equivalent to standard output but only
  ! output from the primary node will be logged, while nulerr is
  ! equivalent to standard error, and text sent to this unit from any
  ! node will be logged. Normally, nulerr should only be used before
  ! calling radiation_abort.
  integer(kind=jpim), parameter :: nulout = 6
  integer(kind=jpim), parameter :: nulerr = 0

  ! This unit may be used for reading radiation configuration files,
  ! but should be closed as soon as the file is read
  integer(kind=jpim), parameter :: nulrad = 25

  interface
    subroutine tls_error_set(code, message, length) bind(C, name="suews_tls_error_set")
      import c_int, c_char
      integer(c_int), value :: code, length
      character(c_char), intent(in) :: message(*)
    end subroutine
    function tls_error_flag() result(flag) bind(C, name="suews_tls_error_flag")
      import c_int
      integer(c_int) :: flag
    end function
  end interface

contains

  logical function radiation_error()
    radiation_error = tls_error_flag() /= 0
  end function radiation_error

  ! Abort the program with optional error message. Normally you would
  ! log details of the error to nulerr before calling this subroutine.
  subroutine radiation_abort(text)
    character(len=*), intent(in), optional :: text
    if (present(text)) then
      call tls_error_set(111_c_int, text, int(len_trim(text), c_int))
    else
      call tls_error_set(111_c_int, 'Error in radiation calculation', 30_c_int)
    end if
  end subroutine radiation_abort

end module radiation_io
