! TEST FILE: Combined test - Fortran part
! This file has formatting issues

subroutine test_combined(x,y,output)
implicit none
real(8),intent(in)::x,y
real(8),intent(out)::output

! Bad formatting
if(x>y)then
output=x*x
else
        output=y*y
endif

end subroutine test_combined