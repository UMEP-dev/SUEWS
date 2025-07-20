! TEST FILE: Fortran formatting issues for workflow testing
! This file intentionally has formatting issues
! Testing workflow with permission fix

module test_formatting
implicit none

contains

subroutine poorly_formatted_subroutine(x,y,z,result)
real(8),intent(in)::x,y,z
real(8),intent(out)::result
integer::i,j,k
real(8)::temp_value

! Bad indentation and spacing
if(x>0.0d0)then
result=x+y*z
else
        result=x-y/z
endif

! Long line that needs wrapping
temp_value=x*y*z+x*x*y*y*z*z+x*x*x*y*y*y*z*z*z+x*x*x*x*y*y*y*y*z*z*z*z

! Inconsistent spacing around operators
do i=1,10
j=i*2
k=j+3
enddo

! Missing spaces in declarations
real(8)::array1(10),array2(20),array3(30)
integer::count1,count2,count3

end subroutine poorly_formatted_subroutine

function badly_spaced_function(a,b,c)result(output)
real(8)::a,b,c
real(8)::output

! Inconsistent indentation
if(a>b)then
if(b>c)then
output=a
else
    output=c
        endif
else
output=b
endif

end function badly_spaced_function

end module test_formatting