module taylorflex                   ! beginning of the function
contains
real function tay(x,m)

use myfunction

implicit none                     ! force all variables to be declare
integer, intent(in) :: m
real, intent(in) :: x 
integer :: i
real :: tay

tay = 0
   do i = 0,m-1
      tay = tay + ((-2.*x)**(i))/f(i)    ! Call on factorial function f(x)
   end do

end function tay
end module taylorflex               ! end of program