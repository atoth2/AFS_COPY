module myfunction                    ! beginning of the function
contains
real function f(x)

implicit none                        ! force all variables to be declare
integer, intent(in) :: x
integer :: i
real :: factorial

factorial = 1        
do i = 1,x            ! main loop for calculation
    factorial = factorial*i
end do                               ! end the loop
f = factorial

end function f
end module myfunction                ! end of program






















