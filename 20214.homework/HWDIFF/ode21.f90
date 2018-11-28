program euler_newton              ! beginning of the program

implicit none                     ! force all variables to be declare
integer :: n             
real, parameter :: tstop=5., y0 = 1., dt = .02
real :: y,ynew,t
open(unit=20,file = 'output.out')
t = 0.                            ! set initial time
y = y0                            ! set initial value of y
n=0                               ! set initial value of n
do while (t<tstop)                ! main loop for calculation
   ynew = (t*y)*dt + y                ! approximate new position 
   n = n+1                        ! update the counter
   t = dt*n                       ! update time
   y = ynew                       ! update y
   print*, t,y                    ! print the value of t,y
   write(20,*) t,y
   enddo                          ! end the loop
end program euler_newton      ! end of program





