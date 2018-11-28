program euler_newton              ! beginning of the program

implicit none                     ! force all variables to be declare
integer :: n             
real, parameter :: tstop=5., y0 = 0., dt = .001
real :: y,ynew,t,error, y1
open(unit=20,file = 'output.out')
t = 0.                            ! set initial time
y = y0                            ! set initial value of y
n=0                               ! set initial value of n
do while (t<tstop)                ! main loop for calculation
   ynew = (sin(t) - 5*y)*dt + y                ! approximate new position 
   n = n+1                        ! update the counter
   t = dt*n                       ! update time
   y = ynew                       ! update y
   y1 = 1./26.*(exp(-5.*t) + 5.*sin(t)- cos(t))
   !print*, t,y                  ! print the value of t,y
   write(20,*) t,y, y1
   enddo                          ! end the loop
error = 1./26.*(exp(-5.*t) + 5.*sin(t)- cos(t)) - y
print*, t,y, error, dt
end program euler_newton      ! end of program
