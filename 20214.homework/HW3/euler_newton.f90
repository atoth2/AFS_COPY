program euler_newton              ! beginning of the program

implicit none                     ! force all variables to be declare
integer :: n             
real, parameter :: tstop=5., y0 = 0.,v0 = 1., m = 1., k = 4., dt = .02
real :: y,v,ynew,t,vnew
t = 0.                            ! set initial time
y = y0                            ! set initial value of y
v = v0                            ! set initial value of v
n=0                               ! set initial value of n
do while (t<tstop)                ! main loop for calculation
   vnew = -k/m*y*dt + v           ! approximate new velocity
   ynew = v*dt + y                ! approximate new position 
   n = n+1                        ! update the counter
   t = dt*n                       ! update time
   y = ynew                       ! update y
   v = vnew                       ! update v
   print*, t,y,v                  ! print the value of t,y, and v
   enddo                          ! end the loop
end program euler_newton      ! end of program


