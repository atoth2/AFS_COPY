program euler_newton              ! beginning of the program

implicit none                     ! force all variables to be declare
integer :: n             
real, parameter :: tstop=30., y0 = 1.,v0 = 0., m = 1., k = 9.
real, parameter :: dt = .02, f0 =0., b = 1., v1 = 0.
real :: ynew,vnew, y, v, t
open(unit=20,file = 'output2.out')
n = 1
t= 0.                            ! set initial time
y= y0                            ! set initial value of y
v= v0                            ! set initial value of v
do while (t<tstop)                ! main loop for calculation
   vnew = (-k/m*y - b/m*v + f0/m*sin(v1*t))*dt  ! approximate new velocity
   ynew = v(n)*dt + y(n)                ! approximate new position 
   n = n+1                        ! update the counter
   t(n) = dt*n                       ! update time
   y(n) = ynew                       ! update y
   v(n) = vnew                       ! update v
   write(20,*)y(n),v(n),t(n)
   enddo                          ! end the loop
print*, y(n),v(n),t(n)

end program euler_newton      ! end of program


