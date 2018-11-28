program euler_newton              ! beginning of the program

implicit none                     ! force all variables to be declare
integer :: n             
real, parameter :: tstop=40., x0 = 3., y0 = 2.,z0 = 1., dt = .001
real, parameter :: sigma = 10., r = 30., b =2.
real, dimension(40000) :: x,y,z,t
real :: xnew, ynew, znew
open(unit=20,file = 'outputlorenz2.out') ! open output file
n = 1
t(n) = 0.                            ! set initial time
x(n) = x0                            ! set initial position of x,y,z
y(n) = y0                           
z(n) = z0
do while (t(n)<tstop)                ! main loop for calculation
   xnew = sigma*(y(n) - x(n))*dt + x(n) !approximate new positions
   ynew = (r*x(n) - y(n) - x(n)*z(n))*dt + y(n) 
   znew = (-b*z(n) + x(n)*y(n))*dt + z(n)
   n = n+1                           ! update the counter
   t(n) = dt*n                       ! update time
   x(n) = xnew                       ! assign new values
   y(n) = ynew                       
   z(n) = znew                       
   write(20,*)t(n),x(n),y(n),z(n) ! write to output file
   enddo                          ! end the loop
print*, x(n),y(n),z(n),t(n)       ! print to screen

end program euler_newton      ! end of program



