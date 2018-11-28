program euler_newton              ! beginning of the program

implicit none                     ! force all variables to be declare
integer :: n             
real, parameter ::  tstop= 2., I0 =0., steps = 10000000.
real, dimension(10000000) :: f,t
real :: I, Inew,tnew,fnew, dt
open(unit=20,file = 'outputrect10000000.out')
n = 1
dt = 2./steps
f(n) = 50                           ! by lhopitals rule, sin(50x)/x = 50cos(50x)
t(n) = 0.                           ! set initial time
I = f(n)*dt                         
n = 2
t(n) = dt
do while (t(n)<tstop)               ! main loop for calculation
   f(n) = sin(50*t(n))/t(n)
   Inew = f(n)*dt                   ! approximate new I
   I = I + Inew                     ! approximate new I
   n = n+1                          ! update the counter
   t(n) = dt*(n-1)                  ! update time
   write(20,*)t(n),f(n), I
   enddo                            ! end the loop
print*, t(n), f(n), I

end program euler_newton            ! end of program




