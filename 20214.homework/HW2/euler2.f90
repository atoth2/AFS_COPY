program euler                     ! beginning of the program
                                  ! approximate
                                  !     dy/dt = lambda*y; y(0)=yo
                                  ! with Euler's method
                                  ! exact solution is
                                  !     y=yo*exp(lambda*t)
implicit none                     ! force all variables to be declared
integer, parameter :: p = 8       ! set the precision for real numbers
integer (kind=4) :: n              
real (kind=p), parameter :: tstop=1._p, yo = 1._p, lambda = -1._p, dt=1.e-10_p
real (kind=p) :: y,ynew,t
t = 0._p                          ! set initial time
y = yo                            ! set initial value of y
n=0                               ! set initial value of n
print*,t,y,yo*exp(lambda*t)       ! print initial values
do while(t<tstop)                 ! main loop for calculation
   ynew = y+lambda*dt*y           ! approximate new solution 
   n = n+1                        ! update the counter
   t = dt*n                       ! update time
   y = ynew                       ! update y
   !print*,t,y,yo*exp(lambda*t)    ! print results to screen
   enddo                          ! end the loop
print*,t,y,yo*exp(lambda*t)    ! print results to screen
print*,'error ',y-yo*exp(lambda*t)! print the final error
end program euler                 ! end of program

