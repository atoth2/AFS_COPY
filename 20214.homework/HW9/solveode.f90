program solveode
! This is the main driver program for solving
! a generic set of neq differential equations.
use commondata
use ode1module
use ode2module
use ode4module
implicit none
integer :: i
real, dimension(1:neq) :: y
real :: t,tout,dt
open(10,file = 'output.dat')
t = 0. !initialize t
dt = tstop/real(nt) !determine dt
do i=1,neq
y(i) = yic(i) !assign y to its initial condition
enddo
print*,t,y !output results
write(10,*)t,y
do i=1,nt !main loop in time
tout = t + dt !set incremental end time
call ode1(y,t,tout) !update y and t ! Changed !!!!!!!!!!!!!!!!!
print*,t,y !output the results
write(10,*)t,y
enddo
print*,'error = ',y(1) - exp(-t),dt  ! Changed !!!!!!!!!!!!!!!!!
!y(1) - exp(t**2./2),dt Problem2
!y(1) - exp(-t) original error
end program solveode







