!-------------------------------------------------------------------------
!
module commondata
integer, parameter :: neq=1 !set the number of equations
integer, parameter :: nt=100 !set the number of time steps
real, parameter :: tstop = 10. !set the stop time
real, dimension(1:neq) :: yic=(/1./) !set the initial conditions
end module commondata
!
!-------------------------------------------------------------------------
!
module rhs
! This subroutine in this module evaluates the right hand side of the
! system of neq ordinary differential equations.
contains
subroutine fcn(t,y,ydot)
use commondata
implicit none
real, intent(in) :: t
real, intent(in), dimension(1:neq) :: y
real, intent(out), dimension(1:neq) :: ydot
real, parameter :: lambda=-1.
ydot(1) = t*y(1)
end subroutine fcn
end module rhs
!
!-------------------------------------------------------------------------
!
module ode1module
! The subroutine in this module is a generic algorithm for
! the first order explicit Euler method
! for solving the system of neq ordinary
! differential equations evaluated by
! subroutine fcn.
contains
subroutine ode1(y,t,tout)
use commondata
use rhs
implicit none
integer :: i
real, intent(inout), dimension(1:neq) :: y
real, intent(inout) :: t
real, intent(in) :: tout
real, dimension(1:neq) :: ydot
real :: dt
dt = tout-t
call fcn(t,y,ydot) !estimate ydot at t
do i=1,neq
y(i) = y(i)+dt*ydot(i) !estimate y at t
enddo
t = tout !update t
end subroutine ode1
end module ode1module
!
!-------------------------------------------------------------------------
!
module ode2module
! The subroutine in this module is a generic algorithm for
! the second order Runge-Kutta method for
! solving the system of neq ordinary differential
! equations evaluated by subroutine fcn.
contains
subroutine ode2(y,t,tout)
use commondata
use rhs
implicit none
integer :: i
real, intent(inout), dimension(1:neq) :: y
real, dimension(1:neq) :: yi,ydot
real, intent(inout) :: t
real, intent(in) :: tout
real :: dt
dt = tout-t
call fcn(t,y,ydot) !estimate ydot at t
do i=1,neq
!yi(i) = y(i)+dt*ydot(i)/2. !estimate y at t+dt/2
enddo
!call fcn(t+dt/2.,yi,ydot) !estimate ydot at t+dt/2
do i=1,neq
y(i) = y(i) + dt*ydot(i) !estimate y at t
enddo
t = tout !update t
end subroutine ode2
end module ode2module
!
!-------------------------------------------------------------------------
!
program solveode
! This is the main driver program for solving
! a generic set of neq differential equations.
use commondata
use ode1module
use ode2module
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
call ode2(y,t,tout) !update y and t
print*,t,y !output the results
write(10,*)t,y
enddo
print*,'error = ',y(1) - exp(-t)
end program solveode









