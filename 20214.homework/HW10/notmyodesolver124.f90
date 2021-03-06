!-------------------------------------------------------------------------
!
module commondata
integer, parameter :: neq=3 !set the number of equations
integer, parameter :: nt=1000000 !set the number of time steps ! Changed !!!!!!!
real, parameter :: tstop = 200. !set the stop time
real, dimension(1:neq) :: yic=(/1.,0.,0./) !set the initial conditions ! Changed !!!!
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
real, parameter :: beta=-1.,delta=0.22,alpha=1.,f=0.3
ydot(1) = y(2)
ydot(2) = -beta*y(1)-delta*y(2)-alpha*y(1)**3+f*cos(y(3))
ydot(3) = 1.

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
yi(i) = y(i)+dt*ydot(i)/2. !estimate y at t+dt/2
enddo
call fcn(t+dt/2.,yi,ydot) !estimate ydot at t+dt/2
do i=1,neq
y(i) = y(i) + dt*ydot(i) !estimate y at t
enddo
t = tout !update t
end subroutine ode2
end module ode2module
!
!-------------------------------------------------------------------------
!
module ode4module
! The subroutine in this module is a generic algorithm for
! the fourth order Runge-Kutta method for
! solving the system of neq ordinary differential
! equations evaluated by subroutine fcn.
contains
subroutine ode4(y,t,tout)
use commondata
use rhs
implicit none
integer :: i
real, intent(inout), dimension(1:neq) :: y
real, dimension(1:neq) :: yi,ydot
real, intent(inout) :: t
real, intent(in) :: tout
real :: dt, k1,k2,k3,k4
dt = tout-t


do i=1,neq
         call fcn(t,y,ydot) !estimate ydot at t
         k1 = ydot(i)*dt

         call fcn(t+0.5*dt,y+0.5*k1,ydot)
         k2 = ydot(i)*dt

         call fcn(t+0.5*dt,y+0.5*k2,ydot)
         k3 = ydot(i)*dt

         call fcn(t+dt,y+k3,ydot)
         k4 = ydot(i)*dt
         y(i) = y(i) + (k1 + 2.0*k2 + 2.0*k3 + k4)/6.0
enddo

t = tout !update t
end subroutine ode4
end module ode4module
!
!-------------------------------------------------------------------------
!
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

!print*,t,y !output the results
write(10,*)t,y
enddo
print*, y(1), 'error = ',y(1) - exp(-t),dt  ! Changed !!!!!!!!!!!!!!!!!
!y(1) - exp(t**2./2),dt Problem2
!y(1) - exp(-t) original error
end program solveode







