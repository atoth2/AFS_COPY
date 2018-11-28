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
