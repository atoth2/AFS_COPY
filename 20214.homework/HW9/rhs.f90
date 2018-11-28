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
real, parameter :: lambda=1. ! Changed !!!!!!!!!!!!!!!!!
ydot(1) = -lambda*y(1) ! Changed !!!!!!!!!!!!!!!!!
end subroutine fcn
end module rhs
