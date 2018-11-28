!-------------------------------------------------------------------------
!
module commondata
integer, parameter :: neq=1 !set the number of equations
integer, parameter :: nt=10 !set the number of time steps ! Changed !!!!!!!
real, parameter :: tstop = 1. !set the stop time
real, dimension(1:neq) :: yic=(/1./) !set the initial conditions ! Changed !!!!
end module commondata
