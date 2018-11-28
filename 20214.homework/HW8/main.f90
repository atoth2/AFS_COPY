program taylor
use myfunction
use taylorflex

implicit none
integer :: i, k, N, m
real, dimension(:), allocatable :: ft,t,f1
real :: x, dt, xmin, xmax, error

open(unit=20,file = 'input.txt')  ! open readable/writable files
open(unit=19,file = 'outputtay.txt')

read(20,100) xmin
read(20,100) xmax
read(20,101) m
read(20,101) N 
100 format(18x,f3.1)              ! format file reading
101 format(18x,i5)

allocate(t(N+1))                  ! allocate variables
allocate(ft(N+1))
allocate(f1(N+1))

dt = (xmax-xmin)/N
k=1.
t(k) = xmin                       ! set initial time

do while (t(k)<xmax)              ! main loop for calculation
   ft(k) = 0.
   x = t(k)
   ft(k) = tay(x,m)               ! call on taylor approximation function
   f1(k) = exp(-2.*t(k))
   write(19,*) t(k),ft(k),f1(k)
   k = k+1                        ! update the counter
   t(k) = xmin + dt*(k-1)         ! update time  
end do                            ! end the loop
error = ft(k-1) - f1(k-1)
print*, error

end program taylor