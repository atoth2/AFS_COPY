program taylor                    ! beginning of the program

implicit none                     ! force all variables to be declare
integer :: k, N
real, dimension(:), allocatable :: f,t,f1
real :: dt, xmin, xmax

open(unit=20,file = 'input.txt')  ! open readable/writable files
open(unit=19,file = 'output.txt')

read(20,100) xmin
read(20,100) xmax
read(20,101) N 
100 format(18x,f3.1)              ! format file reading
101 format(18x,i5)

allocate(t(N+1))                  ! allocate variables
allocate(f(N+1))
allocate(f1(N+1))

dt = (xmax-xmin)/N
k=1.
t(k) = xmin                       ! set initial time
              
do while (t(k)<xmax+dt)              ! main loop for calculation
   f(k) = 1 - 2.*t(k) + 2.*t(k)**2. - 4./3.*t(k)**3 + 2./3.*t(k)**4
   f1(k) = exp(-2.*t(k))
   write(19,*) t(k),f(k),f1(k)
   k = k+1                        ! update the counter
   t(k) = xmin + dt*(k-1)         ! update time  
end do                            ! end the loop

end program taylor                ! end of program









