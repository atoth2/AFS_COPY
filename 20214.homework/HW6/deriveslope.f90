program deriveslope              ! beginning of the program

implicit none                     ! force all variables to be declare
integer :: N, i, j, k, l       
real, dimension(:), allocatable :: f,t, f1, f2, f12, f22, f13, f23
real, dimension(:,:), allocatable :: fmat, Tfmat
real, parameter ::  tstop= 1.
real :: dt, error1, error2
open(unit=20,file = 'exactslope.out')
open(unit=19,file = 'matmulslope.out')
open(unit=18,file = 'matrixslope.out')
print*, 'Enter Number of Steps'
read*, N
dt = 2./N

allocate(t(N+1))                  ! Allocate all variables
allocate(f(N+1))
allocate(f1(N+1))
allocate(f2(N+1))
allocate(f12(N+1))
allocate(f22(N+1))
allocate(f13(N+1))
allocate(f23(N+1))
allocate(fmat(N+1,N+1))
allocate(Tfmat(N+1,N+1))

f1(1) = exp(-1.)                     ! Find exact solution
f2(1) = 2*exp(-1.)

k = 1.
t(k) = -1.                           ! set initial time                         
do while (t(k)<tstop+dt)             ! main loop for calculation
   f(k) = exp(2*t(k))
   f12(k) = 2*exp(2*t(k))
   f22(k) = 4*exp(2*t(k))
   write(20,*) t(k),f(k),f12(k),f22(k)
   k = k+1                          ! update the counter
   t(k) = -1 + dt*(k-1)             ! update time  
end do                               ! end the loop

do i = 1,N+1                         ! initialize fmat matrix with 0s
        do j = 1,N+1
        fmat(i,j)=0
        end do
end do

fmat(1,1) = -3                        ! initialize fmat initial and final row
fmat(1,2) = 4
fmat(1,3) = -1
fmat(N+1,N-1) = 1
fmat(N+1,N) = -4
fmat(N+1,N+1) = 3
 
do i = 1,N-1                          ! initialize fmat diagonals        
        fmat(i+1,i) = -1
        fmat(i+1,i+2) = 1
end do

fmat = fmat/(2.*dt)                   ! make D matrix     

f13 = matmul(fmat,f)                  ! make matmul approximation
f23 = matmul(fmat,f13)              

do i=1,N+1                            ! make matrix-vector approximation
  do j=1,N+1
    f1(i) = f1(i) + fmat(i,j)*f(j)    ! y' approximation
    do l=1,N+1
      f2(i) = f2(i) + fmat(i,j)*fmat(j,l)*f(l)  ! y'' approximation
    end do  
  end do       
  write(19,*) f13(i), f23(i)
  write(18,*) f1(i), f2(i)
end do

i = N/2.+1.
error1 = f2(i)-4
print*, error1

end program deriveslope            ! end of program
