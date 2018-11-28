program main 
use taylor8
implicit none
integer :: m,i,numpoints
real (kind=4) :: xmin, xmax,y,x,yexact,dx
open(12,file='input8.txt')
read(12,100) xmin
read(12,100) xmax
read(12,101) m
read(12,101) numpoints
100 format(20x,f10.5)     !format input for reals
101 format(20x,i10)	  !format input for ints
!loop through x, feed into taylor function
dx=(xmax-xmin)/numpoints
x=0.
do i=0,numpoints
   x=dx*i
   yexact=log(x)
   y=expand(x,m)
   write(15,10) x,y,yexact,yexact-y  
   10 format (e10.4,3x,e10.4,3x,e10.4,3x,e10.4,3x,e10.4,3x,e10.4)
   print*, x, y, yexact,yexact-y 
end do
end program main
	
