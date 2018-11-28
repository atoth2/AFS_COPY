program sine ! beginning of the program
!
! Alexander Toth, AME 20214, August 2016
!
! this program generates points on a cubic
!
implicit none ! force all variables to be declared
integer :: i ! declare i to be an integer
integer, parameter :: n = 20 ! give the number of points
real :: x(n),y(n) ! declare the arrays x and y to be of length n and real
real :: dx ! declare dx to be real
real, parameter :: xmin =0.,xmax=6. ! set values for xmin andxmax
open(unit=20,file='sin.out') ! open an output file
dx = (xmax-xmin)/(n-1.) ! calculate dx
do i=1,n ! do loop
	x(i) = xmin + dx*(i-1.) ! calculate x
	y(i) = sin(x(i)) ! calculate y
	print 100,x(i),y(i) ! print x and y to the screen
	write(20,100) x(i),y(i) ! write x and y to ’sine.out’
	enddo ! end of the do loop
100 format(f10.6,2x,f10.6) ! format the output
end program sine ! end of program
