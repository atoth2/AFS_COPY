module taylor8 	                   !function to calc taylor series approx.
contains
real function expand(x,m)
implicit none
real, intent(in) :: x
integer, intent(in)::m
integer::j
real (kind=4)::y,expand
open(15,file='output8.txt')
do j=1,m                          !Represents an m term Taylor series
      y=(-1)**(j+1)*((x-1)**j)/j  !Evaluate each x val using taylor series
      expand=expand+y             !Sum each individual term of taylor
end do
end function expand
end module taylor8



