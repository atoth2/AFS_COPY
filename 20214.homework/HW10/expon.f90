real function sina(x)
implicit none
real, intent(in) :: x
sina = x - x**3/6. + x**5/120. - x**7/5040. + x**9/362880. - x**11/39916800. + x**13/6227020800.
end function sina

