program therm2        ! beginning of the program

implicit none                     ! force all variables to be declare
integer :: n  
real, parameter :: initialt = -50.,finalt = 100.           
real :: tc,tk,tf,tr,tstep
tc = initialt                        ! set initial time
print*, 'temperature step =' 
read*, tstep
   tk = tc + 273.15
   tr = tk*9./5.
   tf = tr - 459.67

do while (tc<finalt)                ! main loop for calculation
   print*, tc,tk,tf,tr            ! print the value   
   tc = tc + tstep
   tk = tc + 273.15
   tr = tk*9./5.
   tf = tr - 459.67
   enddo                          ! end the loop
   print*, tc,tk,tf,tr            ! print the value   
end program therm2                ! end of program








