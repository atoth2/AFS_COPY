      program ode22

      real :: x,t,dt,tfinish,xe
      integer :: i,steps 

      x = 1.
      dt = 0.05
      tfinish=5
      steps=tfinish/dt
      open(unit=13,file="dataRK2.d")
      
      do i=1,steps
         xe= exp(1./2.*t**2)
         print*, t,x,xe
         write(13,*) t,x,xe
         x = x + dt/2.0*(f(x,t) + f(x+f(x,t)*dt,t+dt))
         t=t+dt
      end do

      write(13,*) t,x,xe

contains
real function f(x,t)
implicit none
real,intent(in) :: x,t
f = x*t
end function f

      end program ode22


