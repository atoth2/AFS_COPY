      program ode22

      real :: x,t,dt,tfinish
      integer :: i,steps 

      x = 0.
t = 0.
      dt = 0.2
      tfinish=5
      steps=tfinish/dt
      open(unit=13,file="dataRK2.d")
      
      do i=1,steps
         print*, t,x
         write(13,*) t,x
         k1 = f(x,t)*dt
         k2 = f(x+k1/2.0,t+dt/2.0)*dt
         k3 = f(x+k2/2.0,t+dt/2.0)*dt
         k4 = f(x+k3,t+dt)*dt
         x = x + (k1 + 2.0*k2 + 2.0*k3 + k4)/6.0
         t=t+dt
      end do

      write(13,*) t,x

contains
real function f(x,t)
implicit none
real,intent(in) :: x,t
f = t*x
end function f

      end program ode22









