      program euler

      real :: x(2),t,dt,tmax,f1,f2
      integer :: i,n 

      x(1)= 0.
      x(2)= 1.
      dt = 0.02
      tmax = 5.
      n = tmax/dt
      open(15,file="data.d")
      
      do 18 i=1,n
         write(15,*) t,x(1), 1
         x(1)=x(1)+x(2)*dt
         x(2)=x(2)+(-4./1.*x(1))*dt
         t=t+dt
   18 continue

      write(15,*) t, x(1), 1./2.*sin(10.) - x(1)
      close(15)

      stop
      end

