      program rk4
      implicit none

      double precision xeuler,t,dt,xexact,f,xrk,k(4)
      open(13,file="data.d")
      dt = 0.05
      xeuler = 0
      xrk = 1

      do 10 t=0,5,dt
         xexact = -exp(-t)+1
         write(13,11) t,xeuler,xexact,xeuler-xexact,xrk,xrk-xexact
         xeuler = xeuler + f(xeuler,t)*dt
         k(1) = f(xrk, t)*dt
         k(2) = f(xrk+0.5*k(1), t+0.5*dt)*dt
         k(3) = f(xrk+0.5*k(2), t+0.5*dt)*dt
         k(4) = f(xrk+k(3), t+dt)*dt
         xrk = xrk + (k(1) + 2.0*k(2) + 2.0*k(3) + k(4))/6.0
 10   continue
 11   format(6(f15.7))
      stop 
      end


      double precision function f(x,t)
      double precision x,t
      f = t*x
      return
      end


