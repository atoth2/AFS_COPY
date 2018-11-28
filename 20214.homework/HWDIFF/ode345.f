c     Program that numerically solves x'+x=1 using Euler's method and the
c     fourth order Runge-Kutta method. 

c     To comple and run it, type 'pgf77 rk4.f' on a unix machine, and
c     then type './a.out' to execute it. The data will be stored in a
c     file called 'data.d'.

      program rk4
      implicit none

      double precision xeuler,t,dt,xexact,f,xrk,k(4)
      open(13,file="data.d")
      dt = 0.02
      xeuler = 0
      xrk = 1./(1. + exp(40.))

      do 10 t=-1,10,dt
         xexact = 1./(1. + exp(-40.*t))
         write(13,11) t,xrk,xrk-xexact
         xeuler = xeuler + f(xeuler,t)*dt
         k(1) = f(xrk, t)*dt
         k(2) = f(xrk+0.5*k(1), t+0.5*dt)*dt
         k(3) = f(xrk+0.5*k(2), t+0.5*dt)*dt
         k(4) = f(xrk+k(3), t+dt)*dt
         xrk = xrk + (k(1) + 2.0*k(2) + 2.0*k(3) + k(4))/6.0
 10   continue
 11   format(6(f15.7))
      print*, t,xrk,xrk-xexact
      stop 
      end


      double precision function f(x,t)
      double precision x,t
      f = 40*x*(1 - x)
      return
      end

