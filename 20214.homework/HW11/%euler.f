      program euler

      real x(2),t,dt,tfinish,f1,f2
      integer i,steps 

      x(1)=0.
      x(2)=1.
      dt = 0.02
      tfinish=5.
      steps=tfinish/dt
      open(unit=13,file="data.d")
      
      do 10 i=1,steps
         write(13,*) t,x(1),x(2)
         x(1)=x(1)+x(2)*dt
         x(2)=x(2)+(-4./1.*x(1))*dt
         t=t+dt
 10   continue

      write(13,*) t,x(1),x(2)

      close(13)

      stop
      end

      real function f1(x,t)
      real x(2),t
      f1 = x(2)
      return 
      end

      real function f2(x,t)
      real x(2),t
      f2=(-4./1.*x(1))
      return 
      end



