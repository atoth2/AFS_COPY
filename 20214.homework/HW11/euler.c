/*Name: euler.c
*Description: Approximates dy/dt = lambda*y; y(0) = y0
* with Euler’s method. Exact solution is
* y = y0*exp(lambda*t)
*/
//Include I/O and math libraries
#include <stdio.h>
#include <math.h>
//Define problem parameters
#define tstop 5.0f
#define y0 0.0f
#define v0 0.0f
#define dt 0.02f
int main ( void ){
FILE *output; //output file syntax
output = fopen("output.txt","w"); //open output file
float y=y0, v=v0, ynew, vnew, t = 0; //set initial time/value
int n = 0; //initialize counter
printf("%.8f\t%.8f\t%.8f\n", t, y); //print initial values
fprintf(output,"%.8f\t%.8f\t%.8f\n", t, y);//initial values
do{
ynew = y+v*dt; //approximate new solution
vnew = v+(-4./1.*y)*dt
n = n+1; //update counter
t = dt*n; //update time
y = ynew; //update y
v = vnew; //update v
printf("%.8f\t%.8f\t%.8f\n", t, y, y0*expf(lambda*t));//print results
fprintf(output,"%.8f\t%.8f\t%.8f\n", t, y, v);//write results
} while(t < tstop); //loop end condition
fclose(output); //close output file
return 0;
}
      real x(2),t,dt,tfinish,f1,f2
      integer i,steps 

      x(1)=0.
      x(2)=1.
      dt = 0.02
      tfinish=5.
      steps=tfinish/dt
      open(unit=13,file="output.out")
      
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



