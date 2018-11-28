#include<stdio.h>
#include<math.h>
int main() {
int step;
float x[2],t,dt,f[2];                   /* float variables */ 
FILE *output;                           /* designate output file */ 
output = fopen("output.out","w");       /* save file designation */ 
step = 0;                               /* step intialization*/ 
dt = 0.02;                              /* step size*/ 
x[0] = 0;                               /* vector for y*/ 
x[1] = 1;                               /* vector for v*/ 
for(t=0;t<5+dt;t+=dt) {
f[0] = x[1];                            /* function for x'*/ 
f[1] = -4.* x[0];                       /* function for x''*/ 
fprintf(output,"%f\t%d\t%f\t ",t,step,x[0]);
x[0] += f[0]*dt;
x[1] += f[1]*dt;
n++;
}
fclose(output);
return 0;
}

