ifort -c commondata.f90
ifort -c rhs.f90
ifort -c ode4.f90
ifort -c solveode.f90
ifort commondata.o rhs.o ode4.o solveode.o
a.out



