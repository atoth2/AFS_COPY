gfortran -c myfunction.f90
gfortran -c taylorflex.f90
gfortran -c main.f90
gfortran myfunction.o taylorflex.o main.o
a.out

