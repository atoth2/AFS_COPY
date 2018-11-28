ifort -c myfunction.f90
ifort -c taylorflex.f90
ifort -c main.f90
ifort -c myfunction.o taylorflex.o main.o
a.out


