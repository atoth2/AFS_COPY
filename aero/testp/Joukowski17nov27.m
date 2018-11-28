%Joukowski transformation structure
clear all
close all
set(0,'defaultAxesFontSize',20)
%Parameters
axis equal
lam=0.2468;                      %Related with the size of the foil
e=0.0124;                        %Related with the thickness of the foil
s=0.0124;                      %Related the roundness and also contibutes the thickness of the foil

a=2*(lam+e+s)+lam^2/(lam+s)+lam^2/(lam+2*e+s); %foil length
xp=0.2;                        %Xp*a is the distance between leading edge of the foil and the shear center (SC, pitching axis) 

d=(lam+2*e+s)+lam*lam/(lam+2*e+s)-(xp*a); %Distance between the shear center (SC, pitching axis)  and CG
b=-d;

R=lam+e+s;                  %Radius of the circle where is mapping the airfoil
theta=0:0.01:2*pi;          %Theta Axis array
f1=R*exp(i*theta);          %Circle
plot(real(f1),imag(f1),'b','LineWidth',4) %Plot circle
f2=f1+lam*lam./(f1-e)+d-e;  %Joukowski transformation to mapping the airfoil
hold on
plot(real(f2),imag(f2),'k','LineWidth',4) %Plot airfoil
plot(real(d),imag(d),'ro-','LineWidth',4) %Plot position of d
plot(-real(xp),imag(xp),'go-','LineWidth',4) %Plot position of b
plot(0,0,'co-','LineWidth',4) %Plot position of b
title('Joukowski Mapping of a Circle to a Foil')   
xlabel('X')
ylabel('Y' )
legend('Original circle before mapping','Joukowski foil after mappping','Center of Gravity/CG (d,0)','Leading Edge (-xp*a,0)','Shear Center/Pitching Axis (0,0)')

