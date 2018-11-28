close all; hold off; clear
rng(901859386);     % put your NDID number inside the rng function
min1 = -2; max1 = 1;  % min and max wave numbers: bumps from 10cm to 100m
N = 61;             % how many terms to include in road surface function
roadlength = 1000;  % 1km track
wavenos = logspace(min1,max1,N);
phi = unifrnd(0,2*pi,1,N);      % individualized phase shifts
k = 6000*9.81;
m = 250;
b = 5370;
vkpr=60; %km/h
v = vkpr/3.6; %m/s

k = 12000*9.81
b = 2*(0.7*2*sqrt(k*m))

%%% change sk0 to change quality of the road
sk0 = (2+8)/2/10^6;     % very good road
%sk0 = (8+32)/2/10^6;    % good road
%sk0 = (32+128)/2/10^6;  % average road
%sk0 = (128+512)/2/10^6; % poor road
%sk0 = (512+2048)/2/10^6; % very poor road

x = linspace(0,roadlength,15000);

%plot(x, dydx(sk0,wavenos,phi,x));
% xlabel('x');
% ylabel('y');
% title('Road Profile');
h=y(sk0,wavenos,phi,x);
dhdx=dydx(sk0,wavenos,phi,x);
% racc=diff(dydx(sk0,wavenos,phi,x));
[t z] = ode45(@(t,z) carrhs(t,z,sk0,wavenos,phi,v,m,b,k) ,[0 1000/v], [h(1) dhdx(1)]);
x=v*t;
h=y(sk0,wavenos,phi,x);
dhdx=dydx(sk0,wavenos,phi,x);
rideheights=z(:,1);
acc=1/m*(b*(dhdx-z(:,2))+k*(h-z(:,1)));
figure(1) %%%%%%%%%%%%%%%%%%%%%%
% plot(x,y(sk0,wavenos,phi,x));
% hold on
plot(x,rideheights(1:length(x)))
title('Ride Height')
xlabel('x (meters)')
ylabel('h (meters)')
figure(2) %%%%%%%%%%%%%%%%%%%%%%%%
compression=rideheights-h;
plot(x,compression,'k')
xlabel('x (meters)')
ylabel('Compression (meters)')
title('Compression')
figure(3) %%%%%%%%%%%%%%%%%%%%%%%%%
plot(x,acc)
title('Net Acceleration of the Vehicle')
xlabel('x (meters)')
ylabel('m/s^2')

subplot(1,3,1);
plot(x,rideheights(1:length(x)))
title('Ride Height')
xlabel('x (meters)')
ylabel('h (meters)')
subplot(1,3,2);
compression=rideheights-h;
plot(x,compression,'k')
xlabel('x (meters)')
ylabel('Compression (meters)')
title('Compression')
subplot(1,3,3);
plot(x,acc)
title('Net Acceleration of the Vehicle')
xlabel('x (meters)')
ylabel('m/s^2')


maxforce = max(abs(acc))*m % N