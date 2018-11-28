%% Set-up
clear all; close all; hold off;
rng(901859386);     % put your NDID number inside the rng function
min = -2; max = 1;  % min and max wave numbers: bumps from 10cm to 100m
N = 61;             % how many terms to include in road surface function
roadlength = 1000;  % 1km track
wavenos = logspace(min,max,N);
phi = unifrnd(0,2*pi,1,N);      % individualized phase shifts

%%% change sk0 to change quality of the road
%sk0 = (2+8)/2/10^6;     % very good road
%sk0 = (8+32)/2/10^6;    % good road
%sk0 = (32+128)/2/10^6;  % average road
%sk0 = (128+512)/2/10^6; % poor road
sk0 = (512+2048)/2/10^6;% very poor road

%vmax = [400 150 80 40 20];

x = linspace(0,roadlength,10000);

%% Project
m = 250;                                %mass of car, kg
b = 5370;                               %damper, N s/m
k = 6000*9.81;                          %spring, N/m
vk = 20;                       %velocity, km/hr to m/s
v = vk/3.6;

h = y(sk0,wavenos,phi,x);               %height of road, m
dhdx = dydx(sk0,wavenos,phi,x)*v;         %height velocity

timetodrive1km = roadlength/v;                   %timetodrive1km, hr/km
icz = h(1);                                %initial condition of z
iczdot = dhdx(1);                             %initial condition of z'

x = linspace(0,roadlength,100000);
h=y(sk0,wavenos,phi,x);
dhdx=dydx(sk0,wavenos,phi,x)*v;
[t z] = ode45(@(t,x) carrhs(t,x,sk0,wavenos,phi,v,m,b,k) ,[0 timetodrive1km], [icz iczdot]);
%%
figure(1)
plot(x,y(sk0,wavenos,phi,x),'k')


%% height of wheel vs x

figure(2)
plot(t*v,z(:,1),'k')
xlabel('x');
ylabel('h');
title('Road height');

%% Compression vs x BADD
%x = linspace(0,roadlength,length(z));
h=y(sk0,wavenos,phi,t*v);
compress = z(:,1) - h;

figure(3)
plot(t*v,compress,t*v,-.04+t*0,t*v,.04+t*0);
xlabel('x');
ylabel('h-y');
title('Compression');

%% acceleration calculation vs x
x11 = gradient(z(:,2),x(2)-x(1));
%x11 = diff(z(:,2))./t(2:length(t));
figure(4)
hold on
plot(t*v,x11,t*v,-9.81+t*0);
xlabel('x');
ylabel('d2hdx2');
title('Acceleration');
%axis([700 900 -10 10])