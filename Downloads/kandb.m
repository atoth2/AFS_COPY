close all; hold off; clear
rng(901859386);     % put your NDID number inside the rng function
min1 = -2; max1 = 1;  % min and max wave numbers: bumps from 10cm to 100m
N = 61;             % how many terms to include in road surface function
roadlength = 1000;  % 1km track
wavenos = logspace(min1,max1,N);
phi = unifrnd(0,2*pi,1,N);      % individualized phase shifts
k = 6000*9.81; %N/m
m = 250; %kg
b = 5370; %N s/m
vkpr=0; %km/h
v = vkpr/3.6; %m/s

compression = 0;
acc = 0;

k = 1000*9.81
b = 0.1*(0.7*2*sqrt(k*m))

lmn = 0; %counter

for i = 1:11
    for j = 1:10
        vkpr=10; %km/h
        v = vkpr/3.6; %m/s
        compression = 0;
        acc = 0;
        while max(abs(compression)) < .05 && max(abs(acc)) < 9.81*2 && vkpr < 150    %150
            %%% change sk0 to change quality of the road
            %sk0 = (2+8)/2/10^6;     % very good road
            %sk0 = (8+32)/2/10^6;    % good road
            %sk0 = (32+128)/2/10^6;  % average road
            %sk0 = (128+512)/2/10^6; % poor road
            sk0 = (512+2048)/2/10^6; % very poor road
            
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
            compression=rideheights-h;
            
            vfinal = vkpr
            vkpr= vkpr + 10; %km/h
            v = vkpr/3.6; %m/s
            
            max(abs(compression));
            max(abs(acc));
            maxforce = max(abs(acc))*m; % N
            
            %% results
            
            compvect(i,j) = max(abs(compression));
            accvect(i,j) = max(abs(acc));
            kvector(i,j) = k;
            bvector(i,j)= b;
            maxfunction(i,j) = maxforce;
            velocity(i,j) = vfinal;
            
            lmn = lmn +1
        end
        b = b + 0.2*(0.7*2*sqrt(k*m));
    end
    k = 1000*9.81 + k;
    b = 0.1*(0.7*2*sqrt(k*m));
end

