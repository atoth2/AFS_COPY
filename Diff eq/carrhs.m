function zdot = carrhs(t,z,sk0,wavenos,phi,v,m,b,k)
x = v*t;
h = y(sk0,wavenos,phi,x);
dhdx = dydx(sk0,wavenos,phi,x)*v; %%
zdot = zeros(2,1);
zdot(1) = z(2);
zdot(2) = 1/m*( b*(dhdx - z(2)) + k*(h - z(1)));
end