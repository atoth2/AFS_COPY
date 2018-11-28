function dhdx = dydx(sk0,wavenos,phi,x)
    dhdx = 0; k0 = 1/(2*pi); n1 = -3; n2 = -2.25;
    coef1 = sqrt(2*pi/(length(wavenos)));
    for n=1:length(wavenos)
        wn = wavenos(n);
        coef2 = sqrt(sk0*(wn/k0)^n1*(wn<=k0) + sk0*(wn/k0)^n2*(wn>k0));
        dhdx = dhdx - 2*pi*wn*coef1*coef2*sin(2*pi*wn*x+phi(n));
    end
end