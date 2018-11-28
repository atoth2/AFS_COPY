step =[50 100 200 400 800 1600];
    dt = 1./step;
erroreuler=[-2.16244459E-02  -1.09006166E-02 -5.47194481E-03 -2.74372101E-03 -1.37293339E-03 -6.85811043E-04];
error2RK=[-1.08838081E-04 -2.62260437E-05 -7.03334808E-06 -3.57627869E-07 4.76837158E-07 8.34465027E-07];
step4 =[1 2 3 4 5 10 20];
    dt4 = 1./step4;
error4RK=[-2.88796425E-03 -1.93357468E-04 -3.75509262E-05 -1.15633011E-05 -4.64916229E-06 -1.19209290E-07 2.38418579E-07];
figure(1)
plot(dt,erroreuler,'x')
xlabel('dt')
ylabel('error')
legend('euler')

figure(2)
plot(dt,error2RK,'x')
xlabel('dt')
ylabel('error')
legend('2nd order RK')

figure(3)
plot(dt4,error4RK,'x')
xlabel('dt')
ylabel('error')
legend('4th order RK')