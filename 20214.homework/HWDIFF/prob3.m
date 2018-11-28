dt = [.09 .08 .07 .05];
error=[-0.573337588 -0.311927287 -0.129237955 -1.11022302E-16]

figure(1)
plot(dt,error,'x')
xlabel('dt')
ylabel('error')
legend('error in RK4')