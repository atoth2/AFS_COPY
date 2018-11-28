step =[50 100 200 400 800 1600];
    dt = 1./step;
error=[
figure(1)
plot(dt,error,'x')
xlabel('dt')
ylabel('error')
legend('error in RK4')