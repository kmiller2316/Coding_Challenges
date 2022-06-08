output = load('problem1.1');

time = output(:,1);
theta = output(:,2);
x = output(:,3);

figure(1)
plot(time,theta)
legend('Theta')
title('Theta Over Time');

figure(2)
plot(time,x)
legend('X')
title('X Over Time');

figure(3)
plot(time,x,'-',time,theta,'--')
legend('X','Theta')
title('Theta and X wrt Time');