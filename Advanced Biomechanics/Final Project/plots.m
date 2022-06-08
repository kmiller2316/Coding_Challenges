output1 = load('kneemodel_inverse.6');
output2 = load('kneemodel_inverse.7');
output3 = load('kneemodel_inverse.8');
output4 = load('kneemodel_inverse.9');
output5 = load('kneemodel_inverse.10');
output6 = load('kneemodel_inverse.11');
output7 = load('kneemodel_inverse.12');
output8 = load('kneemodel_inverse.13');
output9 = load('kneemodel_inverse.14');
output10 = load('kneemodel_inverse.15');
output11 = load('kneemodel_inverse.16');

bw = 867.204; % body weight in N
h = 1.77; % height in M
CONRTOD=180/3.1415927;
flexion = output9(:,3)*CONRTOD;

% Kinetics

time = output1(:,1);
quadforce = output1(:,2)/bw;
patligforce = output1(:,3)/bw;

figure(1)
plot(flexion,quadforce,"-",flexion,patligforce,"--")
legend("Quadriceps Force","Patellar Ligament Force")
xlabel("Flexion (Degrees)")
ylabel("Normalized Force (N/BW)")
title("Quad & Patlig Force by Flexion");

%%%%

time = output2(:,1);
knee = output2(:,2)/bw;
pat = output2(:,3)/bw;

figure(2)
plot(flexion,knee,"-",flexion,pat,"--")
legend("Tibiofemoral Force","Patellofemoral Force")
xlabel("Flexion (Degrees)")
ylabel("Normalized Force (N/BW)")
title("Knee and Patellofemoral Forces by Flexion");

%%%%

time = output3(:,1);
vm = output3(:,2)/bw;
vl = output3(:,3)/bw;
vi = output3(:,4)/bw;
rf = output3(:,5)/bw;

figure(3)
plot(flexion,vm,"-",flexion,vl,"--",flexion,vi,"+",flexion,rf,"x")
legend("Vastus Medialis","Vastus Lateralis","Vastus Intermedius","Rectus Femoris")
xlabel("Flexion (Degrees)")
ylabel("Normalized Force (N/BW)")
title("Quadricep Fiber Forces by Flexion");

%%%%

time = output4(:,1);
lcl = output4(:,2)/bw;
mcl = output4(:,3)/bw;
pcl = output4(:,4)/bw;

figure(4)
plot(flexion,mcl,"-",flexion,lcl,"--",flexion,pcl,"+")
legend("MCL","LCL","PCL")
xlabel("Flexion (Degrees)")
ylabel("Normalized Force (N/BW)")
title("Ligament Forces by Flexion");

%%%%

time = output5(:,1);
foot = output5(:,2);
tib = output5(:,3);
fem = output5(:,4);
pel = output5(:,5);
pat = output5(:,6);

figure(5)
plot(flexion,foot,"-",flexion,tib,"--",flexion,fem,"+",flexion,pel,"x",flexion,pat,"o")
legend("Foot","Tibia","Femur","Pelvis","Patella")
xlabel("Flexion (Degrees)")
ylabel("Torque (N-m)")
title("Body Torques about the 1> Axis by Flexion");

%%%%

time = output6(:,1);
foot = output6(:,2);
tib = output6(:,3);
fem = output6(:,4);
pel = output6(:,5);
pat = output6(:,6);

figure(6)
plot(flexion,foot,"-",flexion,tib,"--",flexion,fem,"+",flexion,pel,"x",flexion,pat,"o")
legend("Foot","Tibia","Femur","Pelvis","Patella")
xlabel("Flexion (Degrees)")
ylabel("Torque (N-m)")
title("Body Torques about the 2> Axis by Flexion");

%%%%

time = output7(:,1);
foot = output7(:,2);
tib = output7(:,3);
fem = output7(:,4);
pel = output7(:,5);
pat = output7(:,6);

figure(7)
plot(flexion,foot,"-",flexion,tib,"--",flexion,fem,"+",flexion,pel,"x",flexion,pat,"o")
legend("Foot","Tibia","Femur","Pelvis","Patella")
xlabel("Flexion (Degrees)")
ylabel("Torque (N-m)")
title("Body Torques about the 3> Axis by Flexion");

%%%%

% Kinematics

time = output8(:,1);
grf1 = output8(:,2)/bw;
grf2 = output8(:,3)/bw;
grf3 = output8(:,4)/bw;

figure(8)
plot(time,grf1,"-",time,grf2,"--",time,grf3,"+")
legend("GRF 1>","GRF 2>","GRF 3>")
xlabel("Time")
ylabel("Normalized Force (N/BW)")
title("Ground Reaction Forces Over Time");

%%%%

time = output9(:,1);
tib = output9(:,2)*CONRTOD;
fem = output9(:,3)*CONRTOD;
pat = output9(:,4)*CONRTOD;

figure(9)
plot(time,tib,"-",time,fem,"--",time,pat,"+")
legend("Theta Tibia","Theta Femur","Theta Patella")
xlabel("Time")
ylabel("Degrees")
title("Changes in Angles Over Time");

%%%%

time = output10(:,1);
tib1 = output10(:,2)/h;
tib2 = output10(:,3)/h;
tib3 = output10(:,4)/h;

figure(10)
plot(time,tib1,"-",time,tib2,"--",time,tib3,"+")
legend("Tibia to Femur about 1> Axis","Tibia to Femur about 2> Axis","Tibia to Femur about 3> Axis")
xlabel("Time")
ylabel("Normalized Distance (M/h)")
title("Change in Femorotibial Contacts Over Time");

%%%%

time = output11(:,1);
fem1 = output11(:,2)/h;
fem2 = output11(:,3)/h;
fem3 = output11(:,4)/h;

figure(11)
plot(time,fem1,"-",time,fem2,"--",time,fem3,"+")
legend("Femur to Patella about 1> Axis","Femur to Patella about 2> Axis","Femur to Patella about 3> Axis")
xlabel("Time")
ylabel("Normalized Distance (M/h)")
title("Change in Patellofemoral Contacts Over Time");