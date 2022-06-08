% left knee model
% assuming hinge joints at each joint
% 1> is +anterior/-posterior
% 2> is +superior/-inferior
% 3> is +medial/-lateral

% step 1
% three bodies (foot, tibia, femur)
autoz on
newtonian n
bodies foot, tibia, femur
constants g

points contactGroundFoot, contactFootGround
points contactFootTibia, contactTibiaFoot
points contactTibiaFemur, contactFemurTibia
points contactFemurNewtonian, contactNewtonianFemur % point at the top of the Femur to allow us to measure hip forces without the hip being modeled
points tibiaShin % kick point

constants contactFootGround2FootO{3}, FootO2contactFootTibia{3} 
constants contactTibiaFoot2TibiaO{3}, TibiaO2contactFemurTibia{3}
constants contactFemurTibia2FemurO{3}, FemurO2contactFemurNewtonian{3}
constants contactTibiaShin{3}

constants footMass, tibiaMass, femurMass
constants footI{3}, tibiaI{3}, femurI{3}

specified thetaFoot{3}''
specified thetaTibia{3}''
specified thetaFemur{3}''
specified kickForce{3}''
specified GRFforce{3}''

% step 2
variables U{18}' % single differentiable because of velocity

variables ankleForce{3}
variables kneeForce{3}
variables hipForce{3}

variables footTorque{3}
variables tibiaTorque{3}
variables femurTorque{3}

% step 3
dircos(n, foot, body321, thetaFoot3, thetaFoot2, thetaFoot1)
dircos(n, tibia, body321, thetaTibia3, thetaTibia2, thetaTibia1)
dircos(n, femur, body321, thetaFemur3, thetaFemur2, thetaFemur1)

angvel(n, foot)
angvel(n, tibia)
angvel(n, femur)

w_foot_n> := w_foot_n> + U1*N1> + U2*N2> + U3*N3>
w_tibia_n> := w_tibia_n> + U7*N1> + U8*N2> + U9*N3>
w_femur_n> := w_femur_n> + U13*N1> + U14*N2> + U15*N3>

p_no_contactGroundFoot> = 0> % if contact point is assumed to be stationary
p_contactGroundFoot_contactFootGround> = 0>

% points on Foot
p_contactFootGround_footO> = contactFootGround2FootO1*foot1> + contactFootGround2FootO2*foot2> +contactFootGround2FootO3*foot3>

p_footO_contactFootTibia> = FootO2contactFootTibia1*foot1> + FootO2contactFootTibia2*foot2> + FootO2contactFootTibia3*foot3>

p_contactFootTibia_contactTibiaFoot> = 0> % coincident points

% points on tibia
p_contactTibiaFoot_tibiaO> = contactTibiaFoot2TibiaO1*tibia1> + contactTibiaFoot2TibiaO2*tibia2> + contactTibiaFoot2TibiaO3*tibia3>

p_tibiaO_tibiaShin> = contactTibiaShin1*tibia1> + contactTibiaShin2*tibia2> + contactTibiaShin3*tibia3>

% remaining points!!!!
p_tibiaO_contactTibiaFemur> = TibiaO2contactFemurTibia1*tibia1> + TibiaO2contactFemurTibia2*tibia2> + TibiaO2contactFemurTibia3*tibia3>

p_contactTibiaFemur_contactFemurTibia> = 0> % conincident points

% points on femur
p_contactFemurTibia_femurO> = contactFemurTibia2FemurO1*femur1> + contactFemurTibia2FemurO2*femur2> + contactFemurTibia2FemurO3*femur3>

p_femurO_contactFemurNewtonian> = FemurO2contactFemurNewtonian1*femur1> + FemurO2contactFemurNewtonian2*femur2> + FemurO2contactFemurNewtonian3*femur3>

p_contactFemurNewtonian_contactNewtonianFemur> = 0> % coincident points

% start in Newtonian
v_contactGroundFoot_N> = v_no_n> + dt(p_no_contactGroundFoot>,n) % assumed to be 0 but this is how we write it when the foot is moving

% move to foot
v_contactFootGround_N> = v_contactGroundFoot_n> + U4*N1> + U5*N2> + U6*N3> % since we move from the ground to the foot (one body to another) we need to pass Us %
v2pts(n, foot, contactFootGround, footO)
v2pts(n, foot, footO, contactFootTibia)

% move to tibia
v_contactTibiaFoot_N> = v_contactFootTibia_n> + dt(p_contactFootTibia_contactTibiaFoot>,n) + U10*N1> + U11*N2> + U12*N3>
v2pts(n, tibia, contactTibiaFoot, tibiaO)
v2pts(n, tibia, tibiaO, tibiaShin)
v2pts(n, tibia, tibiaO, contactTibiaFemur)

% remaining velocites!!!!
% move to femur
v_contactFemurTibia_N> = v_contactTibiaFemur_n> + dt(p_contactTibiaFemur_contactFemurTibia>,n) + U16*N1> + U17*N2> + U18*N3>
v2pts(n, femur, contactFemurTibia, femurO)
v2pts(n, femur, femurO, contactFemurNewtonian)

% step 4 (autolev)

% step 5
mass foot = footMass
mass tibia = tibiaMass
mass femur = femurMass

inertia foot, footI1, footI2, footI3
inertia tibia, tibiaI1, tibiaI2, tibiaI3
inertia femur, femurI1, femurI2, femurI3

force_contactFootGround> = GRFforce1*N1> + GRFforce2*N2> + GRFforce3*N3>
force_tibiaShin> = kickforce1*N1> + kickforce2*N2> + kickforce3*N3>

% joint reaction force
% force(negativePoint/positivePoint, mag*direction>)
force(contactFootTibia/contactTibiaFoot, ankleforce1*N1> + ankleforce2*N2> + ankleforce3*N3>)
force(contactTibiaFemur/contactFemurTibia, kneeforce1*N1> + kneeforce2*N2> + kneeforce3*N3>)
force_contactFemurNewtonian> = hipforce1*N1> + hipforce2*N2> + hipforce3*N3>

% apply torques to body
torque(foot, footTorque1*N1> + footTorque2*N2> + footTorque3*N3>)
torque(tibia, tibiaTorque1*N1> + tibiaTorque2*N2> + tibiaTorque3*N3>)
torque(femur, femurTorque1*N1> + femurTorque2*N2> + femurTorque3*N3>)

constants BW 

% constraints
% constraints and kane command
% foot rotations
auxiliary[1] = U1
auxiliary[2] = U2
auxiliary[3] = U3

% foot translations
auxiliary[4] = U4
auxiliary[5] = U5
auxiliary[6] = U6

% tibia rotations
auxiliary[7] = U7
auxiliary[8] = U8
auxiliary[9] = U9

% tibia translations
auxiliary[10] = U10
auxiliary[11] = U11
auxiliary[12] = U12

% femur rotations 
auxiliary[13] = U13
auxiliary[14] = U14
auxiliary[15] = U15

% femur translations
auxiliary[16] = U16
auxiliary[17] = U17
auxiliary[18] = U18

constrain(auxiliary[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18])

zee_not = [ankleforce1, ankleforce2, ankleforce3, kneeforce1, kneeforce2, kneeforce3, hipforce1, hipforce2, hipforce3, foottorque1, foottorque2, foottorque3, tibiatorque1, tibiatorque2, tibiatorque3, femurtorque1, femurtorque2, femurtorque3]

zero = fr() + frstar()

save problem2.all

kane(ankleforce1, ankleforce2, ankleforce3, kneeforce1, kneeforce2, kneeforce3, hipforce1, hipforce2, hipforce3, foottorque1, foottorque2, foottorque3, tibiatorque1, tibiatorque2, tibiatorque3, femurtorque1, femurtorque2, femurtorque3)

output t, ankleForce1, ankleForce2, ankleForce3
output t, kneeForce1, kneeForce2, kneeForce3
output t, hipForce1, hipForce2, hipForce3
output t, footTorque1, footTorque2, footTorque3
output t, tibiaTorque1, tibiaTorque2, tibiaTorque3
output t, femurTorque1, femurTorque2, femurTorque3
output t, GRFforce1, GRFforce2, GRFforce3
output t, kickForce1, kickForce2, kickForce3

code algebraic() problem2.m