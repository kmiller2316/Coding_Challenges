% step 1
newtonian n
bodies b, p % M1: b (block), M2: p (pendulum)
constants g, L, m1, m2

% step 2
variables x'', theta''
variables U{2}'
x' = U1
x'' = dt(x')

theta' = U2
theta'' = dt(theta')

% step 3 (kinematics)
% angular
simprot(n,b,1,x)
w_b_n> = w_b_n> + U1*0>
p_no_bo> = x*b1>
v_bo_n> = v_no_n> + dt(p_no_bo>,n)

simprot(n,p,3,theta)
angvel(n,p)
p_no_po> = p_no_bo> - L*p2>
v_po_n> = v_no_n> + dt(p_no_po>,n) 

% step 4 (autolev)

% step 5 (kinetics)
specified K
force_bo> = -k*x*b1>
mass b = m1
mass p = m2
inertia b,0,0,0
inertia p,0,0,0
gravity(-g*n2>)

% input g = 9.81, L = 0.3, K = 2, m1 = 1, m2 = 2
% input theta = 30
% input x = 0.2

zero = fr() + frstar()

save problem1.all

output t, theta, x

code dynamics() problem1.m