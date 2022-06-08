function problem1
SolveOrdinaryDifferentialEquations
% File  problem1.m  created by Autolev 4.0 on Sun Nov 14 23:08:12 2021


%===========================================================================
function VAR = ReadUserInput
global   G L M1 M2;
global   K;
global   THETA U1 U2 X;
global   THETAp U1p U2p Xp;
global   DEGtoRAD RADtoDEG COEF RHS SolutionToAxEqualsB;
global   TINITIAL TFINAL INTEGSTP PRINTINT ABSERR RELERR;

%-------------------------------+--------------------------+-------------------+-----------------
% Quantity                      | Value                    | Units             | Description
%-------------------------------|--------------------------|-------------------|-----------------
G                               =  9.81;                    % UNITS               Constant
L                               =  0.3;                    % UNITS               Constant
M1                              =  1.0;                    % UNITS               Constant
M2                              =  2.0;                    % UNITS               Constant

THETA                           =  30.0;                    % UNITS               Initial Value
U1                              =  0.0;                    % UNITS               Initial Value
U2                              =  0.0;                    % UNITS               Initial Value
X                               =  0.2;                    % UNITS               Initial Value

TINITIAL                        =  0.0;                    % UNITS               Initial Time
TFINAL                          =  10.0;                    % UNITS               Final Time
INTEGSTP                        =  0.05;                    % UNITS               Integration Step
PRINTINT                        =  1;                      % Positive Integer    Print-Integer
ABSERR                          =  1.0E-08;                %                     Absolute Error
RELERR                          =  1.0E-07 ;               %                     Relative Error
%-------------------------------+--------------------------+-------------------+-----------------

% Unit conversions
Pi       = 3.141592653589793;
DEGtoRAD = Pi/180.0;
RADtoDEG = 180.0/Pi;

% Reserve space and initialize matrices
COEF = zeros(2,2);
RHS = zeros(1,2);

% Evaluate constants
% Set the initial values of the states
VAR(1) = THETA;
VAR(2) = U1;
VAR(3) = U2;
VAR(4) = X;



%===========================================================================
function OpenOutputFilesAndWriteHeadings
FileIdentifier = fopen('problem1.1', 'wt');   if( FileIdentifier == -1 ) error('Error: unable to open file problem1.1'); end
fprintf( 1,             '%%       T            THETA            X\n' );
fprintf( 1,             '%%    (UNITS)        (UNITS)        (UNITS)\n\n' );
fprintf(FileIdentifier, '%% FILE: problem1.1\n%%\n' );
fprintf(FileIdentifier, '%%       T            THETA            X\n' );
fprintf(FileIdentifier, '%%    (UNITS)        (UNITS)        (UNITS)\n\n' );



%===========================================================================
% Main driver loop for numerical integration of differential equations
%===========================================================================
function SolveOrdinaryDifferentialEquations
global   G L M1 M2;
global   K;
global   THETA U1 U2 X;
global   THETAp U1p U2p Xp;
global   DEGtoRAD RADtoDEG COEF RHS SolutionToAxEqualsB;
global   TINITIAL TFINAL INTEGSTP PRINTINT ABSERR RELERR;

OpenOutputFilesAndWriteHeadings
VAR = ReadUserInput;
OdeMatlabOptions = odeset( 'RelTol',RELERR, 'AbsTol',ABSERR, 'MaxStep',INTEGSTP );
T = TINITIAL;
PrintCounter = 0;
mdlDerivatives(T,VAR,0);
while 1,
  if( TFINAL>=TINITIAL & T+0.01*INTEGSTP>=TFINAL ) PrintCounter = -1; end
  if( TFINAL<=TINITIAL & T+0.01*INTEGSTP<=TFINAL ) PrintCounter = -1; end
  if( PrintCounter <= 0.01 ),
     mdlOutputs(T,VAR,0);
     if( PrintCounter == -1 ) break; end
     PrintCounter = PRINTINT;
  end
  [TimeOdeArray,VarOdeArray] = ode45( @mdlDerivatives, [T T+INTEGSTP], VAR, OdeMatlabOptions, 0 );
  TimeAtEndOfArray = TimeOdeArray( length(TimeOdeArray) );
  if( abs(TimeAtEndOfArray - (T+INTEGSTP) ) >= abs(0.001*INTEGSTP) ) warning('numerical integration failed'); break;  end
  T = TimeAtEndOfArray;
  VAR = VarOdeArray( length(TimeOdeArray), : );
  PrintCounter = PrintCounter - 1;
end
mdlTerminate(T,VAR,0);



%===========================================================================
% mdlDerivatives: Calculates and returns the derivatives of the continuous states
%===========================================================================
function sys = mdlDerivatives(T,VAR,u)
global   G L M1 M2;
global   K;
global   THETA U1 U2 X;
global   THETAp U1p U2p Xp;
global   DEGtoRAD RADtoDEG COEF RHS SolutionToAxEqualsB;
global   TINITIAL TFINAL INTEGSTP PRINTINT ABSERR RELERR;

% Update variables after integration step
THETA = VAR(1);
U1 = VAR(2);
U2 = VAR(3);
X = VAR(4);
Xp = U1;
THETAp = U2;

% Quantities to be specified
K = 2;

COEF(1,1) = -M1 - M2;
COEF(1,2) = -L*M2*cos(THETA);
COEF(2,1) = cos(THETA);
COEF(2,2) = L;
RHS(1) = K*X - L*M2*sin(THETA)*U2^2;
RHS(2) = -G*sin(THETA);
VARp(1) = COEF(1,1)*COEF(2,2) - COEF(1,2)*COEF(2,1);
SolutionToAxEqualsB(2) =(COEF(1,1)*RHS(2)-COEF(2,1)*RHS(1))/VARp(1);
SolutionToAxEqualsB(1) =(COEF(2,2)*RHS(1)-COEF(1,2)*RHS(2))/VARp(1);

% Update variables after uncoupling equations
U1p = SolutionToAxEqualsB(1);
U2p = SolutionToAxEqualsB(2);

% Update derivative array prior to integration step
VARp(1) = THETAp;
VARp(2) = U1p;
VARp(3) = U2p;
VARp(4) = Xp;

sys = VARp';



%===========================================================================
% mdlOutputs: Calculates and return the outputs
%===========================================================================
function Output = mdlOutputs(T,VAR,u)
global   G L M1 M2;
global   K;
global   THETA U1 U2 X;
global   THETAp U1p U2p Xp;
global   DEGtoRAD RADtoDEG COEF RHS SolutionToAxEqualsB;
global   TINITIAL TFINAL INTEGSTP PRINTINT ABSERR RELERR;

% Evaluate output quantities
Output(1)=T;  Output(2)=THETA;  Output(3)=X;
FileIdentifier = fopen('all');
WriteOutput( 1,                 Output(1:3) );
WriteOutput( FileIdentifier(1), Output(1:3) );



%===========================================================================
function WriteOutput( fileIdentifier, Output )
numberOfOutputQuantities = length( Output );
if numberOfOutputQuantities > 0,
  for i=1:numberOfOutputQuantities,
    fprintf( fileIdentifier, ' %- 14.6E', Output(i) );
  end
  fprintf( fileIdentifier, '\n' );
end



%===========================================================================
% mdlTerminate: Perform end of simulation tasks and set sys=[]
%===========================================================================
function sys = mdlTerminate(T,VAR,u)
FileIdentifier = fopen('all');
fclose( FileIdentifier(1) );
fprintf( 1, '\n Output is in the file problem1.1\n' );
fprintf( 1, '\n To load and plot columns 1 and 2 with a solid line and columns 1 and 3 with a dashed line, enter:\n' );
fprintf( 1, '    someName = load( ''problem1.1'' );\n' );
fprintf( 1, '    plot( someName(:,1), someName(:,2), ''-'', someName(:,1), someName(:,3), ''--'' )\n\n' );
sys = [];



%===========================================================================
% Sfunction: System/Simulink function from standard template
%===========================================================================
function [sys,x0,str,ts] = Sfunction(t,x,u,flag)
switch flag,
  case 0,  [sys,x0,str,ts] = mdlInitializeSizes;    % Initialization of sys, initial state x0, state ordering string str, and sample times ts
  case 1,  sys = mdlDerivatives(t,x,u);             % Calculate the derivatives of continuous states and store them in sys
  case 2,  sys = mdlUpdate(t,x,u);                  % Update discrete states x(n+1) in sys
  case 3,  sys = mdlOutputs(t,x,u);                 % Calculate outputs in sys
  case 4,  sys = mdlGetTimeOfNextVarHit(t,x,u);     % Return next sample time for variable-step in sys
  case 9,  sys = mdlTerminate(t,x,u);               % Perform end of simulation tasks and set sys=[]
  otherwise error(['Unhandled flag = ',num2str(flag)]);
end



%===========================================================================
% mdlInitializeSizes: Return the sizes, initial state VAR, and sample times ts
%===========================================================================
function [sys,VAR,stateOrderingStrings,timeSampling] = mdlInitializeSizes
sizes = simsizes;             % Call simsizes to create a sizes structure
sizes.NumContStates  = 4;     % sys(1) is the number of continuous states
sizes.NumDiscStates  = 0;     % sys(2) is the number of discrete states
sizes.NumOutputs     = 3;     % sys(3) is the number of outputs
sizes.NumInputs      = 0;     % sys(4) is the number of inputs
sizes.DirFeedthrough = 1;     % sys(6) is 1, and allows for the output to be a function of the input
sizes.NumSampleTimes = 1;     % sys(7) is the number of samples times (the number of rows in ts)
sys = simsizes(sizes);        % Convert it to a sizes array
stateOrderingStrings = [];
timeSampling         = [0 0]; % m-by-2 matrix containing the sample times
OpenOutputFilesAndWriteHeadings
VAR = ReadUserInput
