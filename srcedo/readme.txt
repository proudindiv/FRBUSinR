

How to run the model:
====================

In Matlab/Octave:

1) Download Dynare Version 4 from the Dynare website: http://www.dynare.org/ 
2) Download the EDO files in a folder you choose.
3) Start Matlab/Octave and change the current directory to the folder in step 2.
4) Link in Matlab/Octave the Dynare folder in the menu under file/Set Path (or use the command
   "addpath path/to/dynare").
5) Run the command "dynare linearized" or "dynare Dynare_edo" from the Matlab/Octave command line to run the two model versions


Content of the EDO folder:
========================

Dynare_edo.mod: Dynare model file containing the latest estimated parameters and nonlinear model equations with the variables defined in levels.

Dynare_edo_steadystate.mod: Dynare steady-state file computes the steady state of the model variables in levels. This file needs to be in the same directory as Dynare_edo.mod.

linearized.mod: Dynare model file containing the latest estimated parameters and nonlinear model equations with the variables defined in percent deviation from steady state.

linearized_steadystate.mod: Dynare steady-state file computes the steady state of the model variables in percent deviation from steady state, which are zero except for the observed variables. This file needs to be in the same directory as linearized.mod.

readme.txt: The file you are currently reading.
