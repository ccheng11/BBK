Oslo, 28th November 2017

This folder contains replication data for Hegre et al. “Evaluating the conflict-reducing effect of UN peacekeeping operations”

The folder contains:
1. a STATA ‘master’ script file that runs prepares data, runs the simulations, and aggregates the simulated results.
2. A set of STATA ado programs that perform particular parts of the simulation
3. The program PRIOSim, in ‘sim’ folder, that contains the program in C++ form
4. All datasets needed to perform analysis
5. State do files that creates the figures and tables in the published manuscript

To run the simulation from beginning to end, run the STATA do file: _master_sim_pko, to run this file you need to change the path on line 54, the rest will run automatically.

This file will prepare data, calculate parameters for the growth model, and draw random effects parameters. Additional information is found in the STATA do files. Also feel free to contact havnyg@prio.org if you have questions. 

The master file will also replicate the estimation results reported in Table 3 in the paper.

The file starts the PRIOSim program. PRIOsim will only run on a Windows machine. 

After the simulations have run, the file will aggregate results and produce an aggregated results file.

After this is done, all figures in the paper can be produced using the PKOFigures.do file.
