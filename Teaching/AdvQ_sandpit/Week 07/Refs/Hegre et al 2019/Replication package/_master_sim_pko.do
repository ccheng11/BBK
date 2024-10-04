***************************************************
**Project: Prediction                     *********
**Purpose: this do file simulates,        *********
**         evaluates and analyses         *********
**	       simulations		          	  *********
** Log file: prediction.log		  		  *********
** Results: Found in figures and 	  	  *********
**			Results          			  *********
***************************************************
*****************************************************************************
**************************  Simulation parameters  **************************
*****************************************************************************
clear all
local project = "PKO"  /* This identifes the project/article the simulation is for */
local simsetname = "PKOsim" /* This identifies the results for this simulation within the project. 
							   All results simulated under this name will be overwritten! */
local simtype = "sim" /* Simulation type: out-of-sample (oos) or simulation (sim) */ 
local simnos = 10 /* Number of simulations in each inner loop (as passed to the priosim parameter file ) */
local scenarios = 7 /* Number of scenarios */
local betadraws = 5
local redraws = 5 /* Number of random effects draws */
local est_fyear = 1950 /* First year in estimation data set */
local est_lyear = 2013 /* Last year in estimation data set; also specifies first year in simulation (the year after) */
local sim_fyear = 2000 /* First year in simulation */
local sim_lyear = 2015 /* Last year in simulation */
local depvar = "conflict"
local depvarvalues = 3
*local mnos = 1 /* Number of models _and_ (maximum number of) columns in matrix M */
local models = "1"  // 2" // 3 4 5 6 7" // 8 9" // 10 11 12 13 14 15 16" 
local logfile = "Log_PKO_sim_insamplev12" /* Name of log fil20es */
*local localpath = "B:\PredictionTemp\"
local localpath = "C:\PredictionTemp\"
local MMatrixname = "ModelMatrix_sim.dta"
local inputdata = "PredictionPKO" /* Stub for input data file name; scenario number is added to this" */
local parafile = "parameters_baseline" /* Baseline parameter file. Must contain all variable name specified in MMatrixname dataset */
*local pfnorig = "parameters_baseline_PKOsimv5" /* Store the original parameter file name stub */
local resultsfilestub = "PKO" /* Stub for stata estimation results file */ 
local zeroline = 22 /* Line number in parameter file for last non-IDEP statement */	
*local endogenous_eq = "NA"
/* Specify endogenous equation: NOTE!!! Only use variables that are included in simulation! Or else!! */
local endogenous_eq = "loggdpgrowth lGDPcap ltimeindep c1 c2 ltsc0 nc ncc1 ncc2 lpop lPKOtrad lPKOtrans llPKObudget llPKObudgsq lPKOneighbor lGDPcapPKOtrans lGDPcapPKOtrad" // dec60 dec70 dec80 dec90"
local endoposition = 35 /* The position (line number) of the endogeneous variable in the parameter file */
local endodraws = 50 /* Number of distinct draws for the fixed-effects model coefficients */
local endovar = "lgdpcap"
local endolastyear = `est_lyear' /* Standard setup! */ 
local compfile = "`inputdata'" + "1"
local seedno 1001

set seed `seedno'
set matsize 10000

/* Preparing */

capture cd "/Users/Havard/Dropbox/Collaborations/PKO/Paper/Replication package"

include "Preparing.do"


****************************************************************
**************************  New data  **************************
****************************************************************
/* The following three lines may sometimes be omitted to save time */
/*
include "../../Data/Dofiles/_DataGenCountryYearMaster.do" /* Compiles data from original sources */
capture cd "../../Collaborations/PKO"
save "InputData/SSPdata_temp.dta", replace
*/ 
capture use "SSPdata_temp.dta"

include "AssembleDataPKO.do" /* Preparing data for SSP simulations */

****************************************************************
********************* RUN IV models ****************************
****************************************************************

/* The following line may be omitted  */

*quietly include "InstrumentAnalysis.do"

****************************************************************
********************* Simulation Models ************************
****************************************************************


simulateit_v3e, simset(`simsetname') workdir(`work_directory') inputdatastub(`inputdata') mmatrix(`MMatrixname') ///
	depvar(`depvar')  depvarvalues(`depvarvalues') modellist(`models') simtype(`simtype') ///
	numpars(`scenarios' `redraws' `simnos' `betadraws') seedno(`seedno') ///
	years(`est_fyear' `est_lyear' `sim_lyear' `sim_fyear' `endolastyear') parafilename(`parafile') resultsfile(`resultsfilestub') logfile(`logfile') ///
	simresdir(`results_directory') zeroline(`zeroline') endoeq(`endogenous_eq') endoposition(`endoposition') endodraws(`endodraws') project("PKO")

capture cd `work_directory'


aggregateresults_v3g Scenario Model RE Beta, simsetname(`simsetname') depvar(`depvar') depvarvalues(3) ///
  simtype(`simtype') collapseover(2) levels(`scenarios' `mnos' `redraws' `betadraws') path(`localpath') ///
  fyear(`sim_fyear') lyear(`sim_lyear') summarize(`endovar' gdpcapssp2  ltsc0 llpkobudget pkobudget lpkotrad lpkotrans)
 
mergemodels_v2b Scenario Model, simsetname(`simsetname') models(`mnos') scenarios(`scenarios') depvar(`depvar') ///
	depvarvalues(3) simtype(`simtype')


