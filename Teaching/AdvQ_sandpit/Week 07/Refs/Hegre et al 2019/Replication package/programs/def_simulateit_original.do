capture program drop simulateit

program simulateit
	version 12
    syntax, simset(namelist) workdir(string) inputdatastub(string) mmatrix(string) modellist(numlist integer) simtype(string) ///
		numpars(numlist min=4 max=4 integer) years(numlist min=3 max=3 integer) parafilename(string) resultsfile(string) logfile(string) ///
		simresdir(string)
		
/* Parsing numpars and years	*/

local scenarios = word("`numpars'",1) 
local redraws = word("`numpars'",2) 
local simnos = word("`numpars'",3)
local betadraws = word("`numpars'",4)
dis "workdir " "`workdir'"
dis "simset name " "`simset'"

local est_fyear = word("`years'", 1)
local est_lyear = word("`years'", 2)
local sim_lyear = word("`years'", 3)
	
local simyears = `sim_lyear' - `est_lyear' 
local zeroline = 9 /* Number of lines in parameter file up to IDEP statements */	
	
*****************************************************************************
**************************  Model specifications   **************************
*****************************************************************************

preserve
use "`mmatrix'", clear
*list *
quietly mkmat M*, matrix(M)
*matrix list M
restore
local maxparas = rowsof(M) /* Maximum number of parameters - number of IDEP lines in parameter file _and_ rows in matrix M */
display `maxparas'
local parafilelength = `zeroline' + `maxparas' + 11
local modelsno = colsof(M)

/* Simulation loop */
display "Running models " "`modellist'" ", " `modelsno' " models in total."

foreach model of local modellist {
	local mlab = string(`model')
	forvalues scenario = 1(1)`scenarios' {
		dis "Scenario number " `scenario'
		local parameter_outfile = "`workdir'/Parameters/parameters_" + "`simset'" + "_" + "`mlab'" + ".txt"
		dis "Parameter outfile: " "`parameter_outfile'"
		use "`inputdatastub'`scenario'.dta", clear
		quietly include "Scripts/Data_manipulation_Temp.do"
		/* random addendum I */
		preserve
		use "`mmatrix'", clear
		keep if M`model'==1
		vallist varname, local(indeps)
		restore
		
		quietly drawrandom conflict `indeps', random(gwno) drawyear(2012) esttabpath("Results/`simset'/re_model_`model'") ///
					   draws(`redraws') saveto("Results/`simset'/re_model_`model'")
		/* end random addendum */	
		forvalues r = 1(1)`redraws' {
			forvalues b = 1(1)`betadraws' {
				use "`inputdatastub'`scenario'.dta", clear
				drop if year > `sim_lyear'
				quietly include "Scripts/Data_manipulation_Temp.do"
				/* random addendum II */
				capture drop random*
				capture drop remerge
				sort gwno year
				merge m:1 gwno using "Results/`simset'/re_model_`model'.dta", gen(remerge) keepusing(random`r')
				rename random`r' random
				/* end random addendum */	
					quietly compress
					preserve
					insheet using parameters/`parafilename', clear
					set obs `parafilelength'
					forvalues l=1(1)`maxparas' { /* Alter lines in parameter file based on the model matrix M */
						replace v1 = "//" + v1 if _n == `zeroline' + `l' & M[`l',`model']==0
					}
					replace v1 = "IFS; if year >= `est_fyear' & year <= `est_lyear'" if _n == `zeroline' + `maxparas' + 3
					replace v1 = "BETADRAWS; 1" if _n == `zeroline' + `maxparas' + 4
					replace v1 = "SPLIT; none" if _n == `zeroline' + `maxparas' + 5
					replace v1 = "ITERATIONS; `simyears'" if _n == `zeroline' + `maxparas' + 6
					replace v1 = "STARTTIME; `est_lyear'" if _n == `zeroline' + `maxparas' + 7
					replace v1 = "LOGFILE; `logfile'.txt" if _n == `zeroline' + `maxparas' + 8
					replace v1 = "SIMULATIONS; `simnos'" if _n == `zeroline' + `maxparas' + 9
					replace v1 = "ENDPARAMS; baseoutcome(0)" if _n == `zeroline' + `maxparas' + 10
					replace v1 = "RESULTFILE; `simresdir'/Res_`simtype'_`scenario'_`mlab'_`r'_`b'.txt" if _n == `zeroline' + `maxparas' + 11
					outsheet using "`parameter_outfile'", replace nonames noquote
					restore
					drop if year >= 2100
					capture cd "Sim/Program"
					Simulator_test "`parameter_outfile'"
					cd `workdir'
			} /* end forvalues b = 1(1)`betadraws' */
			eststo mlogitresults
			esttab mlogitresults using "Results/`simset'/`resultsfile'`simtype'`mlab'", tex wide nogaps replace unstack ///
				title("Estimation results, `simtype', model `model', simulation set `simset'\label{tab:EstResults`simset'}") aic scalars(ll)
		} /*  endforvalues r = 1(1)`redraws' */
	} /* end forvalues scenario */
} /* end forvalues model */

end
