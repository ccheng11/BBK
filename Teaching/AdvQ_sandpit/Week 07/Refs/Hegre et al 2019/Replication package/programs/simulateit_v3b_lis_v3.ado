program simulateit_v3b
	version 12
    syntax, simset(namelist) workdir(string) depvar(string) depvarvalues(int) ///
		inputdatastub(string) mmatrix(string) modellist(numlist integer) simtype(string) ///
		numpars(numlist min=4 max=4 integer) years(numlist min=3 max=4 integer) parafilename(string) resultsfile(string) ///
		logfile(string) simresdir(string) zeroline(int) endoeq(string) endoposition(int) endodraws(int)
		
/* Parsing numpars and years	*/
clear matrix

local scenarios = word("`numpars'",1) 
local redraws = word("`numpars'",2) 
local simnos = word("`numpars'",3)
local betadraws = word("`numpars'",4)
dis "workdir " "`workdir'"
dis "simset name " "`simset'"

local est_fyear = word("`years'", 1)
local est_lyear = word("`years'", 2)
local sim_lyear = word("`years'", 3)
local endo_lyear = word("`years'", 4)
	
local simyears = `sim_lyear' - `est_lyear' 


*****************************************************************************
***********************  Checking dependent variable  ***********************
*****************************************************************************

summarize `depvar'
local depmin = r(min)

local depmax = r(max)

local dep2 = `depmin' + 1
display "Dependent variable " "`depvar'" " stated to have " `depvarvalues' " values. Minimum value: " `depmin' ". Maximum: " `depmax' "."

display "Here we are"
display  `depmin'

if `depmin' != 0 | `depmax'-`depvarvalues' !=-1 {
	display "WARNING: Possibly illegal values for dependent variable"
	stop
}
local re_terms = `depvarvalues' - 1
display "re _terms: " `re_terms'
	
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
display "maxparas: " `maxparas'
local parafilelength = `zeroline' + `maxparas' + `re_terms' + 10
display "parafilelength: " `parafilelength'
local modelsno = colsof(M)

/* Simulation loop */
display "Running models " "`modellist'" ", " `mnos' " models in total."
local pfnorig = "`parafilename'" /* Store the original parameter file name stub */
foreach model of local modellist {
	local mlab = string(`model')
	forvalues scenario = 1(1)`scenarios' {
		display "***********************************************************"
		display "************* Simulating model " `model' ", scenario " `scenario' ". *************"
		display "***********************************************************"
	*	local parameter_outfile = "`workdir'/Parameters/parameters_" + "`simset'" + "_" + "`scenario'" + "_" + "`mlab'" + ".txt"
		local parameter_outfile = "`workdir'/Parameters/parameters_" + "`simset'" + "_" + "`scenario'" + "_" + "`mlab'" + ".txt"
		dis "Parameter outfile: " "`parameter_outfile'"
		local indata = "`inputdatastub'`scenario'.dta"
		/* PKO project addendum: rename parameter baseline file to match scenario */
		disp "Simsetname: `simset'" 
		if "`simset'" == "PKOsimv3" {
			local indata = "`inputdatastub'`mlab'.dta"
*			local parafilename = subinstr("`pfnorig'",".","`scenario'.",1) /* Rename the parameter file to retrieve the scenario-specific one */
			local parafilename = subinstr("`pfnorig'",".","`mlab'.",1) /* Rename the parameter file to retrieve the scenario-specific one */
			local indata = "`inputdatastub'1.dta"
		}
		display "Parameter file to be used now: `parafilename'"
		*sleep 3000
		display "`indata'"
		use "`indata'", clear
		quietly include "Scripts/Data_manipulation_Temp.do"
		
		/* random addendum I: Read estimation model from model matrix dataset */
		preserve
		use "`mmatrix'", clear
		keep if M`model'==1
		vallist varname, local(indeps)
		restore
		/*
		if "`depvar'" == "conflict" { 
		quietly drawrandom `depvar' `indeps', random(gwno) drawyear(2012) esttabpath("Results/`simset'/re_model_`model'") ///
					   draws(`redraws') saveto("Results/`simset'/re_model_`model'")
			}
			*/
		*else {
		*quietly 

		if wordcount("`endoeq'") > 1 { /* If an endogenous equation has been specified */
			display "Running endogenousgrowth_v2..."
			*quietly 
			quietly endogenousgrowth_v2 `endoeq', lyear(`endo_lyear') draws(`endodraws') simset(`simset')
			esttab using "`workdir'/Results/FEmodel_`simset'.tex", replace unstack fragment wide scalars(ll) ///
				title("Fixed-effects model of effect of conflict on growth; 19xx-`est_lyear'") 
		}
		if wordcount("`endoeq'") == 1 { /* If an endogenous equation has not been specified */
			display "Not running endogenousgrowth program"
			capture drop fixedeffect
			gen fixedeffect = 0
		}
		save "`inputdatastub'`scenario'.dta", replace
		display "endogenousgrowth done "
		display "Running drawrandom_v2..."
		
		
		/* I am testing a potential issue wiht drawrandom_v2 and gwno here */ 
		
		/* I commented out the next line */
		/* quietly drawrandom_v2 `indeps', random(gwno) depvar(`depvar') drawyear(2012) esttabpath("Results/`simset'/re_model_`model'") ///
					   draws(`redraws') saveto("Results/`simset'/re_model_`model'") */ 
	
	
	summarize `depvar'

	local depvarvalues = `depmax' - `depmin' + 1
	
	matrix define include_re = J(`depvarvalues', 1, 0)
	
	display "Here 1"
	
	forvalues s = `depmin'(1)`depmax' {
	
	
		preserve
		gen inc = `depvar' == `s'
		tab inc
		

		display "Here 2"
		
		/*run the multilevel logistic model */
		
		/* noisily */ 
		
		melogit inc `varlist' || `random':, startvalues(zero) iterate(100)
		/* test whether estimated variance for random terms is too small for the term to be included in simulation: */
	
		display "Here 3"
	
	
		local parameters = e(k)
		matrix beta = e(b)
		local r = `s'+1
		if beta[1, `parameters'] > 0.001 {
			matrix include_re[`r',1] = 1 
		}
		capture drop random*
		/*store results */
		eststo clear
		estimates save randomeffectsmodel, replace
		eststo randomeffectsmodel
		esttab using `esttabpath'_`s'_o, tex replace
		
		/*gen variables with the mean and standard error of the group specific random effects paraeter */
		predict randommean, remeans reses(randomse)	
		keep if year==`drawyear' /*keep only one year, to make matrix operations simpler */
		levelsof gwno if randommean != .
		local countries = wordcount("`r(levels)'")
		display "Random effects calculated for `countries' countries" 
		/*store estimates in matrices*/
		mkmat gwno year randommean randomse, matrix(random)
		mkmat randommean, matrix(means)
		mkmat randomse, matrix(sds)
		mkmat gwno, matrix(gwno)
		local tempdraws = `draws' + 3 /* +1 to leave room for group identifier and overall mean and se */
		/*generate a matrix with one row for each country, and number of draws + 1 number of columns*/
		/*add the country code to the first column*/
		matrix randomeffects = J(`countries',`tempdraws',.)
		forvalues i = 1(1)`countries' {
			matrix randomeffects[`i',1] = gwno[`i',1] /*add group id to first column of matrix*/
			matrix randomeffects[`i',2] = random[`i',3] /*add overall mean to second column of matrix*/
			matrix randomeffects[`i',3] = random[`i',4] /*add standard error to third column of matrix*/
		}
		
		/*loop over the number of countries, and draw `draws' number of realizations of the 
		group specific random effects distribution. Add everything to the matrix defined above */
		quietly forvalues i = 1(1)`countries' {
			quietly forvalues j = 4(1)`tempdraws' {
				quietly capture drop random
				quietly matrix tempmean = means[`i',1]
				quietly matrix tempse = sds[`i',1]
				quietly drawnorm random, n(1) means(tempmean) sds(tempse) clear
				quietly mkmat random, matrix(tempreal)
				quietly matrix randomeffects[`i',`j'] =  tempreal
				quietly capture drop random
			}
		}
		/*turn matrix into a standard dataset, rename variables and store dataset*/
		svmat randomeffects
		ren randomeffects1 gwno
		ren randomeffects2 randommean
		ren randomeffects3 randomse
		quietly forvalues i = 4(1)`tempdraws' {
			quietly 	local j = `i' - 3
			quietly 	ren randomeffects`i' random`j'_`s'_o
		}
		saveold `saveto'_`s'_o.dta, replace
		restore
	} /*END FORVALUES S */
	matrix list include_re

					   
					   
					   
		*	}
		display "drawrandom done "
*/

		* `indeps' /* Make more general later */

		/* end random addendum */	
		forvalues r = 1(1)`redraws' {
			forvalues b = 1(1)`betadraws' {
				use "`inputdatastub'`scenario'.dta", clear
				drop if year > `sim_lyear'
*				quietly include "Scripts/Data_manipulation_Temp.do"
				/* random addendum II */
				
				capture drop random*
				capture drop remerge
				sort gwno year
				/*
				if "`depvar'" == "conflict"  { 
					merge m:1 gwno using "Results/`simset'/re_model_`model'.dta", gen(remerge) keepusing(random`r')
					rename random`r' random_1
				}
				*/
				*else {
					qui merge m:1 gwno using "Results/`simset'/re_model_`model'_1_o.dta", gen(remerge) keepusing(random`r'_1_o)
					rename random`r' random_1
					capture drop remerge
					qui merge m:1 gwno using "Results/`simset'/re_model_`model'_2_o.dta", gen(remerge) keepusing(random`r'_2_o)
					rename random`r' random_2
					capture drop remerge
				*}
*				summ year 
				save "`inputdatastub'_`simset'_m`model'_s`scenario'_esttemp.dta", replace
				/* end random addendum */	
				*/
					quietly compress
					*testformissing, model(`model') depvar(`depvar') efy(`est_fyear') ely(`est_lyear') sly(`sim_lyear') maxparas(`maxparas')
					
					
					/* Specifying the endogenous equation */
					/* - make more general later!! */
					if wordcount("`endoeq'") > 1 { /* If an endogenous equation has been specified */
						local thisdraw = floor((`endodraws'-1)*runiform() + 1) /* Draw a set of endogenous equation coefficients for this run of PRIOsim */
						display "Draw number: " `thisdraw'
						/* Select fixed effect realization */
						capture drop fixedeffect
						ren fixedeffect`thisdraw' fixedeffect
						/* Construct statement */
						local xvarnos = colsof(coefs)
						matrix thesecoefs = coefs[`thisdraw', 1..`xvarnos']
						*matrix list thesecoefs
						local endoparastatement = "IDEP; lGDPcap; lv(lGDPcap) + "
						
						display "`endoparastatement'"
						local i = 1
						foreach var of varlist  `endoeq' { 
							*display "Variable `i': "  "`var'"
*							if `i' == 1 { /* Constant term */
*								local endoparastatement = "`endoparastatement'" + string(coefs[`thisdraw', `xvarnos']) + " + "
*							}
							if `i' > 1 {
								local endoparastatement = "`endoparastatement'" + "(" + string(coefs[`thisdraw', `i'-1]) + "*lv(`var')) + "
							}
							* display "`endoparastatement'"
							local i = `i' + 1
						} /* end foreach var */
						*/
						local endoparastatement = "`endoparastatement'" + "lv(fixedeffect)"
							display "Endogenous variable constructed statement: `endoparastatement'"		
					}
*stop					
					/* Editing the parameter file before call to PRIOsim */
					preserve
					insheet using parameters/`parafilename', clear
					set obs `parafilelength'
					forvalues l=1(1)`maxparas' { /* Alter lines in parameter file based on the model matrix M */
						quietly replace v1 = "//" + v1 if _n == `zeroline' + `l' & M[`l',`model']==0
						/* Replace endogenous variable statement with the one constructed above: */
						if wordcount("`endoeq'") > 1 { /* If an endogenous equation has been specified */ {
							quietly replace v1 = "`endoparastatement'" if _n == `endoposition' & M[`l',`model']==1 
						}
					}

					/* Remove random-effects terms with insufficient variance from parameter file */
					/* Assume that 0 is the baseline category; e.g. skip category 0 in the loop below */
					local firstdep = `depmin' + 1
					*display "here"
					forvalues dep=`firstdep'(1)`depmax' { 
						local rm = `dep' + 1 /* Matrix starts at line 1, not line 0 */
						quietly replace v1 = "//" + v1 if _n == `zeroline' + `maxparas' + `dep' & include_re[`rm',1]==0
					}
					quietly replace v1 = "IFS; if year >= `est_fyear' & year <= `est_lyear'" if _n == `zeroline' + `maxparas' + `re_terms' + 2
					quietly replace v1 = "BETADRAWS; 1" if _n == `zeroline' + `maxparas' + `re_terms' + 3
					quietly replace v1 = "SPLIT; none" if _n == `zeroline' + `maxparas' + `re_terms' + 4
					quietly replace v1 = "ITERATIONS; `simyears'" if _n == `zeroline' + `maxparas' + `re_terms' + 5
					quietly replace v1 = "STARTTIME; `est_lyear'" if _n == `zeroline' + `maxparas' + `re_terms' + 6
					quietly replace v1 = "LOGFILE; `logfile'.txt" if _n == `zeroline' + `maxparas' + `re_terms' + 7
					quietly replace v1 = "SIMULATIONS; `simnos'" if _n == `zeroline' + `maxparas' + `re_terms' + 8
					quietly replace v1 = "ENDPARAMS; baseoutcome(0)" if _n == `zeroline' + `maxparas' + `re_terms' + 9
					quietly replace v1 = "RESULTFILE; `simresdir'/Res_`simtype'_`scenario'_`mlab'_`r'_`b'.txt" ///
						if _n == `zeroline' + `maxparas' + `re_terms' + 10
					outsheet using "`parameter_outfile'", replace nonames noquote
					restore
					drop if year >= 2100
					capture cd "Sim/Program"
					quietly Simulator_test "`parameter_outfile'"
					
					/* Test whether any output was written */
					display "Testing for existence of file `simresdir'/Res_`simtype'_`scenario'_`mlab'_`r'_`b'.txt"
					capture confirm file "`simresdir'/Res_`simtype'_`scenario'_`mlab'_`r'_`b'.txt"
					if _rc==601 { /* Return code for file not found */
						stopsim, /// 
							error("This file has not been written, probable simulation error. Program terminates.") ///
							logfile("../Log/`logfile'")
					} /* end if _rc==601 */
					else {
						display "File test OK"
					}
			cd `workdir'
			} /* end forvalues b = 1(1)`betadraws' */
			eststo mlogitresults
			esttab mlogitresults using "Results/`simset'/`resultsfile'`simtype'`mlab'", tex wide nogaps replace unstack ///
				title("Estimation results, `simtype', model `model', simulation set `simset'\label{tab:EstResults`simset'}") aic scalars(ll)
		} /*  endforvalues r = 1(1)`redraws' */
	} /* end forvalues scenario */
} /* end forvalues model */

end