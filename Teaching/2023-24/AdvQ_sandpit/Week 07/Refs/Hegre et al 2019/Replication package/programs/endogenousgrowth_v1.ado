program endogenousgrowth_v1
	/*Define the syntax*/ 
	syntax varlist, lyear(int) draws(int) simset(string)
	/*******************************/
	/***Fixed effects GDP model ****/
	/*******************************/
	noisily display "Now in program endogenousgrowth "
	local xvarnos = wordcount("`varlist'")
	sort gwno year

	xtset gwno year
	quietly levelsof(gwno)
	local levellist = r(levels)
	local countrycount = wordcount("`levellist'") /* Number of countries in dataset */
	display "Number of countries: " `countrycount'
	local eq = "`varlist'" + " i.gwno"
	display "Equation: `eq', `xvarnos' independent variables including constant term" 
	display "Year: " `lyear'
	xi: estsimp reg `eq' if year <= `lyear', sims(`draws') genname(cl_)
	/* Make the following five lines more general later */
	macro dir
	local firstfe = "cl_" + string(`xvarnos')
	local lastfe = "cl_" + string(`xvarnos' + `countrycount' - 1)
	display "First: " "`firstfe'" ". Last: " "`lastfe'"
	drop `firstfe'
	ren `lastfe' `firstfe' /* "move" constant to top of list of fixed effects coefficients" */
	/* Obtain constant term from clarify terms */
	summ `firstfe'
	local regconstant = r(mean)
	display `regconstant'
	
	local coefficientlist = ""
	
	/* Create a matrix coefs individual draws from which is later used to fill out parameter file */ 
	forvalues i=1(1)`xvarnos' {
		local coefficientlist = "`coefficientlist'" + "cl_" + string(`i') + " "
	}
	mkmat `coefficientlist' if cl_1!=., matrix(coefs)
	*matrix list coefs	
	drop cl_*
	
	/*
	forvalues i = 1(1)24 {	
		local b`i' = coefs[`r',`i']
		display b`i'
		}
*/

	eststo clear
	xtreg `varlist' if year <= `lyear', fe
	/* Correction of constant term in the coefficient matrix obtained in the reg statement: 
		since the reg model omits the first geographical unit (the US) the constant term is off. 
		Correction is to subtract the difference between the two constant term from each realization of the constant term */
	matrix tempcoefs = e(b)'
	preserve
		svmat tempcoefs 
		keep tempcoefs1
		save "InputData/tempcoefs_`simset'.dta", replace
	restore
	matrix list tempcoefs
	local vars = rowsof(tempcoefs)
	local xtregconstant = tempcoefs[`vars',1]
	display `xtregconstant'
	local correction = `regconstant' - `xtregconstant'
	matrix corr = J(`draws',1,`correction')
	display "Regcons: " "`regconstant'" ". xtregcons: " "`xtregconstant'" 
	*matrix list coefs
	matrix X = coefs[1..`draws', `xvarnos'] - corr
	matrix coefs[1, `xvarnos'] = X
	preserve
		clear
		svmat coefs
		save "InputData/coefs_`simset'.dta", replace
	restore
	*matrix list coefs
	esttab 
	predict fe, u
	capture drop fixedeffect
	ipolate fe year, epolate by(gwno) gen(fixedeffect) /* The estimated unit-level fixed effect is stored as a variable for use in the simulation */
	drop fe
	preserve 
	keep gwno year fixedeffect
	drop if gwno ==. | year ==.
	save "InputData/fe_temp_`simset'.dta", replace
	restore
	estimates save FEmodel, replace
	eststo FEmodel
	stop
end
