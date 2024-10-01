program endogenousgrowth_v2
	/*Define the syntax*/ 
	*	syntax varlist, lyear(int) draws(int) simset(string) 
		syntax varlist, lyear(int) draws(int) simset(string) scenario(int)
	/*******************************/
	/***Fixed effects GDP model ****/
	/*******************************/
	local xvarnos = wordcount("`varlist'")
	sort gwno year

	xtset gwno year
	quietly levelsof(gwno)
	local levellist = r(levels)
	local countrycount = wordcount("`levellist'") /* Number of countries in dataset */
	display "Number of countries: " `countrycount'
	local eq = "`varlist'" + " _Igwno*"
	display "Equation: `eq', `xvarnos' independent variables including constant term" 
	display "Year: " `lyear'
	xi i.gwno, noomit
	/* Clarify to draw coefficients and fixed effects */
	estsimp reg `eq' if year <= `lyear', noconstant sims(`draws') genname(cl_)
	matrix beta = e(b)
	matrix tempcoefs = e(b)'
	preserve
		svmat tempcoefs 
		keep tempcoefs1
		save "InputData/tempcoefs_`simset'.dta", replace
	restore
*	macro dir
*predict pred, xb


	/* Create a matrix coefs individual draws from which is later used to fill out parameter file */ 
	local coefficientlist = ""
*	local constantlocation = `xvarnos' + `countrycount' - 1
*	local coefficientlist = "`coefficientlist'" + "cl_" + string(`constantlocation') + " "
	local mainparas = `xvarnos' - 1
	forvalues i=1(1)`mainparas' {
		local coefficientlist = "`coefficientlist'" + "cl_" + string(`i') + " "
	}
	mkmat `coefficientlist' if cl_1!=., matrix(coefs)

	/* Create a matrix fes individual draws from which is later used as fixedeffects */ 
	/* And a matrix fes2 with only the beta estimate */ 
	local cols = `draws' + 1
	matrix fes = J(`countrycount',`cols',0)
	matrix rownames fes = `levellist'
	matrix fes2 = J(`countrycount',`cols',0)
	matrix rownames fes2 = `levellist'
	forvalues country = 1(1)`countrycount' {
		matrix fes[`country',`cols'] = real(word("`levellist'",`country'))
		matrix fes2[`country',`cols'] = real(word("`levellist'",`country'))
		local var = `country' + `xvarnos' - 1
		mkmat cl_`var', matrix(fe_temp) nomissing
		forvalues j = 1(1)`draws' {
			matrix fes[`country',`j'] = fe_temp[`j',1]
			matrix fes2[`country',`j'] = beta[1, `mainparas'+`country']
		}		
	}

	preserve
		clear
		svmat fes2, names(fixedeffect)
		ren fixedeffect`cols' gwno
		save "InputData/fixedeffects_`simset'_`scenario'.dta", replace
	restore
	*matrix list coefs	
	drop cl_*
	/*
	forvalues i = 1(1)24 {	
		local b`i' = coefs[`r',`i']
		display b`i'
		}
*/

	*/

	merge m:1 gwno using "InputData/fixedeffects_`simset'_`scenario'.dta", nogenerate
	
end
