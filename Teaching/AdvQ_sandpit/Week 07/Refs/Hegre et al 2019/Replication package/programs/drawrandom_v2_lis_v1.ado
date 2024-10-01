program drawrandom_v2, rclass
	/*Define the syntax*/ 
	
	syntax varlist [if], depvar(varname) random(varname) drawyear(real) draws(real) saveto(string) esttabpath(string)	
	*****************************************************************************
	*****************  Generating random effects (drawrandom_v2) ****************
	*summarize `varlist'
	
	
	summarize `depvar'
	local depmin = r(min)
	local depmax = r(max)
	local depvarvalues = `depmax' - `depmin' + 1
	matrix define include_re = J(`depvarvalues', 1, 0)
	
	
	
	forvalues s = `depmin'(1)`depmax' {
		preserve
		gen inc = `depvar' == `s'
		tab inc
		/*run the multilevel logistic model */
		noisily melogit inc `varlist' || `random':, startvalues(zero) iterate(100)
		/* test whether estimated variance for random terms is too small for the term to be included in simulation: */
		
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
		 forvalues i = 1(1)`countries' {
			 forvalues j = 4(1)`tempdraws' {
				display "`i'"
				capture capture drop random
				capture matrix tempmean = means[`i',1]
				capture matrix tempse = sds[`i',1]
				capture drawnorm random, n(1) means(tempmean) sds(tempse) clear
				capture mkmat random, matrix(tempreal)
				capture matrix randomeffects[`i',`j'] =  tempreal
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

	display "here 2"
	matrix list include_re


end


		   
