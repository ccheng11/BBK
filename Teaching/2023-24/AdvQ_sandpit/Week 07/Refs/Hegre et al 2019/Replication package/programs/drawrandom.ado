program drawrandom
	/*Define the syntax*/ 
	syntax varlist [if], random(varname) drawyear(real) draws(real) saveto(string) esttabpath(string)
	preserve	
	
	/*run the multilevel logistic model */
	melogit `varlist'  || `random':
	capture drop random*
	
	/*store results */
	eststo clear
	estimates save randomeffectsmodel, replace
	eststo randomeffectsmodel
	esttab using `esttabpath', replace
	
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
	quietly 	ren randomeffects`i' random`j'
	 	}
	saveold `saveto'.dta, replace
	restore
	matrix define include_re = J(3, 1, 0)
	matrix include_re[2,1]=1
end


		   
