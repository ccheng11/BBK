program mergemodels_v2c
	version 12
    syntax namelist, simsetname(namelist) models(int) scenarios(int) depvar(string) depvarvalues(int) simtype(namelist)

	/* The program merges all scenarios into one results file and calculates a few statistics */
	/* The only difference between b and c is that c calculates 67% precision intervals in addition */
	/* Replaces the 'mergeaggregates' ado file */


	/* Definition of dependent variable */
/*	summarize `depvar'
	local depmin = r(min)
	local depmax = r(max)
*/	local depmin = 0
	local depmax = 5
	local dep2 = `depmin' + 1
	display "Depvar stated to range from `depmin' to `depmax'"
/*
	display "Dependent variable " "`depvar'" " stated to have " `depvarvalues' " values. Minimum value: " `depmin' ". Maximum: " `depmax' "."
	if `depmin' != 0 | `depmax'-`depvarvalues' !=-1 {
		display "WARNING: Possibly illegal values for dependent variable"
		* stop
	}
*/	
	/* Creating empty placeholder dataset */
	local indata = "Results/" + "`simsetname'" + "/" + "Aggregated_c_`simtype'_1_1_x_x.dta"
	use `indata', clear
	drop _all
	
	/* Merging in all the scenarios data */
	forvalues s = 1(1)`scenarios' {
		forvalues m = 1(1)`models' { 
			local indata = "Results/" + "`simsetname'" + "/" + "Aggregated_c_`simtype'_`s'_`m'_x_x.dta"
			capture confirm file `indata'
			if _rc==0 {
				append using `indata'
			} /* if _rc==0 */
		} /* end forvalues m */
	} /* end forvalues s */
*	dis "here"
*	tab n "`namelist'"
		
	forvalues c = `depmin'(1)`depmax' { /* Not yet generalized!!! */
*		gen ss_`c' = c_logit`c'sq_sum-((c_logit`c'_sum^2)/n) /* Variance of logit predictions */
*		gen sd_`c' = sqrt(ss_`c'/n) /* Standard deviation of mean, logit predictions */
*		gen pred_sd_`c' = sqrt(ss_`c'/n) /* Test */
		gen pred_sd_`c' = sqrt(v_logit`c'_o) /* Standard deviation of point prediction, logit predictions category c vs. the other categories*/
		gen logit`c'_90lb = m_logit`c'_o - (1.6449*pred_sd_`c') /* Lower bond, prediction interval, logit category c vs. the other categories */
		gen logit`c'_90ub = m_logit`c'_o + (1.6449*pred_sd_`c') /* Upper bond, predictions interval, logit category c vs. the other categories */
		gen phat`c'_90lb = exp(logit`c'_90lb)/(1+exp(logit`c'_90lb)) /* Lower bond, prediction interval, probability category c */
		gen phat`c'_90ub = exp(logit`c'_90ub)/(1+exp(logit`c'_90ub)) /* Upper bond, prediction interval, probability category c */
		gen logit`c'_50lb = m_logit`c'_o - (0.6745*pred_sd_`c') /* Lower bond, prediction interval, logit */
		gen logit`c'_50ub = m_logit`c'_o + (0.6745*pred_sd_`c') /* Upper bond, predictions interval, logit */
		gen phat`c'_50lb = exp(logit`c'_50lb)/(1+exp(logit`c'_50lb)) /* Lower bond, prediction interval, probability */
		gen phat`c'_50ub = exp(logit`c'_50ub)/(1+exp(logit`c'_50ub)) /* Upper bond, prediction interval, probability */
		gen logit`c'_67lb = m_logit`c'_o - pred_sd_`c' /* Lower bond, prediction interval, logit */
		gen logit`c'_67ub = m_logit`c'_o + pred_sd_`c' /* Upper bond, predictions interval, logit */
		gen phat`c'_67lb = exp(logit`c'_67lb)/(1+exp(logit`c'_67lb)) /* Lower bond, prediction interval, probability */
		gen phat`c'_67ub = exp(logit`c'_67ub)/(1+exp(logit`c'_67ub)) /* Upper bond, prediction interval, probability */
		
		}
		
saveold "Results/`simsetname'/Aggregated_c.dta", replace


	
end
