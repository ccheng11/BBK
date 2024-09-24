capture program drop mergeaggregates
program mergeaggregates
	version 12
    syntax namelist, simsetname(namelist) simtype(namelist) AGGregateover(namelist) ///
		levels(numlist min=3 max=3 integer) [summarize(namelist)]

	set more off
	local namecount = wordcount("`namelist'")
	local levelcount = `namecount'
	local currdir = c(pwd) /* Name of current directory, to take us back to where we belong */
	
	local path = "Results/`simsetname'"
	forvalues l = 1(1)`levelcount' {
		local l`l'name = word("`namelist'",`l')
		display "Level `l' variable: " word("`namelist'",`l')
		local ln`l' = word("`levels'",`l')
	} /* end forvalues l */

	display "Folder: " "`path'"
	display "Levels: " "`levels'"
	display "No. of values, level 1: " `ln1'
	display "No. of values, level 2: " `ln2'
	display "No. of values, level 3: " `ln3'
	display "Aggregate over: " "`aggregateover'"
	display "Location for results files:  `currdir'/Results/`simsetname'" 

	/* Parse additional summarize variables */
	local addvarcount = wordcount("`summarize'")
	local av_m_statement = ""
	local av_sd_statement = ""
	forvalues l = 1(1)`addvarcount' {
		local avname = word("`summarize'",`l') 
		local av_m_statement = "`av_m_statement'" + " " + "`avname'" + "_m=" + "`avname'" + "_m"
*		local av_sd_statement = "`av_sd_statement'" + " " + "`avname'" + "_sd=" + "`avname'" /* Summary of sd not yet implemented; requires sums and squared sums */
	} /* end forvalues l */
	
	*display "Summarize variables: " "`summarize'"

	cd `path'

/* Aggregating the country-level results */

	use "c_Res_`simtype'_comp_1_1_1.dta", clear
*	use "c_rescomp_1_1_1.dta", clear
	drop _all
	
	forvalues i = 1(1)`ln1' {
		display "i: " `i'
		forvalues j = 1(1)`ln2' {
			display "j: " `j'
			forvalues k = 1(1)`ln3' {
				display "k: " `k'
*				capture confirm file "c_rescomp_`i'_`j'_`k'.dta"
				capture confirm file "c_Res_`simtype'_comp_`i'_`j'_`k'.dta"
				if _rc==0 {
					foreach l in "c" {
						display "Reading in file `l'_Res_`simtype'_comp_`i'_`j'_`k'.dta ..."
						append using "`l'_Res_`simtype'_comp_`i'_`j'_`k'.dta"
*						append using "c_rescomp_`i'_`j'_`k'.dta"
	
					} /* end foreach l in "c" "g" */
				} /* end if _rc==0 */
				else {	
					display "The file c_Res_`simtype'_comp_`i'_`j'_`k'.dta does not exist"
				} /* end else */
			} /* end forvalues k = 1(1)`ln3' */
		} /* end forvalues j = 1(1)`ln2' */
	} /* end forvalues i = 1(1)`ln1' */
	
/* Collapsing over `aggregateover' */
summ gwno year `aggregateover' n
local l = "c"
collapse (sum) n=n `l'_logit1_sum `l'_logit2_sum `l'_logitb_sum `l'_logit1sq_sum `l'_logit2sq_sum `l'_logitbsq_sum  ///
	(mean) `l'_minor_m `l'_major_m `l'_both_m `l'_phat1_m `l'_phat2_m `l'_phatb_m `l'_logit1_m ///
	`l'_logit2_m `l'_logitb_m  `av_m_statement' ///
	, by(`aggregateover' gwno year) fast

foreach c in 1 2 b {
	gen ss_`c' = c_logit`c'sq_sum-((c_logit`c'_sum^2)/n) /* Variance of logit predictions */
	gen sd_`c' = sqrt(ss_`c'/n) /* Standard deviation of mean, logit predictions */
	gen pred_sd_`c' = sqrt(ss_`c'/n) /* Test */
*	gen pred_sd_`c' = sqrt(ss_`c') /* Standard deviation of point prediction, logit predictions */
	gen logit`c'_95lb = c_logit`c'_m - (1.96*pred_sd_`c') /* Lower bond, prediction interval, logit */
	gen logit`c'_95ub = c_logit`c'_m + (1.96*pred_sd_`c') /* Upper bond, predictions interval, logit */
	gen phat`c'_95lb = exp(logit`c'_95lb)/(1+exp(logit`c'_95lb)) /* Lower bond, prediction interval, probability */
	gen phat`c'_95ub = exp(logit`c'_95ub)/(1+exp(logit`c'_95ub)) /* Upper bond, prediction interval, probability */
}

save Aggregated_c.dta, replace

/* Aggregating the global-level results */
	use "g_Res_`simtype'_comp_1_1_1.dta", clear
	drop _all

	forvalues i = 1(1)`ln1' {
		display "i: " `i'
		forvalues j = 1(1)`ln2' {
			display "j: " `j'
			forvalues k = 1(1)`ln3' {
				display "k: " `k'
				capture confirm file "g_Res_`simtype'_comp_`i'_`j'_`k'.dta"
				if _rc==0 {
						display "Reading in file g_Res_`simtype'_comp_`i'_`j'_`k'.dta ..."
						append using "g_Res_`simtype'_comp_`i'_`j'_`k'.dta"
				} /* end if _rc==0 */
				else {	
					display "The file g_Res_`simtype'_comp_`i'_`j'_`k'.dta does not exist"
				} /* end else */
			} /* end forvalues k = 1(1)`ln3' */
		} /* end forvalues j = 1(1)`ln2' */
	} /* end forvalues i = 1(1)`ln1' */
	
/* Collapsing over `aggregateover' */
summ year `aggregateover' n
local l = "g"
collapse (sum) n=n `l'_logit1_sum `l'_logit2_sum `l'_logitb_sum `l'_logit1sq_sum `l'_logit2sq_sum `l'_logitbsq_sum  ///
	(mean) `l'_minor_m `l'_major_m `l'_both_m `l'_phat1_m `l'_phat2_m `l'_phatb_m `l'_logit1_m ///
	`l'_logit2_m `l'_logitb_m  `av_m_statement' ///
	, by(`aggregateover' year) fast
	
replace n = n/168 /* To correct an earlier aggregation error - to be changed when aggregatersults is changed !! */
	
foreach c in 1 2 b {
	gen ss_`c' = g_logit`c'sq_sum-((g_logit`c'_sum^2)/n) /* Variance of logit predictions */
	gen sd_`c' = sqrt(ss_`c'/n) /* Standard deviation of mean, logit predictions */
	gen pred_sd_`c' = sqrt(ss_`c'/n) /* Test */
*	gen pred_sd_`c' = sqrt(ss_`c') /* Standard deviation of point prediction, logit predictions */
	gen logit`c'_95lb = g_logit`c'_m - (1.96*pred_sd_`c') /* Lower bond, prediction interval, logit */
	gen logit`c'_95ub = g_logit`c'_m + (1.96*pred_sd_`c') /* Upper bond, predictions interval, logit */
	gen phat`c'_95lb = exp(logit`c'_95lb)/(1+exp(logit`c'_95lb)) /* Lower bond, prediction interval, probability */
	gen phat`c'_95ub = exp(logit`c'_95ub)/(1+exp(logit`c'_95ub)) /* Upper bond, prediction interval, probability */
}

save Aggregated_g.dta, replace


end
