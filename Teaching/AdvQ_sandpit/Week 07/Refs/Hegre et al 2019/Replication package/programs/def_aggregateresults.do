capture program drop aggregateresults

program aggregateresults
	version 12
    syntax namelist, simsetname(namelist) simtype(namelist) AGGregateover(namelist) levels(numlist min=3 max=3 integer) path(string) [summarize(namelist)]

	/* 
	Program aggregateresults reads a set of results files and aggregates them
	namelist: Variable names for the three levels in current simulation file
	simsetname: Name  set of simulations and of folder containing resultsfiles
	simtype: sim (simulation) or oos (for out-of-sample evaluation)
	Generates and fills a directory structure under the current directory: "Results/`inputfiles'"
	levels:		Number of levels for aggregation
	SUMMvars: additional variables to be aggregated 
	*/
	set more off
	local namecount = wordcount("`namelist'")
	local levelcount = `namecount'
	local currdir = c(pwd) /* Name of current directory, to take us back to where we belong */
	
	local path = "`path'"  + "`simsetname'"
	
	forvalues l = 1(1)`levelcount' {
		local l`l'name = word("`namelist'",`l')
		display "Level `l' variable: " word("`namelist'",`l')
		local ln`l' = word("`levels'",`l')
	} /* end forvalues l */

	/* Parse additional summarize variables */
	local addvarcount = wordcount("`summarize'")
	local av_m_statement = ""
	local av_sd_statement = ""
	forvalues l = 1(1)`addvarcount' {
		local avname = word("`summarize'",`l')
		local av_m_statement = "`av_m_statement'" + " " + "`avname'" + "_m=" + "`avname'"
		local av_sd_statement = "`av_sd_statement'" + " " + "`avname'" + "_sd=" + "`avname'"
	} /* end forvalues l */

	display "Folder: " "`path'"
	display "Levels: " "`levels'"
	display "No. of values, level 1: " `ln1'
	display "No. of values, level 2: " `ln2'
	display "No. of values, level 3: " `ln3'
	display "Aggregate over: " "`aggregateover'"
	display "Location for results files:  `currdir'/Results/`simsetname'" 
	display "Summarize variables: " "`summarize'"
	display "Collapse statements: " "`av_m_statement'" ", " "`av_sd_statement'"

	cd `path'

	forvalues i = 1(1)`ln1' {
		display "i: " `i'
		forvalues j = 1(1)`ln2' {
			display "j: " `j'
			forvalues k = 1(1)`ln3' {
				display "k: " `k'
				capture confirm file "Res_`simtype'_`i'_`j'_`k'.txt"
				if _rc==0 {
					foreach l in "c" "g" {
						if "`l'" == "c" { 
							local aggregationtext = "gwno year"
						}
						if "`l'" == "g" {
							local aggregationtext = "year" 
						}
						display "Reading in file Res_`simtype'_`i'_`j'_`k'.txt..."
						insheet using "Res_`simtype'_`i'_`j'_`k'.txt", names clear
						drop if simmed == "False" & gwno == gwno[_n-1]
						quietly gen l1 = `ln1'
						quietly gen minor = 0
						quietly replace minor = 1 if conflict == 1
						quietly gen major = 0
						quietly replace major = 1 if conflict == 2
						quietly gen phat1 = .
*						quietly replace phat1 = cutoff1[_n-1] - cutoff0[_n-1] if gwno == gwno[_n-1] /* Move to right line */
						quietly replace phat1 = cutoff1 - cutoff0 /* New april 2013: this seems correct based on results.txt */
						quietly gen logit1 = ln(phat1/(1-phat1))
						quietly gen logit1sq = logit1^2
						quietly gen phat2 = .
*						quietly replace phat2 = 1-cutoff1[_n-1] if gwno == gwno[_n-1] /* Move to right line */
						quietly replace phat2 = 1-cutoff1 /* New april 2013: this seems correct based on results.txt */
						quietly gen logit2 = ln(phat2/(1-phat2))
						quietly gen logit2sq = logit2^2
						quietly gen both = minor + major /* Both levels combined */
						quietly gen phatb = phat1 + phat2 /* Both levels combined */
						quietly gen logitb = ln(phatb/(1-phatb)) /* Both levels combined */
						quietly gen logitbsq = logitb^2
						display "Collapsing it...."
						collapse (count) n=simno (mean) `l'_minor_m=minor `l'_major_m=major `l'_both_m=both ///
							`l'_phat1_m=phat1 `l'_phat2_m=phat2 `l'_phatb_m=phatb `l'_logit1_m=logit1 ///
							`l'_logit2_m=logit2 `l'_logitb_m=logitb `av_m_statement' ///
							(sd) `l'_minor_sd=minor `l'_major_sd=major `l'_both_sd=both `l'_phat1_sd=phat1 `l'_phat2_sd=phat2 `l'_phatb_sd=phatb ///
							`l'_logit1_sd=logit1 `l'_logit2_sd=logit2 `l'_logitb_sd=logitb `av_sd_statement' ///
							(sum) `l'_phat1_sum=phat1 `l'_logit1_sum=logit1 `l'_logit1sq_sum=logit1sq ///
							`l'_phat2_sum=phat2 `l'_logit2_sum=logit2 `l'_logit2sq_sum=logit2sq ///
							`l'_phatb_sum=phatb `l'_logitb_sum=logitb `l'_logitbsq_sum=logitbsq ///
							(p5) `l'_phat1_p5=phat1 `l'_phat2_p5=phat2 `l'_phatb_p5=phatb /// 
							(p10) `l'_phat1_p10=phat1 `l'_phat2_p10=phat2 `l'_phatb_p10=phatb ///
							(p90) `l'_phat1_p90=phat1 `l'_phat2_p90=phat2 `l'_phatb_p90=phatb ///
							(p95) `l'_phat1_p95=phat1 `l'_phat2_p95=phat2 `l'_phatb_p95=phatb ///
							, by(`aggregationtext') fast
							*/
							/*
							(p1) `l'_phat1_p1=phat1 `l'_phat2_p1=phat2 `l'_phatb_p1=phatb ///
							(p2) `l'_phat1_p2=phat1 `l'_phat2_p2=phat2 `l'_phatb_p2=phatb ///
							(p20) `l'_phat1_p20=phat1 `l'_phat2_p20=phat2 `l'_phatb_p20=phatb ///
							(p80) `l'_phat1_p80=phat1 `l'_phat2_p80=phat2 `l'_phatb_p80=phatb /// 
							(p99) `l'_phat1_p99=phat1 `l'_phat2_p99=phat2 `l'_phatb_p99=phatb /// */
						label variable n "Number of simulations in input file"
						label variable `l'_minor_m "Share of simulations with minor conflict (`l')"
						label variable `l'_major_m "Share of simulations with major conflict (`l')"
						label variable `l'_both_m "Share of simulations with conflict (`l')"
						label variable `l'_minor_sd "Standard deviation across simulations, minor conflict (`l')"
						label variable `l'_major_sd "Standard deviation across simulations, major conflict (`l')"
						label variable `l'_both_sd "Standard deviation across simulations, conflict (`l')"
						label variable `l'_phat1_m "Mean predicted probability of minor conflict (`l')"
						label variable `l'_phat2_m "Mean predicted probability of minor conflict (`l')"
						label variable `l'_phatb_m "Mean predicted probability of conflict (`l')"
						label variable `l'_phat1_sd "Standard deviation, predicted probability of minor conflict (`l')"
						label variable `l'_phat2_sd "Standard deviation, predicted probability of minor conflict (`l')"
						label variable `l'_phatb_sd "Standard deviation, predicted probability of conflict (`l')"
						label variable `l'_logit1_m "Mean predicted log odds of minor conflict (`l')"
						label variable `l'_logit1_sd "Standard deviation, predicted log odds of minor conflict (`l')"
						label variable `l'_logit2_m "Mean predicted log odds of major conflict (`l')"
						label variable `l'_logit2_sd "Standard deviation, predicted log odds of major conflict (`l')"
						label variable `l'_logitb_m "Mean predicted log odds of conflict (`l')"
						label variable `l'_logitb_sd "Standard deviation, predicted log odds of conflict (`l')"
						label variable `l'_phat1_p5 "5-percentile, probability of minor conflict (`l')"
						label variable `l'_phat2_p5 "5-percentile, probability of major conflict (`l')"
						label variable `l'_phatb_p5 "5-percentile, probability of conflict (`l')"
						label variable `l'_phat1_p10 "10-percentile, probability of minor conflict (`l')"
						label variable `l'_phat2_p10 "10-percentile, probability of major conflict (`l')"
						label variable `l'_phatb_p10 "10-percentile, probability of conflict (`l')"
						label variable `l'_phat1_p90 "90-percentile, probability of minor conflict (`l')"
						label variable `l'_phat2_p90 "90-percentile, probability of major conflict (`l')"
						label variable `l'_phatb_p90 "90-percentile, probability of conflict (`l')"
						label variable `l'_phat1_p95 "95-percentile, probability of minor conflict (`l')"
						label variable `l'_phat2_p95 "95-percentile, probability of major conflict (`l')"
						label variable `l'_phatb_p95 "95-percentile, probability of conflict (`l')"
						gen `l1name' = `i'
						gen `l2name' = `j'
						gen `l3name' = `k'
						gen Sourcefile = "inputfiles'/Res_`simtype'_`i'_`j'_`k'.txt"
						local outdata = "`currdir'" + "/Results/" + "`simsetname'" + "/" + "`l'_Res_`simtype'_comp_`i'_`j'_`k'.dta"
						display "Saving compressed file to: `outdata'"
						save "`outdata'", replace
					} /* end foreach l in "c" "g" */
				} /* end if _rc==0 */
				else {	
					display "The file Res_`simtype'_`i'_`j'_`k'.txt does not exist"
				} /* end else */
			} /* end forvalues k = 1(1)`ln3' */
		} /* end forvalues j = 1(1)`ln2' */
	} /* end forvalues i = 1(1)`ln1' */
/*
	use "rescomp111.dta", clear
	drop _all
	forvalues i = 1(1)`ln1' {
		forvalues j = 1(1)`ln2' {
			forvalues k = 1(1)`ln3' {
				append using "rescomp`i'`j'`k'.dta"
			} /* end forvalues k = 1(1)`ln3' */
		} /* end forvalues j = 1(1)`ln2' */
	} /* end forvalues i = 1(1)`ln1' */
	sort gwno year `aggregateover'
	by gwno year `aggregateover': egen count = sum(n)
	by gwno year `aggregateover': egen minormean = mean(minor_m)
	by gwno year `aggregateover': egen majormean = mean(major_m)
	foreach var in logit1 logit2 {
		by gwno year `aggregateover': egen `var'_g_sqsum = sum(`var'sq_sum)
		by gwno year `aggregateover': egen `var'_g_sum = sum(`var'_sum)
		gen `var'_variance = (`var'_g_sqsum - (`var'_g_sum^2/count))/count
	}
	foreach var in phat1 phat2 {
		by gwno year `aggregateover': egen `var'_g_p1 = mean(`var'_m)
		by gwno year `aggregateover': egen `var'_g_p5 = mean(`var'_p5)
		by gwno year `aggregateover': egen `var'_g_p10 = mean(`var'_p10)
		by gwno year `aggregateover': egen `var'_g_p20 = mean(`var'_p20)
		by gwno year `aggregateover': egen `var'_g_p80 = mean(`var'_p80)
		by gwno year `aggregateover': egen `var'_g_p90 = mean(`var'_p90)
		by gwno year `aggregateover': egen `var'_g_p95 = mean(`var'_p95)
	}
*/
cd "`currdir'" /* Back to original directory */

end
