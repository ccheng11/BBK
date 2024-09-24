program aggregateresults_v3
	version 12
    syntax namelist, simsetname(namelist) depvar(string) depvarvalues(int) simtype(namelist) ///
		COLLapseover(int) levels(numlist min=4 max=4 integer) path(string) [summarize(namelist)]

	/* 
	Program aggregateresults_v2b reads a set of results files and aggregates them
	namelist: Variable names for the three levels in current simulation file
	simsetname: Name  set of simulations and of folder containing resultsfiles
	simtype: sim (simulation) or oos (for out-of-sample evaluation)
	Generates and fills a directory structure under the current directory: "Results/`inputfiles'"
	levels:		Number of levels for aggregation
	SUMMvars: additional variables to be aggregated 
	*/
	set more off
	matrix drop _all
	set matsize 10000
	local namecount = wordcount("`namelist'")
	local levelcount = `namecount'
	local currdir = c(pwd) /* Name of current directory, to take us back to where we belong */
	
	local path = "`path'"  + "`simsetname'" /* this is the local path; not the dropbox path */
	
	forvalues l = 1(1)`levelcount' {
		local l`l'name = word("`namelist'",`l')
		display "Level `l' variable: " word("`namelist'",`l')
		local ln`l' = word("`levels'",`l')
	} /* end forvalues l */

	/* Parse additional summarize variables */
	local addvarcount = wordcount("`summarize'")
	local av_m_statement = ""
	forvalues l = 1(1)`addvarcount' {
		local avname = word("`summarize'",`l')
		local av_m_statement = "`av_m_statement'" + " " + "`avname'" + "_m=" + "`avname'"
	} /* end forvalues l */

	display "Folder: " "`path'"
	display "No. of values, level 1: " `ln1' ", level 2: " `ln2' ", level 3: " `ln3' ", level 4: " `ln4'
	display "Collapse over: " `collapseover'

	display "Location for results files:  `currdir'/Results/`simsetname'" 
	display "Collapse statements: " "`av_m_statement'" 
	local aggregationtext = "gwno year"
	cd `path'
	
	/* Error checking */
	/* Collapse levels */
	if `collapseover' != 2 {
		display "Currently, it is only possible to collapse over 2 levels. Program terminates."
		stop
	}
	
forvalues i = 1(1)`ln1' { 
	forvalues j = 1(1)`ln2' {
		/* Will do separate collapses for each level of the two first level variables, typically scenario and model. */
		/* Create matrix to hold aggregate means for variables */
		/* Make empty dataset as point of departure; based on the dimensions of the first results file */
		local l1name = word("`namelist'",1)
		local l2name = word("`namelist'",2)
		display " "
		display "************************************************************************"
		display "Aggregating results for " "`l1name'" " " `i' ", " "`l2name'" " " `j' ", first pass (means)."
		display "************************************************************************"
		capture confirm file "Res_`simtype'_1_1_1_1.txt"
		if _rc==0 {
			foreach summtype in "m" "v" {
				display "Preparing aggregation matrix agg`summtype': Reading in file Res_`simtype'_1_1_1_1.txt... (namelist: `namelist')"
				insheet using "Res_`simtype'_1_1_1_1.txt", names clear
				keep if simno == 1
				*drop if year == `sim_lyear'
				keep `aggregationtext' `depvar'
				
				/* Error checking */
				/* Definition of dependent variable */
				summarize `depvar'
				local depmin = r(min)
				local depmax = r(max)
				local dep2 = `depmin' + 1
				display "Dependent variable " "`depvar'" " stated to have " `depvarvalues' " values. Minimum value: " `depmin' ". Maximum: " `depmax' "."
				if `depmin' != 0 | `depmax'-`depvarvalues' !=-1 {
					display "WARNING: Possibly illegal values for dependent variable"
					* stop
				}
				drop `depvar'
				/* Generating variables to hold summaries */
				quietly gen n = 0
				/* Prepares for storing simulated outcome (only aggm) */
				if "`summtype'" == "m" {
					forvalues dvlb = `depmin'(1)`depmax' {
						quietly gen sim_`dvlb' = 0 
					}
				} /* end if `summtype' == "m" */
				forvalues dvlb = `depmin'(1)`depmax' {
					quietly gen logit`dvlb'_o = 0
				}
				forvalues dvlb = `depmin'(1)`depmax' {
					local next = `dvlb' + 1
					forvalues dvlc = `next'(1)`depmax' { /* Prepares for aggregating logits for all pairwise comparisons */
						quietly gen logit`dvlc'_`dvlb' = 0
					}
				}
				display "Summary variables: " "`summarize'"
				foreach iv in `summarize' {
					quietly gen `summtype'_`iv' = 0
				}
				mkmat *, matrix(agg`summtype') /* Matrix to maintain aggregated means for every country year. ///
					OBS: max 11,000.... due to matsize limitations... */
				*if "`summtype'" == "m" {
					local `summtype'rows = rowsof(agg`summtype')
					local `summtype'cols = colsof(agg`summtype')
				*} /* end if `summtype' == "m" */
				display "Cols in matrix agg`summtype': " `mcols'
			} /* end foreach summtype in "m" "v"  */
		} /* end if _rc==0 */
		else {	
			display "The file Res_`simtype'_1_1_1_1.txt does not exist"
		} /* end else */
		* matrix dir

		/* First pass: Read files in; Calculate global means, save only required variables */
		local agg_n = 0 /* Will hold the total number of simulations */
		local statement = "Will summarize mean for "
		forvalues dvlb = `depmin'(1)`depmax' {
			local sim_`dvlb' = 0
			local logit_`dvlb'_o = 0
			local statement = "`statement'" + "sim_`dvlb', " + "logit_`dvlb'_o, "
			local next = `dvlb' + 1
			forvalues dvlc = `next'(1)`depmax' { /* Prepares for aggregating logits for all pairwise comparisons */
				local logit_`dvlc'_`dvlb' = 0 
				local statement = "`statement'" + "logit_`dvlc'_`dvlb', "
			}
		} /* end forvalues dvlb = `depmin'(1)`depmax' */
		display "`statement'"
		display "Summary variables: " "`summarize'"
*		foreach iv in `summarize' {
*			local `iv' = 0
*		}
			
		forvalues k = 1(1)`ln3' {
			forvalues s= 1(1)`ln4' {
				capture confirm file "Res_`simtype'_`i'_`j'_`k'_`s'.txt"
				if _rc==0 {
					display "First pass. Reading in file Res_`simtype'_`i'_`j'_`k'_`s'.txt... (namelist: `namelist')"
					quietly insheet using "Res_`simtype'_`i'_`j'_`k'_`s'.txt", names clear
					*drop if year == `sim_lyear'
*					display "Here 1"
					/* Generate simulated outcome variables */
					forvalues dvlb = `depmin'(1)`depmax' {
						quietly gen sim_`dvlb' = 0
						quietly replace sim_`dvlb' = 1 if `depvar' == `dvlb'
					} /* end foreach dvlb in `depvarvalues'  */
					quietly gen phat0 = cutoff0
					quietly gen logit0_o = ln(phat0/(1-phat0))
					/* Test for missing cutoffs */
					forvalues dvlb = `depmin'(1)`depmax' {
						quietly summ cutoff`dvlb'
						if r(N) == 0 {
							Display "Problem in results file: cutoff`dvlb' has no observations."
						} /* end if r(N) == 0 */
					} /* end forvalues dvlb = `depmin'(1)`depmax' */

					/* Generate phat and logit variables */
					forvalues dvlb = `dep2'(1)`depmax' {
*						display "dvlb " `dvlb'
						local cutunder = `dvlb' -1
						quietly gen phat`dvlb' = cutoff`dvlb' - cutoff`cutunder'
						quietly gen logit`dvlb'_o = ln(phat`dvlb'/(1-phat`dvlb'))
					} /* end foreach dvlb in `depvarvalues'  */
					forvalues dvlb = `depmin'(1)`depmax' {
						local next = `dvlb' + 1
						forvalues dvlc = `next'(1)`depmax' { /* Prepares for aggregating logits for all pairwise comparisons */
							quietly gen logit`dvlc'_`dvlb' = ln(phat`dvlc'/phat`dvlb')
						}
					} /* end foreach dvlb in `depvarvalues'  */
					keep simno gwno year `depvar' sim_* `summarize' phat* logit* cutoff*
					capture drop _merge
					merge m:1 gwno using Regions.dta, keepusing(region)
					quietly save Res_`simtype'_`i'_`j'_`k'_`s'.dta, replace
					/* Collapsing it */
					collapse (count) n=simno (mean) sim_* logit* `summarize', by(`aggregationtext') fast
					quietly summ n
					local this_n = r(mean)
					local agg_n = `agg_n' + r(mean)
					display "This results batch has " `this_n' " simulations, aggregated there are " `agg_n' " simulations."
					mkmat *, matrix(currentmeans)

					matrix listcurrentm = currentmeans[1..9,1..9]
*					matrix list listcurrentm
*/					
					matrix ns = J(`mrows',1,`agg_n') /* Matrix to hold n temporarily */
					matrix aggm[1,3]=ns /* And fill into the aggregated means matrix in the 3rd ("n") column  */
*	display "here: mrows, mcols " `mrows' ", " `mcols' ". agg_n, this_n " `agg_n' ", " `this_n'
					matrix temp = aggm[1..`mrows',4..`mcols']*((`agg_n'-`this_n')/`agg_n') + currentmeans[1..`mrows',4..`mcols']*(`this_n'/`agg_n')
					matrix aggm[1,4]=temp
					matrix listaggm = aggm[1..9,1..9]
*					matrix list listaggm
*/				
				} /* end if _rc==0 */
				else {	
					display "The file Res_`simtype'_`i'_`j'_`k'_`s'.txt does not exist"
				} /* end else */
			} /* end forvalues l = 1(1)`ln4' */
		} /* end forvalues k = 1(1)`ln3' */

		clear
		svmat aggm, names(col)
		forvalues dvl = `depmin'(1)`depmax' {
			quietly gen m_phat`dvl' = 1/(1+exp(-logit`dvl'_o))
		} /* end foreach dvl in `depvarvalues'  */
		rename logit* m_logit*

		local outdata = "`currdir'" + "/Results/" + "`simsetname'" + "/" + "Aggmeans_`simtype'_`i'_`j'_x_x.dta"
		display "Saving aggregated means to: `outdata'"
		quietly save "`outdata'", replace
	} /* end forvalues j = 1(1)`ln2' */
} /* end forvalues i = 1(1)`ln1' */




/* Second pass: Calculate deviations */	

forvalues i = 1(1)`ln1' {
	forvalues j = 1(1)`ln2' {
		display " "
		display "************************************************************************"
		display "Aggregating results for " "`l1name'" " " `i' ", " "`l2name'" " " `j' ", second pass (variance)."
		display "************************************************************************"
		local indata = "`currdir'" + "/Results/" + "`simsetname'" + "/" + "Aggmeans_`simtype'_`i'_`j'_x_x.dta"
		display "Reading in aggregated means from: `indata'"
		use "`indata'", clear
		mkmat *, matrix(aggm)
		if matmissing(aggm) == 1 {
			display "Missing elements in matrix aggm; will skip aggregating over " "`l1name'" " " `i' ", " "`l2name'" " " `j'
		}
		if matmissing(aggm) == 0 {
			display "No missing elements in matrix aggm"
		}
			local agg_n = 0		
			local tcols = `vcols' - 2
			matrix t = J(`mrows',`tcols',0)
			matrix aggv[1,3]=t /* Replace all values except time and space ids with zeros */
			forvalues k = 1(1)`ln3' {
				forvalues s= 1(1)`ln4' {
				capture confirm file "Res_`simtype'_`i'_`j'_`k'_`s'.dta"
				if _rc==0 {
					display "Second pass. Reading in file Res_`simtype'_`i'_`j'_`k'_`s'.dta... (namelist: `namelist')"
					quietly use "Res_`simtype'_`i'_`j'_`k'_`s'.dta", clear

					/* Merge in accumulated means */
					quietly merge m:1 gwno year using `indata'
					quietly gen d_logit0_o = (logit0_o-m_logit0_o)^2
					forvalues dvlb = `dep2'(1)`depmax' {
						local cutunder = `dvlb'-1
						quietly gen d_logit`dvlb'_o = (logit`dvlb'_o-m_logit`dvlb'_o)^2
					} /* end foreach dvlb in `depvarvalues'  */
					forvalues dvlb = `depmin'(1)`depmax' {
						local next = `dvlb' + 1
						forvalues dvlc = `next'(1)`depmax' { /* Prepares for aggregating logits for all pairwise comparisons */
							quietly gen d_logit`dvlc'_`dvlb' = (logit`dvlc'_`dvlb'-m_logit`dvlc'_`dvlb')^2
						}
					} /* end foreach dvlb in `depvarvalues'  */
					display "Summary variables: " "`summarize'"
					foreach iv in `summarize' {
						quietly gen v_`iv' = (`iv' - m_`iv')^2
						drop `iv'
						rename v_`iv' `iv'
					}
					drop m_*
					/* Collapsing it */
					collapse (count) n=simno (sum) d_logit* `summarize', by(`aggregationtext') fast
					quietly summ n
					local this_n = r(mean)
					local agg_n = `agg_n' + r(mean)
					display "This results batch has " `this_n' " simulations, aggregated there are " `agg_n' " simulations."
					mkmat *, matrix(currentvars) /* 9 columns: gwno, year, n plus all logit deviations from mean */
					
					matrix listcurrentv = currentvars[1..9,1..9]
	*				matrix list listcurrentv
	*/					
					matrix ns = J(`mrows',1,`agg_n') /* Matrix to hold n temporarily */
					matrix aggv[1,3]=ns /* And fill into the aggregated means matrix in the 3rd ("n") column  */
	*				display "before temp. mrows = " `mrows' ", vcols= " `vcols'
					matrix temp = aggv[1..`mrows',4..`vcols'] + currentvars[1..`mrows',4..`vcols'] /* Copy summed deviations into temporary matrix */
	*				dis "before aggv"
					matrix aggv[1,4]=temp /* And copy summed deviations into appropriate places in aggv matrix */
	*				dis "before listaggv"
					matrix listaggv = aggv[1..9,1..9]
	*				matrix list listaggv
	*/				
				} /* end if _rc==0 */
				else {	
					display "The file Res_`simtype'_`i'_`j'_`k'_`s'.dta does not exist"
				} /* end else */
				} /* end forvalues l = 1(1)`ln4' */
			} /* end forvalues k = 1(1)`ln3' */
			clear
			if matmissing(temp) == 1 {
				display "Missing elements in temp matrix}
			}
			
			if `agg_n' > 1 {
				matrix temp = aggv[1..`mrows',4..`vcols'] / (`agg_n'-1)
			}
			if `agg_n' == 1 {
				display "Warning: only one aggregated number of simulations (`agg_n')"
				matrix temp = aggv[1..`mrows',4..`vcols'] / 1 /* If `agg_n' == 1, variance is 0 and the discounting is irrelevant */
			}
			
			dis "here 2 "
		matrix aggcomb = aggm, temp
			dis "here 3 "
		svmat aggcomb, names(col)
		rename logit* v_logit*		
		gen `l1name' = `i'
		gen `l2name' = `j'
		local outdata = "`currdir'" + "/Results/" + "`simsetname'" + "/" + "Aggregated_c_`simtype'_`i'_`j'_x_x.dta"
		display "Saving aggregated results to: `outdata'"
		save "`outdata'", replace
*		} /* end if matmissing(aggm) == 0 */
	} /* end forvalues j = 1(1)`ln2' */
} /* end forvalues i = 1(1)`ln1' */

cd `currdir'


end 

