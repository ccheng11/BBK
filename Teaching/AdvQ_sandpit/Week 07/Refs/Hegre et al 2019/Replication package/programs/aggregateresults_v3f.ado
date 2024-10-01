program aggregateresults_v3f
	version 12
    syntax namelist, simsetname(namelist) depvar(string) depvarvalues(int) simtype(namelist) ///
		COLLapseover(int) levels(numlist min=4 max=4 integer) path(string) fyear(int) lyear(int) /// 
		[summarize(namelist)] 

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
	local regnos = 11 /* Number of artificial countries corresponding to regions; excluding 'globally' */
	local path = "`path'"  + "`simsetname'" /* this is the local psath; not the dropbox path */
	
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
	local yearnos = `lyear' - `fyear' + 1
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
				display "Preparing aggregation matrix agg`summtype': Reading in file Res_`simtype'_1_1_1_1.txt... (namelist: `namelist')"
				insheet using "Res_`simtype'_1_1_1_1.txt", names clear
				keep if simno == 1
				*drop if year == `sim_lyear'
				keep `aggregationtext' `depvar'
				* summa
				/* New version; expanding for regional and global aggregates */
				local observations = _N
				if `i' == 1 & `j' == 1 { /* Create an appendage dataset containing pseudo-countries for regional and global aggregates */
					preserve 
					clear
					local newobs = _N + ((`regnos'+1) * `yearnos')
					set obs `newobs'
					gen simno = 1
					gen gwno = .
					gen year = .
					gen `depvar' = 0
					forvalues reg = 1(1)`regnos' {
						local thisregno = 1000 + `reg'
						quietly replace gwno = `thisregno' if ///
							_n > ((`reg'-1)*`yearnos') & _n <= ((`reg')*`yearnos')
						forvalues y =`fyear'(1)`lyear' {
							quietly replace year = `y' if _n == ((`reg'-1)*`yearnos') + (`y' - `fyear' + 1)
						}
					}
					quietly replace gwno = 1100 if _n > (`regnos'*`yearnos') ///
						& _n <= ((`regnos'+1)*`yearnos') 
					quietly forvalues y =`fyear'(1)`lyear' {
						replace year = `y' if _n == (`regnos'*`yearnos') + (`y' - `fyear' + 1)
					}
					recast int gwno 
					recast int year 
					recast byte `depvar' 
					recast byte simno
					save Res_append.dta, replace
					restore
				} /*  end if `i' == 1 & `j' == 1  */
				append using Res_append.dta, keep(`aggregationtext' `depvar')
				/* End of expansion section */
				
				/* Error checking */
				/* Definition of dependent variable */
				quietly summarize `depvar'
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
				/* Prepares for storing simulated outcome */
*				if "`summtype'" == "m" {
					forvalues dvlb = `depmin'(1)`depmax' {
						quietly gen sim_`dvlb' = 0 
					}
*				} /* end if `summtype' == "m" */
				forvalues dvlb = `depmin'(1)`depmax' {
					quietly gen phat`dvlb' = 0
				}
				forvalues dvlb = `depmin'(1)`depmax' {
					quietly gen logit`dvlb'_o = 0
				}
				forvalues dvlb = `depmin'(1)`depmax' {
					local next = `dvlb' + 1
					forvalues dvlc = `next'(1)`depmax' { /* Prepares for aggregating logits for all pairwise comparisons */
						quietly gen logit`dvlc'_`dvlb' = 0
					}
				}
				
				/* 
				Generate matrices to store average and variance across K's and S's.
				Due to matrix size limitations, we must make one matrix for each year.
				*/
				foreach summtype in "m" "v" {
					display "Summary variables: " "`summarize'"
					foreach iv in `summarize' {
						quietly gen `summtype'_`iv' = 0
					}
					forvalues num = 1(1)`yearnos'{
						preserve
						if("`summtype'" == "m"){
							quietly drop logit*
						} 
						else {
							quietly drop phat*
						}
						quietly drop if year != `fyear'+`num'-1
						mkmat *, matrix(agg`summtype'`num') /* Matrix to maintain aggregated means for every country year. */
						local `summtype'rows = rowsof(agg`summtype'`num')
						local `summtype'cols = colsof(agg`summtype'`num')
						*display "Cols in matrix agg`summtype'`num': " `mcols'
						restore
					}
					foreach iv in `summarize' {
						quietly drop `summtype'_`iv'
					}
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
					
					/* New version; Expanding for regional and global aggregates */
					quietly summarize simno
					local sims = r(max)
					
					local proc_obs = _N
					forvalues sim = 1(1)`sims' { /* Append the regional/global placeholder dataset once for each simulation */
						append using Res_append.dta, keep(simno `aggregationtext' `depvar')
						quietly replace simno = `sim' if _n > `proc_obs' 
						local proc_obs = `proc_obs' +  ((`regnos'+1) * `yearnos')
					}
					capture drop _merge
					quietly merge m:1 gwno using Regions.dta, keepusing(region)
					drop _merge
					forvalues reg = 1(1)`regnos' {
							quietly replace region = `reg' if gwno == `reg' + 1000
					}
					
					foreach var in sim_0 sim_1 sim_2 phat0 phat1 phat2  { /* (removed lgdpcap) Needs generalization */
						quietly egen reg_`var' = mean(`var'), by(simno region year) 
						quietly egen glob_`var' = mean(`var'), by(simno year) 
						forvalues reg = 1(1)`regnos' {
							local gtemp = `reg' + 1000
							quietly replace `var' = reg_`var' if gwno == `gtemp'
						}
						quietly replace `var' = glob_`var' if gwno == 1100
					}
					
					
					/* Generate logit variables for regions (cannot average logits above) */
					quietly replace logit0_o = ln(phat0/(1-phat0)) if gwno>=1000
					forvalues dvlb = `dep2'(1)`depmax' {
						quietly replace logit`dvlb'_o = ln(phat`dvlb'/(1-phat`dvlb')) if gwno>=1000
					} /* end foreach dvlb in `depvarvalues'  */
					forvalues dvlb = `depmin'(1)`depmax' {
						local next = `dvlb' + 1
						forvalues dvlc = `next'(1)`depmax' { /* Prepares for aggregating logits for all pairwise comparisons */
							quietly replace logit`dvlc'_`dvlb' = ln(phat`dvlc'/phat`dvlb') if gwno>=1000
						}
					} /* end foreach dvlb in `depvarvalues'  */		
					
					sort simno gwno year
					*stop
					/* End of expansion section */

					
					quietly save Res_`simtype'_`i'_`j'_`k'_`s'.dta, replace
					
					/* Collapsing it */
					collapse (count) n=simno (mean) sim_* phat* `summarize', by(`aggregationtext') fast
					quietly summ n
					local this_n = r(mean)
					local agg_n = `agg_n' + `this_n'					
					display "This results batch has " `this_n' " simulations, aggregated there are " `agg_n' " simulations."
					
					/* Due to matrix size limitations, we must make one matrix for each year */
					forvalues num = 1(1)`yearnos'{
						preserve
						quietly drop if year != `fyear'+`num'-1
						mkmat *, matrix(currentmeans)
						matrix listcurrentm = currentmeans[1..9,1..9]
						matrix ns = J(`mrows',1,`agg_n') /* Matrix to hold n temporarily */
						matrix aggm`num'[1,3]=ns /* And fill into the aggregated means matrix in the 3rd ("n") column  */
						matrix temp = aggm`num'[1..`mrows',4..`mcols']*((`agg_n'-`this_n')/`agg_n') + currentmeans[1..`mrows',4..`mcols']*(`this_n'/`agg_n')
						matrix aggm`num'[1,4]=temp
						*matrix listaggm = aggm`num'[1..9,1..9] /* I do not think this is doing anything. Delete? */
						restore
					}
				} /* end if _rc==0 */
				else {	
					display "The file Res_`simtype'_`i'_`j'_`k'_`s'.txt does not exist"
				} /* end else */
			} /* end forvalues l = 1(1)`ln4' */
		} /* end forvalues k = 1(1)`ln3' */

		/* Aggregate all aggm matrices into one large dataset */
		clear
		quietly svmat aggm1, names(col)
		quietly save aggm.dta, replace
		forvalues num = 2(1)`yearnos'{
			clear
			quietly svmat aggm`num', names(col)
			quietly append using aggm.dta
			quietly save aggm.dta, replace
		}
		
		/* Generate logit variables */
		quietly gen m_logit0_o = ln(phat0/(1-phat0))
		forvalues dvlb = `dep2'(1)`depmax' {
			quietly gen m_logit`dvlb'_o = ln(phat`dvlb'/(1-phat`dvlb'))
		} /* end foreach dvlb in `depvarvalues'  */
		forvalues dvlb = `depmin'(1)`depmax' {
			local next = `dvlb' + 1
			forvalues dvlc = `next'(1)`depmax' { /* Prepares for aggregating logits for all pairwise comparisons */
				quietly gen m_logit`dvlc'_`dvlb' = ln(phat`dvlc'/phat`dvlb')
			}
		} /* end foreach dvlb in `depvarvalues'  */		
		
		rename phat* m_phat*
		rename sim_* m_sim_*

		local outdata = "`currdir'" + "/Results/" + "`simsetname'" + "/" + "Aggmeans_`simtype'_`i'_`j'_x_x.dta"
		display "Saving aggregated means to: `outdata'"
		quietly save "`outdata'", replace


		display " "
		display "************************************************************************"
		display "Aggregating results for " "`l1name'" " " `i' ", " "`l2name'" " " `j' ", second pass (variance)."
		display "************************************************************************"
		
		local indata = "`currdir'" + "/Results/" + "`simsetname'" + "/" + "Aggmeans_`simtype'_`i'_`j'_x_x.dta"

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
					quietly gen d_sim_`dvlb' = (sim_`dvlb' - m_sim_`dvlb')^2
				} /* end foreach dvlb in `depvarvalues'  */
				display "Summary variables: " "`summarize'"
				foreach iv in `summarize' {
					quietly gen v_`iv' = (`iv' - m_`iv')^2
					quietly drop `iv'
					rename v_`iv' `iv'
				}
				quietly drop m_*

				/* Collapsing it */
				collapse (count) n=simno (sum) d_logit* d_sim_* `summarize', by(`aggregationtext') fast
					
				/* Due to matrix size limitations, we must make one matrix for each year */
				forvalues num = 1(1)`yearnos'{
					preserve
					quietly drop if year != `fyear'+`num'-1	
					mkmat *, matrix(currentvars) /* 9 columns: gwno, year, n plus all logit deviations from mean */
									
					matrix listcurrentv = currentvars[1..9,1..9]
					matrix ns = J(`mrows',1,`agg_n') /* Matrix to hold n temporarily */
					matrix aggv`num'[1,3]=ns /* And fill into the aggregated means matrix in the 3rd ("n") column  */
					matrix temp = aggv`num'[1..`mrows',4..`vcols'] + currentvars[1..`mrows',4..`vcols'] /* Copy summed deviations into temporary matrix */
					if matmissing(temp) == 1 {
						display "Missing elements in temp matrix"}
					}
					matrix aggv`num'[1,4]=temp /* And copy summed deviations into appropriate places in aggv matrix */
					*matrix listaggv = aggv[1..9,1..9] /* I do not think this is doing anything. Delete? */
					restore
				}
			} /* end if _rc==0 */
			else {	
				display "The file Res_`simtype'_`i'_`j'_`k'_`s'.dta does not exist"
			} /* end else */
			} /* end forvalues l = 1(1)`ln4' */
		} /* end forvalues k = 1(1)`ln3' */
		
		/* Aggregate all aggv matrices into one large dataset */
		clear
		quietly svmat aggv1, names(col)
		quietly save aggv.dta, replace
		forvalues num = 2(1)`yearnos'{
			clear
			quietly svmat aggv`num', names(col)
			quietly append using aggv.dta
			quietly save aggv.dta, replace
		}

		
		
		if `agg_n' > 1 {
			/* Then we can calculate variance by dividing summed deviance with n-1 */
			foreach var of varlist logit* sim*{
				replace `var' = `var' / (`agg_n'-1)
			}
			foreach var in `summarize'{
				replace v_`var' = v_`var' / (`agg_n'-1)
			}
		}
		if `agg_n' == 1 {
			/* Then we cannot calculate variance. Set to zero */
			display "Warning: only one aggregated number of simulations (`agg_n')"
			foreach var of varlist logit* sim*{
				replace `var' = 0
			}	
			foreach var in `summarize'{
				replace v_`var' = 0
			}	
		}
		if `agg_n' == 0 {
			display "Zero observations recorded. Please check for errors."
			stop
		}

		rename logit* v_logit*		
		rename sim_* v_sim_*
		
		local aggmeansdata = "`currdir'" + "/Results/" + "`simsetname'" + "/" + "Aggmeans_`simtype'_`i'_`j'_x_x.dta"
		merge 1:1 `aggregationtext' using "`aggmeansdata'"
		
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

