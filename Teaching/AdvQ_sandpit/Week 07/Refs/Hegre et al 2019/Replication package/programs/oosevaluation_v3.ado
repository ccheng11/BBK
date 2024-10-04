program oosevaluation_v3
	version 11
    syntax namelist, simsetname(namelist) depmin(int) depmax(int) depvar(string) depvarvalues(int) ///
		levels(numlist min=2 max=2 integer) compfile(string)
	
	/* 
	Program oosevaluation evaluates simulation results against observed data
	namelist: Variable names for the three levels in current simulation file
	simsetname: Name  set of simulations and of folder containing resultsfiles
	simtype: sim (simulation) or oos (for out-of-sample evaluation)
	Generates and fills a directory structure under the current directory: "Results/`inputfiles'"
	levels:		Number of levels for aggregation
	depmin : minimum value of dependent variable
	depmax : maximum value of dependent variable
	*/
	
	local i10width = 3 /* How many years at the end of the sample to include in i10 evaluation */
	local invertchange = 0
	if "`depvar'" == "conflict" {
		local invertchange = 1 
		/* If `invertchange' is set to 1 the ROC results for the category are the same as if the other categories were merged */
	}
	
	local currdir = c(pwd) /* Name of current directory, to take us back to work directory */
	local path = "`currdir'" + "/Results/" + "`simsetname'"

	set more off
	local levelcount = wordcount("`namelist'")
	
	forvalues l = 1(1)`levelcount' {
		local l`l'name = word("`namelist'",`l')
		display "Level `l' variable: " word("`namelist'",`l')
		local ln`l' = word("`levels'",`l')
	} /* end forvalues l */

	display "Folder: " "`path'"
	display "Levels: " "`levels'"
	display "No. of values, level 1: " `ln1'
	display "No. of values, level 2: " `ln2'
	display "Location for results files:  `currdir'/Results/`simsetname'" 
	
	matrix define ROC = J(367,`ln2', 0)
	matrix define PropCorrect = J(`ln2',1,0)

	cd `path'
	preserve
		use "../../`compfile'", clear
		drop if gwno ==. | year == .
	restore
*	dir 
	forvalues s = `depmin'(1)`depmax' { /* Looping over all values for outcome variable */
		forvalues scenario = 1(1)`ln1' { /* Index for scenario number */
			display "Scenario: " `scenario'
			forvalues model = 1(1)`ln2' { /* Index for model number */
				display "Model: " `model'
				capture confirm file "Aggregated_c_oos_`scenario'_`model'_x_x.dta"
				if _rc==0 {
					use "Aggregated_c_oos_`scenario'_`model'_x_x.dta", replace
					keep gwno year m_phat* 
					forvalues outcome = `depmin'(1)`depmax' { /* Looping over all values for outcome variable */
						ren m_phat`outcome' t_phat`outcome'
						gen m_phat`outcome' = t_phat`outcome'[_n-1] if gwno==gwno[_n-1] /* Move predicted probability to outcome year */
						drop t_phat`outcome'
					}
					matrix ROC[1,`model'] = `model'
					summ year
					local fyear = r(min)
					local lyear = r(max)
					matrix ROC[2,`model'] = `fyear'
					matrix ROC[3,`model'] = `lyear'
					
					merge 1:1 gwno year using "../../`compfile'", keepusing(`depvar') gen(origmerge)
					keep if origmerge == 3
					/* Error checking: Definition of dependent variable */
					summarize `depvar'
					local dmin = r(min)
					local dmax = r(max)
					local dep2 = `dmin' + 1
					display "Dependent variable " "`depvar'" " stated to have " `depvarvalues' " values. Minimum value in data: " `dmin' ". Maximum: " `dmax' "."
					display "Currently checking output variable state " `s'
					if `dmin' != 0 | `dmax'-`depvarvalues' !=-1 {
						display "Illegal values for dependent variable"
					}
					drop origmerge
					gen sim = m_phat`s' 
					gen inc_`s' = `depvar' == `s'
					gen invinc_`s' = 1 - inc_`s'
					tab inc_`s'
					/* inc: Incidence of state `s' */
					/* ons: If state `s' starts */
					/* trm: If state `s' ends */
					gen ons_`s' = 0
					gen trm_`s' = 0
					gen i10_`s' = .
					replace ons_`s' = 1 if inc_`s'-inc_`s'[_n-1]==1 & gwno==gwno[_n-1]
					replace trm_`s' = 1 if inc_`s'-inc_`s'[_n-1]==-1 & gwno==gwno[_n-1]
					replace i10_`s' = inc_`s' if year >= (`lyear'-`i10width'-1) & year <= `lyear'
					local obstype = 0
					saveold ROCTEST_c_oos_`scenario'_`model'_x_x.dta, replace
					local fpos = 3 /* Local to place item in appropriate matrix row */
					foreach obs in inc_`s' ons_`s' trm_`s' i10_`s' { 
						local fpos = 3 + `obstype'*(8*12+3) 
						roctab `obs' sim
						
						display "******** Comparing given estimation model: " "`model'" " ********"
						display "********* Comparing to observation type: " "`obs'" " *********"
						*sleep 1000
						display "fpos for AUC for `obs': " `fpos'
						matrix ROC[`fpos'+1,`model'] = r(area)
						matrix ROC[`fpos'+2,`model'] = r(lb)
						matrix ROC[`fpos'+3,`model'] = r(ub)
						local ptype = 0
						foreach p in 0.005 0.01 0.015 0.025 0.05 0.075 0.10 0.20 0.35 0.50 0.75 1.00 {
							local fpos = 3 + `obstype'*(8*12+3) + `ptype'*8
							display " "
							display "**************************************************************"
							display "******** Comparing given estimation model: " `model' ", outcome " `s' " ********"
							display "Model: " `model' ", obstype: " `obstype' ", ptype: " "`p'" ", p: " `ptype' ", fpos: " `fpos'
							
							/* TP, FP, TN, FN at different thresholds */
							capture drop disim
							gen disim = 0 
							replace disim = 1 if sim > `p'
							tab disim `obs'
							quietly summarize year if disim == 1 & `obs' == 1
							local TP = r(N)
							quietly summarize year if disim == 1 & `obs' == 0
							local FP = r(N)
							quietly summarize year if disim == 0 & `obs' == 0
							local TN = r(N)
							quietly summarize year if disim == 0 & `obs' == 1
							local FN = r(N)
							display "True positives: " `TP' , "false positives: " `FP' ", true negatives: " `TN', ", false negatives: " `FN' "."
							/* Sensitivity/Recall */
							matrix ROC[`fpos'+4,`model'] = `TP'/(`TP'+`FN')
							/* Specificity */
							matrix ROC[`fpos'+5,`model'] =  `TN'/(`FP'+`TN') 
							/* Precision */
							matrix ROC[`fpos'+6,`model'] =  `TP'/(`TP'+`FP') 
							/* F-score */
							matrix ROC[`fpos'+7,`model'] = 2 * ((`TP'/(`TP'+`FP')*(`TP'/(`TP'+`FN')))/((`TP'/(`TP'+`FP'))+(`TP'/(`TP'+`FN'))))
							display "Sensitivity: " `TP'/(`TP'+`FN')
							display "Specificity: " `TN'/(`FP'+`TN') 
							display "Precision: " `TP'/(`TP'+`FP') 
								display "************** Brier function statistics **************"
							/* Brier score */
							quietly brier `obs' disim
							matrix ROC[`fpos'+8,`model'] = r(brier) /* .... */
							local ptype = `ptype' + 1
						} /* end foreach p in 0.01 0.02 0.05 0.10 0.20 0.50 */
	
					local obstype = `obstype' + 1	
					} /* end foreach obs in inc ons trm */
	/*  */
					if `s' == `depmin' { /* Only done once for each scenario and model */
						display " "
						display "**************************************************************"
						display "*** Creating k-category classification table for last year ***"
						display "******** using modal prediction probability ********"
						display "******** Comparing given estimation model: " `model' " ********"
						label define outcome 0 "No conflict" 1 "Minor" 2 "Major"
						label values `depvar' outcome
						gen k_class = `depmin'
						gen temp = max(m_phat0, m_phat1, m_phat2) /* NOTE: needs generalization */
						forvalues k = `dep2'(1)`depmax' {
							replace k_class = `k' if m_phat`k' == temp
						}
						label values k_class outcome
						tab `depvar' k_class if year >= `lyear'-`i10width'-1, row col matcell(k_class_`model')
					*	matrix diag_`model' = vecdiag(k_class_`model')
						local propcorrect = (k_class_`model'[1,1] + k_class_`model'[2,2] + k_class_`model'[3,3]) / r(N)
						display "Proportion correct classifications, model " `model' ", is " `propcorrect' "."
					*	matrix PropCorrect[`model',1] = `propcorrect'
					*	matrix list PropCorrect
						outtable using ClassificationTable_Model`model', mat(k_class_`model') replace ///
							caption("Classification table, Model `model', simulation set `simsetname'") ///
							format(%9.0f %9.0f %9.0f) norowlab

					} /* end if s == `depmin'  */
				} /* end if _rc==0 */
				else {	
					display "The file Aggregated_c_oos_`scenario'_`model'_x_x.dta does not exist"
				} /* end else */
		} /* end forvalues model = 1(1)`ln2' */
	} /* end forvalues scenario = 1(1)`ln1' */
/*
	matrix ROCt = ROC'
*    preserve
	clear
	svmat ROCt
	rename ROCt# (Model From To inc_AUC inc_AUC_l inc_AUC_h ///
		inc_005_Sensitivity inc_005_Specificity inc_005_Precision inc_005_F_score inc_005_Brier inc_005_free1 inc_005_free2 inc_005_free3 ///
		inc_01_Sensitivity inc_01_Specificity inc_01_Precision inc_01_F_score inc_01_Brier inc_01_free1 inc_01_free2 inc_01_free3 ///
		inc_015_Sensitivity inc_015_Specificity inc_015_Precision inc_015_F_score inc_015_Brier inc_015_free1 inc_015_free2 inc_015_free3 ///
		inc_025_Sensitivity inc_025_Specificity inc_025_Precision inc_025_F_score inc_025_Brier inc_025_free1 inc_025_free2 inc_025_free3 ///
		inc_05_Sensitivity inc_05_Specificity inc_05_Precision inc_05_F_score inc_05_Brier inc_05_free1 inc_05_free2 inc_05_free3 ///
		inc_075_Sensitivity inc_075_Specificity inc_075_Precision inc_075_F_score inc_075_Brier inc_075_free1 inc_075_free2 inc_075_free3 ///
		inc_10_Sensitivity inc_10_Specificity inc_10_Precision inc_10_F_score inc_10_Brier inc_10_free1 inc_10_free2 inc_10_free3 ///
		inc_20_Sensitivity inc_20_Specificity inc_20_Precision inc_20_F_score inc_20_Brier inc_20_free1 inc_20_free2 inc_20_free3 ///
		inc_35_Sensitivity inc_35_Specificity inc_35_Precision inc_35_F_score inc_35_Brier inc_35_free1 inc_35_free2 inc_35_free3 ///
		inc_50_Sensitivity inc_50_Specificity inc_50_Precision inc_50_F_score inc_50_Brier inc_50_free1 inc_50_free2 inc_50_free3 ///
		inc_75_Sensitivity inc_75_Specificity inc_75_Precision inc_75_F_score inc_75_Brier inc_75_free1 inc_75_free2 inc_75_free3 ///
		ons_AUC ons_AUC_l ons_AUC_h ///
		ons_005_Sensitivity ons_005_Specificity ons_005_Precision ons_005_F_score ons_005_Brier ons_005_free1 ons_005_free2 ons_005_free3 ///
		ons_01_Sensitivity ons_01_Specificity ons_01_Precision ons_01_F_score ons_01_Brier ons_01_free1 ons_01_free2 ons_01_free3 ///
		ons_015_Sensitivity ons_015_Specificity ons_015_Precision ons_015_F_score ons_015_Brier ons_015_free1 ons_015_free2 ons_015_free3 ///
		ons_025_Sensitivity ons_025_Specificity ons_025_Precision ons_025_F_score ons_025_Brier ons_025_free1 ons_025_free2 ons_025_free3 ///
		ons_05_Sensitivity ons_05_Specificity ons_05_Precision ons_05_F_score ons_05_Brier ons_05_free1 ons_05_free2 ons_05_free3 ///
		ons_075_Sensitivity ons_075_Specificity ons_075_Precision ons_075_F_score ons_075_Brier ons_075_free1 ons_075_free2 ons_075_free3 ///
		ons_10_Sensitivity ons_10_Specificity ons_10_Precision ons_10_F_score ons_10_Brier ons_10_free1 ons_10_free2 ons_10_free3 ///
		ons_20_Sensitivity ons_20_Specificity ons_20_Precision ons_20_F_score ons_20_Brier ons_20_free1 ons_20_free2 ons_20_free3 ///
		ons_35_Sensitivity ons_35_Specificity ons_35_Precision ons_35_F_score ons_35_Brier ons_35_free1 ons_35_free2 ons_35_free3 ///
		ons_50_Sensitivity ons_50_Specificity ons_50_Precision ons_50_F_score ons_50_Brier ons_50_free1 ons_50_free2 ons_50_free3 ///
		ons_75_Sensitivity ons_75_Specificity ons_75_Precision ons_75_F_score ons_75_Brier ons_75_free1 ons_75_free2 ons_75_free3 ///
		trm_AUC trm_AUC_l trm_AUC_h ///
		trm_005_Sensitivity trm_005_Specificity trm_005_Precision trm_005_F_score trm_005_Brier trm_005_free1 trm_005_free2 trm_005_free3 ///
		trm_01_Sensitivity trm_01_Specificity trm_01_Precision trm_01_F_score trm_01_Brier trm_01_free1 trm_01_free2 trm_01_free3 ///
		trm_015_Sensitivity trm_015_Specificity trm_015_Precision trm_015_F_score trm_015_Brier trm_015_free1 trm_015_free2 trm_015_free3 ///
		trm_025_Sensitivity trm_025_Specificity trm_025_Precision trm_025_F_score trm_025_Brier trm_025_free1 trm_025_free2 trm_025_free3 ///
		trm_05_Sensitivity trm_05_Specificity trm_05_Precision trm_05_F_score trm_05_Brier trm_05_free1 trm_05_free2 trm_05_free3 ///
		trm_075_Sensitivity trm_075_Specificity trm_075_Precision trm_075_F_score trm_075_Brier trm_075_free1 trm_075_free2 trm_075_free3 ///
		trm_10_Sensitivity trm_10_Specificity trm_10_Precision trm_10_F_score trm_10_Brier trm_10_free1 trm_10_free2 trm_10_free3 ///
		trm_20_Sensitivity trm_20_Specificity trm_20_Precision trm_20_F_score trm_20_Brier trm_20_free1 trm_20_free2 trm_20_free3 ///
		trm_35_Sensitivity trm_35_Specificity trm_35_Precision trm_35_F_score trm_35_Brier trm_35_free1 trm_35_free2 trm_35_free3 ///
		trm_50_Sensitivity trm_50_Specificity trm_50_Precision trm_50_F_score trm_50_Brier trm_50_free1 trm_50_free2 trm_50_free3 ///
		trm_75_Sensitivity trm_75_Specificity trm_75_Precision trm_75_F_score trm_75_Brier trm_75_free1 trm_75_free2 trm_75_free3 ///
		i10_AUC i10_AUC_l i10_AUC_h ///		
		i10_005_Sensitivity i10_005_Specificity i10_005_Precision i10_005_F_score i10_005_Brier i10_005_free1 i10_005_free2 i10_005_free3 ///
		i10_01_Sensitivity i10_01_Specificity i10_01_Precision i10_01_F_score i10_01_Brier i10_01_free1 i10_01_free2 i10_01_free3 ///
		i10_015_Sensitivity i10_015_Specificity i10_015_Precision i10_015_F_score i10_015_Brier i10_015_free1 i10_015_free2 i10_015_free3 ///
		i10_025_Sensitivity i10_025_Specificity i10_025_Precision i10_025_F_score i10_025_Brier i10_025_free1 i10_025_free2 i10_025_free3 ///
		i10_05_Sensitivity i10_05_Specificity i10_05_Precision i10_05_F_score i10_05_Brier i10_05_free1 i10_05_free2 i10_05_free3 ///
		i10_075_Sensitivity i10_075_Specificity i10_075_Precision i10_075_F_score i10_075_Brier i10_075_free1 i10_075_free2 i10_075_free3 ///
		i10_10_Sensitivity i10_10_Specificity i10_10_Precision i10_10_F_score i10_10_Brier i10_10_free1 i10_10_free2 i10_10_free3 ///
		i10_20_Sensitivity i10_20_Specificity i10_20_Precision i10_20_F_score i10_20_Brier i10_20_free1 i10_20_free2 i10_20_free3 ///
		i10_35_Sensitivity i10_35_Specificity i10_35_Precision i10_35_F_score i10_35_Brier i10_35_free1 i10_35_free2 i10_35_free3 ///
		i10_50_Sensitivity i10_50_Specificity i10_50_Precision i10_50_F_score i10_50_Brier i10_50_free1 i10_50_free2 i10_50_free3 ///
		i10_75_Sensitivity i10_75_Specificity i10_75_Precision i10_75_F_score i10_75_Brier i10_75_free1 i10_75_free2 i10_75_free3 )

	saveold ROC_results_`s'_o.dta, replace
 *   restore

capture drop F_*
foreach obs in inc ons trm i10 { 
	gen F_`obs' = 0
	foreach p in 005 01 015 025 05 075 10 20 35 50 75 { 
		summarize `obs'_`p'_F_score
		replace F_`obs' = F_`obs' + (`obs'_`p'_F_score-r(mean))/r(sd)
	} /* end foreach p */
	replace F_`obs' = F_`obs' / 11
} /* end foreach obs */
gen F_all = (F_inc + F_ons + F_trm + F_i10)/4
gen AUC_all = (inc_AUC + ons_AUC + trm_AUC + i10_AUC )/4
list Model inc_AUC ons_AUC trm_AUC i10_AUC AUC_all F_inc F_ons F_trm F_i10 F_all
mkmat Model inc_AUC ons_AUC trm_AUC i10_AUC AUC_all F_inc F_ons F_trm F_i10 F_all, matrix(oosresults)
outtable using OOSresults_`s'_o, mat(oosresults) replace ///
	caption("Results of OOS evaluation, state `s' vs. others, simulation set `simsetname'") ///
	format(%9.0f %9.3f %9.3f %9.3f %9.3f %9.3f %9.2f %9.2f %9.2f %9.2f %9.2f) norowlab
*/
} /*END forvalues s = `depmin'(1)`depmax'  */

	cd "`currdir'" /* Back to original directory */
	*/
end /* program oosevaluation */
