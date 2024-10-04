capture program drop oosevaluation4

program oosevaluation4
	version 12
    syntax namelist, simsetname(namelist) levels(numlist min=4 max=4 integer) compfile(string)
	/* 
	Program oosevaluation evaluates simulation results against observed data
	namelist: Variable names for the three levels in current simulation file
	simsetname: Name  set of simulations and of folder containing resultsfiles
	simtype: sim (simulation) or oos (for out-of-sample evaluation)
	Generates and fills a directory structure under the current directory: "Results/`inputfiles'"
	levels:		Number of levels for aggregation
	*/
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
	display "No. of values, level 3: " `ln3'
	display "No. of values, level 4: " `ln4'
	display "Location for results files:  `currdir'/Results/`simsetname'" 
	
	matrix define ROC = J(367,`ln2', 0)

	cd `path'
	dir
		forvalues i = 1(1)`ln1' {
		display "i: " `i'
		forvalues j = 1(1)`ln2' { /* Index for model number */
			display "j: " `j'
			forvalues k = 1(1)`ln3' {
				display "k: " `k'
				forvalues s= 1(1)`ln4' {
					display "s: " `s'
					capture confirm file "c_Res_oos_comp_`i'_`j'_`k'_`s'.dta"
					if _rc==0 {
						use "c_Res_oos_comp_`i'_`j'_`k'.dta", replace
						keep gwno year c_minor_m c_major_m c_phat1_m c_phat2_m
						matrix ROC[1,`j'] = `j'
						summ year
						local fyear = r(min)
						local lyear = r(max)
						matrix ROC[2,`j'] = `fyear'
						matrix ROC[3,`j'] = `lyear'
						
						merge 1:1 gwno year using "../../InputData/`compfile'", keepusing(conflict) gen(origmerge)
						keep if origmerge == 3

						drop origmerge
						gen sim = c_minor_m + c_major_m
						gen inc = conflict
						recode inc 2=1

						/* ons: If conflict starts */
						/* trm: If conflict ends */
						gen ons = 0
						gen trm = 0
						gen i10 = .
						replace ons = 1 if inc-inc[_n-1]==1 & gwno==gwno[_n-1]
						replace trm = 1 if inc-inc[_n-1]==-1 & gwno==gwno[_n-1]
						replace i10 = inc if year >= (`lyear'-3) & year <= `lyear'
						local obstype = 0
						local fpos = 3
						foreach obs in inc ons trm i10 {
							local fpos = 3 + `obstype'*(8*11+3) 
							roctab `obs' sim
							display "******** Comparing given estimation model: " "`j'" " ********"
							display "********* Comparing to observation type: " "`obs'" " *********"
							sleep 1000
							display "fpos for AUC for `obs': " `fpos'
							matrix ROC[`fpos'+1,`j'] = r(area)
							matrix ROC[`fpos'+2,`j'] = r(lb)
							matrix ROC[`fpos'+3,`j'] = r(ub)
							local ptype = 0
							foreach p in 0.005 0.01 0.015 0.025 0.05 0.075 0.10 0.20 0.35 0.50 0.75 {
								local fpos = 3 + `obstype'*(8*11+3) + `ptype'*8
								display " "
								display "******************************************************"
								display "******** Comparing given estimation model: " "`j'" " ********"
								display "Model: " `j' ", obstype: " `obstype' ", ptype: " "`p'" ", p: " `ptype' ", fpos: " `fpos'
								
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
								matrix ROC[`fpos'+4,`j'] = `TP'/(`TP'+`FN')
								/* Specificity */
								matrix ROC[`fpos'+5,`j'] =  `TN'/(`FP'+`TN') 
								/* Precision */
								matrix ROC[`fpos'+6,`j'] =  `TP'/(`TP'+`FP') 
								/* F-score */
								matrix ROC[`fpos'+7,`j'] = 2 * ((`TP'/(`TP'+`FP')*(`TP'/(`TP'+`FN')))/((`TP'/(`TP'+`FP'))+(`TP'/(`TP'+`FN'))))
								display "Sensitivity: " `TP'/(`TP'+`FN')
								display "Specificity: " `TN'/(`FP'+`TN') 
								display "Precision: " `TP'/(`TP'+`FP') 
								display "************** Brier function statistics **************"
								/* Brier score */
								brier `obs' disim
								matrix ROC[`fpos'+8,`j'] = r(brier) /* .... */
								local ptype = `ptype' + 1
							} /* end foreach p in 0.01 0.02 0.05 0.10 0.20 0.50 */
							local obstype = `obstype' + 1	
						} /* end foreach obs in inc ons trm */
						
					} /* end if _rc==0 */
					else {	
						display "The file c_Res_oos_comp_`i'_`j'_`k'.dta does not exist"
					} /* end else */
				} /* end forvalues s = 1(1)`ln4' */
			} /* end forvalues k = 1(1)`ln3' */
		} /* end forvalues j = 1(1)`ln2' */
	} /* end forvalues i = 1(1)`ln1' */

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

    save ROC_results.dta, replace
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
list Model inc_AUC ons_AUC trm_AUC AUC_all i10_AUC F_inc F_ons F_trm F_i10 F_all

	cd "`currdir'" /* Back to original directory */
	*/
end /* program oosevaluation */
