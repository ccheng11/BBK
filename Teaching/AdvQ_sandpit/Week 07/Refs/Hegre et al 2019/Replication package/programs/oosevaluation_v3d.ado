program oosevaluation_v3d
	version 11
    syntax namelist, simsetname(namelist) depmin(int) depmax(int) depvar(string) depvarvalues(int) ///
		levels(numlist min=2 max=2 integer) compfile(string) models(numlist) 
	
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
	
	local i10width = 4 /* How many years at the end of the sample to include in i10 evaluation */
	local invertchange = 0
	if "`depvar'" == "conflict" {
		local invertchange = 1 
		/* If `invertchange' is set to 1 the ROC results for the category are the same as if the other categories were merged */
	}
	
	local currdir = c(pwd) /* Name of current directory, to take us back to work directory */
	local path = "`currdir'" + "/Results/" + "`simsetname'"

	set more off
	local levelcount = wordcount("`namelist'")
	local modelcount = wordcount("`models'")
	forvalues l = 1(1)`levelcount' {
		local l`l'name = word("`namelist'",`l')
		display "Level `l' variable: " word("`namelist'",`l')
		local ln`l' = word("`levels'",`l')
	} /* end forvalues l */

	display "Folder: " "`path'"
	display "Levels: " "`levels'"
	display "No. of values, level 1: " `ln1'
	display "No. of values, level 2: " `ln2'
	display "No of models: " `modelcount'
	display "List of models: " `models'
	display "Location for results files:  `currdir'/Results/`simsetname'" 
	
	matrix define ROC = J(367,`ln2', 0)
	forvalues s = `depmin'(1)`depmax' { /* Looping over all values for outcome variable */
		foreach obs in inc_`s' ons_`s' trm_`s' i10_`s' { 
			foreach p in 005 01 025 05 075 10 20 35 50 75 100 {
				matrix define Summ_`obs'_`p' = J(`ln2',8,0)
				matrix colnames Summ_`obs'_`p' = Model Sensitivity Specificity Precision F-score Brier xx
	

			}
		}
	}
	matrix define Summary = J(`ln2', 10, 0)
	matrix define PropCorrect_l = J(`ln2',1,0)
	matrix define PropCorrect_a = J(`ln2',1,0)
	matrix define Average = J(15, 3, 0)

	cd `path'
	preserve
		use "../../`compfile'", clear
		drop if gwno ==. | year == .
		
	restore
*	dir 
	forvalues s = 2(1)`depmax' { /* Looping over all values for outcome variable */
*	forvalues s = `depmin'(1)`depmax' { /* Looping over all values for outcome variable */
		forvalues scenario = 1(1)`ln1' { /* Index for scenario number */
			display "Scenario: " `scenario'
			foreach model of local models { /* Index for model number */
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
					local fyear = r(min) + 1
					local lyear = r(max)
					drop if year > `lyear' | year < `fyear'
					matrix ROC[2,`model'] = `fyear'
					matrix ROC[3,`model'] = `lyear'                                
					drop if gwno == . | year == .
					merge 1:m gwno year using "../../`compfile'", keepusing(`depvar') gen(origmerge) keep(match)
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
						local fpos = 3 + `obstype'*(8*11+3) 
						roctab `obs' sim
						
						display "******** Comparing given estimation model: " "`model'" " ********"
						display "********* Comparing to observation type: " "`obs'" " *********"
						*sleep 1000
						display "fpos for AUC for `obs': " `fpos'
						matrix ROC[`fpos'+1,`model'] = r(area)
						matrix ROC[`fpos'+2,`model'] = r(lb)
						matrix ROC[`fpos'+3,`model'] = r(ub)
*						if "`obs'" == "i10_`s'" {
*							stop
*						}
						local ptype = 0
						foreach p in 0.005 0.01 0.025 0.05 0.075 0.10 0.20 0.35 0.50 0.75 1.00 {
							local pstring = substr(string(`p'),2,.)
							if `p' == 0.10 {
								local pstring = "10"
							}
							if `p' == 0.20 {
								local pstring = "20"
							}
							if `p' == 0.50 {
								local pstring = "50"
							}
							if `p' == 1.00 {
								local pstring = "100"
							}
							
							local fpos = 3 + `obstype'*(8*11+3) + `ptype'*8
							display " "
							display "**************************************************************"
							display "******** Comparing given estimation model: " `model' ", outcome " `s' " ********"
							display "Model: " `model' ", obstype: " `obstype' ", ptype: " "`p'" ", p: " `ptype' ", fpos: " `fpos'
							display "pstring: `pstring'"
							
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
							matrix Summ_`obs'_`pstring'[`model',1] = `model'
							/* Sensitivity/Recall */
							matrix ROC[`fpos'+4,`model'] = `TP'/(`TP'+`FN')
							matrix Summ_`obs'_`pstring'[`model',2] = `TP'/(`TP'+`FN')
							/* Specificity */
							matrix ROC[`fpos'+5,`model'] =  `TN'/(`FP'+`TN') 
							matrix Summ_`obs'_`pstring'[`model',3] = `TN'/(`FP'+`TN') 
							/* Precision */
							matrix ROC[`fpos'+6,`model'] =  `TP'/(`TP'+`FP') 
							matrix Summ_`obs'_`pstring'[`model',4] = `TP'/(`TP'+`FP') 
							/* F-score */
							matrix ROC[`fpos'+7,`model'] = 2 * ((`TP'/(`TP'+`FP')*(`TP'/(`TP'+`FN')))/((`TP'/(`TP'+`FP'))+(`TP'/(`TP'+`FN'))))
							matrix Summ_`obs'_`pstring'[`model',5] = 2 * ((`TP'/(`TP'+`FP')*(`TP'/(`TP'+`FN')))/((`TP'/(`TP'+`FP'))+(`TP'/(`TP'+`FN'))))
							display "Sensitivity: " `TP'/(`TP'+`FN')
							display "Specificity: " `TN'/(`FP'+`TN') 
							display "Precision: " `TP'/(`TP'+`FP') 
								display "************** Brier function statistics **************"
							/* Brier score */
							quietly brier `obs' disim
							matrix ROC[`fpos'+8,`model'] = r(brier) /* .... */
							matrix Summ_`obs'_`pstring'[`model',6] = r(brier)
							*local ptype = `ptype' + 1
							quietly brier `obs' sim
							matrix ROC[`fpos'+10,`model'] = r(brier) /* .... */
							local ptype = `ptype' + 1
						} /* end foreach p in 0.01 0.02 0.05 0.10 0.20 0.50 */
	
					local obstype = `obstype' + 1	
					} /* end foreach obs in inc ons trm */
/*	
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
						/* Last years of oos window */
						display "Checking: Depvar is `depvar', lyear is `lyear', i10width is `i10width', model is `model' "
						tab `depvar' k_class if year >= `lyear'-`i10width'-1, row col matcell(k_class_l_`model')
						matrix diag_l_`model' = vecdiag(k_class_l_`model')
						local propcorrect_l = (k_class_l_`model'[1,1] + k_class_l_`model'[2,2] + k_class_l_`model'[3,3]) / r(N)
						display "Proportion correct classifications, last years, model " `model' ", is " `propcorrect_l' "."
						matrix PropCorrect_l[`model',1] = `propcorrect_l'
						matrix list PropCorrect_l
						outtable using ClassificationTable_l_Model`model', mat(k_class_l_`model') replace ///
							caption("Classification table, last years, Model `model', simulation set `simsetname'") ///
							format(%9.0f %9.0f %9.0f) norowlab
						/* All years of oos window */
						tab `depvar' k_class if year >= `fyear', row col matcell(k_class_a_`model')
						matrix diag_a_`model' = vecdiag(k_class_a_`model')
						local propcorrect_a = (k_class_a_`model'[1,1] + k_class_a_`model'[2,2] + k_class_a_`model'[3,3]) / r(N)
						display "Proportion correct classifications, all years, model " `model' ", is " `propcorrect_a' "."
						matrix PropCorrect_a[`model',1] = `propcorrect_a'
						matrix list PropCorrect_a
						outtable using ClassificationTable_a_Model`model', mat(k_class_a_`model') replace ///
							caption("Classification table, all years, Model `model', simulation set `simsetname'") ///
							format(%9.0f %9.0f %9.0f) norowlab

					} /* end if s == `depmin'  */
*/
					*/
				} /* end if _rc==0 */
				else {	
					display "The file Aggregated_c_oos_`scenario'_`model'_x_x.dta does not exist"
				} /* end else */
		} /* end forvalues model = 1(1)`ln2' */
	} /* end forvalues scenario = 1(1)`ln1' */

	matrix ROCt = ROC'
*    preserve
	clear
	svmat ROCt
	rename ROCt1 Model
	rename ROCt2 From
	rename ROCt3 To
	local i = 4
	foreach obs in inc ons trm i10 { 
		rename ROCt`i' `obs'_AUC
		local i = `i' + 1
		rename ROCt`i' `obs'_AUC_l
		local i = `i' + 1
		rename ROCt`i' `obs'_AUC_h
		local i = `i' + 1
		foreach p in 005 01 025 05 075 10 20 35 50 75 100 {
			foreach var in Sensitivity Specificity Precision F_score Brier free1 free2 free3 {
				local varname = "`obs'_`p'_`var'"
				*display " ROCt`i': `varname'"
				rename ROCt`i' `varname'
				local i = `i' + 1
			} /* end foreach var */
		} /* foreach p */
	} /* foreach obs  */
	
	saveold ROC_results_`s'_o.dta, replace
 *   restore

capture drop F_*
foreach obs in inc ons trm i10 { 
	gen F_`obs' = 0
	foreach p in 005 01 025 05 075 10 20 35 50 75 100 { 
		summarize `obs'_`p'_F_score
		replace F_`obs' = F_`obs' + (`obs'_`p'_F_score-r(mean))/r(sd)
	} /* end foreach p */
	replace F_`obs' = F_`obs' / 11
} /* end foreach obs */
gen F_all = (F_inc + F_ons + F_trm + F_i10)/4
gen AUC_all = (inc_AUC + ons_AUC + trm_AUC + i10_AUC )/4

foreach p in 005 01 025 05 075 10 20 35 50 75 100 { 
	gen Brier_`p'_all = (inc_`p'_Brier + ons_`p'_Brier + trm_`p'_Brier + i10_`p'_Brier) / 4
}
gen Brier_all = (Brier_005_all + Brier_01_all + Brier_025_all + Brier_05_all + ///
	Brier_075_all + Brier_10_all + Brier_20_all + Brier_35_all + Brier_50_all + Brier_75_all + Brier_100_all) / 11
mkmat Brier_all, mat(Brier`s')
*replace AUC_all = ln((AUC_all/(1-AUC_all)) )
mkmat AUC_all, mat(AUC`s')


mkmat Model inc_AUC ons_AUC trm_AUC i10_AUC AUC_all ///
	i10_20_Sensitivity i10_20_Specificity i10_20_Precision Brier_all ///
	, matrix(oosresults)
	
	matrix colnames oosresults = Model inc_AUC ons_AUC trm_AUC i10_AUC AUC_all 	i1020Sens i1020Spec i1020Prec Brier_all 
	
/*	foreach outc in inc i10 {
		foreach level in 20 50 {
			rename `outc'_`level'_Sensitivity `outc'`level'Sens
			rename `outc'_`level'_Specificity `outc'`level'Spec
			rename `outc'_`level'_Precision `outc'`level'Prec
		}
	} */
outtable using OOSresults_`s'_o, mat(oosresults) replace ///
	caption("Results of OOS evaluation, state `s' vs. others, simulation set `simsetname'") ///
	format(%9.0f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f ) norowlab
*/
preserve
keep Model inc_AUC ons_AUC trm_AUC i10_AUC AUC_all ///
	i10_20_Sensitivity i10_20_Specificity i10_20_Precision Brier_all
export delimited using "OOSState`s'" , delim(",") replace
restore


*	dis "HERE: `s'"
	matrix Summary[1,4+`s'] = oosresults[1...,5]
	matrix Summary[1,7+`s'] = oosresults[1...,2]
	
	matrix colnames Summary = Model PropCorrLastyrs PropCorrAllyrs i10AUC0 i10AUC1 i10AUC2 AUC0 AUC1 AUC2  j
	

} /*END forvalues s = `depmin'(1)`depmax'  */

	matrix Summary[1,1] = oosresults[1...,1]
	matrix Summary[1,2] = PropCorrect_l
	matrix Summary[1,3] = PropCorrect_a
	outtable using Summary, mat(Summary) replace ///
	caption("Results of OOS evaluation, simulation set `simsetname'") ///
	format(%9.0f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f ) norowlab

	matrix Brier = (Brier0 + Brier1 + Brier2) / 3
	matrix AUC = (AUC0 + AUC1 + AUC2)/3
	matrix Average = Brier , AUC
	outtable using Average, mat(Average) replace
	preserve
	clear 
	svmat Average
	ren Average1 Brier
	ren Average2 AUC
	export delimited using "Average" , delim(",") replace
	restore
	
	cd "`currdir'" /* Back to original directory */
	
	*/
end /* program oosevaluation */
