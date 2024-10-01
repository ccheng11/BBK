
capture cd "/Users/Havard/Dropbox/Collaborations/PKO/Paper/Replication package"
capture cd PKO
*capture net install grc1leg
set more off
*ssc inst _gwtmean
local lyear = 2013
local fyear = 1970
local ma = 1 /* moving average bandwidth (1 means averaging over t-1, t, t+1) */
local fScenario = 1 /* */
local lScenario = 7 /* */
local fsimyear = 2001
local lsimyear = 2013
local regnos = 11
local yearnos = `lyear' - `fyear' + 1
local simsetname = "PKOsimv13"
local sumcountries = 171

use "Sim/Program/estimationdata.dta", clear
keep if year >= `fyear' & year <= `lyear'

keep gwno year conflict PKOtrad PKOtrans lPKObudget PKObudget lGDPcap 

local observations = _N
		preserve 
		clear
		local newobs = _N + ((`regnos'+1) * `yearnos')
		set obs `newobs'
		gen gwno = .
		gen year = .
		gen conflict = 0
		gen PKOtrad = 0 
		gen PKOtrans = 0
		gen lPKObudget = 0 
		gen PKObudget = 0
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
		save InputData/Res_append.dta, replace
		restore
append using "InputData/Res_append.dta"
sort gwno year
merge m:1 gwno using InputData/Regions.dta, keepusing(region)
drop _merge
	forvalues reg = 1(1)`regnos' {
		quietly replace region = `reg' if gwno == `reg' + 1000
}
ren PKOtrad pkotrad
ren PKOtrans pkotrans
ren lPKObudget lpkobudget 
ren PKObudget pkobudget 
gen sim_1 = conflict == 1
gen sim_2 = conflict == 2
label variable sim_1 "Minor conflict, s`s'"
label variable sim_2 "Major conflict, s`s'"
gen sim_both = sim_1 + sim_2
label variable sim_both  "Minor or major conflict, s`s'"

/*START REGIONAL AND GLOBAL SUMMARY OBSERVED DATA */

foreach c in 1 2 both {
	foreach gwno in 1001 1002 1003 1004 1005 1006 1007 1008 1009 { 
		by year region, sort: egen sim_`c'_temp = mean(sim_`c') 
		replace sim_`c' = sim_`c'_temp if gwno == `gwno' & gwno > 1000
		drop sim_`c'_temp
		}
}
foreach c in 1 2 both {
	by year, sort: egen sim_`c'_temp = mean(sim_`c') 
	replace sim_`c' = sim_`c'_temp if gwno == 1100
	drop sim_`c'_temp
}

/*END REGIONAL AND GLOBAL SUMMARY OBSERVED DATA */ 

saveold InputData/Observed.dta, replace	

/*LOAD SIMULATED DATA */	

/* If run separately */

set more off
set scheme s1mono
set scheme s1color
local area50 = "blue"
local area90 = "ltblue"
local area_2 = "white" // 
local area_2_50 = "0 0 0 80" // 
local area_2_90 = "0 0 0 40"
local area_1 = "0 0 255 0"
local area_1_50 = "0 0 255 0"
local area_1_90 = "0 0 128 0"
local area_0 = "0 128 255 0"
local area_0_50 = "0 128 255 0"
local area_0_90 = "0 64 128 0"

local cband = .674 /* Parameter to adjust width of confidence band; 1.96 for 95%, 1.65 for 90, .674 for 50% etc. */
local CI = 50

local FirstScenario = `fScenario'
local LastScenario = `lScenario'

local sleeptime = 5000
local currdir = c(pwd) /* Name of current directory, to take us back to where we belong */

local firstModel = 1
local lastModel = 7

use Aggregated_c.dta", clear
/* Rename variables */
rename m_llpkobudget m_lpkobudget
rename m_lpkotrad m_pkotrad
rename m_lpkotrans m_pkotrans
rename v_llpkobudget v_lpkobudget
rename v_lpkotrad v_pkotrad
rename v_lpkotrans v_pkotrans

/* --- */
sort Scenario gwno year 
capture drop m_gdpcapssp2
gen m_gdpcapssp2 = exp(m_lgdpcap)
capture drop v_gdpcapssp2
gen v_gdpcapssp2 = exp(v_lgdpcap)
replace m_pkobudget = m_pkobudget[_n+1] if gwno == gwno[_n+1] 
replace m_pkobudget = 0 if m_pkobudget == 1 & m_pkobudget != .
replace m_lgdpcap = m_lgdpcap[_n+1] if gwno == gwno[_n+1] 
replace m_gdpcapssp2 = m_gdpcapssp2[_n+1] if gwno == gwno[_n+1] 
replace v_pkobudget = v_pkobudget[_n+1]
replace v_lgdpcap = v_lgdpcap[_n+1]
replace v_gdpcapssp2 = v_gdpcapssp2[_n+1] 
replace m_sim_0 = m_sim_0[_n+1] if gwno == gwno[_n+1] 
replace m_sim_1 = m_sim_1[_n+1] if gwno == gwno[_n+1] 
replace m_sim_2 = m_sim_2[_n+1] if gwno == gwno[_n+1] 

/* Labeling variables */
label variable m_sim_0 "No conflict"
label variable m_sim_1 "Minor"
label variable m_sim_2 "Major"

label variable m_logit0_o "No conflict"
label variable m_logit1_o "Minor"
label variable m_logit2_o "Major"


/* Correcting for last year with data and calculate moving averages */
foreach k in 0 1 2 {
	foreach p in 50 90 {
		foreach l in "ub" "lb" {
			quietly replace logit`k'_`p'`l' = m_logit`k'_o if year == `fsimyear' - 1
			if `ma' == 1 {
				if `p' == 50 & "`l'" == "ub" {
					quietly replace m_sim_`k' = ((m_sim_`k'[_n-1] + m_sim_`k' + m_sim_`k'[_n+1]) / 3 ) ///
						if year > `fsimyear' & year < `lsimyear'
					quietly replace m_logit`k'_o = ((m_logit`k'_o[_n-1] + m_logit`k'_o + m_logit`k'_o[_n+1]) / 3 ) ///
						if year > `fsimyear' & year < `lsimyear'
				} /* end if `p' == 50 & `l' == "ub" */
				quietly replace logit`k'_`p'`l' = ((logit`k'_`p'`l'[_n-1] + logit`k'_`p'`l' + logit`k'_`p'`l'[_n+1]) / 3 ) ///
					if year > `fsimyear' & year < `lsimyear'
			} /* end if `ma' == 1 */
		}
	}
}

display `lsimyear'
drop if year > `lsimyear'


/* Recoding for area graph */
capture drop sim_a_*
gen sim_a_zero = 0
gen sim_a_0 = m_sim_2
gen sim_a_1 = m_sim_2 + m_sim_1
gen sim_a_2 = m_sim_2 + m_sim_1 + m_sim_0


/*logged budget */
sort gwno year Scenario
forvalues s = `FirstScenario'(1)`LastScenario' {
	quietly gen lpkobudget_`s'_temp = m_lpkobudget if Scenario == `s'
	quietly gen elpkobudget_`s' = exp(lpkobudget_`s')
	quietly gen lpkobudget_`s'_se_temp = sqrt(v_lpkobudget) if Scenario == `s'
	by gwno year: egen lpkobudget_`s' = max(lpkobudget_`s'_temp)
	by gwno year: egen lpkobudget_`s'_se = max(lpkobudget_`s'_se_temp)
	label variable lpkobudget_`s' "Log PKO budget, scenario `s'"
		foreach outcome in 0 1 2 {
		quietly capture drop sim_`outcome'_`s'
		quietly gen sim_`outcome'_`s' = m_sim_`outcome' if Scenario == `s'
		quietly gen phat_`outcome'_`s' = m_phat`outcome' if Scenario == `s'
		label variable sim_`outcome'_`s' "Scenario `s'"
		label variable phat_`outcome'_`s' "Scenario `s'"
	} /* end foreach outcome */
	quietly gen conflict_`s' = 1-sim_0_`s' if Scenario == `s'
} /* end fovalues s */

/*unlogged gdp and budget */ 

sort gwno year Scenario
forvalues s = `FirstScenario'(1)`LastScenario' {
	foreach var in lgdpcap pkobudget gdpcapssp2 {
		quietly gen `var'_`s'_temp = m_`var' if Scenario == `s'
		quietly gen `var'_`s'_se_temp = sqrt(v_`var') if Scenario == `s'
		by gwno year: egen `var'_`s' = max(`var'_`s'_temp)
		by gwno year: egen `var'_`s'_se = max(`var'_`s'_se_temp)
	}
	label variable pkobudget_`s' "PKO budget, scenario `s'"
	label variable lgdpcap_`s' "Log GDP per capita, scenario `s'"
	label variable gdpcapssp2_`s' "GDP per capita, scenario `s'"
} /* end foevalues s */


forvalues s = `LastScenario'(-1)`FirstScenario' {
	summ n
	local n = r(mean)
	foreach outcome in 0 1 2 {
		capture drop temp*
		quietly by gwno year: egen temp1 = min(sim_`outcome'_`s')
		quietly replace sim_`outcome'_`s' = temp1
		quietly by gwno year: egen temp2 = min(phat_`outcome'_`s')
		quietly replace phat_`outcome'_`s' = temp2
	} /* end foreach outcome */
	label variable sim_1_`s' "Minor conflict, s`s'"
	label variable sim_2_`s' "Major conflict, s`s'"
	capture drop temp3
	quietly by gwno year: egen temp3 = min(conflict_`s')
	quietly replace conflict_`s' = temp3
	label variable conflict_`s' "Minor or major conflict, s`s'"
	gen sim_both_`s' = sim_1_`s' + sim_2_`s'
	/* one standard deviation high and low bands for gdp */
	foreach var in lgdpcap pkobudget gdpcapssp2 {	
	gen ub_`var'_`s' = `var'_`s' + (1.000 * `var'_`s'_se)
	gen lb_`var'_`s' = `var'_`s' - (1.000 * `var'_`s'_se)
	} /* end foreacg var s */
	} /* end fovalues s */


/*Variables for plotting excess conflict and uncertainty */

/* 	CALCULATE DIFFERENCE IN PREDICTED CONFLICT FROM NO PKO SCENARIO, SCENARIO 7 */
	
sort gwno year Scenario
summ n 
local n = r(mean) 
display `n'
forvalues s = `LastScenario'(-1)`FirstScenario' {
	display "scenario: `s'"
*	capture drop t_all_`s'
*	capture drop t_maj_`s'
*	gen t_all_`s' = m_sim_both if Scenario == `s'
*	gen t_maj_`s' = m_sim_2 if Scenario == `s'
*	gen tc_logitb_`s' = -m_logit0_o if Scenario == `s' 
	gen tc_logitb_`s' = 1-m_sim_0 if Scenario == `s' 
*	gen tc_logit2_`s' = m_logit2_o if Scenario == `s'
	gen tc_logit2_`s' = m_sim_2 if Scenario == `s'
	gen t_sd_b_`s' = pred_sd_0 if Scenario == `s'
	gen t_sd_2_`s' = pred_sd_2 if Scenario == `s'
*	by year gwno, sort: egen sh_cnt_c_`s' = max(t_all_`s')
*	by year gwno, sort: egen sh_cnt_t2_`s' = max(t_maj_`s')
	by year gwno, sort: egen c_logitb_`s' = max(tc_logitb_`s')
	by year gwno, sort: egen c_logit2_`s' = max(tc_logit2_`s')
	by year gwno, sort: egen sd_b_`s' = max(t_sd_b_`s')
	by year gwno, sort: egen sd_2_`s' = max(t_sd_2_`s')

	/* Difference, both levels */
	gen dflogitb_cnt_`s' = c_logitb_`s' - c_logitb_7
	/* Calculate confidence interval based on http://www.stat.wmich.edu/s216/book/node85.html */
	gen dftempb_cnt_`s' = sqrt((sd_b_`s'/n)+(sd_b_7/n))
	summ dftempb_cnt_`s' sd_b_`s' sd_b_7
	gen dfub_b_cnt_`s' = dflogitb_cnt_`s' + (1.000 * dftempb_cnt_`s')
	gen dflb_b_cnt_`s' = dflogitb_cnt_`s' - (1.000 * dftempb_cnt_`s')

	/* Difference, major conflict */
	gen dflogit2_cnt_`s' = c_logit2_`s' - c_logit2_7
	/* Calculate confidence interval based on http://www.stat.wmich.edu/s216/book/node85.html */
	gen dftemp2_cnt_`s' = sqrt((sd_2_`s'/n)+(sd_2_7/n))
	summ dftemp2_cnt_`s' sd_2_`s' sd_2_7
	gen dfub_2_cnt_`s' = dflogit2_cnt_`s' + (1.000 * dftemp2_cnt_`s')
	gen dflb_2_cnt_`s' = dflogit2_cnt_`s' - (1.000 * dftemp2_cnt_`s')

} /* end forvalues s = `firstScenario'(1)`lastScenario' */

/*MERGE INN OBSERVED DATA */
capture drop _merge
sort gwno year
merge m:1 gwno year using "InputData/Observed.dta"
sort gwno year 


/*Merge inn population data for weighting*/

capture drop _merge
sort gwno year
merge m:1 gwno year using "Sim/Program/estimationdata.dta", keepusing(lpop)
sort gwno year 
gen pop = exp(lpop)

/*Done*/

/*Generate yearly GDP cap figures*/


forvalues s = `FirstScenario'(1)`LastScenario' {
	capture drop temp_bnp_`s'
	gen temp_bnp_`s' = exp(lgdpcap_`s')
*	by year, sort: egen glob_lgdpcap_`s' = mean(lgdpcap_`s')
*	label variable glob_lgdpcap_`s' "Log GDP per capita, scenario `s'"
*	by year, sort: egen glob_gdpcap_`s' = mean(gdpcapssp2_`s')
	by year, sort: egen glob_gdpcap_`s' = wtmean(temp_bnp_`s'), weight(pop)
	label variable glob_gdpcap_`s' "GDP per capita, scenario `s'"
} /* end foevalues s */


sort gwno year
line glob_gdpcap_1 glob_gdpcap_7 glob_gdpcap_3 glob_gdpcap_6 year if gwno==2 ///
	& year >= `fsimyear' & year <= `lsimyear', ///
	title("Expected GDP per capita, `cntname'", size(small)) ///
	legend(order(1 2 3 4) rows(2) cols(3) size(vsmall) ///
	label(1 "S2: Observed PKO") label(2 "S1: No PKOs") label(3 "S3: Trad, 100M USD/year") label(4 "S5: Trans, 800M USD/year, all countries") )  /// 
	xlabel(2001(2)2013, labsize(small)) ytitle("GDP per capita") xtitle("") xscale(range( 2001 2013) ) ///
	lpattern(solid solid shortdash longdash longdash longdash) lcolor(black blue dkgreen red ) 
graph save "Figures/gdpcap_Globally.gph", replace		
graph export "Figures/gdpcap_Globally.pdf", replace	
graph export "Figures/gdpcap_Globally.eps", replace	

/*Done yearly GDP*/

/*Generate regional GDP*/

forvalues s = `FirstScenario'(1)`LastScenario' {
	capture drop temp_bnp_`s'
	gen temp_bnp_`s' = exp(lgdpcap_`s')
*	by region year, sort: egen regional_gdpcap_`s' = mean(temp_bnp_`s')
	by region year, sort: egen regional_gdpcap_`s' = wtmean(temp_bnp_`s'), weight(pop)
	label variable regional_gdpcap_`s' "GDP per capita, scenario `s'"	
*    drop temp_bnp_`s'
	
} /* end foevalues s */


foreach cnt in 1 2 3 4 5 6 7 8 9 { 
	if `cnt' == 1 {
		local cntname = "Southern_Africa"
	}
	if `cnt' == 2 {
		local cntname = "Central_and_South_America"
	}
	if `cnt' == 3 {
		local cntname = "West_Europe"
	}
	if `cnt' == 4 {
		local cntname = "East_Europe"
	}
	if `cnt' == 5 {
		local cntname = "MENA"
	}
	if `cnt' == 6 {
		local cntname = "West_Africa"
	}
	if `cnt' == 7 {
		local cntname = "East_Africa"
	}
	if `cnt' == 8 {
		local cntname = "Central_Asia"
	}
	if `cnt' == 9 {
		local cntname = "East_and_South_Asia"
	}

line regional_gdpcap_1 regional_gdpcap_7 regional_gdpcap_3 regional_gdpcap_6 year ///
	if region == `cnt' & year >= `fsimyear' & year <= `lsimyear', ///
	title("Expected GDP per capita, `cntname'", size(small)) ///
	legend(order(1 2 3 4) rows(2) cols(3) size(vsmall) ///
	label(1 "S2: Observed PKO") label(2 "S1: No PKOs") label(3 "S3: Trad, 100M USD/year") label(4 "S5: Trans, 800M USD/year, all countries") )  /// 
	xlabel(2001(2)2013, labsize(small)) ytitle("GDP per capita") xtitle("") xscale(range( 2001 2013) ) ///
	lpattern(solid solid shortdash longdash longdash longdash) lcolor(black blue dkgreen red ) 
graph save "Figures/gdpcap_`cntname'.gph", replace		
graph export "Figures/gdpcap_`cntname'.pdf", replace	

}

/*Done regional GDP*/


/*
replace sim_1_1 = sim_1 if sim_1_1 == . & year <= 2013
replace sim_2_1 = sim_2 if sim_2_1 == . & year <= 2013
replace sim_both_1 = sim_both if sim_both_1 == . & year <= 2013
replace sim_both_5 = sim_both if sim_both_5 == . & year <= 2013
replace sim_2_5 = sim_2_1 if sim_2_5 == . & year <= 2013
*/

forvalues s = `FirstScenario'(1)`LastScenario' {
replace sim_2_`s' = sim_2 if year == 2000
replace sim_both_`s' = sim_both if year == 2000
}

/*create total regional PKO budget */

capture drop countries
by year Scenario region, sort: egen countries = count(gwno)
replace countries = 171 if gwno == 1100

forvalues s = `FirstScenario'(1)`LastScenario' {
replace pkobudget_`s' = (pkobudget_`s' * countries) / 1000 if gwno >= 1000 
replace ub_pkobudget_`s' = (ub_pkobudget_`s' * countries) / 1000 if gwno >= 1000 
replace lb_pkobudget_`s' = (lb_pkobudget_`s' * countries) / 1000 if gwno >= 1000 
}

gen numberofconflicts = sim_2 * `sumcountries'
gen numberofwars = sim_both * `sumcountries'

	
/* Naming relevant countries for graphing purposes */
capture drop country
gen country = ""
replace country = "Colombia" if gwno == 100
replace country = "DRC" if gwno == 490
replace country = "Uganda" if gwno == 500
replace country = "Kenya" if gwno == 501
replace country = "Tanzania" if gwno == 510
replace country = "Burundi" if gwno == 516
replace country = "Rwanda" if gwno == 517
replace country = "Afghanistan" if gwno == 700
replace country = "Pakistan" if gwno == 770
replace country = "Lebanon" if gwno == 660
replace country = "Saudi_Arabia" if gwno == 670
/* Regions */
replace country = "Southern_Africa" if gwno == 1001
replace country = "Central_South_America" if gwno == 1002
replace country = "West_Europe" if gwno == 1003
replace country = "East_Europe" if gwno == 1004
replace country = "MENA" if gwno == 1005
replace country = "West_Africa" if gwno == 1006
replace country = "East_Africa" if gwno == 1007
replace country = "Central_Asia" if gwno == 1008
replace country = "East_South_Asia" if gwno == 1009
*replace country = "Arab League" if gwno == 1010
*replace country = "Rest of the world" if gwno == 1011
replace country = "Globally" if gwno == 1100

/* Figures for countries pluss regions */
/*   490 500 501 510 1001 1002 1006 1007 100 490 500 501 510 516 517 700 770 1001 1002 1003 1004 1005 1006 1007 1008 1009*/

gen dfdummy = 0
*
foreach cnt in 490 516 517 510 501 500 660 670 1001 1002 1003 1004 1005 1006 1007 1008 1009 1010 1100 { 
	local ylabel = ""
	if `cnt' == 100 {
		local cntname = "Colombia"
	}
	if `cnt' == 490 {
		local cntname = "DRC"
	}
	if `cnt' == 500 {
		local cntname = "Uganda"
	}
	if `cnt' == 501 {
		local cntname = "Kenya"
	}
	if `cnt' == 510 {
		local cntname = "Tanzania"
	}
	if `cnt' == 516 {
		local cntname = "Burundi"
	}
	if `cnt' == 517 {
		local cntname = "Rwanda"
	}
	if `cnt' == 700 {
		local cntname = "Afghanistan"
	}
	if `cnt' == 770 {
		local cntname = "Pakistan"
	}
	if `cnt' == 660 {
		local cntname = "Lebanon"
	}
	if `cnt' == 670 {
		local cntname = "Saudi_Arabia"
	}
	if `cnt' == 1001 {
		local cntname = "Southern_Africa"
	}
	if `cnt' == 1002 {
		local cntname = "Central_and_South_America"
	}
	if `cnt' == 1003 {
		local cntname = "West_Europe"
	}
	if `cnt' == 1004 {
		local cntname = "East_Europe"
	}
	if `cnt' == 1005 {
		local cntname = "MENA"
	}
	if `cnt' == 1006 {
		local cntname = "West_Africa"
	}
	if `cnt' == 1007 {
		local cntname = "East_Africa"
	}
	if `cnt' == 1008 {
		local cntname = "Central_Asia"
	}
	if `cnt' == 1009 {
		local cntname = "East_and_South_Asia"
	}
	if `cnt' == 1100 {
		local cntname = "Globally"
	}

	forvalues scen = `FirstScenario'(1)`LastScenario' {
	/* Graph with minor and major */
		if `scen' == 1 {

				graph twoway || ///
				(line sim_2 year  if gwno == `cnt' & year >= 1990 & year <= `lsimyear', yaxis(1) lpattern(solid) lcolor(black )) || ///
				(line sim_2_1 year  if gwno == `cnt' & year >= 1990 & year <= `lsimyear', yaxis(1) lpattern(dash) lcolor(black )) || ///
				(line sim_2_7 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(solid) lcolor(blue)) || ///
				(line sim_2_3 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(shortdash) lcolor(green)) || /// 
				(line sim_2_5 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(longdash) lcolor(red)) || ///
				(line sim_2_6 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(longdash) lcolor(orange)) || ///				
				(line numberofconflicts year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(2) lpattern(blank) lcolor(black)) || ///					
				(line sim_both year  if gwno == `cnt' & year >= 1990 & year <= `lsimyear', yaxis(1)lpattern(solid) lcolor(black)) || ///
				(line numberofwars year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(2) lpattern(blank) lcolor(black)) || ///
				(line sim_both_1 year  if gwno == `cnt' & year >= 1990 & year <= `lsimyear', yaxis(1) lpattern(dash) lcolor(black))  || ///
				(line sim_both_7 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(solid) lcolor(blue) ) || ///
				(line sim_both_3 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(shortdash) lcolor(green )) || ///
				(line sim_both_5 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(longdash) lcolor(red )) || ///
				(line sim_both_6 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(longdash) lcolor(orange ) ///
						title("Proportion in conflict (either or major), `cntname', all scenarios", size(small)) ///
						legend(order(1 2 3 4 5 6) rows(4) cols(2) size(vsmall) ///
						label(1 "Observed conflict") label(2 "S2: Observed PKO levels")  label(3 "S1: No PKOs") ///
						label(4 "S3: Trad, 100M USD/year") label(5 "S4: Trans, 800M USD/year") label(6 "S5: Trans, 800M USD/year, all countries") )  /// 
						xline(2000.5) xlabel(1990(5)2010, labsize(small)) xtitle("") xscale(range(1990 2010)) )
					graph save "Figures/p_`cntname'.gph", replace		
					graph export "Figures/p_`cntname'.pdf", replace				

				graph twoway || ///
				(line sim_2_1 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(dash) lcolor(black )) || ///
				(line sim_2_7 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(solid) lcolor(blue)) || ///
				(line sim_2_3 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(shortdash) lcolor(green)) || /// 
				(line sim_2_6 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(longdash) lcolor(orange)) || ///								
				(line sim_both_1 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(dash) lcolor(black))  || ///
				(line sim_both_7 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(solid) lcolor(blue) ) || ///
				(line sim_both_3 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(shortdash) lcolor(green )) || ///
				(line sim_both_6 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', yaxis(1) lpattern(longdash) lcolor(orange ) ///
						title("Proportion in conflict (either or major), `cntname', all scenarios", size(small)) ///
						legend(order(1 2 3 4) rows(2) cols(3) size(vsmall) ///
						label(1 "S2: Observed PKO") label(2 "S1: No PKOs") label(3 "S3: Trad, 100M USD/year") label(4 "S5: Trans, 800M USD/year, all countries" ) )  /// 
						xlabel(2001(2)2013, labsize(small)) ytitle("Prop in conflict") xtitle("") xscale(range( 2001 2013) ) ///
						lpattern(solid solid shortdash longdash solid solid shortdash longdash) lcolor(black blue dkgreen red black blue dkgreen red) )
					graph save "Figures/p_red_`cntname'.gph", replace		
					graph export "Figures/p_red_`cntname'.pdf", replace	
					
									
				graph twoway || ///
				(line sim_2 year  if gwno == `cnt' & year >= 1990 & year <= `lsimyear', lpattern(solid) lcolor(black )) || ///
				(line sim_2_1 year  if gwno == `cnt' & year >= 1990 & year <= `lsimyear', lpattern(dash) lcolor(black )) || ///
				(line sim_2_7 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', lpattern(solid) lcolor(blue)) || ///
			  	(line sim_2_2 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', lpattern(shortdash) lcolor(grey)) || ///   
				(line sim_2_3 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', lpattern(shortdash) lcolor(green)) || /// 
			  	(line sim_2_4 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', lpattern(longdash) lcolor(green)) || ///   
				(line sim_2_5 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', lpattern(longdash) lcolor(red)) || ///
				(line sim_2_6 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', lpattern(longdash) lcolor(orange)) || ///
				(line sim_both year  if gwno == `cnt' & year >= 1990 & year <= `lsimyear',lpattern(solid) lcolor(black)) || ///
				(line sim_both_1 year  if gwno == `cnt' & year >= 1990 & year <= `lsimyear',lpattern(dash) lcolor(black))  || ///
				(line sim_both_7 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear',lpattern(solid) lcolor(blue) ) || ///
			  	(line sim_both_2 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', lpattern(shortdash) lcolor(grey )) || ///   
				(line sim_both_3 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', lpattern(shortdash) lcolor(green )) || ///
			  	(line sim_both_4 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear',lpattern(longdash) lcolor(green)) || ///   
				(line sim_both_5 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear',lpattern(longdash) lcolor(red )) || ///
				(line sim_both_6 year  if gwno == `cnt' & year >= 2000 & year <= `lsimyear', lpattern(longdash) lcolor(orange ) ///
						title("Proportion in conflict (either or major), `cntname', all scenarios", size(small)) ///
						legend(order(1 2 3 4 5 6 7 8) rows(4) cols(2) size(vsmall) ///
						/*label(1 "Observed conflict") label(2 "S2: Observed PKO levels")  label(3 "S1: No PKOs") label(4 "S3: Trad, 25M USD/year")  ///
						*label(5 "S4: Trad, 100M USD/year") label(6 "S5: Trans, 100M USD/year") label(7 "S6: Trans, 800M USD/year") label(8 "S7: Trans, 800M USD/year, all countries") )  /// */
						label(1 "Observed conflict") label(2 "S2: Observed PKO levels")  label(3 "S1: No PKOs") ///
						label(4 "S4: Trad, 100M USD/year") label(5 "S6: Trans, 800M USD/year") label(6 "S7: Trans, 800M USD/year, all countries") )  /// 
						xline(2000.5) xlabel(1990(5)2010, labsize(small)) xtitle("") xscale(range(1990 2010)) )
					graph save "Figures/p_`cntname'.gph", replace		
					graph export "Figures/p_`cntname'.pdf", replace				
			
			

	if `cnt' <= 999 { 					
			/*	twoway ///
					(rarea ub_gdpcapssp2_1 lb_gdpcapssp2_1 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs15)) || ///
					(rarea ub_gdpcapssp2_7 lb_gdpcapssp2_7 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(ltblue)) || ///
					(rarea ub_gdpcapssp2_3 lb_gdpcapssp2_3 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(green))  || ///
					(rarea ub_gdpcapssp2_6 lb_gdpcapssp2_6 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(orange_red) ///
					) || /// */
					line gdpcapssp2_1 gdpcapssp2_7 gdpcapssp2_3 gdpcapssp2_6 year ///
					 if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', ///
					title("Expected GDP per capita, `cntname'", size(small)) ///
						legend(order(1 2 3 4) rows(2) cols(3) size(vsmall) ///
						label(1 "S2: Observed PKO") label(2 "S1: No PKOs") label(3 "S3: Trad, 100M USD/year") label(6 "S5: Trans, 800M USD/year, all countries") )  /// 
						xlabel(2001(2)2013, labsize(small)) ytitle("GDP per capita") xtitle("") xscale(range( 2001 2013) ) ///
						lpattern(solid solid shortdash longdash longdash longdash) lcolor(black blue dkgreen red ) 
				graph save "Figures/gdpcap_`cntname'.gph", replace		
				graph export "Figures/gdpcap_`cntname'.pdf", replace			
	}

	if `cnt' >= 1000  { 				
	
			twoway ///
					(rarea ub_pkobudget_7 lb_pkobudget_7 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(ltblue)) || ///
					(rarea ub_pkobudget_3 lb_pkobudget_3 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(green))  || ///
					(rarea ub_pkobudget_6 lb_pkobudget_6 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(orange_red) ///
					) || /// 
					(line pkobudget_1 pkobudget_7 pkobudget_3 pkobudget_6 year ///
					 if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', ///
					title("Expected PKO budget allocations (billion USD), `cntname'", size(small)) ///
						legend(order(4 5 6 7) rows(2) cols(3) size(vsmall) ///
						label(4 "S2: Observed PKO") label(5 "S1: No PKOs") label(6 "S3: Trad, 100M USD/year") label(7 "S5: Trans, 800M USD/year, all countries" ) ) /// 
						xlabel(2001(2)2013, labsize(small)) ytitle("(log) PKO budget") xtitle("") xscale(range( 2001 2013) ) ///
						lpattern(solid solid shortdash longdash longdash longdash) lcolor(black blue dkgreen red ) )
				graph save "Figures/budget_`cntname'.gph", replace		
				graph export "Figures/budget_`cntname'.pdf", replace	
	}
	
	if `cnt' <= 999 {  				
	preserve
	replace lb_pkobudget_6 = 0 if lb_pkobudget_6 < 0
			twoway ///
					(rarea ub_pkobudget_7 lb_pkobudget_7 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(ltblue)) || ///
					(rarea ub_pkobudget_3 lb_pkobudget_3 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(green))  || ///
					(rarea ub_pkobudget_6 lb_pkobudget_6 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(orange_red) ///
					) || /// 
					(line pkobudget_1 pkobudget_7 pkobudget_3 pkobudget_6 year ///
					 if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', ///
					title("Expected PKO budget allocations (million USD), `cntname'", size(small)) ///
						legend(order(4 5 6 7) rows(2) cols(3) size(vsmall) ///
						label(4 "S2: Observed PKO") label(5 "S1: No PKOs") label(6 "S3: Trad, 100M USD/year") label(7 "S5: Trans, 800M USD/year, all countries" ) )  /// 
						xlabel(2001(2)2013, labsize(small)) ytitle("(log) PKO budget") xtitle("") xscale(range( 2001 2013) ) ///
						lpattern(solid solid shortdash longdash longdash longdash) lcolor(black blue dkgreen red ) )
				graph save "Figures/budget_`cntname'.gph", replace		
				graph export "Figures/budget_`cntname'.pdf", replace	
	restore
	}
						
				graph twoway line pkobudget_1 pkobudget_7 pkobudget_3 pkobudget_4 pkobudget_5 pkobudget_6 year  if gwno == `cnt'  & year >= `fsimyear' & year <= `lsimyear', ///
					title("Expected PKO budget allocations, `cntname', scenarios 1-6", size(small)) ///
						legend(order(1 2 3 4 5 6) rows(2) cols(3) size(vsmall) label(1 "Observed") label(2 "No PKOs") label(3 "Trad, 100M USD/year") label(4 "Trans, 100M USD/year") label(5 "Trans, 800M USD/year") label(6 "Trans, 800M USD/year, all countries") )  /// 
						xlabel(`fsimyear'(2)`lsimyear', labsize(small)) xtitle("") xscale(range(`fsimyear' `lsimyear') ) ///
						lpattern(solid solid shortdash longdash longdash longdash) lcolor(black blue green purple red orange )
				graph save "Figures/budget_allscenarios_`cntname'.gph", replace		
				graph export "Figures/budget_allscenario_`cntname'.pdf", replace	
				
	if `cnt' <= 999 { 	
				graph twoway line gdpcapssp2_1 gdpcapssp2_7 gdpcapssp2_3 gdpcapssp2_4 gdpcapssp2_5 gdpcapssp2_6 year  if gwno == `cnt'  & year >= `fsimyear' & year <= `lsimyear', ///
					title("Expected GDP per capita, `cntname', scenarios 1-6", size(small)) ///
						legend(order(1 2 3 4 5 6) rows(2) cols(3) size(vsmall) label(1 "Observed") label(2 "No PKOs") label(3 "Trad, 100M USD/year") label(4 "Trans, 100M USD/year") label(5 "Trans, 800M USD/year") label(6 "Trans, 800M USD/year, all countries") )  /// 
						xlabel(`fsimyear'(10)`lsimyear', labsize(small)) xtitle("") xscale(range(`fsimyear' `lsimyear') ) ///
						lpattern(solid solid shortdash longdash longdash longdash) lcolor(black blue green purple red orange )
				graph save "Figures/gdpcap_`cntname'.gph", replace		
				graph export "Figures/gdpcap_`cntname'.pdf", replace
   }

	if `cnt' <= 999 { 
				graph twoway line lgdpcap_1 lgdpcap_7 lgdpcap_3 lgdpcap_4 lgdpcap_5 lgdpcap_6 year  if gwno == `cnt'  & year >= `fsimyear' & year <= `lsimyear', ///
					title("Expected GDP per capita, `cntname', scenarios 1-6", size(small)) ///
						legend(order(1 2 3 4 5 6) rows(2) cols(3) size(vsmall) label(1 "S2: Observed PKO") label(2 "S1: No PKOs") label(3 "S4: Trad, 100M USD/year") label(4 "S5: Trans, 100M USD/year") label(5 "S6: Trans, 800M USD/year") label(6 "S7: Trans, 800M USD/year, all countries") )  /// 
						xlabel(`fsimyear'(2)`lsimyear', labsize(small)) ytitle("GDP per capita") xtitle("") xscale(range(`fsimyear' `lsimyear') ) ///
						lpattern(solid solid shortdash longdash longdash longdash) lcolor(black blue green purple red orange )
				graph save "Figures/lgdpcap_`cntname'.gph", replace		
				graph export "Figures/lgdpcap_`cntname'.pdf", replace
	}			
*/				
				/* Combine Graphs */
/*
				grc1leg "Figures/budget_Globally.gph"  "Figures/gdpcap_Globally.gph", cols(1) xcommon
				graph export "Figures/CombinedBudgetGDP_Globally.pdf", replace			
*/
				grc1leg "Figures/budget_`cntname'.gph"  "Figures/gdpcap_`cntname'.gph", cols(1) xcommon
				graph export "Figures/CombinedBudgetGDP_`cntname'.pdf", replace	

				grc1leg "Figures/p_red_`cntname'" "Figures/budget_`cntname'.gph" , cols(1) xcommon
				graph export "Figures/CombinedBudgetConflict_`cntname'.pdf", replace					
				
		} /* end if `scen' == 1 */
		
	} /* end 	forvalues scen = `FirstScenario'(1)`LastScenario' */	
		
	if `cnt' >= 1000 { 
	
		/* twoway ///
			(rarea dflb_b_cnt_1 dfub_b_cnt_1 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14)) || ///
			(rarea dflb_b_cnt_3 dfub_b_cnt_3 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14)) || ///
			(rarea dflb_b_cnt_5 dfub_b_cnt_5 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14))  || ///
			(rarea dflb_b_cnt_6 dfub_b_cnt_6 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14) ///
			) || /// */
			line dfdummy dflogitb_cnt_1  dflogitb_cnt_3  dflogitb_cnt_6 year ///
			 if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', ///
			lpattern(solid solid shortdash longdash) color(blue black green orange) yline(0) ylabel(-.1(.05).1) ///
			title("Excess minor or major conflict, `cntname'") xlabel(2001(2)2013) ///
			legend(order(1 2 3 4) label(1 "S1: No PKO") label(2 "S2: Observed PKO") label(3 "S3: Trad, 100M USD/year")  label(4 "S5: Trans, 800M USD/year, all countries") ) 
		graph export "Figures/diff_both_`cntname'.pdf", replace
		
		/* twoway ///
			(rarea dflb_2_cnt_1 dfub_2_cnt_1 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14)) || ///
			(rarea dflb_2_cnt_3 dfub_2_cnt_3 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14)) || ///
			(rarea dflb_2_cnt_5 dfub_2_cnt_5 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14))  || ///
			(rarea dflb_2_cnt_6 dfub_2_cnt_6 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14) ///
			) || /// */
			line dfdummy dflogit2_cnt_1  dflogit2_cnt_3   dflogit2_cnt_6 year ///
			 if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', ///
			lpattern(solid solid shortdash longdash) color(blue black green orange) yline(0) ylabel(-.1(.05).1) ///
			title("Excess minor or major conflict, `cntname'") xlabel(2001(2)2013) ///
			legend(order(1 2 3 4) label(1 "S1: No PKO") label(2 "S2: Observed PKO") label(3 "S3: Trad, 100M USD/year")  label(4 "S5: Trans, 800M USD/year, all countries") )  
		graph export "Figures/diff_major_`cntname'.pdf", replace

		}
	
	else {  /*differnece in secenario effect on specific countries larger so wider y-range */
	
		/* twoway ///
			(rarea dflb_b_cnt_1 dfub_b_cnt_1 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14)) || ///
			(rarea dflb_b_cnt_3 dfub_b_cnt_3 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14)) || ///
			(rarea dflb_b_cnt_5 dfub_b_cnt_5 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14))  || ///
			(rarea dflb_b_cnt_6 dfub_b_cnt_6 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14) ///
			) || /// */
			line dfdummy dflogitb_cnt_1  dflogitb_cnt_3  dflogitb_cnt_6 year ///
			 if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', ///
			lpattern(solid solid shortdash longdash) color(blue black green orange) yline(0) ylabel(-.1(.05).1) ///
			title("Excess minor or major conflict, `cntname'") xlabel(2001(2)2013) ///
			legend(order(1 2 3 4) label(1 "S1: No PKO") label(2 "S2: Observed PKO") label(3 "S3: Trad, 100M USD/year")  label(4 "S5: Trans, 800M USD/year, all countries") ) 
		graph export "Figures/diff_both_`cntname'.pdf", replace
		
		/* twoway ///
			(rarea dflb_2_cnt_1 dfub_2_cnt_1 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14)) || ///
			(rarea dflb_2_cnt_3 dfub_2_cnt_3 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14)) || ///
			(rarea dflb_2_cnt_5 dfub_2_cnt_5 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14))  || ///
			(rarea dflb_2_cnt_6 dfub_2_cnt_6 year if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', color(gs14) ///
			) || /// */
			line dfdummy dflogit2_cnt_1  dflogit2_cnt_3   dflogit2_cnt_6 year ///
			 if gwno == `cnt' & year >= `fsimyear' & year <= `lsimyear', ///
			lpattern(solid solid shortdash longdash) color(blue black green orange) yline(0) ylabel(-.1(.05).1) ///
			title("Excess minor or major conflict, `cntname'") xlabel(2001(2)2013) ///
			legend(order(1 2 3 4) label(1 "S1: No PKO") label(2 "S2: Observed PKO") label(3 "S3: Trad, 100M USD/year")  label(4 "S5: Trans, 800M USD/year, all countries") )  
		graph export "Figures/diff_major_`cntname'.pdf", replace

	
		} /* end if else	*/
		
} /* end foreach cnt in .... */



