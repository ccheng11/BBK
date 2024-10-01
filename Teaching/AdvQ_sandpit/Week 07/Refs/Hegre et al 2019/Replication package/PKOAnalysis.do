clear all
capture log close
set scheme s1color

************************************************
/* Do-file for sections 5.1-5.2, PKO paper    */
************************************************

capture cd "/Users/Havard/Dropbox/Collaborations/PKO/Paper/Replication package"
log using "Results\PKO_analysis.log", replace

use "PredictionPKO1.dta", clear
keep if year >= 1970 & year <= 2013
replace DS_ordinal  = 0 if DS_ordinal  ==.
replace DS_ord_1  = 0 if DS_ordinal  ==.
replace DS_ord_2  = 0 if DS_ordinal  ==.
replace DS_ord_3  = 0 if DS_ordinal  ==.
replace DS_ord_4  = 0 if DS_ordinal  ==.

label variable PKOtrad "Traditional peace-keeping operation"
label variable PKOtrans "Transformational peace-keeping operation"

replace PKObudget = 0 if PKObudget == . | PKObudget < 0

/*
replace year_exp = 44.22 if ds_ordinal == 1 & year_exp == -99
replace year_exp = 176.81 if ds_ordinal == 2 & year_exp == -99
replace year_exp = 159.27 if ds_ordinal == 3 & year_exp == -99
replace year_exp = 562.61 if ds_ordinal == 4 & year_exp == -99
replace year_exp = 0 if ds_ordinal == 0 & year_exp == -99
*/


/* Create Categorical PKO Variable*/

gen pko_cat = 0
replace pko_cat=1 if DS_ordinal!=0

/*Collapse enforcement and multidimensional operations into one category */

gen multi_enforce = 0
replace multi_enforce = 1 if DS_ord_3==1
replace multi_enforce = 1 if DS_ord_4==1

/* Labeling */
capture label drop pko_label
label define pko_label 0 "No PKO" 1 "Observer" 2 "Traditional" 3 "Multidimensional" 4 "Enforcement"
label values DS_ordinal pko_label
tab DS_ordinal


*****************************
/********Graphs*************/
*****************************

sort year ds_ordinal
/*  Figure of total expenditures by year and type */
capture drop expsum*
by year, sort: egen expsum_tot = sum(PKObudget) 
by year, sort: egen expsum_obs = sum(PKObudget*DS_ord_1/1000) 
by year, sort: egen expsum_tra = sum(PKObudget*DS_ord_2/1000)
by year, sort: egen expsum_mul = sum(PKObudget*DS_ord_3/1000)
by year, sort: egen expsum_enf = sum(PKObudget*DS_ord_4/1000) 

replace expsum_tra = expsum_obs + expsum_tra
replace expsum_mul = expsum_tra + expsum_mul
replace expsum_enf = expsum_mul + expsum_enf

label variable expsum_tot "Total expenditures"
label variable expsum_obs "Observer missions"
label variable expsum_tra "Traditional missions"
label variable expsum_mul "Multidimensional missions"
label variable expsum_enf "Enforcement missions"

twoway area expsum_enf expsum_mul expsum_tra expsum_obs year ///
	if gwno == 2, ///
	xlabel(1973 1983 1993 2003 2013) ///
	note("PKO classification from Doyle & Sambanis 2000. Figures in billion US dollars.")
graph export "Figures/PKOBudgetExpenditures.pdf", as(pdf) replace

/*  Figure of number of missions by year and type */

capture drop pkocount*
by year, sort: egen pkocount_obs = sum(DS_ord_1)
by year, sort: egen pkocount_tra = sum(DS_ord_2) 
by year, sort: egen pkocount_mul = sum(DS_ord_3) 
by year, sort: egen pkocount_enf = sum(DS_ord_4) 

replace pkocount_tra = pkocount_obs + pkocount_tra
replace pkocount_mul = pkocount_tra + pkocount_mul
replace pkocount_enf = pkocount_mul + pkocount_enf

label variable pkocount_obs "Observer missions"
label variable pkocount_tra "Traditional missions"
label variable pkocount_mul "Multidimensional missions"
label variable pkocount_enf "Enforcement missions"

twoway area pkocount_enf pkocount_mul pkocount_tra pkocount_obs year ///
	if  gwno == 2, ///
	xlabel(1973 1983 1993 2003 2013) ylabel(0 10 20 30) ///
	note("PKO classification from Doyle & Sambanis 2000.")
graph export "Figures/PKOMandatesCount.pdf", as(pdf) replace


/*END OF DO FILE */
STOP!







/* Removing international pkos */

capture drop uniimog
gen uniimog = 0
replace uniimog = 1 if un_mandate=="UNIIMOG"
foreach ds of varlist ds_* year_exp {
	replace `ds' = 0 if uniimog==1
}

capture drop untso
gen untso = 0
replace untso = 1 if un_mandate=="UNTSO"
foreach ds of varlist ds_* year_exp {
	replace `ds' = 0 if untso==1
}

capture drop unikom
gen unikom = 0
replace unikom = 1 if un_mandate=="UNIKOM"
foreach ds of varlist ds_* year_exp {
	replace `ds' = 0 if unikom==1
}

capture drop unmee
gen unmee = 0
replace unmee = 1 if un_mandate=="UNMEE"
foreach ds of varlist ds_* year_exp {
	replace `ds' = 0 if unmee==1
}

capture drop unmogip
gen unmogip = 0
replace unmogip = 1 if un_mandate=="UNMOGIP"
foreach ds of varlist ds_* year_exp {
	replace `ds' = 0 if unmogip==1
}

capture drop unef2
gen unef2 = 0
replace unef2 = 1 if un_mandate=="UNEF II"
foreach ds of varlist ds_* year_exp {
	replace `ds' = 0 if unef2==1
}
capture drop ungomap
gen ungomap = 0
replace ungomap = 1 if un_mandate=="UNGOMAP"
foreach ds of varlist ds_* year_exp {
	replace `ds' = 0 if ungomap==1 & gwno == 770
}






/* Distribution of expenditures */
hist year_exp if year >= 2005 & year <= 2009 & year_exp > 0 & pkotrad == 1, bin(30) ///
	title("Traditional PKOs, 2005--2009")
hist year_exp if year >= 2005 & year <= 2009 & year_exp > 0 & pkotrans == 1, bin(30) ///
	title("Transitional PKOs, 2005--2009")
summ year_exp if year >=2005 & year <=2009 & ds_ordinal > 0 & year_exp >0, detail
twoway scatter ds_ordinal year_exp if year == 2000 & ds_ordinal > 0 & year_exp >0, jitter(5) ///
	xscale(log) mlabel(statename) mlabposition(6) mlabangle(30) ///
	xlabel(10 20 50 100 200 500 1000 2000) ///
	ylabel(1 "Observer" 2 "Traditional" 3 "Multidimensional" 4 "Enforcement")
graph export "Figures/PKO_budget_2000.png", replace

twoway scatter ds_ordinal year_exp if year == 2009 & ds_ordinal > 0 & year_exp >0, jitter(10) ///
	xscale(log) mlabel(statename) mlabposition(6) mlabangle(30) ///
	xlabel(10 20 50 100 200 500 1000 2000) ///
	ylabel(1 "Observer" 2 "Traditional" 3 "Multidimensional" 4 "Enforcement")
graph export "Figures/PKO_budget_2009.png", replace
/* Other transformations */

gen lnpop = ln(pop)
gen cyear = year
replace cyear = cyear - 1989
capture drop postCW
gen postCW = 0
replace postCW = 1 if year >= 1989
gen confdur = ltsc1 + ltsc2

/* Lagged ds_ordinal */


/* Postconflict */
sort gwno year
gen postconf = 0
replace postconf = 1  if conflict == 0 & conflict[_n-1] > 0 & gwno==gwno[_n-1]
replace postconf = 1  if conflict == 0 & conflict[_n-2] > 0 & gwno==gwno[_n-2]
replace postconf = 1  if conflict == 0 & conflict[_n-3] > 0 & gwno==gwno[_n-3]
replace postconf = 1  if conflict == 0 & conflict[_n-4] > 0 & gwno==gwno[_n-4]
replace postconf = 1  if conflict == 0 & conflict[_n-5] > 0 & gwno==gwno[_n-5]
replace postconf = 1  if conflict == 0 & conflict[_n-6] > 0 & gwno==gwno[_n-6]
replace postconf = 1  if conflict == 0 & conflict[_n-7] > 0 & gwno==gwno[_n-7]
replace postconf = 1  if conflict == 0 & conflict[_n-8] > 0 & gwno==gwno[_n-8]
replace postconf = 1  if conflict == 0 & conflict[_n-9] > 0 & gwno==gwno[_n-9]
replace postconf = 1  if conflict == 0 & conflict[_n-10] > 0 & gwno==gwno[_n-10]

replace postconf = 2  if conflict == 0 & conflict[_n-1] ==2 & gwno==gwno[_n-1]
replace postconf = 2  if conflict == 0 & conflict[_n-2] ==2 & gwno==gwno[_n-2]
replace postconf = 2  if conflict == 0 & conflict[_n-3] ==2 & gwno==gwno[_n-3]
replace postconf = 2  if conflict == 0 & conflict[_n-4] ==2 & gwno==gwno[_n-4]
replace postconf = 2  if conflict == 0 & conflict[_n-5] ==2 & gwno==gwno[_n-5]
replace postconf = 2  if conflict == 0 & conflict[_n-6] ==2 & gwno==gwno[_n-6]
replace postconf = 2  if conflict == 0 & conflict[_n-7] ==2 & gwno==gwno[_n-7]
replace postconf = 2  if conflict == 0 & conflict[_n-8] ==2 & gwno==gwno[_n-8]
replace postconf = 2  if conflict == 0 & conflict[_n-9] ==2 & gwno==gwno[_n-9]
replace postconf = 2  if conflict == 0 & conflict[_n-10] ==2 & gwno==gwno[_n-10]

/* Postconflict variant */
capture drop pc* 
sort gwno year
gen pc0 = 0
gen pc3 = 0
gen pc6 = 0
gen pc9 = 0
replace pc0 = 1  if conflict == 0 & conflict[_n-1] > 0 & gwno==gwno[_n-1]
replace pc0 = 1  if conflict == 0 & conflict[_n-2] > 0 & gwno==gwno[_n-2]
replace pc0 = 1  if conflict == 0 & conflict[_n-3] > 0 & gwno==gwno[_n-3]
replace pc3 = 1  if conflict == 0 & conflict[_n-4] > 0 & gwno==gwno[_n-4]
replace pc3 = 1  if conflict == 0 & conflict[_n-5] > 0 & gwno==gwno[_n-5]
replace pc3 = 1  if conflict == 0 & conflict[_n-6] > 0 & gwno==gwno[_n-6]
replace pc6 = 1  if conflict == 0 & conflict[_n-7] > 0 & gwno==gwno[_n-7]
replace pc6 = 1  if conflict == 0 & conflict[_n-8] > 0 & gwno==gwno[_n-8]
replace pc6 = 1  if conflict == 0 & conflict[_n-9] > 0 & gwno==gwno[_n-9]
replace pc6 = 1  if conflict == 0 & conflict[_n-10] > 0 & gwno==gwno[_n-10]
replace pc9 = 1  if conflict == 0 & conflict[_n-11] > 0 & gwno==gwno[_n-11]
replace pc9 = 1  if conflict == 0 & conflict[_n-12] > 0 & gwno==gwno[_n-12]
replace pc9 = 0 if pc6 > 0 | pc3 > 0 | pc0 > 0 | c1 == 1 | c2 == 1
replace pc6 = 0 if pc3 > 0 | pc0 > 0 | c1 == 1 | c2 == 1
replace pc3 = 0 if pc0 > 0 | c1 == 1 | c2 == 1
replace pc0 = 0 if c1 == 1 | c2 == 1

/* Decade dummies */
gen d70s = 0
gen d80s = 0
gen d90s = 0
replace d70s = 1 if year >= 1970 & year <= 1979
replace d80s = 1 if year >= 1980 & year <= 1989
replace d90s = 1 if year >= 1990 & year <= 1999

capture drop ds_triko
gen ds_triko = 0
replace ds_triko = 1 if ds_ordinal == 1 | ds_ordinal == 2
replace ds_triko = 2 if ds_ordinal == 3 | ds_ordinal == 4
capture label drop tri_lab
label define tri_lab 0 "No PKO" 1 "Traditional" 2 "Transformational"
label values ds_triko tri_lab

sort gwno year
gen l_triko_1 = 0
gen l_triko_2 = 0
replace l_triko_1 = 1 if ds_triko[_n-1] == 1 & gwno == gwno[_n-1]
replace l_triko_2 = 1 if ds_triko[_n-1] == 2 & gwno == gwno[_n-1]


