
****************************************************************
*********************  Assembling data *************************
****************************************************************

/* Stripping the data */ 
keep gwno year conflict c0t c1t c2t lts* ltis timeinstatus timeindep countryname PopSSP* GDPcapSSP* dec*  ///
	PKO* UNMandate DS* changeSource* PSource Source polity2 TimeSinceRegimeChange2 TimeSinceRegimeChange2_start TimeSinceRegimeChange3 ///
	TimeSinceRegimeChange3_start TimeSinceRegimeChange4 TimeSinceRegimeChange4_start PolityStdAuth NA_elect maj_elect pr_elect no_elect
	

*keep gwno year conflict c0t c1t c2t lts* timeinstatus countryname gwabb timeindep PopSSP* GDPcapSSP* changeSource* PSource Source
drop if year < 1950

replace conflict = 0 if conflict < 1 & conflict > 0
capture drop _merge
*merge 1:1 gwno year using "../../Data/Temp/GDPCAPipolated.dta", keepusing(igdpPerCapita)

sort gwno
by gwno: egen temp = count(conflict)
tab gwno if temp == 0
drop if temp == 0
drop if year < 1960

replace conflict = 0 if conflict == .
replace c0t = 0 if c0t == .
replace c1t = 0 if c1t == .
replace c2t = 0 if c2t == .
gen c0 = c0t
gen c1 = c1t
gen c2 = c2t

gen c0_nl = 0 /* non-lagged version of c0 as suppvar*/	
replace c0_nl = 1 if conflict == 0


/* Squared democracy (using a revised version of polity2) */
gen polity2sq = polity2^2
/* Interaction terms */
gen polity2_polity2sq = polity2*polity2sq
gen polity2_c0 = polity2*c0t
gen polity2_c1 = polity2*c1t
gen polity2_c2 = polity2*c2t

/* Remove international PKOs */


foreach ds of varlist DS_* PKObudget PKOmandate {
	replace `ds' = 0 if (UNMandate=="UNIIMOG" | UNMandate=="UNIKOM" | UNMandate=="UNMEE" | UNMandate=="UNMOGIP" | UNMandate=="UNTSO")
}




/* Log transformations */
/* Transformations of PKO variables*/
gen PKOtrad = 0
gen PKOtrans = 0
replace PKOtrad = 1 if PKOmandate == 1 | PKOmandate == 2
replace PKOtrans = 1 if PKOmandate == 3 | PKOmandate == 4
gen lPKObudget = ln(PKObudget)
replace lPKObudget = 0 if lPKObudget == . | lPKObudget < 0
gen lPKObudgsq = lPKObudget*lPKObudget


forvalues i = 1(1)4 {
	gen DS_ord_`i' = 0
	replace DS_ord_`i' = 1 if DS_ordinal == `i'
}
include "regions.do"
*tab region conflict  if year >= 1960 & year <= 2000

/* Code the neighborhood variables */

/* Code conflict neighborhood */
nblag_v2 conflict, primkey(gwno year) neighborfile("smallmdd_test.dta") limit(0) prefix(nb_) op(max) 
replace nb_conflict = 0 if nb_conflict == . /* If missing, set conflict in neighborhood to 0 (probably due to no neighbors) */
label var nb_conflict "Neighboring incidence of conflict from UCDP (extended with COW before 1946)"

nblag_v2 PKOtrans, primkey(gwno year) neighborfile("smallmdd_test.dta") limit(0) prefix(nb_) op(max) 
replace nb_PKOtrans = 0 if nb_PKOtrans == . /* If missing, set conflict in neighborhood to 0 (probably due to no neighbors) */
label var nb_PKOtrans "Presence of transformational PKO in neighboring country"
rename nb_PKOtrans PKOneighbor 

*capture drop n
*by gwno year, sort: gen n =_n
*keep if n == 1
capture drop nc1 nc2 nc1c1 nc1c2 nc1ts0 nc2c1 nc2c2 nc2ts0 
capture drop nc ncc1 ncc2 ncts0
replace nb_conflict = 0 if year == `sim_lyear' + 1
quietly gen nc = nb_conflict >= 1
quietly gen nc0 = nb_conflict == 0
quietly gen nc1 = nb_conflict == 1
quietly gen nc2 = nb_conflict == 2
quietly gen nc1c1 = nc1 * c1
quietly gen nc1c2 = nc1 * c2
quietly gen nc1ts0 = nc1 * ltsc0
quietly gen nc2c1 = nc2 * c1
quietly gen nc2c2 = nc2 * c2
quietly gen nc2ts0 = nc2 * ltsc0
quietly gen ncc1 = nc * c1
quietly gen ncc2 = nc * c2
quietly gen ncts0 = nc0 * ltsc0
quietly gen nc2l = nc[_n-1]  if gwno==gwno[_n-1]
list gwno year conflict c1 c2 nc if (gwno == 432 | gwno == 433 | gwno == 435 | gwno == 436 | gwno == 482 | gwno == 483 |  gwno == 501 | gwno == 522 | gwno == 530 | gwno == 531 | gwno == 600 | gwno == 615 | gwno == 616 | gwno == 620 | gwno == 625 | gwno == 630 | gwno == 640 | gwno == 645 | gwno == 651 | gwno == 652 | gwno == 660 | gwno == 663 | gwno == 666 | gwno == 670 | gwno == 678 | gwno == 690) & (year >= 2012 & year <= 2013)

/* Redo this based on the nblag_v2 statement below? */
sort gwno year
capture drop timeinstatusnc
gen timeinstatusnc=0 if (gwno!=gwno[_n-1]) | year < 1950
replace timeinstatusnc=0 if (gwno==gwno[_n-1]) & (nc[_n-0]!=nc[_n-1])
replace timeinstatusnc=timeinstatusnc[_n-1]+1 if (gwno==gwno[_n-1]) & (nc[_n-0]==nc[_n-1]) & timeinstatusnc==.
replace timeinstatusnc=timeinstatusnc+1 /* Add 1 to facilitate log transformation (???) */
capture drop ltisnc
gen ltisnc = timeinstatusnc[_n-1] if gwno==gwno[_n-1]

gen ltsnc = ln(ltisnc) /* Log of number of years in peace in all neighboring countries */


/* Log transform; Generate interactions between pop, gdp, decades, and conflict */
gen ltimeindep = ln(timeindep)
disp "here"

	*preserve
	gen lpop = ln(PopSSP2)
	gen lGDPcap = ln(GDPcapSSP2)
	/* Variable for endogenous growth equation */
	sort gwno year
	quietly gen loggdpgrowth = .
	quietly replace loggdpgrowth = lGDPcap - lGDPcap[_n-1] if gwno == gwno[_n-1]
	gen lpop_c1 = lpop * c1
	gen lpop_c2 = lpop * c2
	gen lpop_ltsc0 = lpop * ltsc0
	gen lGDPcap_c1 = lGDPcap * c1
	gen lGDPcap_c2 = lGDPcap * c2
	gen lGDPcap_ltsc0 = lGDPcap * ltsc0
	gen polity2_GDPcap = polity2 * lGDPcap
	gen polity2sq_GDPcap = polity2sq * lGDPcap
	gen polity2sq_c1 = polity2sq * c1t
	gen polity2sq_c2 = polity2sq * c2t
	
	foreach d in 60 70 80 90 {
		gen dec`d'_lpop = dec`d' * lpop
		gen dec`d'_lGDPcap = dec`d' * lGDPcap
	} /* end foreach d */
	drop if year < 1960
	
	by gwno: gen llGDPcap = lGDPcap[_n-1]
replace lGDPcap = llGDPcap	
	
/* Remove countries that involve missing-data problems */
/* Zanzibar, Palestine, Yemen, People's Republic of and Vietnam, Republic of */
quietly drop if gwno == 511 | gwno == 667 | gwno == 680 | gwno == 817
/* Microstates: Bahamas, Barbados, Belize, Malta, Iceland, Maldives, Brunei */
quietly drop if gwno == 31 | gwno == 53 | gwno == 80 | gwno== 338 | gwno == 395 | gwno == 781 | gwno == 835
/* Defunct states: Czechoslovakia and Serbia/Yugoslavia and Germany and 711 */
quietly drop if gwno == 315 | gwno == 345 | gwno == 265 | gwno == 711

/* Missing data for Polity because of different independence definitions */
drop if gwno == 552 & year <= 1969 /* Zimbabwe (Rhodesia) */
drop if gwno == 771 & year == 1971 /* Bangladesh */
drop if gwno == 690 & year <= 1962 /* Kuwait */
drop if gwno == 452	& year <=1959 /* Ghana */
drop if gwno == 616	& year <=1958 /* Tunisia */
drop if gwno == 817	& year == 1954 /* Vietnam, Republic of */



/* Remove countries that create problems in simulations for out-of-sample evaluation  */
if `sim_fyear' < 2011 {
	drop if gwno == 626 /* South Sudan */
}
if `sim_fyear' < 2008 {
	drop if gwno == 347 /* Kosovo */
}
if `sim_fyear' < 2006 {
	drop if gwno == 345 | gwno == 340 | gwno == 341 /* Serbia and Montenegro */
}
if `sim_fyear' < 2002 {
	drop if gwno == 860 /* East Timor */
}
if `sim_fyear' < 1994 {
	drop if gwno == 315 | gwno = 316 | gwno == 317 | gwno == 531 /* Czechoslovakia, Czekh Rep., Slovakia, Eritrea */
}

drop if gwno == 626 /* Drop it anyhow; no growth projections available just now */

/* Take out some country years with missing data  */
drop if gwno == 411 & year <= 1967
drop if gwno == 760 & year <= 1969
drop if gwno == 940 & year <= 1989
drop if gwno == 349 & year == 1991
	
drop if gwno == . | year == .

/* No pko variable as constraint on scenarios */

gen nopko = 0
replace nopko = 1 if (gwno == 2 | gwno == 200 | gwno == 220 | gwno == 365 | gwno == 710) /* UNSC permanent members */
replace nopko = 1 if (gwno == 70 | gwno == 140 | gwno == 475 | gwno == 740 | gwno == 750 | gwno == 770 | gwno == 771 | gwno == 850) /* UNSC permanent members */
tab gwno if year == 2013 & nopko == 1

gen lPKObudgetmarker = 0
replace lPKObudgetmarker = 1 if lPKObudget > 0
replace PKOprevious = 0 if PKOprevious == .

include "RotatingSecurityCouncilMembership.do"

/*lag PKO varaibles */
sum PKOtrad PKOtrans lPKObudget lPKObudgsq PKOneighbor
sort gwno year
gen lPKOtrad = PKOtrad[_n-1] if gwno == gwno[_n-1]
gen lPKOtrans = PKOtrans[_n-1] if gwno == gwno[_n-1]
gen llPKObudget = lPKObudget[_n-1] if gwno == gwno[_n-1]
gen llPKObudgsq = lPKObudgsq[_n-1] if gwno == gwno[_n-1]
gen lPKOneighbor = PKOneighbor[_n-1] if gwno == gwno[_n-1]

gen dummyconflict = conflict >= 1
tab conflict dummyconflict

/*PKO gdp interactions */
gen lGDPcapPKOtrans = lGDPcap * lPKOtrans
gen lGDPcapPKOtrad = lGDPcap * lPKOtrad
gen lGDPcapllPKObudget = lGDPcap * llPKObudget
gen lGDPcapllPKObudgsq = lGDPcap * llPKObudgsq

save "PredictionPKO1.dta", replace
*restore


