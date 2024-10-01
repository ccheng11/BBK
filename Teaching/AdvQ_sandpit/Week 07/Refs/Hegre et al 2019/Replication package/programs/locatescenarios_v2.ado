capture program drop locatescenarios_v2

program locatescenarios_v2
	version 12
    syntax namelist, scenariostyle(string) simsetname(namelist) simtype(string) levels(numlist min=3 max=3 integer) path(string) ///
		fyear(int) lyear(int)

	/* 
	Program ..
	*/
	display "This program assignes countries to scenarios based in their conflict and development status in "
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
	local fyear = `fyear' 
	local lyear = `lyear' 
	local yspan = 1+`lyear'-`fyear'
	display "yspan: " `yspan'
	display "first year: " `fyear'
	display "last year: " `lyear'
	display "This program assignes countries to scenarios based in their conflict and development status in `fyear'"
	display "Folder: " "`path'"
	display "Levels: " "`levels'"
	display "No. of values, level 1: " `ln1'
	display "No. of values, level 2: " `ln2'
	display "No. of values, level 3: " `ln3'
	display "Location for results files:  `currdir'/Results/`simsetname'" 
	display "Scenario style: " "`scenariostyle'"

	cd `path'

	forvalues i = 1(1)`ln1' {
		display "i: " `i'
		forvalues j = 1(1)`ln2' {
			display "j: " `j'
			forvalues k = 1(1)`ln3' {
				display "k: " `k'
				capture confirm file "Res_`simtype'_1_`i'_`j'_`k'.txt"
				if _rc==0 {
						display "Reading in file Res_`simtype'_1_`i'_`j'_`k'.txt..."
						insheet using "Res_`simtype'_1_`i'_`j'_`k'.txt", names clear
						capture mkdir orig
						export delimited "orig/Res_`simtype'_1_`i'_`j'_`k'.txt", replace
						drop if simmed == "False" & gwno == gwno[_n-1]		
						sort simno gwno year
*						summ
						capture drop sc_cnt
						gen sc_cnt = .
						capture drop temp
						gen temp = .
						replace temp = conflict if year == `fyear'
						replace temp = temp[_n-1]+conflict if year > `fyear' & year <= `lyear'
						replace sc_cnt = 1 if year == `lyear' & temp==0 
						replace sc_cnt = 2 if year == `lyear' & temp>0 & temp<=`yspan' 
						replace sc_cnt = 3 if year == `lyear' & temp>`yspan' & temp<=(2*`yspan') 
						sort simno gwno year
						/*
						local scenariostyle = "country"
						capture drop scenario
						capture drop sc_temp_cnt
						capture drop sc_temp
						if "`scenariostyle'" == "country" {
						gen sc_temp = 1
							by simno gwno: egen sc_temp_cnt = max(sc_cnt)
							replace sc_cnt = sc_temp_cnt
							replace sc_temp = 2 if sc_temp_cnt == 2 & gwno == 510
							replace sc_temp = 8 if sc_temp_cnt == 3 & gwno == 510
							replace sc_temp = 4 if sc_temp_cnt == 2 & gwno == 94
							replace sc_temp = 10 if sc_temp_cnt == 3 & gwno == 94
							replace sc_temp = 11 if sc_temp_cnt == 1 & gwno == 625
							replace sc_temp = 1.5 if sc_temp_cnt == 2 & gwno == 625
							by simno: egen sc_temp_g = max(sc_temp)
							replace sc_temp = sc_temp_g
							drop sc_temp_g
							drop sc_temp_cnt
*							capture drop temp
							/* The sc_temp values have been set to order them according to frequency. Recode to the old number system: */
						quietly	recode sc_temp (1=1) (2=2) (8=3) (4=4) (10=5) (11=6) (1.5=7), gen(scenario)
						quietly	drop sc_temp
						}
						quietly tab scenario
						*/

/* MUST DEFINE ANOTHER BASELINE SCENARIO FOR WAR/CONFLICT COUTNRIES; MAKE SURE THEY ARE IN WAR */


						if "`scenariostyle'" == "group" {
							capture drop scenario
							gen scenario = 1
							/* Group scenarios: tab 
							Group 1: 6 low/middle-income war countries (gdpcap<8000) ; conflict == 2, lagged conflict > 0
							369 Ukraine, 475 Nigeria, 520 Somalia, 625 Sudan, 645 Iraq, 652 Syria, 678 Yemen, 700 Afghanistan, 770 Pakistan
							
							Group 3: 10 Low-income peaceful countries (gdpcap <2000 (xxx); conflict = 0, ltsc0 >3 (20 years))
							434 Benin, 420 Gambia, 439 Burkina Faso, 461 Togo, 501 Kenya, 510 Tanzania, 551 Zambia, 552 Zimbabwe, 
							553 Malawi, 580 Madagsacar, 731 North Korea
							
							Group 4: 20 Lower middle-income peaceful countries (2000(xxx)<gdpcap <8000 (8.29); conflict = 0, ltsc0 >3 (20 years))
							51, 91, 92, 93, 130, 145, 150, 402, 434, 452, 471, 501, 572, 600, 703, 760, 812, 816 Vietnam, 940, 950 Fiji
							
							
							Group 2:  13 low/middle-income conflict countries gpdcap < 10000; conflict == 1; lagged confilct > 0
							100 Colombia, 432 Mali, 482 CAR, 490 DRC, 517 Rwanda, 530 Ethiopia
							615 Algeria, 640 Turkey, 645 Iraq, 750 India, 775 Myanmar, 840 Philippines
							
							Group 5: 24 Upper middle-income peaceful countries (4000(8.29)<gdpcap <10000 (9.21); conflict = 0, ltsc0 >3 (20 years))
							40, 42, 80, 95, 110, 140, 339, 360 Rumania, 369 Ukraine, 481 Gabon, 560 S Africa, 565, 571, 616 Tunisia, 660  Lebanon, 
							663 Jordan, 670, 692, 698, 701 Turkmenistan, 781 Maldives
							*/

 							replace scenario =   2 if /// /* Deescalation in middle/poor war countries Gr. 1 */
								(sc_cnt == 2 & gwno == 520) | (sc_cnt == 2 & gwno == 625) | (sc_cnt == 2 & gwno == 652) | ///
								(sc_cnt == 2 & gwno == 678) | (sc_cnt == 2 & gwno == 700) | (sc_cnt == 2 & gwno == 770) | ///
								(sc_cnt == 2 & gwno == 475) | (sc_cnt == 2 & gwno == 369) | (sc_cnt == 2 & gwno == 645) 
							
							 replace scenario =  4  if  /// /* Peace in poor war countries Gr. 1  */
								(sc_cnt == 1 & gwno == 520) | (sc_cnt == 1 & gwno == 625) | (sc_cnt == 1 & gwno == 652) | ///
								(sc_cnt == 1 & gwno == 678) | (sc_cnt == 1 & gwno == 700) | (sc_cnt == 1 & gwno == 770) | ///
								(sc_cnt == 1 & gwno == 475) | (sc_cnt == 1 & gwno == 369) | (sc_cnt == 1 & gwno == 645) 
								
							 replace scenario =  3 if  /// /* Continued Major conflict in poor war countries Gr. 1  */
								(sc_cnt == 3 & gwno == 520) | (sc_cnt == 3 & gwno == 625) | (sc_cnt == 3 & gwno == 652) | ///
								(sc_cnt == 3 & gwno == 678) | (sc_cnt == 3 & gwno == 700) | (sc_cnt == 3 & gwno == 770) | ///
								(sc_cnt == 3 & gwno == 475) | (sc_cnt == 3 & gwno == 369) | (sc_cnt == 3 & gwno == 645) 
							
							
							tab scenario
							capture drop gsc_temp_g
							by simno: egen gsc_temp_g = max(scenario)
							tab gsc_temp_g
							quietly replace scenario = 1 if gsc_temp_g == 4 /*Peace in poor war countries*/
							quietly replace scenario = 2 if gsc_temp_g == 2 /*Deescalation in poor war countrie*/
							quietly replace scenario = 3 if gsc_temp_g == 3 /*Continued Major conflict in poor war countries*/
							drop gsc_temp_g
						}
						tab scenario
							*/
						forvalues gsc = 1(1)3{
							preserve
							keep if scenario == `gsc'
							if _N > 0 {
								export delimited "Res_`simtype'_L_`gsc'_`i'_`j'_`k'.txt", replace
							}
							restore
						}
						
						
						if "`scenariostyle'" == "group" {
							capture drop scenario
							gen scenario = 1
							/* Group scenarios: tab 
							Group 1: 6 low/middle-income war countries (gdpcap<8000) ; conflict == 2, lagged conflict > 0
							369 Ukraine, 475 Nigeria, 520 Somalia, 625 Sudan, 645 Iraq, 652 Syria, 678 Yemen, 700 Afghanistan, 770 Pakistan
							
							Group 3: 10 Low-income peaceful countries (gdpcap <2000 (xxx); conflict = 0, ltsc0 >3 (20 years))
							434 Benin, 420 Gambia, 439 Burkina Faso, 461 Togo, 501 Kenya, 510 Tanzania, 551 Zambia, 552 Zimbabwe, 
							553 Malawi, 580 Madagsacar, 731 North Korea
							
							Group 4: 20 Lower middle-income peaceful countries (2000(xxx)<gdpcap <8000 (8.29); conflict = 0, ltsc0 >3 (20 years))
							51, 91, 92, 93, 130, 145, 150, 402, 434, 452, 471, 501, 572, 600, 703, 760, 812, 816 Vietnam, 940, 950 Fiji
							
							
							Group 2:  13 low/middle-income conflict countries gpdcap < 10000; conflict == 1; lagged confilct > 0
							100 Colombia, 432 Mali, 482 CAR, 490 DRC, 517 Rwanda, 530 Ethiopia
							615 Algeria, 640 Turkey, 645 Iraq, 750 India, 775 Myanmar, 840 Philippines
							
							Group 5: 24 Upper middle-income peaceful countries (4000(8.29)<gdpcap <10000 (9.21); conflict = 0, ltsc0 >3 (20 years))
							40, 42, 80, 95, 110, 140, 339, 360 Rumania, 369 Ukraine, 481 Gabon, 560 S Africa, 565, 571, 616 Tunisia, 660  Lebanon, 
							663 Jordan, 670, 692, 698, 701 Turkmenistan, 781 Maldives
							*/
							 replace scenario = 2  if /// /* Peace in peaceful low-income countries (gr. 3) */
								(sc_cnt == 1 & gwno == 420) | (sc_cnt == 1 & gwno == 439) | (sc_cnt == 1 & gwno == 461) | ///
								(sc_cnt == 1 & gwno == 510) | (sc_cnt == 1 & gwno == 551) | (sc_cnt == 1 & gwno == 552) | /// 
								(sc_cnt == 1 & gwno == 553) | (sc_cnt == 1 & gwno == 580) | (sc_cnt == 1 & gwno == 712) | ///
								(sc_cnt == 1 & gwno == 731) | (sc_cnt == 1 & gwno == 434) | (sc_cnt == 1 & gwno == 501) | ///
								(sc_cnt == 1 & gwno == 452) 							
								
							 replace scenario =  3 if /// /* Conflict in peaceful low-income countries (gr. 3) */
								(sc_cnt == 2 & gwno == 420) | (sc_cnt == 2 & gwno == 439) | (sc_cnt == 2 & gwno == 461) | ///
								(sc_cnt == 2 & gwno == 510) | (sc_cnt == 2 & gwno == 551) | (sc_cnt == 2 & gwno == 552)  | /// 
								(sc_cnt == 2 & gwno == 553) | (sc_cnt == 2 & gwno == 580) | (sc_cnt == 2 & gwno == 712) | ///
								(sc_cnt == 2 & gwno == 731) | (sc_cnt == 2 & gwno == 434) | (sc_cnt == 2 & gwno == 501) | ///
								(sc_cnt == 2 & gwno == 452) 	
								
							replace scenario = 4  if /// /* Major conflict in peaceful low-income countries (gr. 3) */
								(sc_cnt == 3 & gwno == 420) | (sc_cnt == 3 & gwno == 439) | (sc_cnt == 3 & gwno == 461) | ///
								(sc_cnt == 3 & gwno == 510) | (sc_cnt == 3 & gwno == 551) | (sc_cnt == 3 & gwno == 552)  | /// 
								(sc_cnt == 3 & gwno == 553) | (sc_cnt == 3 & gwno == 580) | (sc_cnt == 3 & gwno == 712) | ///
								(sc_cnt == 3 & gwno == 731) | (sc_cnt == 3 & gwno == 434) | (sc_cnt == 3 & gwno == 501) | ///
								(sc_cnt == 3 & gwno == 452) 																								
							
							tab scenario
							capture drop gsc_temp_g
							by simno: egen gsc_temp_g = max(scenario)
							tab gsc_temp_g
							quietly replace scenario = 4 if gsc_temp_g == 2 /*Peace in peaceful low-income countries */
							quietly replace scenario = 5 if gsc_temp_g == 3 /*Conflict in peaceful low-income countries*/
							quietly replace scenario = 6 if gsc_temp_g == 4 /* Major conflict in peaceful low-income countries */
							drop gsc_temp_g
						}
						tab scenario
							*/
						forvalues gsc = 4(1)6{
							preserve
							keep if scenario == `gsc'
							if _N > 0 {
								export delimited "Res_`simtype'_L_`gsc'_`i'_`j'_`k'.txt", replace
							}
							restore
						}
						erase "Res_`simtype'_1_`i'_`j'_`k'.txt"
						display "The file Res_`simtype'_1_`i'_`j'_`k'.txt deleted, replaced with 6 scenario-specific files"
				} /* end if _rc==0 */
				else {	
					display "The file Res_`simtype'_1_`i'_`j'_`k'.txt does not exist"
					
				} /* end else */
			} /* end forvalues k = 1(1)`ln3' */
		} /* end forvalues j = 1(1)`ln2' */
	} /* end forvalues i = 1(1)`ln1' */


cd "`currdir'" /* Back to original directory */

end
