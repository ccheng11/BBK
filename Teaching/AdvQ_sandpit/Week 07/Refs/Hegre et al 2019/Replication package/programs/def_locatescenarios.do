capture program drop locatescenarios

program locatescenarios
	version 12
    syntax namelist, scenariostyle(string) simsetname(namelist) simtype(string) levels(numlist min=3 max=3 integer) path(string) ///
		years(numlist min=2 max=2 integer)

	/* 
	Program ..
	*/
	set more off
	local namecount = wordcount("`namelist'")
	local levelcount = `namecount'
	local currdir = c(pwd) /* Name of current directory, to take us back to where we belong */
	
	local path = "`path'"  + "`simsetname'"
*	local path = "C:\PredictionTemp\" + "`simsetname'"
*	local path = "\\lh-sv\sv\psi-lager\prediksjon\PredictionTemp\" + "`simsetname'"
	forvalues l = 1(1)`levelcount' {
		local l`l'name = word("`namelist'",`l')
		display "Level `l' variable: " word("`namelist'",`l')
		local ln`l' = word("`levels'",`l')
	} /* end forvalues l */

	
	local fyear = word("`years'", 1)
	local lyear = word("`years'", 2)
	local yspan = 1+`lyear'-`fyear'
	display "yspan: " `yspan'
	

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
				capture confirm file "Res_`simtype'_`i'_`j'_`k'.txt"
				if _rc==0 {
						display "Reading in file Res_`simtype'_`i'_`j'_`k'.txt..."
						insheet using "Res_`simtype'_`i'_`j'_`k'.txt", names clear
						drop if simmed == "False" & gwno == gwno[_n-1]
						sort simno gwno year
*						summ
						gen sc_cnt = .
						gen temp = .
						replace temp = conflict if year == `fyear'
						replace temp = temp[_n-1]+conflict if year > `fyear' & year <= `lyear'
						replace sc_cnt = 1 if year == `lyear' & temp==0 
						replace sc_cnt = 2 if year == `lyear' & temp>0 & temp<=`yspan' 
						replace sc_cnt = 3 if year == `lyear' & temp>`yspan' & temp<=(2*`yspan') 
						sort simno gwno year
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
							recode sc_temp (1=1) (2=2) (8=3) (4=4) (10=5) (11=6) (1.5=7), gen(scenario)
							drop sc_temp
						}
						tab scenario
*						stop

/* MUST DEFINE ANOTHER BASELINE SCENARIO FOR WAR/CONFLICT COUTNRIES; MAKE SURE THEY ARE IN WAR */


						if "`scenariostyle'" == "group" {
							gen scenario = 1
							/* Group scenarios: 
							Group 1: 6 low/middle-income war countries (gdpcap<10000) ; conflict == 2, lagged conflict > 0
							520 Somalia, 625 Sudan, 652 Syria, 678 Yemen, 700 Afghanistan, 770 Pakistan
							
							Group 2:  13 low/middle-income conflict countries gpdcap < 10000; conflict == 1; lagged confilct > 0
							100 Colombia, 432 Mali, 475 Nigeria, 482 CAR, 490 DRC, 517 Rwanda, 530 Ethiopia
							615 Algeria, 640 Turkey, 645 Iraq, 750 India, 775 Myanmar, 840 Philippines
							
							Group 3: 10 Low-income peaceful countries (gdpcap <1200 (7.09); conflict = 0, ltsc0 >3 (20 years))
							420 Gambia, 439 Burkina Faso, 461 Togo, 510 Tanzania, 551 Zambia, 552 Zimbabwe, 
							553 Malawi, 580 Madagsacar, 712 Mongolia, 731 North Korea
							
							Group 4: 20 Lower middle-income peaceful countries (1200(7.09)<gdpcap <4000 (8.29); conflict = 0, ltsc0 >3 (20 years))
							51, 91, 92, 93, 130, 145, 150, 402, 434, 452, 471, 501, 572, 600, 703, 760, 812, 816 Vietnam, 940, 950 Fiji
							
							Group 5: 24 Upper middle-income peaceful countries (4000(8.29)<gdpcap <10000 (9.21); conflict = 0, ltsc0 >3 (20 years))
							40, 42, 80, 95, 110, 140, 339, 360 Rumania, 369 Ukraine, 481 Gabon, 560 S Africa, 565, 571, 616 Tunisia, 660  Lebanon, 
							663 Jordan, 670, 692, 698, 701 Turkmenistan, 781 Maldives
							*/
							replace scenario = 2 if /// /* Deescalation in war countries */
								(sc_cnt == 2 & gwno == 520) | (sc_cnt == 2 & gwno == 625) | (sc_cnt == 2 & gwno == 652) | ///
								(sc_cnt == 2 & gwno == 678) | (sc_cnt == 2 & gwno == 700) | (sc_cnt == 2 & gwno == 770)  
							replace scenario = 11 if  /// /* Peace in war countries */
								(sc_cnt == 1 & gwno == 520) | (sc_cnt == 1 & gwno == 625) | (sc_cnt == 1 & gwno == 652) | ///
								(sc_cnt == 1 & gwno == 678) | (sc_cnt == 1 & gwno == 700) | (sc_cnt == 1 & gwno == 770)  
							replace scenario = 4 if /// /* Peace in minor conflict countries */
								(sc_cnt == 1 & gwno == 100) | (sc_cnt == 1 & gwno == 432) | (sc_cnt == 1 & gwno == 475) | ///
								(sc_cnt == 1 & gwno == 482) | (sc_cnt == 1 & gwno == 490) | (sc_cnt == 1 & gwno == 517) | ///  
								(sc_cnt == 1 & gwno == 530) | (sc_cnt == 1 & gwno == 615) | (sc_cnt == 1 & gwno == 640) | ///
								(sc_cnt == 1 & gwno == 645) | (sc_cnt == 1 & gwno == 750) | (sc_cnt == 1 & gwno == 775) | ///  
								(sc_cnt == 1 & gwno == 840) 
							replace scenario = 5 if /// /* Minor conflict in peaceful low-income countries */
								(sc_cnt == 2 & gwno == 420) | (sc_cnt == 2 & gwno == 439) | (sc_cnt == 2 & gwno == 461) | ///
								(sc_cnt == 2 & gwno == 510) | (sc_cnt == 2 & gwno == 551) | (sc_cnt == 2 & gwno == 552)  | /// 
								(sc_cnt == 2 & gwno == 553) | (sc_cnt == 2 & gwno == 580) | (sc_cnt == 2 & gwno == 712) | ///
								(sc_cnt == 2 & gwno == 731) 
							replace scenario = 12 if /// /* Major conflict in peaceful low-income countries */
								(sc_cnt == 3 & gwno == 420) | (sc_cnt == 3 & gwno == 439) | (sc_cnt == 3 & gwno == 461) | ///
								(sc_cnt == 3 & gwno == 510) | (sc_cnt == 3 & gwno == 551) | (sc_cnt == 3 & gwno == 552 ) | /// 
								(sc_cnt == 3 & gwno == 553) | (sc_cnt == 3 & gwno == 580) | (sc_cnt == 3 & gwno == 712) | ///
								(sc_cnt == 3 & gwno == 731) 
							replace scenario = 7 if /// /* Minor conflict in peaceful lower-middle-income countries */
								(sc_cnt == 2 & gwno == 51) | (sc_cnt == 2 & gwno == 91) | (sc_cnt == 2 & gwno == 92) | ///
								(sc_cnt == 2 & gwno == 93) | (sc_cnt == 2 & gwno == 130) | (sc_cnt == 2 & gwno == 145) | /// 
								(sc_cnt == 2 & gwno == 150) | (sc_cnt == 2 & gwno == 402) | (sc_cnt == 2 & gwno == 434) | ///
								(sc_cnt == 2 & gwno == 452) | (sc_cnt == 2 & gwno == 471) | (sc_cnt == 2 & gwno == 501) | ///
								(sc_cnt == 2 & gwno == 572) | (sc_cnt == 2 & gwno == 600) | (sc_cnt == 2 & gwno == 703) | ///
								(sc_cnt == 2 & gwno == 760) | (sc_cnt == 2 & gwno == 812) | (sc_cnt == 2 & gwno == 816) | ///
								(sc_cnt == 2 & gwno == 940) | (sc_cnt == 2 & gwno == 950) 
							replace scenario = 8 if /// /* Major conflict in peaceful lower-middle-income countries */
								(sc_cnt == 3 & gwno == 51) | (sc_cnt == 3 & gwno == 91) | (sc_cnt == 3 & gwno == 92) | ///
								(sc_cnt == 3 & gwno == 93) | (sc_cnt == 3 & gwno == 130) | (sc_cnt == 3 & gwno == 145) | /// 
								(sc_cnt == 3 & gwno == 150) | (sc_cnt == 3 & gwno == 402) | (sc_cnt == 3 & gwno == 434) | ///
								(sc_cnt == 3 & gwno == 452) | (sc_cnt == 3 & gwno == 471) | (sc_cnt == 3 & gwno == 501) | ///
								(sc_cnt == 3 & gwno == 572) | (sc_cnt == 3 & gwno == 600) | (sc_cnt == 3 & gwno == 703) | ///
								(sc_cnt == 3 & gwno == 760) | (sc_cnt == 3 & gwno == 812) | (sc_cnt == 3 & gwno == 816) | ///
								(sc_cnt == 3 & gwno == 940) | (sc_cnt == 3 & gwno == 950) 
							replace scenario = 9 if /// /* Minor conflict in peaceful upper-middle-income countries */
								(sc_cnt == 2 & gwno == 40) | (sc_cnt == 2 & gwno == 42) | (sc_cnt == 2 & gwno == 80) | ///
								(sc_cnt == 2 & gwno == 95) | (sc_cnt == 2 & gwno == 110) | (sc_cnt == 2 & gwno == 140) | /// 
								(sc_cnt == 2 & gwno == 339) | (sc_cnt == 2 & gwno == 360) | (sc_cnt == 2 & gwno == 369) | ///
								(sc_cnt == 2 & gwno == 481) | (sc_cnt == 2 & gwno == 560) | (sc_cnt == 2 & gwno == 565) | ///
								(sc_cnt == 2 & gwno == 571) | (sc_cnt == 2 & gwno == 616) | (sc_cnt == 2 & gwno == 660) | ///
								(sc_cnt == 2 & gwno == 663) | (sc_cnt == 2 & gwno == 670) | (sc_cnt == 2 & gwno == 692) | ///
								(sc_cnt == 2 & gwno == 698) | (sc_cnt == 2 & gwno == 701) | (sc_cnt == 2 & gwno == 781) 
							replace scenario = 10 if /// /* Major conflict in peaceful upper-middle-income countries */
								(sc_cnt == 3 & gwno == 40) | (sc_cnt == 3 & gwno == 42) | (sc_cnt == 3 & gwno == 80) | ///
								(sc_cnt == 3 & gwno == 95) | (sc_cnt == 3 & gwno == 110) | (sc_cnt == 3 & gwno == 140) | /// 
								(sc_cnt == 3 & gwno == 339) | (sc_cnt == 3 & gwno == 360) | (sc_cnt == 3 & gwno == 369) | ///
								(sc_cnt == 3 & gwno == 481) | (sc_cnt == 3 & gwno == 560) | (sc_cnt == 3 & gwno == 565) | ///
								(sc_cnt == 3 & gwno == 571) | (sc_cnt == 3 & gwno == 616) | (sc_cnt == 3 & gwno == 660) | ///
								(sc_cnt == 3 & gwno == 663) | (sc_cnt == 3 & gwno == 670) | (sc_cnt == 3 & gwno == 692) | ///
								(sc_cnt == 3 & gwno == 698) | (sc_cnt == 3 & gwno == 701) | (sc_cnt == 3 & gwno == 781) 
							by simno: egen gsc_temp_g = max(scenario)
							replace scenario = gsc_temp_g
							drop gsc_temp_g
							drop sc_cnt
							replace scenario = 3 if scenario == 11
							replace scenario = 6 if scenario == 12
							summarize simno
							local sims = r(max)
							replace scenario = 1 if (scenario == 7 | scenario == 9) & simno > `sims'*.6
						}
							*/
						forvalues gsc = 1(1)10{
							preserve
							keep if scenario == `gsc'
							if _N > 0 {
								export delimited "Res_`simtype'_`i'_`gsc'_`j'_`k'.txt", replace
							}
							restore
						}
						erase "Res_`simtype'_`i'_`j'_`k'.txt"
						display "The file Res_`simtype'_`i'_`j'_`k'.txt deleted, replaced with 10 scenario-specific files"
				} /* end if _rc==0 */
				else {	
					display "The file Res_`simtype'_`i'_`j'_`k'.txt does not exist"
				} /* end else */
			} /* end forvalues k = 1(1)`ln3' */
		} /* end forvalues j = 1(1)`ln2' */
	} /* end forvalues i = 1(1)`ln1' */


cd "`currdir'" /* Back to original directory */

end
