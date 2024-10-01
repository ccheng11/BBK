capture program drop locatescenarios_v2c

program locatescenarios_v2c
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

						display "First pass, scenarios 1-3"
						capture drop scenario
						capture drop sc_temp_cnt
						capture drop sc_temp
						if "`scenariostyle'" == "country" {
						gen sc_temp = 1
							by simno gwno: egen sc_temp_cnt = max(sc_cnt)
							replace sc_cnt = sc_temp_cnt
							replace sc_temp = 1 if sc_temp_cnt == 1 & gwno == 510
							replace sc_temp = 2 if sc_temp_cnt == 2 & gwno == 510
							replace sc_temp = 3 if sc_temp_cnt == 3 & gwno == 510
							by simno: egen sc_temp_g = max(sc_temp)
							replace sc_temp = sc_temp_g
							drop sc_temp_g
							drop sc_temp_cnt
						quietly	recode sc_temp (1=1) (2=2) (3=3), gen(scenario)
						quietly	drop sc_temp
						}
						tab scenario
						forvalues gsc = 1(1)3{
							preserve
							keep if scenario == `gsc'
							if _N > 0 {
								export delimited "Res_`simtype'_L_`gsc'_`i'_`j'_`k'.txt", replace
							}
							restore
						}
						
						
						display "Second pass, scenarios 4-6"
						capture drop scenario
						capture drop sc_temp_cnt
						capture drop sc_temp
						if "`scenariostyle'" == "country" {
						gen sc_temp = 1
							by simno gwno: egen sc_temp_cnt = max(sc_cnt)
							replace sc_cnt = sc_temp_cnt
							replace sc_temp = 5 if sc_temp_cnt == 1 & gwno == 475
							replace sc_temp = 4 if sc_temp_cnt == 2 & gwno == 475
							replace sc_temp = 6 if sc_temp_cnt == 3 & gwno == 475
							by simno: egen sc_temp_g = max(sc_temp)
							replace sc_temp = sc_temp_g
							drop sc_temp_g
							drop sc_temp_cnt
						quietly	recode sc_temp (5=4) (4=5) (6=6), gen(scenario)
						quietly	drop sc_temp
						}
						tab scenario
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
