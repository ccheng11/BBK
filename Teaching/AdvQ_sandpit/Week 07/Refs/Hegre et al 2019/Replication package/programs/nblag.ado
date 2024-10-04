program nblag, rclass
	version 10
	syntax varlist [, PRIMkey(string) PREfix(string) Limit(real 0.0) op(string)] 
	di "`primkey'"
	di "`prefix'"
	di `limit'
	if "`primkey'" == "" {
		local primkey = "primkey"
	}
	if "`prefix'" == "" {
		local prefix = "nb_"
	}
	confirm numeric variable `primkey'
	confirm new variable `prefix'`varlist'
	tempfile nb_temp
	tempfile var_temp

	preserve
	keep `varlist' `primkey'
	sort `primkey'
	save `var_temp'
	capture sysuse "../OriginalData/Neighborhood/Mindist1945_2050"
	capture sysuse Mindist1945_2050.dta, clear
	sort `primkey'
	merge primkey using `var_temp'
	sum mindist
	tab _merge
	drop if mindist>=`limit'
	replace `primkey' = (gwnoB * 10000) + year
	collapse (`op') `varlist', by(`primkey')
	rename `varlist' `prefix'`varlist'
	sort `primkey'
	save `nb_temp'
	restore
	sort `primkey'
	capture drop _merge
	merge `primkey' using `nb_temp'
	*sum
end
