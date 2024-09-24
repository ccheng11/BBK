program nblag_v2, rclass
	version 10
	syntax varlist, PRIMkey(namelist min=2 max=2) neighborfile(string) [PREfix(string) Limit(real 0.0) op(string)] 
	/* PRIMkey: first item is geographical unit `gu', second item temporal unit `tu' */
	/* Neighborhood file has key: geographical unit name (gun) + a and gun + b; e.g. gwnoa and gwnob */
	local gu = word("`primkey'", 1)
	local tu = word("`primkey'", 2)
	local gua = "`gu'"+"a"
	local gub = "`gu'"+"b"
	
	capture confirm file "`neighborfile'"
	if !_rc == 0 {
			display "File `neighborfile' not found - nblag_v2 terminates"
			stop
	}

	display "Neighborfile: `neighborfile'"
	
	display "Merge variables: Geographical: " "`gu'" ", temporal: " "`tu'"
	display "Prefix for resulting neighborhood variable: " "`prefix'"
	display "Maximum distance defining neighborhood: " `limit'
	/*
	if "`primkey'" == "" {
		local primkey = "primkey"
	}
	*/
	
	if "`prefix'" == "" {
		local prefix = "nb_"
	}
	confirm numeric variable `primkey'
	confirm numeric variable `gu'
	confirm numeric variable `tu'
	
	confirm new variable `prefix'`varlist'

	tempfile nb_temp
	tempfile var_temp

	preserve
	keep `varlist' `primkey'
	sort `gu' `tu'
	save `var_temp'
	use "`neighborfile'"
	gen `gu' = `gua'
	sort `gu' `tu'
	quietly merge `gu' `tu' using `var_temp'
	sum mindist
	tab _merge
	drop if mindist>`limit'
	replace `gu' = `gub'
	collapse (`op') `varlist', by(`primkey')
	rename `varlist' `prefix'`varlist'
	display "HERE"
	sort `primkey'
	save `nb_temp'
	restore
	sort `primkey'
	capture drop _merge
	merge `primkey' using `nb_temp'
	capture drop _merge
end
