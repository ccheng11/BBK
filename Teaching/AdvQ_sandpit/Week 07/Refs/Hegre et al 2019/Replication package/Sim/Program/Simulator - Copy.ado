program Simulator
	version 9
		
	/* 
		Handle parameters to simulation
	*/
	args paramfile
	
	/* 
		Read Parameters from file 
	*/
	file open pms using "`paramfile'", read
	file read pms line
	
	local idep = ""
	local idepno = 0
	local keyno = 2
	local suppno = 0
	local idepnolag = ""
	local supvar = ""
	local data = ""
	local intcountry = ""
	local intyear = ""
	local idepnolag = ""
	scalar expressions = ""
	
	
	while r(eof)==0{
		display "`line'"
		tokenize `line', p(";")
		
		scalar expressions = expressions + "`5'"
		if("`1'" == "CONSTRAINT" ){
			`3'
		}
		if("`1'" == "UNITID" ){
			tokenize `3'
			//display "`1'"
			local unitid = "`1'"
			capture confirm variable `unitid'
			if(_rc!=0){
				display "Variable `unitid' is not loaded!"
				exit _rc
			}
			local data = "`data'" + "`1'"
		}
		if("`1'" == "TIMEID" ){
			tokenize `3'
			local timeid = "`1'"
			capture confirm variable `timeid'
			if(_rc!=0){
				display "Variable `timeid' is not loaded!"
				exit _rc
			}
			local data = "`data'" + " `1'"
		}
		if("`1'" == "DEP" ){
			tokenize `3'
			local dep = "`1'"
			capture confirm variable `dep'
			if(_rc!=0){
				display "Variable `dep' is not loaded!"
				exit _rc
			}
			local data = "`data'" + " `1'"
		}	
		if("`1'" == "IDEP" ){
			tokenize `3' 
			capture confirm variable `1'
			if(_rc!=0){
				display "Variable `1' is not loaded!"
				exit _rc
			}
			local data = "`data'" + " `1'"
			local idepnolag = "`idepnolag'" + " `1'"
			if("`2'" == "lagged" ){
				local lvarname = "l" + "`1'"
				local lagvars = "`lagvars'" + " `1'"
				/* removed equal and plus signs */
				local idep "`idep'" " `lvarname'"
			}
			else{
				/* removed equal and plus signs */
				local idep "`idep'" " `1'"
			}
			
			local idepno = `idepno' + 1
		}
		if("`1'" == "IFS" ){
			local ifs = "`3'"
		}
		if("`1'" == "ENDPARAMS" ){
			local ends = ",`3'"
		}
		if("`1'" == "SPLIT" ){
			local splitsample = "`3'"
				/* 
		Drawing Random Sample of countries
	*/
	
	local sp = 0.0
	
	if(trim("`splitsample'") == "diakron"){
		gen rnd = uniform()
		capture confirm variable rndst
		if !_rc{
			drop rndst
		}
		sort `unitid'
		by `unitid': egen rndst = mean(rnd)
		summarize rndst, d
		local sp = r(p50)
		local ifs = "`ifs'" + " & rndst > `sp'"
		local data = "`data' " + "rndst"
	}
	
	if (trim("`splitsample'") == "synkron"){
		local sp = 1
		local ifs = "`ifs'" + " & year < `startyear'"
	}
	gen double splitpoint = `sp' 
	local data = "`data' " + "splitpoint"
		}
		if("`1'" == "SUPPVAR" ){
			tokenize `3'
			capture confirm variable `1'
			if(_rc!=0){
				display "Variable `1' is not loaded!"
				exit _rc
			}
			local suppno = `suppno' + 1
			local supvar = "`supvar'" + " `1'"
			local data = "`data'" + " `1'"
		}
		if("`1'" == "ITERATIONS" ){
			tokenize `3'
			local iterations = "`1'"
		}
		if("`1'" == "SIMULATIONS" ){
			tokenize `3'
			local simulations = "`1'"
		}
		if("`1'" == "STARTYEAR" ){
			tokenize `3'
			local startyear = "`1'"
		}
		if("`1'" == "RESULTFILE" ){
			tokenize `3'
			local resultfile = "`1'"
		}
		if("`1'" == "LOGFILE" ){
			tokenize `3'
			local logfile = "`1'"
		}
		if("`1'" == "INTERVENTION" ){
			tokenize `3'
			local intcountry = "`1'"
			local intyear = "`2'"
		}
		if("`1'" == "LINKS" ){
			tokenize `3'
			local links = "`1'"
			capture confirm variable `links'
			if(_rc!=0){
				display "Variable `links' is not loaded!"
				exit _rc
			}
			
		}
		/*
		if("`1'" == "BETADRAWS" ){
			tokenize `3'
			local betadraws = "`1'"
		}
		*/
		
		file read pms line
	}
	file close pms
	
	
	local data = "`data' " + "`links'" 
	
	local cmd = "sort " + "`unitid' " + "`timeid'" 
	`cmd'
	
	/*
		Lagging variables
	*/
	foreach v of local lagvars { 
		local lvarname = "l" + "`v'" 
		by `unitid': gen `lvarname' = `v'[_n-1]
	}
	
	/*
		Run Regression
		To increase size of string I have removed the equal and plus signs.
	*/
	local cmd "mlogit" " `dep'" " `idep'" " `ifs'" " `ends'"
	// local cmd "mlogit" " `dep'" " `idep'" " `ifs'" " `ends'"
	
	// Write command to file (current dir)
	file open cmdd using cmd.do, write replace
	file write cmdd "`cmd'" _n
	file close cmdd

/* HH additions */
	display "Testing command length"
	display "`cmd'"
	display length("`cmd'")
/* end HH additions */	

	// Do command
	do cmd.do
	
	// `cmd'
	

/* HH additions */
	display "Data:"
/* end HH additions */	

	
	 display "`data'"

/* HH additions */
	display "Expressions:"
/* end HH additions */	

	 display expressions
	/*
		Copy beta coeff and variance matrices to variables 
	*/
	matrix b = e(b)
	matrix vcv = e(V)
	local betanames: colfullnames b
	local idepno = `idepno' // Include constant in count!
	
	preserve
	drop *
	
	local cols = colsof("b")	
	local cmd = "drawnorm"
		
	forvalues i = 1(1) `cols'{
			local cmd = "`cmd'" + " b" + "`i'"
	}

/* HH additions */
	display "After extraction of betas"
	display "Current content of cmd string:"
	display "`cmd'"
/* end HH additions */	
	
	// Betadraws = the number of total sets of betas needed
	set matsize 2000
	local betadraws = 1000
	// `iterations'*`simulations'
	local cmd = "`cmd'" + " ,n(`betadraws') cov(vcv) means(b)"
	
	`cmd'

/* HH additions */
	display "Current content of cmd string:"
	display "`cmd'"
/* end HH additions */	
	
	 mat list vcv
	mkmat *, matrix(bd)
	 mat list bd
	
	local rows = rowsof("bd")
	local cols = colsof("bd")
	
	forvalues i = 1(1)`rows'{
		forvalues j = 1(1)`cols'{
			if(bd[`rows', `cols'] > 1000 | bd[`rows', `cols'] < -1000){
				display as error "Betas out of range >1000 or <1000"
				return(1)
			}
		}
	}

/* HH additions */
	display "After definition of bd matrix":
	matrix list bd
/* end HH additions */	
	
	restore
	
	// order `data'
	
	keep `data'
	
	sort `unitid' `timeid'
	
	// Run simulation
	capture program sim, plugin using("Simulator.dll")
	
	plugin call sim `data', "`paramfile'" "`resultfile'" "`logfile'"
	
end