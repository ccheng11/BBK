program testformissing
	/*Define the syntax*/ 
	syntax, model(int) depvar(string) efy(int) ely(int) sly(int) maxparas(int)
	/* 
	model: model number in model matrix
	depvar: dependent variable
	efy: first year for estimation
	ely: last year for estimation
	sly: last year for simulation
	*/
	display "********************************************"
	display "***** Testing for missing observations *****"
	display "********************************************"

/*	insheet using parameters/`parafilename', clear
	set obs `parafilelength'
	forvalues l=1(1)`maxparas' { /* Alter lines in parameter file based on the model matrix M */
		replace v1 = "//" + v1 if _n == `zeroline' + `l' & M[`l',`model']==0
	}
*/	
	display "Model " `model'
	summ `depvar'
	stop

	assert(`missing'==0) /* Terminates execution */
	/* Also test for high collinearity */
	
end


		   
