
/* This dofile collects all preparatory statements in master do files for the prediction project */
/* Last updated April 11, 2014 */

set more off

adopath + c:/ado
adopath + m:/ado

capture log close

include "programs/mergescenarios_v2b.ado"
include "programs/mergemodels_v2b.ado"
include "programs/def_oosevaluation.do"
include "programs/vallist.ado"
include "programs/drawrandom.ado"
include "programs/drawrandom_v2.ado"
include "programs/drawrandom_v2b.ado"
include "programs/testformissing.ado"
include "programs/simulateit_v3e.ado"
include "programs/stopsim.ado"
include "programs/nblag.ado"
include "programs/nblag_v2.ado"
include "programs/def_aggregateresults.do"
include "programs/aggregateresults_v3g.ado"
include "programs/oosevaluation_v2.ado"
include "programs/oosevaluation_v2b.ado"
include "programs/oosevaluation_v3.ado"
include "programs/oosevaluation_v3b.ado"
include "programs/oosevaluation_v3c.ado"
include "programs/oosevaluation_v3d.ado"
include "programs/oosevaluation_v3e.ado"
include "programs/oosevaluation_v3f.ado"
include "programs/endogenousgrowth_v1.ado"
include "programs/endogenousgrowth_v2.ado"
include "programs/Simulator_v2.ado"
include "programs/locatescenarios_v1.ado"
include "programs/locatescenarios_v2.ado"
include "programs/locatescenarios_v2b.ado"
include "programs/locatescenarios_v2c.ado"
include "programs/simulateit_v3fTEST.ado"

/* Secondary parameters */
local modelsno = wordcount("`models'")

capture include "Sim/Program/Simulator_2.ado"

capture mkdir "Results/`simsetname'"
log using "results/`simsetname'/`logfile'.log", replace

local work_directory = c(pwd) /* Work directory is current directory */
local results_directory = "`localpath'" + "`simsetname'"

display "`results_directory'"
capture mkdir "`results_directory'"

local mnos = wordcount("`models'") /* Number of models _and_ (maximum number of) columns in matrix M */

local regiondatatemp = "`localpath'" + "`simsetname'" + "/Regions.dta"
copy "Regions.dta" "`regiondatatemp'", replace
