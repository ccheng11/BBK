rm(list=ls())
setwd("~/Dropbox/2018 Summer/PS 50/Lectures/L11 Authoritarianism/Data")

#library(readstata13)
#dta = read.dta13("PEI country-level data (PEI_6.0) 01-03-2018.dta")
#save(dta, file="PEI.RData")

load("PEI.RData")

dta$PR_type = "Partly Free"
dta$PR_type[dta$PR <= 2.5] = "Free"
dta$PR_type[dta$PR >= 5.5] = "Not Free"
dta$PR_type[is.na(dta$PR) == T] = NA
table(dta$PR_type)

dta$CL_type = "Partly Free"
dta$CL_type[dta$CL <= 2.5] = "Free"
dta$CL_type[dta$CL >= 5.5] = "Not Free"
dta$CL_type[is.na(dta$CL) == T] = NA
table(dta$CL_type)

dta$PR_type = as.factor(dta$PR_type)
dta$PR_type = factor(dta$PR_type, levels(dta$PR_type)[c(1,3,2)])
levels(dta$PR_type)

dta$CL_type = as.factor(dta$CL_type)
dta$CL_type = factor(dta$CL_type, levels(dta$CL_type)[c(1,3,2)])
levels(dta$CL_type)

par(mfrow=c(1,2))
boxplot(PEIIndexp ~ PR_type, data=dta, ylim=c(0,100), xlab="Press Freedom", ylab="Electoral Integrity Index")
boxplot(PEIIndexp ~ CL_type, data=dta, ylim=c(0,100), xlab="Civil Liberties", ylab="Electoral Integrity Index")
