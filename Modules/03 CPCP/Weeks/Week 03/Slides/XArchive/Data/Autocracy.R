rm(list=ls())
setwd("~/Dropbox/2018 Summer/PS 50/Lectures/L11 Authoritarianism/Data")

#library(foreign)
#dta = read.dta("GWF_AllPoliticalRegimes.dta")
#save(dta, file="GWF.RData")

load("GWF.RData")
dta = dta[dta$year >= 1945,]
names(dta)

table(dta$year)
table(dta$gwf_nonautocracy)

##### History #####
sel = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2015)
gwfh = dta[grep(paste(sel, collapse="|"), dta$year),]

gwfh$type = NA
gwfh$type[gwfh$gwf_nonautocracy == "NA"] = "Autocracy"
gwfh$type[gwfh$gwf_nonautocracy == "democracy"] = "Democracy"
table(gwfh$type)

counts = table(gwfh$type, gwfh$year); counts
barplot(counts, main="Autocracy vs. Democracy in the World\n(Geddes et al)", ylim=c(0,200), col=c("red","gold"))
legend("topleft", inset=.02, rownames(counts), fill=c("red","gold"), horiz=F, cex=0.8)
