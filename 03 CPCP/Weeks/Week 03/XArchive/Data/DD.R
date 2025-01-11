rm(list=ls())
setwd("~/Dropbox/2018 Summer/PS 50/Lectures/L11 Authoritarianism/Data")

library(foreign)
dta = read.dta("ddrevisited_data_v1.dta")
save(dta, file="DD.RData")

load("DD.RData")
names(dta)

dta = dta[dta$year >= 1945,]

table(dta$year)

##### History #####
sel = c(1950, 1960, 1970, 1980, 1990, 2000, 2005, 2010)
gwfh = dta[grep(paste(sel, collapse="|"), dta$year),]

gwfh$type = NA
gwfh$type[gwfh$democracy == 1] = "Democracy"
gwfh$type[gwfh$democracy == 0] = "Autocracy"
table(gwfh$type)

counts = table(gwfh$type, gwfh$year); counts
barplot(counts, main="Autocracy vs. Democracy in the World\n(Cheibub et al)", ylim=c(0,200), col=c("red","gold"))
legend("topleft", inset=.02, rownames(counts), fill=c("red","gold"), horiz=F, cex=0.8)
