rm(list=ls())
setwd("~/Dropbox/2018 Summer/PS 50/Lectures/L11 Authoritarianism/Data")

load("Polity.RData")
dta = dta[dta$year >= 1945,]
table(dta$year)

par(mfrow=c(1,2))

##### History #####
dta$type = "Anocracy"
dta$type[dta$polity >= 6] = "Democracy"
dta$type[dta$polity <= -6] = "Autocracy"
table(dta$type)

dta$type = as.factor(dta$type)
dta$type = factor(dta$type, levels(dta$type)[c(2,1,3)])
levels(dta$type)

sel = c(1950, 1960, 1975, 1980, 1990, 2000, 2010, 2015)
p = dta[grep(paste(sel, collapse="|"), dta$year),]

counts = table(p$type, p$year); counts
barplot(counts, main="Autocracy and Democracy in the World", ylim=c(0,200), col=c("red","gold", "darkgreen"))
legend("topleft", inset=.02, rownames(counts), fill=c("red","gold", "darkgreen"), horiz=F, cex=0.8)

##### USA #####
p = dta[dta$scode %in% "USA",]
plot(p$year, p$polity, ylim=c(-10,10), type="l", xlab="Year", ylab="Polity", main="USA")
abline(h=-6, col="red", lty=2)
abline(h=6, col="red", lty=2)

##### China #####
dta$country = as.character(dta$country)
p = dta[dta$country %in% "China",]
plot(p$year, p$polity, ylim=c(-10,10), type="l", xlab="Year", ylab="Polity", main="China")
abline(h=-6, col="red", lty=2)
abline(h=6, col="red", lty=2)
