rm(list=ls())
setwd("C:/Users/polar/Downloads/BBK/06 Intro CP/Slides/Week 2/Data")

load("CNTS.RData")
names(dta)

vars = c("cowcode", "year", "gwf_country", "gdppc")
gdppc = dta[,vars]
summary(gdppc)
rm(dta, vars)

hist(gdppc$gdppc[gdppc$year == 2010], xlab="GDP per capita", main="Average Income by Country, 2010")

load("Polity.RData")
vars = c("ccode", "year", "country", "polity", "polity2")
polity = dta[,vars]
colnames(polity)[1] = "cowcode"
summary(polity)
rm(dta, vars)

gdppc = gdppc[gdppc$year >= 1945,]
polity = polity[polity$year >= 1945,]

dta = merge(gdppc, polity, by=c("cowcode", "year"), all=T)
table(dta$year)

##### Most recent year #####
s = dta[dta$year == 2010,]
summary(s)

s$gdppc = s$gdppc/1000
summary(s$gdppc)

### UK
sel = c("UK")
p = s[grep(paste(sel, collapse="|"), s$gwf_country),]; p
plot(p$polity2, p$gdppc, pch=16, xlim=c(-10,10), ylim=c(0,30), xlab="Polity Score", ylab="GDP per capita (in 1,000 USD)")
text("UK", x=9.5, y=22.5)

### UK, France, Germany, and Norway
sel = c("UK", "France", "Germany", "Norway")
p = s[grep(paste(sel, collapse="|"), s$gwf_country),]; p
plot(p$polity2, p$gdppc, pch=16, xlim=c(-10,10), ylim=c(0,30), xlab="Polity Score", ylab="GDP per capita (in 1,000 USD)")

### UK, France, Germany, and Norway, Singapore
sel = c("UK", "France", "Germany", "Norway", "Singapore")
p = s[grep(paste(sel, collapse="|"), s$gwf_country),]; p
plot(p$polity2, p$gdppc, pch=16, xlim=c(-10,10), ylim=c(0,30), xlab="Polity Score", ylab="GDP per capita (in 1,000 USD)")
text("Singapore", x=-4, y=28.5)

### UK, France, Germany, and Norway, Singapore, India
sel = c("UK", "France", "Germany", "Norway", "Singapore", "India")
p = s[grep(paste(sel, collapse="|"), s$gwf_country),]; p
plot(p$polity2, p$gdppc, pch=16, xlim=c(-10,10), ylim=c(0,30), xlab="Polity Score", ylab="GDP per capita (in 1,000 USD)")
text("India", x=7.5, y=3)

### Oil countries
s$country = as.character(s$country)
sel = c("United Kingdom", "France", "Germany", "Norway", "Kuwait", "Oman", "Saudi Arabia", "Qatar")
p = s[grep(paste(sel, collapse="|"), s$country),]; p
plot(p$polity2, p$gdppc, pch=16, xlim=c(-10,10), ylim=c(0,30), xlab="Polity Score", ylab="GDP per capita (in 1,000 USD)")
text("Saudi Arabia", x=-7.5, y=11)
text("Kuwait", x=-5, y=9)
text("Oman", x=-7.5, y=6.5)

### All countries
plot(s$polity2, s$gdppc, pch=16, xlim=c(-10,10), ylim=c(0,30), xlab="Polity Score", ylab="GDP per capita (in 1,000 USD)")
abline(lm(s$gdppc ~ s$polity2), col="red", lwd=2)

### All countries
p = dta[dta$year == 1980,]
p$gdppc = p$gdppc/1000
plot(p$polity2, p$gdppc, pch=16, xlim=c(-10,10), ylim=c(0,30), xlab="Polity Score", ylab="GDP per capita (in 1,000 USD)")
abline(lm(p$gdppc ~ p$polity2), col="red", lwd=2)

### Two Koreas
sel = c("Korea North", "Korea South")
p = dta[grep(paste(sel, collapse="|"), dta$gwf_country),]
p$gdppc = p$gdppc/1000
plot(p$year[p$cowcode == 732], p$gdppc[p$cowcode == 732], type="l", col="blue", lwd=2, xlab="Year", ylab="GDP per capita (in 1,000)")
lines(p$year[p$cowcode == 731], p$gdppc[p$cowcode == 731], col="red", lwd=2)
legend("topleft", inset=.02, c("S Korea", "N Korea"), fill=c("blue", "red"), horiz=F, cex=0.8)

### China
sel = c("China")
p = dta[grep(paste(sel, collapse="|"), dta$gwf_country),]
p$gdppc = p$gdppc/1000
plot(p$year, p$gdppc, type="l", xlab="Year", ylab="GDP per capita (in 1,000)")
abline(v=1978, col="red", lwd=2)
