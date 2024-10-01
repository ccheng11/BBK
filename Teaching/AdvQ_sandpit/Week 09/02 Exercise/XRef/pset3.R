rm(list=ls())
setwd("C:/Users/polar/Downloads/Sandbox/Teaching/AdvQ/Week 09/02 Exercise/Data")

library(haven)
library(stargazer)
library(AER)
library(lmtest)

##### Load data #####
dta = read_dta("arj.dta")
ls(dta)

##### Variables #####
summary(dta$avexpr) # expropriation risk
summary(dta$logem4) # logged settler mortality
summary(dta$lat_abst) # latitude
summary(dta$logpgp95) # logged GDP per capita (1995)

##### First-stage #####
mod.f1 <- lm(avexpr ~ logem4, data=dta)
mod.f1r <- coeftest(mod.f1, vcov = vcovHC(mod.f1, type = "HC2"))
mod.f1r

mod.f2 <- lm(avexpr ~ logem4 + lat_abst, data=dta)
mod.f2r <- coeftest(mod.f2, vcov = vcovHC(mod.f2, type = "HC2"))
mod.f2r

##### Reduced form #####
mod.r1 <- lm(logpgp95 ~ logem4, data=dta)
mod.r1r <- coeftest(mod.r1, vcov = vcovHC(mod.r1, type = "HC2"))
mod.r1r

mod.r2 <- lm(logpgp95 ~ logem4 + lat_abst, data=dta)
mod.r2r <- coeftest(mod.r2, vcov = vcovHC(mod.r2, type = "HC2"))
mod.r2r

##### 2SLS #####
mod.i1 <- ivreg(logpgp95 ~ avexpr | logem4, data=dta)
mod.i1r <- coeftest(mod.i1, vcov = vcovHC(mod.i1, type = "HC2"))
mod.i1r

mod.i2 <- ivreg(logpgp95 ~ avexpr + lat_abst | logem4 + lat_abst, data=dta)
mod.i2r <- coeftest(mod.i2, vcov = vcovHC(mod.i2, type = "HC2"))
mod.i2r
