rm(list=ls())
setwd("C:/Users/polar/Downloads/Sandbox/Teaching/AdvQ/Week 07 08/02 Exercise/Data")

library(ggplot2)
library(stargazer)
library(tidyverse)
library(Matching)
library(ebal)

##### Load dataset #####
dta <- readRDS("EAR_selection.RData")

##### Variables #####
summary(dta$leg_elec) # treatment
summary(dta$fh_CL) # outcome

##### Covariates #####
summary(dta$lg_fh_CL)
summary(dta$lg_epr_gdpcapl)
summary(dta$lg_grow)
summary(dta$ross_population)
summary(dta$epr_ethfrac)
summary(dta$arc_turn)

##### Subset #####
dta_sel <- dta %>%
  dplyr::select(leg_elec, fh_CL, lg_fh_CL, lg_epr_gdpcapl, lg_grow, ross_population,
                epr_ethfrac, arc_turn,
                cowcode, year) %>%
  drop_na()

##### OLS #####
mod_ols_1 <- lm(fh_CL ~ leg_elec + lg_epr_gdpcapl + lg_grow + ross_population + epr_ethfrac + arc_turn, data=dta_sel)
mod_ols_2 <- lm(fh_CL ~ leg_elec + lg_epr_gdpcapl + lg_grow + ross_population + epr_ethfrac + arc_turn + as.factor(cowcode) + as.factor(year), data=dta_sel)
mod_ols_3 <- lm(fh_CL ~ lg_fh_CL + leg_elec + lg_epr_gdpcapl + lg_grow + ross_population + epr_ethfrac + arc_turn, data=dta_sel)

stargazer(list(mod_ols_1, mod_ols_2, mod_ols_3),
          omit.stat = c("f", "rsq", "ser"),
          covariate.labels = c("Lagged civil liberties",
                               "Legislative election (=1)",
                               "GDP per capita",
                               "Economic growth",
                               "Population",
                               "Ethnic diversity",
                               "Leadership turnover"),
          omit = c("as.factor"),
          type = "text",
          digits = 3, 
          no.space = T,
          intercept.bottom = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001))

##### Matching #####

### Step 1: Pre-test
pre_balance <- lm(leg_elec ~ lg_fh_CL + lg_epr_gdpcapl + lg_grow + ross_population + epr_ethfrac + arc_turn, data=dta_sel)
summary(pre_balance)

### Step 2: Balance test (before matching)
vars <- c("Lagged civil liberties", "GDP per capita", "Economic growth", "Population", "EFL", "Leadership turnover")
mb <- MatchBalance(leg_elec ~ lg_fh_CL + lg_epr_gdpcapl + lg_grow + ross_population + epr_ethfrac + arc_turn, data=dta_sel)
btest <- baltest.collect(mb, var.names=vars, after=F)
round(btest[, c("mean.Tr","mean.Co","T pval")], 3)

### Step 3: Matching
matchout <- Match(Y=dta_sel[,2], Tr=dta_sel[,1], X=dta_sel[,3:8],
                  M=5,
                  exact=rep(FALSE, 6),
                  estimand="ATT",
                  BiasAdjust=TRUE)
summary(matchout)

### Step 4: Balance test (after matching)
vars <- c("Lagged civil liberties", "GDP per capita", "Economic growth", "Population", "EFL", "Leadership turnover")
mb.out <- MatchBalance(match.out=matchout,
                       leg_elec ~ lg_fh_CL + lg_epr_gdpcapl + lg_grow + ross_population + epr_ethfrac + arc_turn, data=dta_sel)
btest_after <- baltest.collect(mb.out, var.names=vars, after=T)
round(btest_after[,c("mean.Tr","mean.Co","T pval")], 3)

##### Matching by propensity score #####

### Step 1: Estimate propensity scores
pi.out <- glm(leg_elec ~ lg_fh_CL + lg_epr_gdpcapl + lg_grow + ross_population + epr_ethfrac + arc_turn,
              data=dta_sel, family=binomial(link="probit"))

plot(density(pi.out$fit[dta_sel$leg_elec==1]), lwd=2, main="Distribution of p-scores")
lines(density(pi.out$fit[dta_sel$leg_elec==0]), lwd=2, lty=2)
legend("topleft", legend=c("treated","controls"), lty=c(1,2), lwd=2)

### Step 2: Matching
matchout.pi <- Match(Y=dta_sel$fh_CL, Tr=dta_sel$leg_elec, X=pi.out$fit,
                     M=5, exact=FALSE, estimand="ATT", BiasAdjust=T)
summary(matchout.pi)

### Step 3: Balance test (after)
vars <- c("Lagged civil liberties", "GDP per capita", "Economic growth", "Population", "EFL", "Leadership turnover")
mb.out.pi <- MatchBalance(match.out=matchout.pi,
                          leg_elec ~ lg_fh_CL + lg_epr_gdpcapl + lg_grow + ross_population + epr_ethfrac + arc_turn, data=dta_sel)
btest_after <- baltest.collect(mb.out.pi, var.names=vars, after=T)
round(btest_after[,c("mean.Tr","mean.Co","T pval")], 3)
