rm(list=ls())
setwd("~/Dropbox/Github/Workstation")
options(scipen=999)

##### Packages #####
library(lme4) # for regression
library(lmerTest) # for p-value
library(lattice) # for visualization
library(ggplot2) # for visualization
library(tidyverse) # for data wrangling

##### Load data #####
dta <- readRDS(url("https://ccheng11.github.io/files/WVS_7.RData","rb"))

##### Data wrangling #####
dta_new <- dta %>%
  dplyr::select(Q46P, Q275, regtype) %>%
  rename(edu = Q275) %>%
  mutate(Q46P = ifelse(Q46P <= 0, NA, Q46P),
         edu = ifelse(edu <= 0, NA, edu),
         regtype = ifelse(regtype <= 0, NA, regtype)) %>%
  mutate(regtype = as.character(regtype),
         Q46P = as.character(Q46P)) %>%
  mutate(happy = recode(Q46P,
                        "4" = "1",
                        "3" = "2",
                        "2" = "3",
                        "1" = "4"),
         reg_type = recode(regtype,
                           "5" = "full democracy",
                           "4" = "democracy",
                           "3" = "open anocracy",
                           "2" = "closed anocracy",
                           "1" = "autocracy")) %>%
  mutate(happy = as.numeric(happy)) %>%
  tidyr::drop_na(regtype, happy)

##### Linear model #####
mod_lm <- lm(happy ~ edu, data=dta_new)
summary(mod_lm)

##### Linear multilinear model #####

## Random/varying intercept
mod_lmm_1 <- lmer(happy ~ edu + (1|reg_type), data=dta_new)
summary(mod_lmm_1)
coef(mod_lmm_1)

fixef(mod_lmm_1) # fixed part of each intercept
ranef(mod_lmm_1) # random part of each intercept
dotplot(ranef(mod_lmm_1, condVar = TRUE))

## Random/varying slope
mod_lmm_2 <- lmer(happy ~ edu + (0+edu|reg_type), data=dta_new)
summary(mod_lmm_2)
coef(mod_lmm_2)

fixef(mod_lmm_2) # fixed part of each intercept
ranef(mod_lmm_2) # random part of each intercept
dotplot(ranef(mod_lmm_2, condVar = TRUE))

## Random/varying intercept and slope
mod_lmm_3 <- lmer(happy ~ edu + (1+edu|reg_type), data=dta_new)
summary(mod_lmm_3)
coef(mod_lmm_3)

fixef(mod_lmm_3) # fixed part of each intercept
ranef(mod_lmm_3) # random part of each intercept
dotplot(ranef(mod_lmm_3, condVar = TRUE))

##### Confidence intervals #####
mod_lmm_1 <- lmer(happy ~ edu + (1|reg_type), data=dta_new)
res_lmm_1_fix <- fixef(mod_lmm_1)[1]
res_lmm_1_ran <- ranef(mod_lmm_1, condVar = TRUE)
res_lmm_1_ran_dd <- as_tibble(res_lmm_1_ran) %>%
  mutate(lwr = condval - 1.96*condsd + res_lmm_1_fix,
         upr = condval + 1.96*condsd + res_lmm_1_fix)
res_lmm_1_ran_dd

mod_lmm_2 <- lmer(happy ~ edu + (0+edu|reg_type), data=dta_new)
res_lmm_2_fix <- fixef(mod_lmm_1)[2]
res_lmm_2_ran <- ranef(mod_lmm_1, condVar = TRUE)
res_lmm_2_ran_dd <- as_tibble(res_lmm_2_ran) %>%
  mutate(lwr = condval - 1.96*condsd + res_lmm_2_fix,
         upr = condval + 1.96*condsd + res_lmm_2_fix)
res_lmm_2_ran_dd

mod_lmm_3 <- lmer(happy ~ edu + (1+edu|reg_type), data=dta_new)
res_lmm_3_fix <- fixef(mod_lmm_3)
res_lmm_3_ran <- ranef(mod_lmm_3, condVar = TRUE)

res_lmm_3_ran_dd <- as_tibble(res_lmm_3_ran) %>%
  filter(term == "(Intercept)") %>%
  mutate(lwr = condval - 1.96*condsd + res_lmm_3_fix[1],
         upr = condval + 1.96*condsd + res_lmm_3_fix[1])
res_lmm_3_ran_dd

res_lmm_3_ran_dd <- as_tibble(res_lmm_3_ran) %>%
  filter(term == "edu") %>%
  mutate(lwr = condval - 1.96*condsd + res_lmm_3_fix[2],
         upr = condval + 1.96*condsd + res_lmm_3_fix[2])
res_lmm_3_ran_dd

##### Model comparison #####
anova(mod_lmm_1, mod_lmm_2, mod_lmm_3)

##### Residuals #####

## Get the errors
error_lm <- mod_lm$residuals
error_lmm_1 <- (summary(mod_lmm_1))$residuals
error_lmm_2 <- (summary(mod_lmm_2))$residuals
error_lmm_3 <- (summary(mod_lmm_3))$residuals

## Check normality
summary(error_lm)
summary(error_lmm_1) 
summary(error_lmm_2)
summary(error_lmm_3) 

par(mfrow=c(2,2))
hist(error_lm, main="Fixed a and b")
hist(error_lmm_1, main="Vary a; fixed b")
hist(error_lmm_2, main="Fixed a; vary b")
hist(error_lmm_3, main="Vary a and b")

library(car)
par(mfrow=c(2,2))
qqPlot(error_lm, main="Fixed a and b")
qqPlot(error_lmm_1, main="Vary a; fixed b")
qqPlot(error_lmm_2, main="Fixed a; vary b")
qqPlot(error_lmm_3, main="Vary a and b")

## Check predicted Y and residuals
par(mfrow=c(2,2))
plot(resid(mod_lm) ~ predict(mod_lm), main="Fixed a and b")
plot(resid(mod_lmm_1) ~ predict(mod_lmm_1), main="Vary a; fixed b")
plot(resid(mod_lmm_2) ~ predict(mod_lmm_2), main="Fixed a; vary b")
plot(resid(mod_lmm_3) ~ predict(mod_lmm_3), main="Vary a and b")

summary(lm(resid(mod_lm) ~ predict(mod_lm)))
summary(lm(resid(mod_lmm_1) ~ predict(mod_lmm_1)))
summary(lm(resid(mod_lmm_2) ~ predict(mod_lmm_2)))
summary(lm(resid(mod_lmm_3) ~ predict(mod_lmm_3)))
