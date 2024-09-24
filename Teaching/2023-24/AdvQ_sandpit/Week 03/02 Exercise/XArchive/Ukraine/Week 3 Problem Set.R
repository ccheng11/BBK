rm(list=ls())
setwd("C:/Users/polar/Downloads/Sandbox/Teaching/AdvQ/Week 03/02 Exercise/Data")

library(haven)
library(tidyverse)
library(stargazer)

##### Import the data #####
dta <- read_dta("Pew data JPR.dta")
summary(dta) # get summary statistics
head(dta) # see the first couple of rows
ls(dta) # list variables

##### Subset data #####
dta_sel <- dta |>
  dplyr::select(secede, above3Khryv, age, female, ukrspeakhome, Russiathreat, oblastname, weight) |>
  mutate(id = 1:nrow(dta))
summary(dta_sel)

##### Task 1: Logit with no predictor #####
mod_intercept <- glm(secede ~ 1,
                     data = dta_sel,
                     family = binomial)
summary(mod_intercept)

coefs <- coef(mod_intercept) # extracting log odds
exp(coefs) # calculating odds
exp(coefs)/(1+exp(coefs)) # calculating p
mean(dta_sel$secede, na.rm=T) # obtaining p using mean

confint(mod_intercept) # CI of log odds
exp(confint(mod_intercept))/(1+exp(confint(mod_intercept)))

##### Task 2: Logit with one predictor #####
mod_speak <- glm(secede ~ ukrspeakhome, data = dta_sel, family = binomial)
summary(mod_speak)
ls(mod_speak)

coef(mod_speak)
exp(coef(mod_speak)) # odds ratio

confint(mod_speak)
exp(confint(mod_speak)) # CIs of odds ratio

fit_prob <- predict(mod_speak, type="response")

##### Extra: Survey weights #####
library(survey)
library(srvyr)

### Create the survey design object
dta_sel_survey <- dta_sel %>%
  as_survey(ids = id,
            strata = oblastname,
            #fpc = population,
            weights = weight)
dta_sel_survey

### Fit logit with svyglm
mod_s_speak <- svyglm(secede ~ ukrspeakhome,
                      design = dta_sel_survey, # the survey design object
                      family = binomial)
summary(mod_s_speak)
exp(coef(mod_s_speak)) # odds ratio

##### Extra: Predicted probabilities #####
data_predict <- data.frame(ukrspeakhome = c(0,1))
data_predict

fit_prob <- predict(mod_speak, newdata=data_predict, type="response")
fit_log_odds <- predict(mod_speak, newdata=data_predict)
fit_mod_s <- data.frame(ukrspeakhome = c(0:1),
                        fit_prob = as.matrix(fit_prob),
                        fit_log_odds = as.matrix(fit_log_odds))
fit_mod_s$fit_odds <- exp(fit_mod_s$fit_log_odds)
fit_mod_s

##### Extra: Logit with more than one predictor #####
dta_sel <- dta_sel %>%
  dplyr::select(secede, ukrspeakhome, Russiathreat, age, female) %>%
  drop_na()

mod1 <- glm(secede ~ ukrspeakhome + age + female, data = dta_sel, family = binomial)
mod2 <- glm(secede ~ Russiathreat + age + female, data = dta_sel, family = binomial)
mod3 <- glm(secede ~ ukrspeakhome + Russiathreat + age + female, data = dta_sel, family = binomial)

stargazer(list(mod1, mod2, mod3),
          omit.stat = c("f", "rsq", "ser"),
          covariate.labels = c("Speak Ukranian at Home (=1)",
                               "Russia is a Threat",
                               "Age",
                               "Female (=1)"),
          type = "text",
          digits = 3, 
          no.space = T,
          intercept.bottom = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001))

## Sum of squared residuals
sum(residuals(mod1)^2)
sum(residuals(mod2)^2)
sum(residuals(mod3)^2)

## Model comparison
anova(mod1, mod2, mod3, test="Chi")

## Simulated probabilities
data_predict <- data.frame(ukrspeakhome = c(0,1),
                           Russiathreat = mean(dta_sel$Russiathreat, na.rm=T),
                           age = mean(dta_sel$age, na.rm=T),
                           female = mean(dta_sel$female, na.rm=T))
data_predict

fit_prob <- predict(mod3, newdata=data_predict, type="response")
fit_log_odds <- predict(mod3, newdata=data_predict)
fit_mod_s <- data.frame(ukrspeakhome = c(0,1),
                        fit_prob = as.matrix(fit_prob),
                        fit_log_odds = as.matrix(fit_log_odds))
fit_mod_s$fit_odds <- exp(fit_mod_s$fit_log_odds)
fit_mod_s
