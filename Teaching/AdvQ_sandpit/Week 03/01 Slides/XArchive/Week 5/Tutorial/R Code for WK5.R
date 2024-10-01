rm(list=ls())
setwd("~/Dropbox/Birkbeck/Teaching/IQSR/Weeks/Week 4/Tutorial")
options(scipen=999)

##### Load packages #####
library(survey)
library(srvyr)
library(dplyr)
library(ggplot2)
library(purrr)
library(foreign)

##### Load data #####
ces <- read.csv("ces11.csv", stringsAsFactors=TRUE) # load data from the folder
#ces <- carData::CES11 # load data directly from the package "carData"

##### Recap: Use `srvyr` to carry out summary statistics #####
ces_s <- ces %>%
  as_survey(ids = id,
            strata = province,
            fpc = population,
            weights = weight) # use "weights" to include weights in the analysis
ces_s # some basic info (that is nice)

ces_s %>%
  group_by(abortion) %>%
  summarise(n = survey_total(),
            mean = survey_mean())

##### Recode the outcome variable #####
ces_new <- ces %>% 
  mutate(against_abortion = if_else(abortion == "Yes", 1, 0))
table(ces_new$abortion) # original variable
table(ces_new$against_abortion) # new variable
table(ces_new$abortion, ces_new$against_abortion)
table(ces_new$abortion)

##### Logit regression with no predictor #####
mod_intercept <- glm(against_abortion ~ 1,
                     data = ces_new,
                     family = binomial)
summary(mod_intercept)

coefs <- coef(mod_intercept) # return coefficients (log-odds)
coefs

exp(coefs) # remove log from log-odds
exp(coefs)/(1+exp(coefs)) # calculated the probability

confint(mod_intercept) # CIs of coefficients

##### Logit regression with one predictor #####
ces_new <- ces_new %>%
  mutate(religion = recode(importance,
                           "very" = 4,
                           "somewhat" = 3,
                           "notvery" = 2,
                           "not" = 1))
table(ces_new$religion)
table(ces_new$importance)

mod_religion <- lm(against_abortion ~ religion, data = ces_new)
summary(mod_religion)

mod_religion <- glm(against_abortion ~ religion, data = ces_new, family = binomial)
summary(mod_religion)

coefs <- coef(mod_religion) # get coefficients (log-odds)
coefs

exp(coefs) # get odds ratio

### # create the prediction data frame
fit_religion <- data.frame(religion = ces_new$religion, # use "religion" in "ces_new"
                           importance = ces_new$importance)
fit_religion <- fit_religion %>%
  mutate(prob = predict(mod_religion, type="response"),
         log_odds = predict(mod_religion)) %>%
  mutate(odds = exp(log_odds)) %>%
  distinct() %>% # remove duplicates
  arrange(religion)
fit_religion

fit_religion$fit_log_odds[fit_religion$religion == 2] - fit_religion$fit_log_odds[fit_religion$religion == 1]

##### Logit regression with weights #####
ces_s <- ces_new %>%
  as_survey(ids = id,
            strata = province,
            fpc = population,
            weights = weight) # use "weights" to include weights in the analysis
ces_s # some basic info (that is nice)

mod_s_religion <- svyglm(against_abortion ~ religion,
                         design = ces_s, # be sure to use the survey object
                         family = binomial)
summary(mod_s_religion) # with weights
summary(mod_religion) # without weights

mod_s_intercept <- svyglm(against_abortion ~ 1,
                          design = ces_s,
                          family = binomial)
summary(mod_s_intercept) # with weights
summary(mod_intercept) 

anova(mod_s_intercept, mod_s_religion, test="Chi") # model comparison
