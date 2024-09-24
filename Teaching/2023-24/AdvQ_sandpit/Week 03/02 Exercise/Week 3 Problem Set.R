rm(list=ls())
setwd("/Users/chaoyocheng/Dropbox/Github/Sandbox/Teaching/AdvQ_sandpit/Week 03/02 Exercise/Data")

library(haven)
library(readxl)
library(tidyverse)
library(stargazer)

##### Import the data #####
dta <- read_spss("ATP W99.sav")

summary(dta) # get summary statistics
head(dta) # see the first couple of rows

names(dta) # list variables
ls(dta) 

##### Codebook #####

### generate codebook from SPSS file
#var_labels <- sapply(dta, attr, "label")
#dta_codebook <- data.frame(variable = colnames(dta),
#                           label = var_labels)
#rownames(dta_codebook) <- NULL

#library(openxlsx)
#write.xlsx(dta_codebook, "Codebook.xlsx")

codebook <- read_xlsx("Codebook.xlsx")

##### Subset data #####
dta_sel <- dta %>%
  dplyr::select(QKEY,
                FACEREC2_W99,
                F_METRO, F_AGECAT, F_GENDER, F_EDUCCAT, F_HISP, F_YEARSINUS,
                F_RACECMB, F_RACETHNMOD, F_RELIG, F_ATTEND, F_PARTY_FINAL, F_INC_SDT1,
                F_REG, F_IDEO,
                WEIGHT_W99, F_CREGION, F_CDIVISION) %>%
  unique()
#rm(dta)

##### Read variables #####

## Dependent variable
dta_sel$FACEREC2_W99
table(dta_sel$FACEREC2_W99)
attr(dta_sel$FACEREC2_W99, "label")
attr(dta_sel$FACEREC2_W99, "labels")

## Explanatory variables
dta_sel$F_EDUCCAT
table(dta_sel$F_EDUCCAT)
attr(dta_sel$F_EDUCCAT, "label")
attr(dta_sel$F_EDUCCAT, "labels")

dta_sel$F_GENDER
table(dta_sel$F_GENDER)
attr(dta_sel$F_GENDER, "label")
attr(dta_sel$F_GENDER, "labels")

dta_sel$F_PARTY_FINAL
table(dta_sel$F_PARTY_FINAL)
attr(dta_sel$F_PARTY_FINAL, "label")
attr(dta_sel$F_PARTY_FINAL, "labels")

##### Recoding variables #####
dta_sel$yes_fr <- ifelse(dta_sel$FACEREC2_W99 == 1, 1, 0)
dta_sel$yes_fr[is.na(dta_sel$yes_fr) == T] = 0
summary(dta_sel$yes_fr)

dta_sel$highedu <- ifelse(dta_sel$F_EDUCCAT == 1, 1, 0)
summary(dta_sel$highedu)

dta_sel$female <- ifelse(dta_sel$F_GENDER == 2, 1, 0)
summary(dta_sel$female)

dta_sel$democrat <- ifelse(dta_sel$F_PARTY_FINAL == 2, 1, 0)
summary(dta_sel$democrat)

##### Task 1: Logit with no predictor #####
mod_intercept <- glm(yes_fr ~ 1,
                     data = dta_sel,
                     family = binomial)
summary(mod_intercept)

coefs <- coef(mod_intercept) # extracting log odds
exp(coefs) # calculating odds
exp(coefs)/(1+exp(coefs)) # calculating p
mean(dta_sel$yes_fr, na.rm=T) # obtaining p using mean

confint(mod_intercept) # CI of log odds
exp(confint(mod_intercept))/(1+exp(confint(mod_intercept)))

##### Task 2: Logit with one predictor #####
mod_edu <- glm(yes_fr ~ highedu,
                data = dta_sel,
                family = binomial)
summary(mod_edu)
ls(mod_edu)

coef(mod_edu)
exp(coef(mod_edu)) # odds ratio

confint(mod_edu)
exp(confint(mod_edu)) # CIs of odds ratio

##### Extra: Survey weights #####
library(survey)
library(srvyr)

### Create the survey design object
dta_sel_survey <- dta_sel %>%
  as_survey(ids = QKEY,
            #strata =,
            #fpc =,
            weights = WEIGHT_W99)
dta_sel_survey

### Fit logit with svyglm
mod_s_edu <- svyglm(yes_fr ~ highedu,
                    design = dta_sel_survey, # the survey design object
                    family = binomial)
summary(mod_s_edu)
exp(coef(mod_s_edu)) # odds ratio

##### Extra: Predicted probabilities #####
data_predict <- data.frame(highedu = c(0,1))
data_predict

fit_prob <- predict(mod_edu, newdata=data_predict, type="response")
fit_log_odds <- predict(mod_edu, newdata=data_predict)

fit_mod_s <- data.frame(highedu = c(0:1),
                        fit_prob = as.matrix(fit_prob),
                        fit_log_odds = as.matrix(fit_log_odds))
fit_mod_s$fit_odds <- exp(fit_mod_s$fit_log_odds)
fit_mod_s

##### Extra: Logit with more than one predictor #####
dta_sel <- dta_sel %>%
  dplyr::select(yes_fr, highedu, female, democrat) %>%
  drop_na()

mod1 <- glm(yes_fr ~ highedu, data = dta_sel, family = binomial)
mod2 <- glm(yes_fr ~ highedu + democrat, data = dta_sel, family = binomial)
mod3 <- glm(yes_fr ~ highedu + democrat + female, data = dta_sel, family = binomial)

stargazer(list(mod1, mod2, mod3),
          omit.stat = c("f", "rsq", "ser"),
          covariate.labels = c("University graduate (=1)",
                               "Democrat (=1)",
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
data_predict <- data.frame(highedu = c(0,1),
                           democrat = mean(dta_sel$democrat, na.rm=T),
                           female = mean(dta_sel$female, na.rm=T))
data_predict

fit_prob <- predict(mod3, newdata=data_predict, type="response")
fit_log_odds <- predict(mod3, newdata=data_predict)

fit_mod_s <- data.frame(highedu = c(0,1),
                        fit_prob = as.matrix(fit_prob),
                        fit_log_odds = as.matrix(fit_log_odds))
fit_mod_s$fit_odds <- exp(fit_mod_s$fit_log_odds)
fit_mod_s
