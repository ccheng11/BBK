rm(list=ls())
setwd("C:/Users/polar/Downloads/Sandbox/Teaching/AdvQ/Week 04/02 Exercise/Data")
options(scipen=999) # turn off scientific notations

library(tidyverse)
library(stargazer)
library(lattice)
library(lme4)
library(lmerTest)

#install.packages("lmerTest")

##### Import and merge data #####
dta_country <-  readRDS("Country Covariates.RData")
dta_country$country <- rownames(dta_country)
dta_country <- dta_country %>%
  dplyr::select(struggle, # percent seeing a reformer-Islamist struggle
                pctmus, # percent Muslim in country
                log.gdppc, # (natural) log of per capita GDP
                country)
ls(dta_country)

dta <- readRDS("Pew GAP 2007.RData") 
dta_gap <- dta %>%
  dplyr::select(country,
                us.scale, # Anti-Americanism scale: 1=most AA, 0=least AA
                pious, # Individual: 1=highly religious, 0=less religious
                news, # Follow intl news: 1=frequently, 0=only when important
                satisfied, # satisfied with how things going in country
                age,
                male,
                ses,
                ed2,
                ed3) %>% 
  mutate(age = age/100,
         serial = 1:nrow(dta)) #%>%
  #filter(male == 1,
  #       age >= 0.5)
ls(dta_gap)
summary(dta_gap)
head(dta_gap)

### Merge data
dta_all <- merge(dta_gap, dta_country, by="country") %>%
  unique() %>%
  drop_na()
summary(dta_all)
head(dta_all)

##### Task 1: LMM with random intercept #####
mod_lmm_1 <- lmer(us.scale ~ 1 + (1|country), data=dta_all)
summary(mod_lmm_1)
coef(mod_lmm_1)

fixef(mod_lmm_1) # fixed part of each intercept
ranef(mod_lmm_1) # random part of each intercept

dotplot(ranef(mod_lmm_1, condVar = TRUE)) # show the random part of each intercept with CIs

##### Task 2: LMM with varying intercept and constant slope #####
mod_lmm_2 <- lmer(us.scale ~ ed3 + (1|country), data=dta_all)
summary(mod_lmm_2)
coef(mod_lmm_2)

fixef(mod_lmm_2) # fixed part of each intercept
ranef(mod_lmm_2) # random part of each intercept

dotplot(ranef(mod_lmm_2, condVar = TRUE)) # show the random part of each intercept with CIs

##### Task 3: LMM with constant intercept and varying slope #####
mod_lmm_3 <- lmer(us.scale ~ ed3 + (0+ed3|country), data=dta_all)
summary(mod_lmm_3)
coef(mod_lmm_3)

fixef(mod_lmm_3) # fixed part of each slope
ranef(mod_lmm_3) # random part of each slope

dotplot(ranef(mod_lmm_3, condVar = TRUE)) # show the random part of each slope with CIs

##### Task 4: LMM with varying intercept and slope #####
mod_lmm_4 <- lmer(us.scale ~ ed3 + (ed3|country), data=dta_all)
summary(mod_lmm_4)
coef(mod_lmm_4)

fixef(mod_lmm_4) # fixed part of each intercept/slope
ranef(mod_lmm_4) # random part of each intercept/slope

dotplot(ranef(mod_lmm_4, condVar = TRUE)) # show the random part of each slope with CIs

##### Task 5: Evaluation #####

### Anova
anova(mod_lmm_2, mod_lmm_3, mod_lmm_4, test="Chisq")

### SSR
mod_ols_base <- lm(us.scale ~ ed3 + as.factor(country), data=dta_all)
summary(mod_ols_base)

sum(residuals(mod_ols_base)^2)
sum(residuals(mod_lmm_2)^2)
sum(residuals(mod_lmm_3)^2)
sum(residuals(mod_lmm_4)^2)

##### Single-level v multi-level #####

### Single-level group FEs and multi-level with varying intercepts
ols_country_fe <- lm(us.scale ~ as.factor(country)-1, data=dta_all)
summary(ols_country_fe)

lmm_country_fe <- lmer(us.scale ~ 1 + (1|country), data=dta_all)
coef(lmm_country_fe)

### Single-level interactions and multi-level with varying slopes
ols_country_slo <- lm(us.scale ~ ed3 + as.factor(country) + ed3:as.factor(country), data=dta_all)
summary(ols_country_slo)

lmm_country_slo <- lmer(us.scale ~ ed3 + (0+ed3|country), data=dta_all)
coef(lmm_country_slo)

##### Extra: Confidence intervals #####
mod_lmm_2 <- lmer(us.scale ~ ed3 + (0+ed3|country), data=dta_all)

### Fixed effects
lmm_2_fix <- fixef(mod_lmm_2)
lmm_2_fix

### Random effects
lmm_2_ran <- ranef(mod_lmm_2, condVar = TRUE)
lmm_2_ran_dd <- as.tibble(lmm_2_ran) # turn "lmm_2_ran" into tibble
lmm_2_ran_dd

### Calculating the 95% CIs
lmm_2_ran_dd <- lmm_2_ran_dd |>
  dplyr::select(grp, condval, condsd) |>
  rename(country = grp, ranef = condval) |>
  mutate(fixef = lmm_2_fix[1]) |>
  mutate(est = fixef + ranef, # coef
         ci95_upper = fixef + ranef + 1.96*condsd,
         ci95_lower = fixef + ranef - 1.96*condsd)
lmm_2_ran_dd

##### Extra: Multiple LMM #####
mlmm_1 <- lmer(us.scale ~ pious + news + age + male + ed2 + ed3 + ses + satisfied + (1|country), data=dta_all)
summary(mlmm_1)

mlmm_2 <- lmer(us.scale ~ pious + news + age + male + ed2 + ed3 + ses + satisfied + struggle + pctmus + log.gdppc + (1|country), data=dta_all)
summary(mlmm_2)

mlmm_3 <- lmer(us.scale ~ pious + news + age + male + ed2 + ed3 + ses + satisfied + struggle + pctmus + log.gdppc + (pious|country), data=dta_all)
summary(mlmm_3)

anova(mlmm_1, mlmm_2, mlmm_3, test="Chisq")

##### Extra: Multiple OLS #####
#mod_ols_1 <- lm(us.scale ~ pious + news + age + male + ed2 + ed3 + ses + satisfied, data=dta_all)
#summary(mod_ols_2)

#mod_ols_2 <- lm(us.scale ~ pious + news + age + male + ed2 + ed3 + ses + satisfied + as.factor(country), data=dta_all)
#summary(mod_ols_3)

#stargazer(list(mod_ols_1, mod_ols_2),
#          omit.stat = c("f", "rsq", "ser"),
#          covariate.labels = c("Piety",
#                               "Follow intl news",
#                               "Age",
#                               "Male",
#                               "Secondary education",
#                               "University education",
#                               "Income level",
#                               "Policy satisfaction"),
#          omit = c("as.factor"),
#          type = "text",
#          digits = 3, 
#          no.space = T,
#          intercept.bottom = TRUE,
#          star.cutoffs = c(0.05, 0.01, 0.001))

##### Extra: Bayesian multilevel modeling #####
#library(brms)

#mod_blmm <- brm(us.scale ~ ed3 + (ed3|country),
#                data = dta_all,
#                family = gaussian,
#                iter = 2000,
#                chains = 4)
