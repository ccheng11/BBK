rm(list=ls())
setwd("C:/Users/polar/Downloads/Sandbox/Teaching/AdvQ/Week 08/02 Exercise/Data")

library(haven)
library(ggplot2)
library(stargazer)
library(tidyverse)
library(rdrobust)

##### Load dataset #####
dta <- read_dta("electoral_data.dta")
ls(dta)

##### Variables #####
summary(dta$vote_margin) # running variable
summary(dta$win)
#summary(dta$vote)

##### Subset for RDD #####
dta_rdd <- dta %>%
  dplyr::select(win, vote_margin, educ_t) %>%
  mutate(treat = ifelse(vote_margin >= 0, 1, 0)) %>%
  drop_na()
summary(dta_rdd$vote_margin)

dta_rdd_sel <- dta_rdd %>%
  filter(vote_margin >= -0.1) %>%
  filter(vote_margin <= 0.1) 
summary(dta_rdd_sel$vote_margin)

##### Visualization #####
rdplot(dta_rdd$win, dta_rdd$vote_margin)
rdplot(dta_rdd_sel$win, dta_rdd_sel$vote_margin)

##### Balance #####
t.test(educ_t ~ treat, data=dta_rdd)
t.test(educ_t ~ treat, data=dta_rdd_sel)

##### RDD (without the package) #####
rdd_simple_1 <- lm(win ~ treat, data=dta_rdd)
rdd_simple_2 <- lm(win ~ treat, data=dta_rdd_sel)

stargazer(list(rdd_simple_1, rdd_simple_2),
          omit.stat = c("f", "rsq", "ser"),
          covariate.labels = c("Treatment"),
          omit = c("as.factor"),
          type = "text",
          digits = 3, 
          no.space = T,
          intercept.bottom = TRUE,
          star.cutoffs = c(0.1, 0.05, 0.01))

##### RDD (with the package) #####
rdd_robust <- rdrobust(dta_rdd$win, dta_rdd$vote_margin, c=0)
summary(rdd_robust)

rdd_robust$bws
