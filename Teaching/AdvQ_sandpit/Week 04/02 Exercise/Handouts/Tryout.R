rm(list=ls())
setwd("C:/Users/polar/Downloads/Sandbox/Teaching/AdvQ/Week 04/02 Exercise/Data")
options(scipen=999)

library(haven)
library(tidyverse)
library(stargazer)
library(lattice)
library(ggplot2)
library(lme4)
library(lmerTest)

##### Import and merge data #####
dta_country <- readRDS("Country Covariates.RData")
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
         serial = 1:nrow(dta))
ls(dta_gap)
summary(dta_gap)
head(dta_gap)

dta_all <- merge(dta_gap, dta_country, by="country") %>%
  unique() %>%
  drop_na()
summary(dta_all)
head(dta_all)

##### Multilevel model #####
lmerfit <- lmer(us.scale ~ ed3 + (ed3|country), data=dta_all)
coef(lmerfit)

##### Add predictions #####
newavg <- data.frame(ed3 = 0:1)
newavg$us.scale <- predict(lmerfit, re.form=NA, newavg)
newavg

newvary <- expand.grid(ed3=0:1, country=unique(dta_all$country))
newvary$us.scale <- predict(lmerfit, newvary)
newvary

library(ggplot2)
library(patchwork)

p1 <- ggplot(dta_all, aes(x=ed3, y=us.scale)) +
  geom_point(shape = 1) +
  scale_x_continuous(breaks = c(0, 3, 6, 9)) +
  geom_smooth(method = "lm", fill = "dodgerblue", level = .95)
p2 <- p1 + facet_wrap(~country, nrow = 4)
p1 | p2

p1 + geom_line(data = newavg, col = "black", linewidth = 1) |
  p2 + geom_line(data = newvary, col = "black", linewidth = 1)
