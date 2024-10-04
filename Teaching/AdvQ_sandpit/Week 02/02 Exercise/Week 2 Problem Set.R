rm(list=ls())
setwd("~/Dropbox/Birkbeck/Modules/04 Quants/05 Weeks/Week 02/02 Exercise/Data")

library(car)
library(haven)
library(tidyverse)
library(stargazer)

##### Import the data #####
dta <- read_csv("05_01_23_0912pm_wep.csv")

##### Subset data #####
dta_sel <- dta |>
  dplyr::select(country, year, gdppc_WDI_PW, lngdppc_WDI_PW, democracy_DD, v2x_polyarchy_VDEM, v2x_corr_VDEM) |>
  mutate(v2x_polyarchy_VDEM_10 = v2x_polyarchy_VDEM*10,
         v2x_corr_VDEM_10 = v2x_corr_VDEM*10) |>
  filter(year == 2000) |>
  unique() |>
  drop_na()

head(dta_sel)

##### Task 1: Scatterplot #####
plot(gdppc_WDI_PW ~ v2x_corr_VDEM, data=dta_sel,
     xlab = "Political Corruption",
     ylab = "GDP per capita")
abline(lm(gdppc_WDI_PW ~ v2x_corr_VDEM, data=dta_sel),
       col = "red",
       lwd = 3)

## Log transformation
par(mfrow=c(1,2))
plot(density(dta_sel$gdppc_WDI_PW), main="GDP per capita")
plot(density(dta_sel$lngdppc_WDI_PW), main="GDP per capita (log)")

plot(lngdppc_WDI_PW ~ v2x_corr_VDEM, data=dta_sel,
     xlab = "Political Corruption",
     ylab = "GDP per capita")
abline(lm(lngdppc_WDI_PW ~ v2x_corr_VDEM, data=dta_sel),
       col = "red",
       lwd = 3)

## Regression by group
plot(lngdppc_WDI_PW ~ v2x_corr_VDEM_10, data=dta_sel,
     col = ifelse(dta_sel$democracy_DD == 1, "red", "blue"),
     pch = 16,
     xlab = "Political Corruption",
     ylab = "GDP per capita")
abline(lm(lngdppc_WDI_PW ~ v2x_corr_VDEM_10, data=dta_sel[dta_sel$democracy_DD == 1,]), col="red", lwd=2)
abline(lm(lngdppc_WDI_PW ~ v2x_corr_VDEM_10, data=dta_sel[dta_sel$democracy_DD == 0,]), col="blue", lwd=2)

##### Task 2: Bivariate linear regression #####
b_corruption = lm(gdppc_WDI_PW ~ v2x_corr_VDEM_10, data=dta_sel)
summary(b_corruption)

coefficients(b_corruption)
confint(b_corruption)
resid(b_corruption)
fitted(b_corruption)

#plot(b_corruption)
#ls(b_corruption)

## Use democracy as X
b_democracy = lm(gdppc_WDI_PW ~ v2x_polyarchy_VDEM_10, data=dta_sel)
summary(b_democracy)

## Present regression table
stargazer(list(b_corruption, b_democracy),
          omit.stat = c("f", "rsq", "ser"),
          covariate.labels = c("Corruption", "Democracy"),
          type = "text",
          digits = 3, 
          no.space = T,
          intercept.bottom = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001))

##### Task 3: Multiple linear regression #####
m_mod = lm(gdppc_WDI_PW ~ v2x_corr_VDEM_10 + v2x_polyarchy_VDEM_10, data=dta_sel)

## Check collinearity
car::vif(m_mod)

cor(dta_sel$v2x_corr_VDEM_10,
    dta_sel$v2x_polyarchy_VDEM_10,
    use="na.or.complete")

cor.test(dta_sel$v2x_corr_VDEM_10,
         dta_sel$v2x_polyarchy_VDEM_10,
         use="na.or.complete")

## Model comparison
anova(b_corruption, b_democracy, m_mod)

## Present regression table
stargazer(list(b_corruption, b_democracy, m_mod),
    omit.stat = c("f", "rsq", "ser"),
    covariate.labels = c("Corruption", "Democracy"),
    type = "text",
    digits = 3, 
    no.space = T,
    intercept.bottom = TRUE,
    star.cutoffs = c(0.05, 0.01, 0.001))

#########################################################################################################
#########################################################################################################
#########################################################################################################

##### Extra: Log transformation #####
m_log_1 = lm(lngdppc_WDI_PW ~ v2x_corr_VDEM_10, data=dta_sel)
m_log_2 = lm(lngdppc_WDI_PW ~ v2x_polyarchy_VDEM_10, data=dta_sel)
m_log_3 = lm(lngdppc_WDI_PW ~ v2x_corr_VDEM_10 + v2x_polyarchy_VDEM_10, data=dta_sel)
stargazer(m_log_1, m_log_2, m_log_3, type="text")

##### Extra: Interaction #####
m_int_1 = lm(gdppc_WDI_PW ~ v2x_corr_VDEM_10 + democracy_DD, data=dta_sel)
m_int_2 = lm(gdppc_WDI_PW ~ v2x_corr_VDEM_10 + democracy_DD + v2x_corr_VDEM_10:democracy_DD, data=dta_sel)
stargazer(m_int_1, m_int_2, type="text", omit=c("as.factor"))

plot(lngdppc_WDI_PW ~ v2x_corr_VDEM_10, data=dta_sel,
     col = ifelse(dta_sel$democracy_DD == 1, "red", "blue"),
     pch = 16,
     xlab = "Political Corruption",
     ylab = "GDP per capita")
abline(lm(lngdppc_WDI_PW ~ v2x_corr_VDEM_10, data=dta_sel[dta_sel$democracy_DD == 1,]), col="red", lwd=2)
abline(lm(lngdppc_WDI_PW ~ v2x_corr_VDEM_10, data=dta_sel[dta_sel$democracy_DD == 0,]), col="blue", lwd=2)

##### Extra: Panel #####
dta_sel_2 = dta |>
  dplyr::select(country, year, gdppc_WDI_PW, lngdppc_WDI_PW, democracy_DD, v2x_polyarchy_VDEM, v2x_corr_VDEM) |>
  mutate(v2x_polyarchy_VDEM_10 = v2x_polyarchy_VDEM*10,
         v2x_corr_VDEM_10 = v2x_corr_VDEM*10) |>
  filter(year >= 2000 & year <= 2005) |>
  unique() |>
  drop_na()

m_long_1 = lm(gdppc_WDI_PW ~ v2x_corr_VDEM + v2x_polyarchy_VDEM + as.factor(country) + as.factor(year), data=dta_sel_2)
m_long_2 = lm(lngdppc_WDI_PW ~ v2x_corr_VDEM + v2x_polyarchy_VDEM + as.factor(country) + as.factor(year), data=dta_sel_2)
stargazer(m_long_1, m_long_2, type="text", omit=c("as.factor"))
