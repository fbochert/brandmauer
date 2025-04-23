# setwd("M:/user/bochert/RA/Daniel/Brandmauer/Analyse")

library(tidyverse)
library(modelsummary)
library(cobalt)
library(MatchIt)
library(kableExtra)

match <- read_csv("data/match.csv") %>%
  mutate(east = ifelse(Kreis > 11000, 1, 0))

######################### ROBUSTNESS CHECKS ###############################

# Coarsened Exact Matching - four variables
library(cem)
mat1 <- match %>%
  select(vprop_bin, D_AfD, dichte, laenge, aktiv, cdu) %>%
  cem(treatment = "vprop_bin", drop = "D_AfD", keep.all = TRUE)
att(mat1, D_AfD ~ vprop_bin, data = as.data.frame(match))
# Output
# SATT point estimate: 0.027764 (p.value=0.011344)
# 95% conf. interval: [0.006588, 0.048940]

dichte_cut <- c(0, 110, 220, 330, 3000)
laenge_cut <- c(0, 2, 4, 7)
aktiv_cut <- c(0, 0.01, 0.2, 1, 6)
cdu_cut <- c(0, 0.25, 0.3, 0.35, 1)
mat2 <- match %>%
  dplyr::select(vprop_bin, D_AfD, dichte, laenge, aktiv, cdu) %>%
  cem(treatment = "vprop_bin",
      drop = "D_AfD",
      cutpoints = list(dichte = dichte_cut, laenge = laenge_cut,
                       aktiv = aktiv_cut, cdu = cdu_cut))
att(mat2, D_AfD ~ vprop_bin, data = as.data.frame(match))
# Output:
# SATT point estimate: 0.030723 (p.value=0.005066)
# 95% conf. interval: [0.009645, 0.051802]

# Coarsened Exact Matching - all variables
mat3 <- match %>%
  select(vprop_bin, D_AfD, dichte, arb, migr, bip, east, laenge, aktiv, cdu) %>%
  cem(treatment = "vprop_bin", drop = "D_AfD", keep.all = TRUE)
att(mat3, D_AfD ~ vprop_bin, data = as.data.frame(match))
# Output
# SATT point estimate: -0.008387 (p.value=0.703537)
# 95% conf. interval: [-0.051124, 0.034351]


# Coarsened Exact Matching - demographic variables
mat4 <- match %>%
  select(vprop_bin, D_AfD, dichte, arb, migr, bip, east) %>%
  cem(treatment = "vprop_bin", drop = "D_AfD", keep.all = TRUE)
att(mat4, D_AfD ~ vprop_bin, data = as.data.frame(match))
# Output
# SATT point estimate: -0.010828 (p.value=0.267063)
# 95% conf. interval: [-0.029873, 0.008217]


## Weighting - four, all, and demographic variables
ps1 <- glm(vprop_bin ~ dichte + laenge + aktiv + cdu, data = match, family = binomial())
match$ps1value <- predict(ps1, type = "response")
match$weight1.ATE <- ifelse(match$vprop_bin == 1, 1/match$ps1value, 1/(1-match$ps1value))

match1 <- match %>%
  filter(!is.na(arb))
ps2 <- glm(vprop_bin ~ dichte + arb + migr + bip + east + laenge + aktiv + cdu, data = match1, family = binomial())
match1$ps2value <- predict(ps2, type = "response")
match1$weight2.ATE <- ifelse(match1$vprop_bin == 1, 1/match1$ps2value, 1/(1-match1$ps2value))

ps3 <- glm(vprop_bin ~ dichte + arb + migr + bip + east, data = match1, family = binomial())
match1$ps3value <- predict(ps3, type = "response")
match1$weight3.ATE <- ifelse(match1$vprop_bin == 1, 1/match1$ps3value, 1/(1-match1$ps3value))

fit5 <- lm(D_AfD ~ vprop_bin, data = match, weights = (weight1.ATE))
fit6 <- lm(D_AfD ~ vprop_bin + I(log(dichte)) + laenge + aktiv + cdu, data = match, weights = (weight1.ATE))
fit7 <- lm(D_AfD ~ vprop_bin, data = match1, weights = (weight2.ATE))
fit8 <- lm(D_AfD ~ vprop_bin + I(log(dichte)) + arb + migr + I(log(bip)) + east + laenge + aktiv + cdu, data = match1, weights = (weight2.ATE))
fit9 <- lm(D_AfD ~ vprop_bin, data = match1, weights = (weight3.ATE))
fit10 <- lm(D_AfD ~ vprop_bin + I(log(dichte)) + arb + migr + I(log(bip)) + east, data = match1, weights = (weight3.ATE))


modelsummary::modelsummary(list("Model 1" = fit5, "Model 2" = fit6, "Model 3" = fit7, "Model 4" = fit8, "Model 5" = fit9, "Model 6" = fit10), 
                           stars = c('*' = .1, '**' = .05, '***' = .01), 
                           statistic = 'std.error',
                           gof_omit = 'AIC|BIC|RMSE|Log.Lik.|Std.Errors|F',
                           coef_map = c("vprop_bin" = "Cooperation",
                                        "laenge" = "Professionalism",
                                        "aktiv" = "AfD Activity",
                                        "cdu" = "CDU strength",
                                        "east" = "Eastern Germany",
                                        "I(log(dichte))" = "Population Density (logged)",
                                        "arb" = "Unemployment Rate",
                                        "migr" = "Migratory Background (%)",
                                        "I(log(bip))" = "GDP (logged)",
                                        "(Intercept)" = "Intercept"),
                           fmt = 2,
                           output = "tables/22-robustness1.html")


## Different Independent Variable

fit11 <- lm(D_AfD ~ vprop, 
           data = match)
fit12 <- lm(D_AfD ~ vprop + dichte + laenge + aktiv + cdu, 
           data = match)
fit13 <- lm(D_AfD ~ vprop + dichte + arb + migr + I(log(bip)) + east, 
            data = match)
fit14 <- lm(D_AfD ~ vprop + dichte + arb + migr + I(log(bip)) + east + laenge + aktiv + cdu, 
           data = match)

modelsummary::modelsummary(list("Model 1" = fit11, "Model 2" = fit12, "Model 3" = fit13, "Model 4" = fit14), 
                           stars = c('*' = .1, '**' = .05, '***' = .01), 
                           statistic = 'std.error',
                           gof_omit = 'AIC|BIC|RMSE|Log.Lik.|Std.Errors|F',
                           coef_map = c("vprop" = "Cooperation (in %)",
                                        "laenge" = "Professionalism",
                                        "aktiv" = "AfD Activity",
                                        "cdu" = "CDU strength",
                                        "east" = "Eastern Germany",
                                        "dichte" = "Population Density",
                                        "arb" = "Unemployment Rate",
                                        "migr" = "Migratory Background (%)",
                                        "I(log(bip))" = "GDP (logged)",
                                        "(Intercept)" = "Intercept"),
                           fmt = 2,
                           output = "tables/22-robustness2.html")
