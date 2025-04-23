# setwd("M:/user/bochert/RA/Daniel/Brandmauer/Analyse")

library(tidyverse)
library(modelsummary)
library(cobalt)
library(MatchIt)
library(kableExtra)

match <- read_csv("data/match.csv")

###################### BEFORE MATCHING #############################

# Balance table for covariates
list_cov <- c("laenge", "aktiv", "cdu", "dichte")
match %>%
  dplyr::summarize_at(list_cov, ~list(broom::tidy(t.test(. ~ vprop_bin)))) %>% 
  purrr::map(1) %>%
  dplyr::bind_rows(.id='variables') %>% 
  dplyr::select(variables, estimate1, estimate2, p.value) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>% 
  knitr::kable(align = rep('c', 4), col.names = c("Variable", "Control (Cooperation = 0)", "Treat (Cooperation = 1)", "P-value")) %>% 
  kableExtra::kable_styling() %>%
  save_kable("tables/22-bal_tab1.html")

##################### MATCHING WITH 4 COVARS ##########################
matches1 <- match %>%
  MatchIt::matchit(vprop_bin ~ I(log(laenge + 1)) + I(log(aktiv + 1)) + cdu + dichte,
                            method = "nearest",
                            distance = "logit",
                            ratio = 1,
                            replace = TRUE, 
                   data = .)

matched1 <- MatchIt::get_matches(matches1) 

# Show that matching worked - much better!
matched1 %>%
  dplyr::summarize_at(list_cov, ~list(broom::tidy(t.test(. ~ vprop_bin)))) %>% 
  purrr::map(1) %>%
  dplyr::bind_rows(.id='variables') %>% 
  dplyr::select(variables, estimate1, estimate2, p.value) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>% 
  knitr::kable(align = rep('c', 4), col.names = c("Variable", "Control (Cooperation = 0)", "Treat (Cooperation = 1)", "P-value")) %>% 
  kableExtra::kable_styling() %>%
  save_kable("tables/22-bal_tab1_1.html")

love.plot(matches1, 
          stats = "mean.diffs",
          drop.distance = TRUE, 
          thresholds = c(m = .1),
          var.names = c(`I(log(laenge + 1))` = "Professionalism",
                        `I(log(aktiv + 1))` = "AfD Activeness",
                        cdu = "CDU Strength",
                        dichte = "Population Density"),
          colors = c("red", "blue"))
ggsave("tables/22-loveplot1.png", width = 6, height = 4)

# Simple t.test
t.test(D_AfD ~ vprop_bin, data = matched1)
# data:  D_AfD by vprop_bin
# t = -4.0617, df = 159.38, p-value = 7.623e-05
# mean in group 0 mean in group 1 
# 0.02344890      0.06197709

# Matching Model Summary
summary(matches1)
# Sample Sizes:
#             Control   Treated
# All            188.        81
# Matched (ESS)   35.09      81
# Matched         52.        81
# Unmatched      136.         0
# Discarded        0.         0

# Running regressions
fit1 <- lm(D_AfD ~ vprop_bin, 
           weights = weights, 
           data = matched1)
fit2 <- lm(D_AfD ~ vprop_bin + I(log(dichte)) + laenge + aktiv + cdu, 
           weights = weights, 
           data = matched1)

# Reporting output
modelsummary::modelsummary(list("Model 1" = fit1, "Model 2" = fit2), 
                           stars = c('*' = .1, '**' = .05, '***' = .01), 
                           statistic = 'std.error',
                           gof_omit = 'AIC|BIC|RMSE|Log.Lik.|Std.Errors|F',
                           coef_map = c("vprop_bin" = "Cooperation",
                                        "laenge" = "Professionalism",
                                        "aktiv" = "AfD Activity",
                                        "cdu" = "CDU strength",
                                        "I(log(dichte))" = "Population Density (logged)",
                                        "(Intercept)" = "Intercept"),
                           fmt = 2,
                           output = "tables/22-regression1.html")

######################## MORE COVARIATES #############################
# Additional variables
match <- match %>%
  mutate(east = ifelse(Kreis > 11000, 1, 0))

# Balance table for covariates
list_cov1 <- c("dichte", "migr", "bip", "east", "laenge", "aktiv", "cdu")
match %>%
  dplyr::summarize_at(list_cov1, ~list(broom::tidy(t.test(. ~ vprop_bin)))) %>% 
  purrr::map(1) %>%
  dplyr::bind_rows(.id='variables') %>% 
  dplyr::select(variables, estimate1, estimate2, p.value) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>% 
  knitr::kable(align = rep('c', 4), col.names = c("Variable", "Control (Cooperation = 0)", "Treat (Cooperation = 1)", "P-value")) %>% 
  kableExtra::kable_styling() %>%
  save_kable("tables/22-bal_tab2.html")

matches2 <- match %>%
  filter(laenge > 0) %>%
  filter(!is.na(arb)) %>%
  MatchIt::matchit(vprop_bin ~ laenge + aktiv + cdu + east + dichte + arb + bip + migr,
                   method = "nearest",
                   distance = "logit",
                   ratio = 1,
                   replace = TRUE, 
                   data = .)

matched2 <- MatchIt::get_matches(matches2) 

# Show that matching worked
matched2 %>%
  dplyr::summarize_at(list_cov1, ~list(broom::tidy(t.test(. ~ vprop_bin)))) %>% 
  purrr::map(1) %>%
  dplyr::bind_rows(.id='variables') %>% 
  dplyr::select(variables, estimate1, estimate2, p.value) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>% 
  knitr::kable(align = rep('c', 4), col.names = c("Variable", "Control (Cooperation = 0)", "Treat (Cooperation = 1)", "P-value")) %>% 
  kableExtra::kable_styling() %>%
  save_kable("tables/22-bal_tab2_1.html")

love.plot(matches2, 
          stats = "mean.diffs",
          drop.distance = TRUE, 
          thresholds = c(m = .1),
          var.names = c(laenge = "Professionalism",
                        aktiv = "AfD Activeness",
                        cdu = "CDU Strength",
                        east = "Eastern Germany",
                        dichte = "Population Density",
                        arb = "Unemployment Rate",
                        migr = "Migratory Background (%)",
                        bip = "GDP"),
          colors = c("red", "blue"))
ggsave("tables/22-loveplot2.png", width = 6, height = 4)

# Simple t.test
t.test(D_AfD ~ vprop_bin, data = matched2)
# data:  D_AfD by vprop_bin
# t = 0.020764, df = 141.36, p-value = 0.9835
# mean in group 0 mean in group 1 
# 0.06499346      0.06480894 

# Matching Model Summary
summary(matches2)
# Sample Sizes:
#   Control Treated
# All            172.        75
# Matched (ESS)   18.81      75
# Matched         39.        75
# Unmatched      133.         0
# Discarded        0.         0

# Running regressions
fit3 <- lm(D_AfD ~ vprop_bin, 
           weights = weights, 
           data = matched2)
fit4 <- lm(D_AfD ~ vprop_bin + I(log(dichte)) + arb + migr + I(log(bip)) + east + laenge + aktiv + cdu,
           weights = weights, 
           data = matched2)

######################## OTHER COVARIATES #############################
# Additional variables
match <- match %>%
  mutate(east = ifelse(Kreis > 11000, 1, 0))

# Balance table for covariates
list_cov2 <- c("dichte", "migr", "bip", "east", "arb")
match %>%
  dplyr::summarize_at(list_cov2, ~list(broom::tidy(t.test(. ~ vprop_bin)))) %>% 
  purrr::map(1) %>%
  dplyr::bind_rows(.id='variables') %>% 
  dplyr::select(variables, estimate1, estimate2, p.value) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>% 
  knitr::kable(align = rep('c', 4), col.names = c("Variable", "Control (Cooperation = 0)", "Treat (Cooperation = 1)", "P-value")) %>% 
  kableExtra::kable_styling() %>%
  save_kable("tables/22-bal_tab3.html")

matches3 <- match %>%
  filter(!is.na(arb)) %>%
  MatchIt::matchit(vprop_bin ~ east + dichte + arb + bip + migr,
                   method = "nearest",
                   distance = "logit",
                   ratio = 1,
                   replace = TRUE, 
                   data = .)

matched3 <- MatchIt::get_matches(matches3) 

# Show that matching worked
matched3 %>%
  dplyr::summarize_at(list_cov2, ~list(broom::tidy(t.test(. ~ vprop_bin)))) %>% 
  purrr::map(1) %>%
  dplyr::bind_rows(.id='variables') %>% 
  dplyr::select(variables, estimate1, estimate2, p.value) %>%
  dplyr::mutate_if(is.numeric, round, 2) %>% 
  knitr::kable(align = rep('c', 4), col.names = c("Variable", "Control (Cooperation = 0)", "Treat (Cooperation = 1)", "P-value")) %>% 
  kableExtra::kable_styling() %>%
  save_kable("tables/22-bal_tab3_1.html")

love.plot(matches3, 
          stats = "mean.diffs",
          drop.distance = TRUE, 
          thresholds = c(m = .1),
          var.names = c(laenge = "Professionalism",
                        aktiv = "AfD Activeness",
                        cdu = "CDU Strength",
                        east = "Eastern Germany",
                        dichte = "Population Density",
                        arb = "Unemployment Rate",
                        migr = "Migratory Background (%)",
                        bip = "GDP"),
          colors = c("red", "blue"))
ggsave("tables/22-loveplot3.png", width = 6, height = 4)

# Simple t.test
t.test(D_AfD ~ vprop_bin, data = matched3)
# data:  D_AfD by vprop_bin
# t = 1.4081, df = 151.4, p-value = 0.1612
# mean in group 0 mean in group 1 
# 0.07530921      0.06262502 

# Matching Model Summary
summary(matches3)
# Sample Sizes:
#               Control Treated
# All            182.        79
# Matched (ESS)   25.47      79
# Matched         41.        79
# Unmatched      141.         0
# Discarded        0.         0

# Running regressions
fit5 <- lm(D_AfD ~ vprop_bin, 
           weights = weights, 
           data = matched3)
fit6 <- lm(D_AfD ~ vprop_bin + I(log(dichte)) + arb + migr + I(log(bip)) + east,
           weights = weights, 
           data = matched3)

# Reporting output
modelsummary::modelsummary(list("Model 1" = fit1, "Model 2" = fit2, "Model 3" = fit3, "Model 4" = fit4, "Model 5" = fit5, "Model 6" = fit6), 
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
                           output = "tables/22-regression2.html")
