library(dplyr)
library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(readr)
library(fixest)
library(modelsummary)


df <- read_csv("processed_data/df_amazon_newpca.csv") |> 
  select(-c(...1, forest, gdp_pc, population, pop_dens, pa_tot_ha, n_fined, brl_fined)) |> 
  filter(between(year, 2002, 2022))

df_share <- df %>%
  group_by(muni_id) %>%
  summarise(avg_garimpo_ha = mean(garimpo_ha, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    total_avg_garimpo = sum(avg_garimpo_ha, na.rm = TRUE),
    share_zi0 = avg_garimpo_ha / total_avg_garimpo
  ) %>%
  select(muni_id, share_zi0)

sum(df_share$share_zi0)


df_bartik <- df |> 
  left_join(
    df_share,
    by = c("muni_id")
  )

####################################################
# using difference in gold price
df_bartik_final <- df_bartik |> 
  mutate(
    bartik = shift_gold1 * share_zi0,
    bartik2 = shift_gold2 * share_zi0,
    bartik3 = shift_gold3 * share_zi0,
    bartik4 = shift_gold4 * share_zi0
  )

####################################################
# using difference in log gold price
df_bartik_final <- df_bartik |> 
  mutate(
    bartik = log_gold1 * share_zi0,
    bartik2 = log_gold2 * share_zi0,
    bartik3 = log_gold3 * share_zi0,
    bartik4 = log_gold4 * share_zi0
  )

####################################################
# using percentage price change
df_bartik_final <- df_bartik |> 
  mutate(
    bartik = prc_gold1 * share_zi0,
    bartik2 = prc_gold2 * share_zi0,
    bartik3 = prc_gold3 * share_zi0,
    bartik4 = prc_gold4 * share_zi0
  )

####################################################
# using difference in tin price
df_bartik_final <- df_bartik |> 
  mutate(
    bartik = shift_tin1 * share_zi0,
    bartik2 = shift_tin2 * share_zi0,
    bartik3 = shift_tin3 * share_zi0,
    bartik4 = shift_tin4 * share_zi0
  )

####################################################
# using difference in log tin price
df_bartik_final <- df_bartik |> 
  mutate(
    bartik = log_tin1 * share_zi0,
    bartik2 = log_tin2 * share_zi0,
    bartik3 = log_tin3 * share_zi0,
    bartik4 = log_tin4 * share_zi0
  )

#####################################################
# using percentage price change
df_bartik_final <- df_bartik |> 
  mutate(
    bartik = prc_tin1 * share_zi0,
    bartik2 = prc_tin2 * share_zi0,
    bartik3 = prc_tin3 * share_zi0,
    bartik4 = prc_tin4 * share_zi0
  )

####################################################
# using difference in iron ore price
df_bartik_final <- df_bartik |> 
  mutate(
    bartik = shift_iron1 * share_zi0,
    bartik2 = shift_iron2 * share_zi0,
    bartik3 = shift_iron3 * share_zi0,
    bartik4 = shift_iron4 * share_zi0
  )

###################################################
# using difference in log iron ore price
df_bartik_final <- df_bartik |> 
  mutate(
    bartik = log_iron1 * share_zi0,
    bartik2 = log_iron2 * share_zi0,
    bartik3 = log_iron3 * share_zi0,
    bartik4 = log_iron4 * share_zi0
  )

##################################################
# using percentage price change
df_bartik_final <- df_bartik |> 
  mutate(
    bartik = prc_iron1 * share_zi0,
    bartik2 = prc_iron2 * share_zi0,
    bartik3 = prc_iron3 * share_zi0,
    bartik4 = prc_iron4 * share_zi0
  )

#####################################################
# lag 3 and 4 iron and thin
df_bartik_final <- df_bartik |> 
  mutate(
    bartik3i = shift_iron3 * share_zi0,
    bartik4i = shift_iron4 * share_zi0,
    bartik3t = shift_tin3 * share_zi0,
    bartik4t = shift_tin4 * share_zi0
  )



## REGRESSION: Reduced form
# Join temporarily
df_model <- df_bartik_final %>%
  mutate_all(~replace(., is.na(.), 0))

## count NAs

na_count <- sum(is.na(df_model))
colSums(is.na(df_model))

######### FILTER GARIMPO ONLY ########
df_model <- df_model |> 
  group_by(muni_id, year) |>
  filter(garimpo_ha > 0)

####################################

####### OLS #########
ols_model <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                     population_change + pop_dens_change + pa_tot_ha_change + 
                     n_fined_change + brl_fined_change | year,
                   data = df_model)

###### IV SPECIFICATION #########
###### SECOND STAGE #########

# second stage for change_in_area
second_stage1 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik,
                        data = df_model)
summary(second_stage1)

second_stage2 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik2,
                        data = df_model)
summary(second_stage2)

second_stage3 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik3,
                        data = df_model)
summary(second_stage3)

second_stage4 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik4,
                        data = df_model)
summary(second_stage4)

# Combine second-stage models into a list
second_stage_models_change <- list(
  "t-1" = second_stage1,
  "t-2" = second_stage2,
  "t-3" = second_stage3,
  "t-4" = second_stage4
)
# Create a summary table for second-stage models
modelsummary(
  second_stage_models_change,
  output = "latex",
  title = "Second Stage Estimates – Change in Area",
  coef_map = c(
    "fit_garimpo_ha_change" = "Change in Garimpo Area"
  ),
  statistic = c("({std.error})", "p.value"),
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Deviance|RMSE",
  escape = FALSE
)

second_stage_models_ols <- list(
  "OLS" = ols_model,
  "t-3" = second_stage3,
  "t-4" = second_stage4
)

etable(
  second_stage_models_ols,
  dict = c(
    garimpo_ha_change     = "Change in Garimpo Area",
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change", "%fit_garimpo_ha_change"),
  headers = c("OLS", "t-3", "t-4"),
  fitstat = ~ n + r2 + ar2 + ivf,
  tex     = TRUE,
  title   = "Second Stage Estimates – Change in Area",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full")
  )
)




summary(second_stage1, stage = 1)
summary(second_stage2, stage = 1)
summary(second_stage3, stage = 1)
summary(second_stage4, stage = 1)

fs_t3 <- feols(garimpo_ha_change ~ spei_dry + gdp_pc_change + population_change +
                 pop_dens_change + pa_tot_ha_change + n_fined_change +
                 brl_fined_change + bartik3 | year,
               data = df_model)

fs_t4 <- feols(garimpo_ha_change ~ spei_dry + gdp_pc_change + population_change +
                 pop_dens_change + pa_tot_ha_change + n_fined_change +
                 brl_fined_change + bartik4 | year,
               data = df_model)

first_stage_models_change <- list(
  "t-3 (1st stage)" = fs_t3,
  "t-4 (1st stage)" = fs_t4
)

etable(
  first_stage_models_change,
  dict = c(
    bartik3 = "Bartik",
    bartik4 = "Bartik"
  ),
  keep   = c("%bartik3", "%bartik4"),
  headers = c("t-3", "t-4"),
  fitstat = ~ n + r2 + ar2 + f,
  tex = TRUE,
  title = "First Stage Estimates – Change in Area",
  extralines = list("Covariates" = c("Full", "Full"))
)


first_stage_models_change <- list(
  "t-1" = summary(second_stage1, stage = 1),
  "t-2" = summary(second_stage2, stage = 1),
  "t-3" = summary(second_stage3, stage = 1),
  "t-4" = summary(second_stage4, stage = 1)
)

modelsummary(
  first_stage_models_change,
  output = "latex",
  title = "Second Stage Estimates – Change in Area",
  coef_map = c(
    "bartik" = "Bartik",
    "bartik2" = "Bartik",
    "bartik3" = "Bartik",
    "bartik4" = "Bartik"
  ),
  statistic = c("({std.error})", "p.value"),
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Deviance|RMSE",
  escape = FALSE
)

# second stage with multiple bartiks

second_stage1B <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik,
                        data = df_model)
summary(second_stage1B)

second_stage2B <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | 
                          garimpo_ha_change ~ bartik + bartik2,
                        data = df_model)
summary(second_stage2B)

second_stage3B <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | 
                          garimpo_ha_change ~ bartik + bartik2 + bartik3,
                        data = df_model)
summary(second_stage3B)

second_stage4B <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | 
                          garimpo_ha_change ~ bartik + bartik2 + bartik3 + bartik4,
                        data = df_model)
summary(second_stage4B)

# Combine second-stage models into a list
second_stage_models_changeB <- list(
  "t-1" = second_stage1B,
  "t-2" = second_stage2B,
  "t-3" = second_stage3B,
  "t-4" = second_stage4B
)

# Create a summary table for second-stage models
modelsummary(
  second_stage_models_changeB,
  output = "latex",
  title = "Second Stage Estimates – Change in Area",
  coef_map = c(
    "fit_garimpo_ha_change" = "Change in Garimpo Area"
  ),
  statistic = c("({std.error})", "p.value"),
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Deviance|RMSE",
  escape = FALSE
)

summary(second_stage1B, stage = 1)
summary(second_stage2B, stage = 1)
summary(second_stage3B, stage = 1)
summary(second_stage4B, stage = 1)

fist_stage_models_changeB <- list(
  "Model 1" = summary(second_stage1B, stage = 1),
  "Model 2" = summary(second_stage2B, stage = 1),
  "Model 3" = summary(second_stage3B, stage = 1),
  "Model 4" = summary(second_stage4B, stage = 1)
)

modelsummary(
  fist_stage_models_changeB,
  output = "latex",
  title = "Second Stage Estimates – Change in Area",
  coef_map = c(
    "bartik" = "Bartik t-1",
    "bartik2" = "Bartik t-2",
    "bartik3" = "Bartik t-3",  
    "bartik4" = "Bartik t-4"
  ),
  statistic = c("({std.error})", "p.value"),
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Deviance|RMSE",
  escape = FALSE
)




second_stage5 <- feols(forest_change ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik,
                       data = df_model)
summary(second_stage5)

second_stage6 <- feols(forest_change ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik2,
                       data = df_model)
summary(second_stage6)

second_stage7 <- feols(forest_change ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik3,
                       data = df_model)
summary(second_stage7)

second_stage8 <- feols(forest_change ~ spei_dry + gdp_pc_change + 
                         population_change + pop_dens_change + pa_tot_ha_change + 
                         n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik4,
                       data = df_model)
summary(second_stage8)

# Combine second-stage models into a list
second_stage_forest_change <- list(
  "t-1" = second_stage5,
  "t-2" = second_stage6,
  "t-3" = second_stage7,
  "t-4" = second_stage8
)
# Create a summary table for second-stage models
modelsummary(
  second_stage_forest_change,
  output = "latex",
  title = "Second Stage Estimates – Change in Area",
  coef_map = c(
    "fit_garimpo_ha_change" = "Change in Garimpo Area"
  ),
  statistic = c("({std.error})", "p.value"),
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Deviance|RMSE",
  escape = FALSE
)

summary(second_stage5, stage = 1)
summary(second_stage6, stage = 1)
summary(second_stage7, stage = 1)
summary(second_stage8, stage = 1)

# second stage with multiple bartiks

second_stage5B <- feols(forest_change ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik,
                        data = df_model)
summary(second_stage5B)

second_stage6B <- feols(forest_change ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | 
                          garimpo_ha_change ~ bartik + bartik2,
                        data = df_model)
summary(second_stage6B)

second_stage7B <- feols(forest_change ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | 
                          garimpo_ha_change ~ bartik + bartik2 + bartik3,
                        data = df_model)
summary(second_stage7B)

second_stage8B <- feols(forest_change ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | 
                          garimpo_ha_change ~ bartik + bartik2 + bartik3 + bartik4,
                        data = df_model)
summary(second_stage8B)

# Combine second-stage models into a list
second_stage_forest_changeB <- list(
  "t-1" = second_stage5B,
  "t-2" = second_stage6B,
  "t-3" = second_stage7B,
  "t-4" = second_stage8B
)

# Create a summary table for second-stage models
modelsummary(
  second_stage_models_change,
  output = "latex",
  title = "Second Stage Estimates – Change in Area",
  coef_map = c(
    "fit_garimpo_ha_change" = "Change in Garimpo Area"
  ),
  statistic = c("({std.error})", "p.value"),
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Deviance|RMSE",
  escape = FALSE
)

summary(second_stage5B, stage = 1)
summary(second_stage6B, stage = 1)
summary(second_stage7B, stage = 1)
summary(second_stage8B, stage = 1)

#####################################
second_stage3i <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik3i,
                        data = df_model)
summary(second_stage3i)

second_stage4i <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik4i,
                        data = df_model)
summary(second_stage4i)

second_stage3t <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik3t,
                        data = df_model)
summary(second_stage3t)

second_stage4t <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik4t,
                        data = df_model)
summary(second_stage4t)

# Combine second-stage models into a list
# Combine second-stage models into a list
etable(
  list(
    "Lag 3 – Iron"     = second_stage3i, 
    "Lag 3 – Tin" = second_stage3t,
    "Lag 4 – Iron"     = second_stage4i, 
    "Lag 4 – Tin" = second_stage4t
  ),
  dict = c(
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%fit_garimpo_ha_change"),
  headers = c("Lag 3 – Iron", "Lag 3 – Tin", "Lag 4 – Iron", "Lag 4 – Tin"),
  fitstat = ~ n + r2 + ar2 + ivf,
  tex     = TRUE,
  title   = "IV Estimates – Iron vs Tin (Lag 3 vs Lag 4)",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full")
  )
)
