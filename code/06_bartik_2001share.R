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
  filter(between(year, 2001, 2022))

df_share <- df %>%
  group_by(year) %>%
  mutate(total_yearly_mining_area = sum(garimpo_ha, na.rm = TRUE)) %>%
  ungroup()|> 
  mutate(
    share_zi0 = garimpo_ha / total_yearly_mining_area
  ) |> 
  filter(year == 2001) |>
  select(muni_id, year, share_zi0)

sum(df_share$share_zi0)


df_bartik <- df |> 
  left_join(
    df_share,
    by = c("muni_id")
  ) |>
  rename(
    year = year.x
  ) |> 
  select(-c(year.y)) |>
  filter(between(year, 2002, 2022))

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

####### OLS #########
ols_model <- feols(forest_loss_all_gross ~ garimpo_ha_change + spei_dry + gdp_pc_change + 
                     population_change + pop_dens_change + pa_tot_ha_change + 
                     n_fined_change + brl_fined_change | year,
                   data = df_model)

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
  fitstat = ~ n + r2 + ar2 + f + ivf,
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
