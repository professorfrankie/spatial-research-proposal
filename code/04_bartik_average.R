library(dplyr)
library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(readr)
library(fixest)
library(modelsummary)


df <- read_csv("processed_data/df_amazon_new.csv") |> 
  select(-c(...1, forest, gdp_pc, population, pop_dens, pa_tot_ha, n_fined, brl_fined))


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


df_bartik_final <- df_bartik |> 
  mutate(
    bartik = shift_1 * share_zi0,
    bartik2 = shift_2 * share_zi0,
    bartik3 = shift_3 * share_zi0,
    bartik4 = shift_4 * share_zi0
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

summary(second_stage1, stage = 1)
summary(second_stage2, stage = 1)
summary(second_stage3, stage = 1)
summary(second_stage4, stage = 1)

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

summary(second_stage1B, stage = 1)
summary(second_stage2B, stage = 1)
summary(second_stage3B, stage = 1)
summary(second_stage4B, stage = 1)


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
