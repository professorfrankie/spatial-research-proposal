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

pca_shocks <- read.csv("processed_data/pca_shocks.csv")

df_bartik1 <- df_bartik |> 
  left_join(
    pca_shocks,
    by = c("year")
  )

####################################################
# using gold pca shock
df_bartik_final <- df_bartik1 |> 
  mutate(
    bartik_align0 = gold_aligned_shock * share_zi0,
    bartik_orth0 = gold_orthogonal_shock * share_zi0,
    bartik_align1 = gold_aligned1 * share_zi0,
    bartik_orth1 = gold_orthogonal1 * share_zi0,
    bartik_align2 = gold_aligned2 * share_zi0,
    bartik_orth2 = gold_orthogonal2 * share_zi0,
    bartik_align3 = gold_aligned3 * share_zi0,
    bartik_orth3 = gold_orthogonal3 * share_zi0,
    bartik_align4 = gold_aligned4 * share_zi0,
    bartik_orth4 = gold_orthogonal4 * share_zi0
  )


## REGRESSION: Reduced form
# Join temporarily
df_model <- df_bartik_final %>%
  mutate_all(~replace(., is.na(.), 0))

## count NAs

na_count <- sum(is.na(df_model))
colSums(is.na(df_model))

###### IV SPECIFICATION #########
###### SECOND STAGE #########

# second stage for change_in_area
second_stage_align0 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_align0,
                        data = df_model)

second_stage_align1 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_align1,
                        data = df_model)

second_stage_align2 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_align2,
                        data = df_model)

second_stage_align3 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_align3,
                        data = df_model)

second_stage_align4 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_align4,
                        data = df_model)

second_stage_orth0 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_orth0,
                        data = df_model)

second_stage_orth1 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                              population_change + pop_dens_change + pa_tot_ha_change + 
                              n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_orth1,
                            data = df_model)

second_stage_orth2 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                              population_change + pop_dens_change + pa_tot_ha_change + 
                              n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_orth2,
                            data = df_model)

second_stage_orth3 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                              population_change + pop_dens_change + pa_tot_ha_change + 
                              n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_orth3,
                            data = df_model)

second_stage_orth4 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                              population_change + pop_dens_change + pa_tot_ha_change + 
                              n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_orth4,
                            data = df_model)

second_stage_align <- list(
  "t" = second_stage_align0,
  "t-1" = second_stage_align1,
  "t-2" = second_stage_align2,
  "t-3" = second_stage_align3,
  "t-4" = second_stage_align4
)

etable(
  second_stage_align,
  dict = c(
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change", "%fit_garimpo_ha_change"),
  headers = c("t", "t-1", "t-2", "t-3", "t-4"),
  fitstat = ~ n + r2 + ar2 + ivf,
  tex     = TRUE,
  title   = "Second Stage Estimates – Change in Area",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full", "Full")
  )
)


second_stage_orth <- list(
  "t" = second_stage_orth0,
  "t-1" = second_stage_orth1,
  "t-2" = second_stage_orth2,
  "t-3" = second_stage_orth3,
  "t-4" = second_stage_orth4
)

etable(
  second_stage_orth,
  dict = c(
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change", "%fit_garimpo_ha_change"),
  headers = c("t", "t-1", "t-2", "t-3", "t-4"),
  fitstat = ~ n + r2 + ar2 + ivf,
  tex     = TRUE,
  title   = "Second Stage Estimates – Change in Area",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full", "Full")
  )
)


grouped_pca <- read.csv("processed_data/pca_grouped_shocks.csv")


df_bartik1 <- df_bartik |> 
  left_join(
    grouped_pca,
    by = c("year")
  )

####################################################
# using gold pca shock
df_bartik_final <- df_bartik1 |> 
  mutate(
    bartik_precious0 = precious_shock * share_zi0,
    bartik_nonprecious0 = nonprecious_shock * share_zi0,
    bartik_precious1 = precious1 * share_zi0,
    bartik_nonprecious1 = nonprecious1 * share_zi0,
    bartik_precious2 = precious2 * share_zi0,
    bartik_nonprecious2 = nonprecious2 * share_zi0,
    bartik_precious3 = precious3 * share_zi0,
    bartik_nonprecious3 = nonprecious3 * share_zi0,
    bartik_precious4 = precious4 * share_zi0,
    bartik_nonprecious4 = nonprecious4 * share_zi0
  )


## REGRESSION: Reduced form
# Join temporarily
df_model <- df_bartik_final %>%
  mutate_all(~replace(., is.na(.), 0))

## count NAs

na_count <- sum(is.na(df_model))
colSums(is.na(df_model))

###### IV SPECIFICATION #########
###### SECOND STAGE #########
# second stage for change_in_area

second_stage_precious0 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                              population_change + pop_dens_change + pa_tot_ha_change + 
                              n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_precious0,
                            data = df_model)
second_stage_precious1 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                                  population_change + pop_dens_change + pa_tot_ha_change + 
                                  n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_precious1,
                                data = df_model)
second_stage_precious2 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                                  population_change + pop_dens_change + pa_tot_ha_change + 
                                  n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_precious2,
                                data = df_model)
second_stage_precious3 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                                  population_change + pop_dens_change + pa_tot_ha_change + 
                                  n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_precious3,
                                data = df_model)
second_stage_precious4 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                                  population_change + pop_dens_change + pa_tot_ha_change + 
                                  n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_precious4,
                                data = df_model)

second_stage_nonprecious0 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                                  population_change + pop_dens_change + pa_tot_ha_change + 
                                  n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_nonprecious0,
                                data = df_model)
second_stage_nonprecious1 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                                  population_change + pop_dens_change + pa_tot_ha_change + 
                                  n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_nonprecious1,
                                data = df_model)
second_stage_nonprecious2 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                                  population_change + pop_dens_change + pa_tot_ha_change + 
                                  n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_nonprecious2,
                                data = df_model)
second_stage_nonprecious3 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                                  population_change + pop_dens_change + pa_tot_ha_change + 
                                  n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_nonprecious3,
                                data = df_model)
second_stage_nonprecious4 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                                  population_change + pop_dens_change + pa_tot_ha_change + 
                                  n_fined_change + brl_fined_change | year | garimpo_ha_change ~ bartik_nonprecious4,
                                data = df_model)

second_stage_precious <- list(
  "t" = second_stage_precious0,
  "t-1" = second_stage_precious1,
  "t-2" = second_stage_precious2,
  "t-3" = second_stage_precious3,
  "t-4" = second_stage_precious4
)

etable(
  second_stage_precious,
  dict = c(
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change", "%fit_garimpo_ha_change"),
  headers = c("t", "t-1", "t-2", "t-3", "t-4"),
  fitstat = ~ n + r2 + ar2 + ivf,
  tex     = TRUE,
  title   = "Second Stage Estimates – Change in Area",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full", "Full")
  )
)

second_stage_nonprecious <- list(
  "t" = second_stage_nonprecious0,
  "t-1" = second_stage_nonprecious1,
  "t-2" = second_stage_nonprecious2,
  "t-3" = second_stage_nonprecious3,
  "t-4" = second_stage_nonprecious4
)

etable(
  second_stage_nonprecious,
  dict = c(
    fit_garimpo_ha_change = "Change in Garimpo Area",
    forest_loss_all_gross = "Forest Loss"
  ),
  keep    = c("%garimpo_ha_change", "%fit_garimpo_ha_change"),
  headers = c("t", "t-1", "t-2", "t-3", "t-4"),
  fitstat = ~ n + r2 + ar2 + ivf,
  tex     = TRUE,
  title   = "Second Stage Estimates – Change in Area",
  extralines = list(
    "Covariates" = c("Full", "Full", "Full", "Full", "Full")
  )
)

