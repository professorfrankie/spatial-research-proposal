library(dplyr)
library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(readr)
library(fixest)
library(modelsummary)
library(ggplot2)
library(geobr)


df <- read_csv("processed_data/df_amazon_new.csv") |> 
  select(-c(...1, forest, gdp_pc, population, pop_dens, pa_tot_ha, n_fined, brl_fined))

muni_ha <- read_xls("raw_data/AR_BR_RG_UF_RGINT_RGI_MUN_2023.xls") |> 
  select(muni_id = CD_MUN, area_km2 = AR_MUN_2023) |> 
  mutate(muni_ha = area_km2*10^4,
         muni_id = as.numeric(muni_id)) |> 
  select(muni_id, muni_ha)

df <- df |>
  group_by(muni_id) |>
  left_join(muni_ha, by = "muni_id")

############################

# share -> garimpo_ha_2005/ total_garimpo_ha_2005

df_share <- df %>%
  group_by(year) %>%
  mutate(total_yearly_garimpo_ha = sum(garimpo_ha, na.rm = TRUE)) %>%
  ungroup()|> 
  mutate(
    share_zi0 = garimpo_ha / total_yearly_garimpo_ha
  ) |> 
  filter(year == 2005) |>
  select(muni_id, year, share_zi0)

sum(df_share$share_zi0)

############################

# share -> garimpo_ha_2005/muni_ha
df_share <- df %>%
  mutate(
    share_zi0 = garimpo_ha / muni_ha
  ) |> 
  filter(year == 2005) |>
  select(muni_id, year, share_zi0)

sum(df_share$share_zi0)

############################

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
df_2005 <- df_bartik_final %>%
  mutate_all(~replace(., is.na(.), 0)) |>
  rename(year = year.x) |>
  select(-c(year.y))

## count NAs

na_count <- sum(is.na(df_2005))
colSums(is.na(df_2005))


#filter years>2005
df_model <- df_2005 |>
  filter(year > 2005)


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

###############################################################################
# analyse 2005 garimpo 
df_graph <- df_2005 |>
  group_by(muni_id) |>
  filter(any(garimpo_ha > 0, na.rm = TRUE)) |>  # <-- filter out municipalities with no garimpo ever
  summarize(
    garimpo_flag_2005 = as.integer(any(garimpo_ha[year == 2005] > 0, na.rm = TRUE)),
    garimpo_2022 = sum(garimpo_ha[year == 2022], na.rm = TRUE),
    garimpo_2006 = sum(garimpo_ha[year == 2006], na.rm = TRUE)
  ) |>
  mutate(
    garimpo_change_06_22 = garimpo_2022 - garimpo_2006
  ) |>
  select(muni_id, garimpo_flag_2005, garimpo_change_06_22)

df_graph |> count(garimpo_flag_2005)
#print the municipalities with no garimpo in 2005
df_graph |> filter(garimpo_flag_2005 == 0) |> 
  select(muni_id, garimpo_flag_2005, garimpo_change_06_22)


legal_amazon <- read_sf("raw_data/Limites_Amazonia_Legal_2022.shp") %>%
  st_set_crs(4674) %>%
  st_transform(crs = 4674)

muni_sf <- read_municipality(year = 2022, showProgress = FALSE) %>%
  mutate(muni_id = as.numeric(code_muni)) |>
  select(muni_id, geom) %>%
  st_transform(4674)

df_graph <- muni_sf %>%
  left_join(df_graph, by = "muni_id") 

data("municipalities")
legal_amazon_munis <- municipalities %>%
  filter(legal_amazon == 1) %>%
  pull(code_muni)

df_graph <- df_graph |>
  filter(muni_id %in% legal_amazon_munis)

ggplot() +
  # Base map: Legal Amazon region (light gray/white fill)
  geom_sf(data = legal_amazon, fill = "white", alpha = 0.3) +
  # Main layer: fill by garimpo change, black border
  geom_sf(data = df_graph, aes(fill = garimpo_change_06_22), color = "black", size = 0.5) +
  # Yellow contour for municipalities without mining in 2005
  geom_sf(
    data = df_graph |> filter(garimpo_flag_2005 == 0),
    fill = NA, color = "red", size = 2
  ) +
  # Fill scale for mining change
  scale_fill_viridis_c(direction = -1, option = "D", name = "Change in Garimpo ha",
                       na.value = "white") +
  # Labels and theme
  labs(title = "Change in Garimpo Area by Municipality in the Brazilian Legal Amazon (2006 vs 2022)",
       subtitle = "with highlighted the municipalities with no garimpo in 2005") +
  theme_void() +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(size = 16, face = "bold")
  ) +
  # Optional coordinate cropping
  coord_sf(ylim = c(-19, 6))
