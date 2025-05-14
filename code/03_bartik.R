library(dplyr)
library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(readr)
library(fixest)
library(modelsummary)


df <- read_csv("processed_data/df_amazon_raw.csv") |> 
  select(-c(...1))


df_share <- df %>%
  group_by(year) %>%
  mutate(total_yearly_mining_area = sum(artisanal_mining_area_ha, na.rm = TRUE)) %>%
  ungroup()|> 
  mutate(
    share_zi0 = artisanal_mining_area_ha / total_yearly_mining_area
  ) |> 
  filter(year == 2001) |>
  select(muni_id, year, share_zi0)

sum(df_share$share_zi0)


df_shift <- df |> 
  mutate(
    shift_1 = GoldPrice - lag(GoldPrice, 1),
    shift_2 = GoldPrice - lag(GoldPrice, 2),
    shift_3 = GoldPrice - lag(GoldPrice, 3),
    shift_4 = GoldPrice - lag(GoldPrice, 4)
  ) |> filter(year > 2001) |> 
  select(muni_id, year, shift_1, shift_2, shift_3, shift_4)


df_bartik <- df |> 
  left_join(
    df_share,
    by = c("muni_id")
  ) |> 
  rename(
    year = year.x
  ) |> 
  select(-c(year.y))

df_bartik <- df_bartik |> 
  left_join(
    df_shift,
    by = c("muni_id", "year")
  ) |> 
  filter(year > 2001)


df_bartik_final <- df_bartik |> 
  mutate(
    bartik = shift_1 * share_zi0,
    bartik2 = shift_2 * share_zi0,
    bartik3 = shift_3 * share_zi0,
    bartik4 = shift_4 * share_zi0
  ) |> 
  filter(year <= 2022)

## REGRESSION: Reduced form

controls <- readRDS("raw_data/brazil_munis_indicators.RDS") |> 
  mutate(
    gdp_pc_change = gdp_pc - lag(gdp_pc, 1),
    population_change = population - lag(population, 1),
    pop_dens_change = pop_dens - lag(pop_dens, 1),
    pa_tot_ha_change = pa_tot_ha - lag(pa_tot_ha, 1),
    n_fined_change = n_fined - lag(n_fined, 1),
    brl_fined_change = brl_fined - lag(brl_fined, 1)
  ) |>
  select(-c(muni, state, biome_ama, biome_cer, biome_caa, biome_mat, biome_pam, biome_pan, biome_maj, 
            pa_fed_km2, pa_ind_km2, pa_oth_km2, pa_sub_km2, pa_own_km2, pa_loc_km2, pa_tot_km2, soy_brl,
            gdp, pa_fed_ha, pa_ind_ha, pa_oth_ha, pa_sub_ha, gdp_pc, population, pop_dens,
            pa_tot_ha, n_fined, brl_fined,
            pa_loc_ha, cattle, soy_ton, p_beef, p_crops, cattle_dens, soy_dens,
            
            area_ha, gva_agric, gva_ind, gva_serv, gva_public, gva_total, tax_total
  ))



# Join temporarily
df_model <- df_bartik_final %>%
  left_join(controls, by = c("muni_id", "year")) |> 
  #select(-c(area_ha.y)) |> 
  # rename(
  #   area_ha = area_ha.x
  # ) |> 
  mutate_all(~replace(., is.na(.), 0))

## count NAs

na_count <- sum(is.na(df_model))
colSums(is.na(df_model))


# Control variable names
control_var <- setdiff(names(df_model), c("muni_id", "year", "artisanal_mining_area_ha", "change_in_area", 
                                          "mining", "mining_gain_gross", "mining_net",
                                          "forest_to_pasture_gross", "forest_to_pasture_net",
                                          "forest_to_soy_gross", "forest_to_soy_net",
                                          "f_vegetation_gross", "forest_to_mining_gross", "forest_to_mining_net",
                                          "GoldPrice", "area_ha", "share_zi0", "shift_1", "shift_2", "shift_3", 
                                          "shift_4", "bartik", "bartik2", "bartik3", "bartik4",
                                          "forest_loss_all_gross", "forest_loss_all_net"))


########## FIRST STAGE ##########

### dependent variable change in area ###

fml1b <- reformulate(c("bartik", control_var), response = "change_in_area")
full_formula1b <- as.formula(paste("change_in_area ~", rhs1, "| year"))
first_stage1b <- feols(full_formula1b, data = df_model)
summary(first_stage1b)

fml2b <- reformulate(c("bartik2", control_var), response = "change_in_area")
full_formula2b <- as.formula(paste("change_in_area ~", rhs2, "| year"))
first_stage2b <- feols(full_formula2b, data = df_model)
summary(first_stage2b)

fml3b <- reformulate(c("bartik3", control_var), response = "change_in_area")
# Convert formula to character, then append fixed effects
rhs3b <- paste(all.vars(fml3b)[-1], collapse = " + ")
full_formula3b <- as.formula(paste("change_in_area ~", rhs3, "| year"))
# Estimate fixed effects model
first_stage3b <- feols(full_formula3b, data = df_model)
summary(first_stage3b)

fml4b <- reformulate(c("bartik4", control_var), response = "change_in_area")
# Convert formula to character, then append fixed effects
rhs4b <- paste(all.vars(fml4b)[-1], collapse = " + ")
full_formula4b <- as.formula(paste("change_in_area ~", rhs4, "| year"))
# Estimate fixed effects model
first_stage4b <- feols(full_formula4b, data = df_model)
summary(first_stage4b)


# Combine first-stage models into a list  
first_stage_models_change <- list(
  "t-1" = first_stage1b,
  "t-2" = first_stage2b,
  "t-3" = first_stage3b,
  "t-4" = first_stage4b
)

# Create a summary table for first-stage models
modelsummary(
  first_stage_models_change,
  output = "latex",
  title = "First Stage Estimates - Change in Area",
  coef_map = c(
    "bartik" = "Bartik",
    "bartik2" = "Bartik",
    "bartik3" = "Bartik",
    "bartik4" = "Bartik"
  ),
  statistic = c("({std.error})", "p.value"),  # similar to stargazer's default
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Deviance|RMSE",  # omit extra stats
  add_rows = NULL,
  escape = FALSE  # to allow LaTeX symbols in labels
)

###### SECOND STAGE #########

# second stage for change_in_area
second_stage1 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                        population_change + pop_dens_change + pa_tot_ha_change + 
                        n_fined_change + brl_fined_change | year | change_in_area ~ bartik,
                        data = df_model)
summary(second_stage1)

second_stage2 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                        population_change + pop_dens_change + pa_tot_ha_change + 
                        n_fined_change + brl_fined_change | year | change_in_area ~ bartik2,
                        data = df_model)
summary(second_stage2)

second_stage3 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                        population_change + pop_dens_change + pa_tot_ha_change + 
                        n_fined_change + brl_fined_change | year | change_in_area ~ bartik3,
                        data = df_model)
summary(second_stage3)

second_stage4 <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                        population_change + pop_dens_change + pa_tot_ha_change + 
                        n_fined_change + brl_fined_change | year | change_in_area ~ bartik4,
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
    "fit_change_in_area" = "Change in Garimpo Area"
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

# multiple bartiks

second_stage1B <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | change_in_area ~ bartik,
                        data = df_model)
summary(second_stage1B)

second_stage2B <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | 
                          change_in_area ~ bartik + bartik2,
                        data = df_model)
summary(second_stage2B)

second_stage3B <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | 
                          change_in_area ~ bartik + bartik2 + bartik3,
                        data = df_model)
summary(second_stage3B)

second_stage4B <- feols(forest_loss_all_gross ~ spei_dry + gdp_pc_change + 
                          population_change + pop_dens_change + pa_tot_ha_change + 
                          n_fined_change + brl_fined_change | year | 
                          change_in_area ~ bartik + bartik2 + bartik3 + bartik4,
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
    "fit_change_in_area" = "Change in Garimpo Area"
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

############################################################
#analysis 2001 garimpo

# analyse 2005 garimpo 
df_graph <- df_model |>
  rename(
    garimpo_ha = artisanal_mining_area_ha,
    garimpo_ha_change = change_in_area
  ) |>
  group_by(muni_id) |>
  filter(any(garimpo_ha > 0, na.rm = TRUE)) |>  # <-- filter out municipalities with no garimpo ever
  summarize(
    garimpo_flag_2001 = as.integer(any(share_zi0 > 0, na.rm = TRUE)),
    garimpo_2022 = sum(garimpo_ha[year == 2022], na.rm = TRUE),
    garimpo_2002 = sum(garimpo_ha[year == 2002], na.rm = TRUE)
  ) |>
  mutate(
    garimpo_change_02_22 = garimpo_2022 - garimpo_2002
  ) |>
  select(muni_id, garimpo_flag_2001, garimpo_change_02_22)

df_graph |> count(garimpo_flag_2001)
#print the municipalities with no garimpo in 2005
df_graph |> filter(garimpo_flag_2001 == 0) |> 
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

df_model1 <- muni_sf %>%
  left_join(df_model, by = "muni_id") 

ggplot() +
  # Base map: Legal Amazon region (light gray/white fill)
  geom_sf(data = legal_amazon, fill = "white", alpha = 0.3) +
  # Main layer: fill by garimpo change, black border
  geom_sf(data = df_graph, aes(fill = garimpo_change_02_22), color = "black", size = 0.5) +
  # Yellow contour for municipalities without mining in 2005
  geom_sf(
    data = df_model1 |> 
      group_by(muni_id) |> 
      filter(any(share_zi0 == 0, na.rm = TRUE) & any(change_in_area > 0, na.rm = TRUE)) |> 
      ungroup()
    ,
    fill = NA, color = "red", size = 2
  ) +
  # Fill scale for mining change
  scale_fill_viridis_c(direction = -1, option = "D", name = "Change in Garimpo ha",
                       na.value = "white") +
  # Labels and theme
  labs(title = "Change in Garimpo Area by Municipality in the Brazilian Legal Amazon (2002 vs 2022)",
       subtitle = "with highlighted the municipalities with no garimpo in 2001") +
  theme_void() +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(size = 16, face = "bold")
  ) +
  # Optional coordinate cropping
  coord_sf(ylim = c(-19, 6))
