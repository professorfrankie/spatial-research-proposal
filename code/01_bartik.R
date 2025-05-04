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
    shift_1 = lag(GoldPrice, 1),
    shift_2 = lag(GoldPrice, 2),
    shift_3 = lag(GoldPrice, 3),
    shift_4 = lag(GoldPrice, 4)
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
    pa_own_ha = pa_own_km2*10^4,
    gdp_change = gdp - lag(gdp, 1),
    gdp_pc_change = gdp_pc - lag(gdp_pc, 1),
    population_change = population - lag(population, 1),
    pop_dens_change = pop_dens - lag(pop_dens, 1),
    pa_fed_ha_change = pa_fed_ha - lag(pa_fed_ha, 1),
    pa_ind_ha_change = pa_ind_ha - lag(pa_ind_ha, 1),
    pa_oth_ha_change = pa_oth_ha - lag(pa_oth_ha, 1),
    pa_sub_ha_change = pa_sub_ha - lag(pa_sub_ha, 1),
    pa_own_ha_change = pa_own_ha - lag(pa_own_ha, 1),
    pa_loc_ha_change = pa_loc_ha - lag(pa_loc_ha, 1),
    pa_tot_ha_change = pa_tot_ha - lag(pa_tot_ha, 1),
    cattle_change = cattle - lag(cattle, 1),
    soy_ton_change = soy_ton - lag(soy_ton, 1),
    p_beef_change = p_beef - lag(p_beef, 1),
    p_crops_change = p_crops - lag(p_crops, 1),
    cattle_dens_change = cattle_dens - lag(cattle_dens, 1),
    soy_dens_change = soy_dens - lag(soy_dens, 1),
    n_fined_change = n_fined - lag(n_fined, 1),
    brl_fined_change = brl_fined - lag(brl_fined, 1),
    gva_agric_change = gva_agric - lag(gva_agric, 1),
    gva_ind_change = gva_ind - lag(gva_ind, 1),
    gva_serv_change = gva_serv - lag(gva_serv, 1),
    gva_public_change = gva_public - lag(gva_public, 1),
    gva_total_change = gva_total - lag(gva_total, 1),
    tax_total_chnage = tax_total - lag(tax_total, 1),
  ) |>
  select(-c(muni, state, biome_ama, biome_cer, biome_caa, biome_mat, biome_pam, biome_pan, biome_maj, 
            pa_fed_km2, pa_ind_km2, pa_oth_km2, pa_sub_km2, pa_own_km2, pa_loc_km2, pa_tot_km2, soy_brl,
            gdp, gdp_pc, population, pop_dens, pa_fed_ha, pa_ind_ha, pa_oth_ha, pa_sub_ha, pa_own_ha, 
            pa_loc_ha, pa_tot_ha, cattle, soy_ton, p_beef, p_crops, cattle_dens, soy_dens,
            
            area_ha, n_fined, brl_fined, gva_agric, gva_ind, gva_serv, gva_public, gva_total, tax_total
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
                                          "f_vegetation_gross", "forest_to_mining_gross", "forest_to_mining_net",
                                          "GoldPrice", "area_ha", "share_zi0", "shift_1", "shift_2", "shift_3", 
                                          "shift_4", "bartik", "bartik2", "bartik3", "bartik4"))

##### REDUCED FORM #####

# lag t-1
# Build formula
fml1 <- reformulate(c("bartik", control_var), response = "forest_to_mining_gross")
# Convert formula to character, then append fixed effects
rhs1 <- paste(all.vars(fml1)[-1], collapse = " + ")
full_formula1 <- as.formula(paste("forest_to_mining_gross ~", rhs1, "| year"))
# Estimate fixed effects model
modelt1 <- feols(full_formula1, data = df_model)
summary(modelt1)

# lag t-2
# Build formula
fml2 <- reformulate(c("bartik2", control_var), response = "forest_to_mining_gross")
# Convert formula to character, then append fixed effects                      
rhs2 <- paste(all.vars(fml2)[-1], collapse = " + ")
full_formula2 <- as.formula(paste("forest_to_mining_gross ~", rhs2, "| year"))
# Estimate fixed effects model
modelt2 <- feols(full_formula2, data = df_model)
summary(modelt2)

# lag t-3
# Build formula
fml3 <- reformulate(c("bartik3", control_var), response = "forest_to_mining_gross")
# Convert formula to character, then append fixed effects
rhs3 <- paste(all.vars(fml3)[-1], collapse = " + ")
full_formula3 <- as.formula(paste("forest_to_mining_gross ~", rhs3, "| year"))
# Estimate fixed effects model
modelt3 <- feols(full_formula3, data = df_model)
summary(modelt3)

# lag t-4
# Build formula
fml4 <- reformulate(c("bartik4", control_var), response = "forest_to_mining_gross")
# Convert formula to character, then append fixed effects
rhs4 <- paste(all.vars(fml4)[-1], collapse = " + ")
full_formula4 <- as.formula(paste("forest_to_mining_gross ~", rhs4, "| year"))
# Estimate fixed effects model
modelt4 <- feols(full_formula4, data = df_model)
summary(modelt4)


# Example with feols models
modelsRF <- list(
  "t-1" = modelt1,
  "t-2" = modelt2,
  "t-3" = modelt3,
  "t-4" = modelt4
)

modelsummary(
  modelsRF,
  output = "latex",
  title = "Reduced Form Estimates",
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


########## FIRST STAGE ##########

### dependent variable artisanal_mining_area_ha ###

# Build formula
fml1a <- reformulate(c("bartik", control_var), response = "artisanal_mining_area_ha")

# Convert formula to character, then append fixed effects
rhs1a <- paste(all.vars(fml1a)[-1], collapse = " + ")
full_formula1a <- as.formula(paste("artisanal_mining_area_ha ~", rhs1a, "| muni_id + year"))

# Estimate fixed effects model
first_stage1a <- feols(full_formula1a, data = df_model)

summary(first_stage1a)

### dependent variable change in area ###

fml1b <- reformulate(c("bartik", control_var), response = "change_in_area")
full_formula1b <- as.formula(paste("change_in_area ~", rhs1, "| year"))
first_stage1b <- feols(full_formula1b, data = df_model)
summary(first_stage1b)


fml2a <- reformulate(c("bartik2", control_var), response = "artisanal_mining_area_ha")
# Convert formula to character, then append fixed effects
rhs2a <- paste(all.vars(fml2a)[-1], collapse = " + ")
full_formula2a <- as.formula(paste("artisanal_mining_area_ha ~", rhs2a, "| muni_id + year"))
# Estimate fixed effects model
first_stage2a <- feols(full_formula2a, data = df_model)
summary(first_stage2a)

fml2b <- reformulate(c("bartik2", control_var), response = "change_in_area")
full_formula2b <- as.formula(paste("change_in_area ~", rhs2, "| year"))
first_stage2b <- feols(full_formula2b, data = df_model)
summary(first_stage2b)

fml3a <- reformulate(c("bartik3", control_var), response = "artisanal_mining_area_ha")
# Convert formula to character, then append fixed effects
rhs3a <- paste(all.vars(fml3a)[-1], collapse = " + ")
full_formula3a <- as.formula(paste("artisanal_mining_area_ha ~", rhs3a, "| muni_id + year"))
# Estimate fixed effects model
first_stage3a <- feols(full_formula3a, data = df_model)
summary(first_stage3a)

fml3b <- reformulate(c("bartik3", control_var), response = "change_in_area")
# Convert formula to character, then append fixed effects
rhs3b <- paste(all.vars(fml3b)[-1], collapse = " + ")
full_formula3b <- as.formula(paste("change_in_area ~", rhs3, "| year"))
# Estimate fixed effects model
first_stage3b <- feols(full_formula3b, data = df_model)
summary(first_stage3b)

fml4a <- reformulate(c("bartik4", control_var), response = "artisanal_mining_area_ha")
# Convert formula to character, then append fixed effects
rhs4a <- paste(all.vars(fml4a)[-1], collapse = " + ")
full_formula4a <- as.formula(paste("artisanal_mining_area_ha ~", rhs4a, "| muni_id + year"))
# Estimate fixed effects model
first_stage4a <- feols(full_formula4a, data = df_model)
summary(first_stage4a)

fml4b <- reformulate(c("bartik4", control_var), response = "change_in_area")
# Convert formula to character, then append fixed effects
rhs4b <- paste(all.vars(fml4b)[-1], collapse = " + ")
full_formula4b <- as.formula(paste("change_in_area ~", rhs4, "| year"))
# Estimate fixed effects model
first_stage4b <- feols(full_formula4b, data = df_model)
summary(first_stage4b)

# Combine first-stage models into a list
first_stage_models <- list(
  "t-1" = first_stage1a,
  "t-2" = first_stage2a,
  "t-3" = first_stage3a,
  "t-4" = first_stage4a
)

# Create a summary table for first-stage models
modelsummary(
  first_stage_models,
  output = "latex",
  title = "First Stage Estimates",
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

# Build second-stage (2SLS) formula

rhs_controls <- paste(control_var, collapse = " + ")

# second stage for artisanal_mining_area_ha
m1_hat <- fitted.values(first_stage1a)
df_model$m1_hat <- m1_hat

fml1A <- reformulate(c("m1_hat", rhs_controls), response = "forest_to_mining_gross")
# Convert formula to character, then append fixed effects
rhs1A <- paste(all.vars(fml1A)[-1], collapse = " + ")
full_formula1A <- as.formula(paste("forest_to_mining_gross ~", rhs1A, "|muni_id + year"))
# Estimate fixed effects model
second_stage1A <- feols(full_formula1A, data = df_model)

summary(second_stage1A)

m2_hat <- fitted.values(first_stage2a)
df_model$m2_hat <- m2_hat
fml2A <- reformulate(c("m2_hat", rhs_controls), response = "forest_to_mining_gross")
# Convert formula to character, then append fixed effects
rhs2A <- paste(all.vars(fml2A)[-1], collapse = " + ")
full_formula2A <- as.formula(paste("forest_to_mining_gross ~", rhs2A, "| year"))
# Estimate fixed effects model
second_stage2A <- feols(full_formula2A, data = df_model)
summary(second_stage2A)

m3_hat <- fitted.values(first_stage3a)
df_model$m3_hat <- m3_hat
fml3A <- reformulate(c("m3_hat", rhs_controls), response = "forest_to_mining_gross")
# Convert formula to character, then append fixed effects
rhs3A <- paste(all.vars(fml3A)[-1], collapse = " + ")
full_formula3A <- as.formula(paste("forest_to_mining_gross ~", rhs3A, "| year"))
# Estimate fixed effects model
second_stage3A <- feols(full_formula3A, data = df_model)
summary(second_stage3A)

m4_hat <- fitted.values(first_stage4a)
df_model$m4_hat <- m4_hat
fml4A <- reformulate(c("m4_hat", rhs_controls), response = "forest_to_mining_gross")
# Convert formula to character, then append fixed effects
rhs4A <- paste(all.vars(fml4A)[-1], collapse = " + ")
full_formula4A <- as.formula(paste("forest_to_mining_gross ~", rhs4A, "| year"))
# Estimate fixed effects model
second_stage4A <- feols(full_formula4A, data = df_model)
summary(second_stage4A)

# second stage for change_in_area
dm1_hat <- fitted.values(first_stage1b)
df_model$dm1_hat <- dm1_hat
fml1B <- reformulate(c("dm1_hat", rhs_controls), response = "forest_to_mining_gross")
# Convert formula to character, then append fixed effects
rhs1B <- paste(all.vars(fml1B)[-1], collapse = " + ")
full_formula1B <- as.formula(paste("forest_to_mining_gross ~", rhs1B, "| year"))
# Estimate fixed effects model
second_stage1B <- feols(full_formula1B, data = df_model)
summary(second_stage1B)

dm2_hat <- fitted.values(first_stage2b)
df_model$dm2_hat <- dm2_hat
fml2B <- reformulate(c("dm2_hat", rhs_controls), response = "forest_to_mining_gross")
# Convert formula to character, then append fixed effects
rhs2B <- paste(all.vars(fml2B)[-1], collapse = " + ")
full_formula2B <- as.formula(paste("forest_to_mining_gross ~", rhs2B, "| year"))
# Estimate fixed effects model
second_stage2B <- feols(full_formula2B, data = df_model)
summary(second_stage2B)
dm3_hat <- fitted.values(first_stage3b)
df_model$dm3_hat <- dm3_hat
fml3B <- reformulate(c("dm3_hat", rhs_controls), response = "forest_to_mining_gross")
# Convert formula to character, then append fixed effects
rhs3B <- paste(all.vars(fml3B)[-1], collapse = " + ")
full_formula3B <- as.formula(paste("forest_to_mining_gross ~", rhs3B, "| year"))
# Estimate fixed effects model
second_stage3B <- feols(full_formula3B, data = df_model)
summary(second_stage3B)
dm4_hat <- fitted.values(first_stage4b)
df_model$dm4_hat <- dm4_hat
fml4B <- reformulate(c("dm4_hat", rhs_controls), response = "forest_to_mining_gross")
# Convert formula to character, then append fixed effects
rhs4B <- paste(all.vars(fml4B)[-1], collapse = " + ")
full_formula4B <- as.formula(paste("forest_to_mining_gross ~", rhs4B, "| year"))
# Estimate fixed effects model
second_stage4B <- feols(full_formula4B, data = df_model)
summary(second_stage4B)

# Combine second-stage models into a list
second_stage_models <- list(
  "t-1" = second_stage1A,
  "t-2" = second_stage2A,
  "t-3" = second_stage3A,
  "t-4" = second_stage4A
)

# Create a summary table for second-stage models
modelsummary(
  second_stage_models,
  output = "latex",
  title = "Second Stage Estimates",
  coef_map = c(
    "m1_hat" = "mining",
    "m2_hat" = "mining",
    "m3_hat" = "mining",
    "m4_hat" = "mining"
  ),
  statistic = c("({std.error})", "p.value"),  # similar to stargazer's default
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Deviance|RMSE",  # omit extra stats
  add_rows = NULL,
  escape = FALSE  # to allow LaTeX symbols in labels
)

# Combine second-stage models into a list
second_stage_models_change <- list(
  "t-1" = second_stage1B,
  "t-2" = second_stage2B,
  "t-3" = second_stage3B,
  "t-4" = second_stage4B
)
# Create a summary table for second-stage models
modelsummary(
  second_stage_models_change,
  output = "latex",
  title = "Second Stage Estimates - Change in Area",
  coef_map = c(
    "dm1_hat" = " minig",
    "dm2_hat" = " minig",
    "dm3_hat" = " minig",
    "dm4_hat" = " minig"
  ),
  statistic = c("({std.error})", "p.value"),  # similar to stargazer's default
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik|Deviance|RMSE",  # omit extra stats
  add_rows = NULL,
  escape = FALSE  # to allow LaTeX symbols in labels
)

