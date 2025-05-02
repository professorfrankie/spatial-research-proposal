library(dplyr)
library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(readr)


df <- read_csv("processed_data/df_amazon_raw.csv") |> 
  select(-c(...1))

df_share <- df |> 
  filter(year == 2001) |>
  mutate(
    share_zi0 = artisanal_mining_area_ha / area_ha
  ) |> 
  select(muni_id, year, share_zi0)


df_shift <- df |> 
  mutate(
    shift_1 = lag(MeanGoldPrice, 1),
    shift_2 = lag(MeanGoldPrice, 2),
    shift_3 = lag(MeanGoldPrice, 3),
    shift_4 = lag(MeanGoldPrice, 4)
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

library(fixest)

controls <- readRDS("raw_data/brazil_munis_indicators.RDS") 
  #select(-c(pa_own_km2, cattle, cattle_dens, p_beef))

# Join temporarily
df_model <- df_bartik_final %>%
  left_join(controls, by = c("muni_id", "year")) |> 
  select(-c(area_ha.y)) |> 
  rename(
    area_ha = area_ha.x
  ) |> 
  mutate_all(~replace(., is.na(.), 0))

## count NAs

na_count <- sum(is.na(df_model))
colSums(is.na(df_model))


# Control variable names
control_var <- setdiff(names(controls), c("muni_id", "year"))

##### REDUCED FORM #####

# Build formula
fml <- reformulate(c("bartik3", control_var), response = "forest_to_mining_gross")

# Convert formula to character, then append fixed effects
rhs <- paste(all.vars(fml)[-1], collapse = " + ")
full_formula <- as.formula(paste("forest_to_mining_gross ~", rhs, "| year"))

# Estimate fixed effects model
model1 <- feols(full_formula, data = df_model)

summary(model1)

library(stargazer)

stargazer(model1, type = "text")


########## FIRST STAGE ##########

### dependent variable artisanal_mining_area_ha ###

# Build formula
fml1 <- reformulate(c("bartik3", control_var), response = "artisanal_mining_area_ha")

# Convert formula to character, then append fixed effects
rhs1 <- paste(all.vars(fml1)[-1], collapse = " + ")
full_formula1 <- as.formula(paste("artisanal_mining_area_ha ~", rhs1, "| year"))

# Estimate fixed effects model
first_stage <- feols(full_formula1, data = df_model)

summary(first_stage)

### dependent variable change in area ###

fml1_1 <- reformulate(c("bartik3", control_var), response = "change_in_area")

full_formula1_1 <- as.formula(paste("change_in_area ~", rhs1, "| year"))

first_stage_1 <- feols(full_formula1_1, data = df_model)

summary(first_stage_1)

###### SECOND STAGE #########

# Build second-stage (2SLS) formula

################################################################################
rhs_controls <- paste(control_var, collapse = " + ")

c_hat <- fitted.values(first_stage_1)

second_stage <- feols(
  as.formula(paste(forest_loss_all_net ~ c_hat, rhs_controls, "| year")),
  data = df_model)

iv_formula <- as.formula(
  paste0("forest_loss_all_net ~ ", rhs_controls, 
         " + forest_to_mining_gross | year | (forest_to_mining_gross ~ bartik3)")
)

second_stage <- feols(iv_formula, data = df_model)
summary(second_stage)

# View results
summary(second_stage)

################################################################################


# First get fitted values from first stage
c_hat <- fitted.values(first_stage_1)

# Add c_hat to your data
df_model$c_hat <- c_hat

# Construct right-hand side: "c_hat + control1 + control2 + ..."
rhs_with_controls <- paste("c_hat", rhs_controls, sep = " + ")

# Final formula string
formula_str <- paste("forest_to_mining_gross ~", rhs_with_controls, "| year")

# Convert to formula
iv_formula_manual <- as.formula(formula_str)

# Run second-stage manually with predicted values
second_stage <- feols(iv_formula_manual, data = df_model)

summary(second_stage)
