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
    share_zi0 = area_ha_mining / area_ha
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
  )

df_bartik <- df_bartik |> 
  left_join(
    df_shift,
    by = c("muni_id", "year")
  ) |> 
  filter(year > 2001) |> 
  select(-year.y)


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

controls <- readRDS("raw_data/brazil_munis_indicators.RDS") |> 
  select(-pa_own_km2)

# Join temporarily
df_model <- df_bartik_final %>%
  left_join(controls, by = c("muni_id", "year")) |> 
  select(-c(area_ha.y)) |> 
  rename(
    area_ha = area_ha.x
  ) |> 
  mutate_all(~replace(., is.na(.), 0))
  )

## count NAs

na_count <- sum(is.na(df_model))
colSums(is.na(df_model))


# Control variable names
control_var <- setdiff(names(controls), c("muni_id", "year"))

# Build formula
fml <- reformulate(c("bartik4", control_var), response = "forest_loss_all_net")

# Convert formula to character, then append fixed effects
rhs <- paste(all.vars(fml)[-1], collapse = " + ")
full_formula <- as.formula(paste("forest_loss_all_net ~", rhs, "| year"))

# Estimate fixed effects model
model1 <- feols(full_formula, data = df_model)

summary(model1)

library(stargazer)

stargazer(model1, type = "text")


# Assuming df_model is already available and contains the relevant columns

# Model 1 with Bartik 1
model1 <- feols(forest_loss_all_net ~ bartik1 + control1 + control2 + control3 | muni_id + year, data = df_model)

# Model 2 with Bartik 2
model2 <- feols(forest_loss_all_net ~ bartik2 + control1 + control2 + control3 | muni_id + year, data = df_model)

# Model 3 with Bartik 3
model3 <- feols(forest_loss_all_net ~ bartik3 + control1 + control2 + control3 | muni_id + year, data = df_model)

# Model 4 with Bartik 4
model4 <- feols(forest_loss_all_net ~ bartik4 + control1 + control2 + control3 | muni_id + year, data = df_model)

# Display the models in a stargazer table
stargazer(model1, model2, model3, model4, type = "text", 
          title = "Regression Results with Different Bartik Variables",
          dep.var.labels = "Forest Loss (Net)",
          covariate.labels = c("Bartik 1", "Bartik 2", "Bartik 3", "Bartik 4", "Control1", "Control2", "Control3"),
          keep = c("Bartik1", "Bartik2", "Bartik3", "Bartik4", "control1", "control2", "control3"))
               