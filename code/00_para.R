library(dplyr)
library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(readr)
library(fixest)
library(modelsummary)
library(geobr)
library(sf)


df <- read_csv("processed_data/df_amazon_newpca.csv") |> 
  select(-c(...1, forest, gdp_pc, population, pop_dens, pa_tot_ha, n_fined, brl_fined)) |> 
  filter(between(year, 2002, 2022))

df <- df %>%
  mutate(para = ifelse(substr(muni_id, 1, 2) == "15", 1, 0))

# municipality data
munis <- read_municipality(year = 2020)
munis_area <- munis %>%
  mutate(area_km2 = as.numeric(st_area(geom))*100 / 10^6) %>% 
  select(code_muni, area_km2)

# join with df
df <- df %>%
  left_join(munis_area, by = c("muni_id" = "code_muni"))

df_para <- df %>%
  filter(para == 1)

df_para_sum <- df_para %>%
  group_by(year) %>%
  summarise(
    total_garimpo_ha = sum(garimpo_ha, na.rm = TRUE),
    total_area_ha = sum(area_km2, na.rm = TRUE))

df_nonpara <- df %>%
  filter(para == 0)

df_nonpara_sum <- df_nonpara %>%
  group_by(year) %>%
  summarise(
    total_garimpo_ha = sum(garimpo_ha, na.rm = TRUE),
    total_area_ha = sum(area_km2, na.rm = TRUE))

para_garimpo_forest_loss <- (df_para_sum[21,1]-df_para_sum[1,1] )*9.7/df_para_sum[1,2]
nonpara_garimpo_forest_loss <- (df_nonpara_sum[21,1]-df_nonpara_sum[1,1] )*31.7/df_nonpara_sum[1,2]
