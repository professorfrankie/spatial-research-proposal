library(dplyr)
library(tidyverse)
library(stringr)
library(sf)
library(readxl)
library(readr)
library(datazoom.amazonia)

transition <- readRDS("raw_data/land_use_change_v9.rds") |> 
  select(muni_id, year, forest_loss_all_gross, forest) |>
  filter(between(year, 2002, 2022))

controls <- readRDS("raw_data/brazil_munis_indicators.RDS") |> 
  select(muni_id, year, gdp_pc, population, pop_dens,
         pa_tot_ha, n_fined, brl_fined, spei_dry)

mining <- read_csv("raw_data/mining_munis.csv", quote = "") |> 
  transmute(muni_id = CD_MUN, 
            muni_id = str_replace(muni_id, '"', ""),
            year = as.numeric(substr(bandName, 17, 20)), 
            histogram = replace(histogram, histogram == "{}", NA), 
            histogram = str_replace_all(histogram, "[{}]", ""),
            histogram = str_replace_all(histogram, '"', "")) |> 
  filter(!is.na(histogram)) |> 
  separate_rows(histogram, sep = ",") |> 
  separate(histogram, into = c("mining_id", "area"), sep = "=") |>  
  mutate(mining_id = as.numeric(mining_id)) |> 
  transmute(muni_id, year, mining_id, 
            area_ha_mining = as.numeric(area) * 30^2 / 10^4) |> 
  arrange(muni_id, year)

substances_all <- tibble(
  mining_id = c(
    # Artisanal
    202:213, 214, 215, 217, 218:222, 224, 225, 227:229,
    
    # Industrial
    102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
    112, 113, 114, 115, 117, 118, 119, 120, 121, 122,
    124, 125, 127, 128, 129,
    
    # Other
    302, 303, 304, 305, 306, 307, 308, 309, 310, 311,
    312, 313, 314, 315, 317, 318, 319, 320, 321, 322,
    324, 325, 327, 328, 329
  ),
  substance = c(
    # Artisanal
    rep("artisanal_metallic_other", 12),     # 202–213
    "artisanal_metallic_tin",               # 214
    "artisanal_metallic_gold",              # 215
    rep("artisanal_non-metallic", 6), # 217–222
    rep("artisanal_precious stones",2),              # 224-225
    rep("artisanal_non_identified", 3),       # 227–229
    
    # Industrial
    rep("industrial_metallic_other", 12),
    "industrial_metallic_tin",               # 214
    "industrial_metallic_gold",              # 215
    rep("industrial_non-metallic", 6),
    rep("industrial_non_identified", 2), 
    rep("industrial_energetic", 3),
    # Other
    rep("mining_OTHER", 25)
  ),
)

mining_spec <- mining |> 
  left_join(substances_all, by = "mining_id") |> 
  mutate(mining_type = case_when(
    str_detect(substance, "artisanal") ~ "artisanal",
    str_detect(substance, "industrial") ~ "industrial",
    TRUE ~ "other"
  ))

garimpo <- mining_spec |> 
  filter(str_detect(substance, "artisanal")) |> 
  group_by(muni_id, year) |>
  summarise(garimpo_ha = sum(area_ha_mining, na.rm = TRUE), 
            .groups = "drop") |> 
  mutate(garimpo_ha_change = garimpo_ha - lag(garimpo_ha)) |>
  filter(between(year, 2002, 2022)) |>
  mutate(muni_id = as.numeric(muni_id))

gold <- read_csv("raw_data/annual.csv")
gold_yearly <- gold |>
  rename(year = Date) |>
  rename(GoldPrice = Price) |>
  filter(between(year, 2002, 2022))

#merge datasets

df <- transition |> 
  left_join(controls, by = c("muni_id", "year")) |>
  left_join(gold_yearly, by = c("year")) |> 
  left_join(garimpo, by = c("muni_id", "year")) |>
  mutate(
    garimpo_ha_change = ifelse(is.na(garimpo_ha_change), 0, garimpo_ha_change),
    garimpo_ha = ifelse(is.na(garimpo_ha), 0, garimpo_ha)
  ) |>
  filter(between(year, 2002, 2022))

data("municipalities")
legal_amazon_munis <- municipalities %>%
  filter(legal_amazon == 1) %>%
  pull(code_muni)

df_amazon <- df |> 
  filter(muni_id %in% legal_amazon_munis)

na_count <- sum(is.na(df_amazon))
colSums(is.na(df_amazon))

write.csv(df_amazon, 
          "processed_data/df_amazon_new.csv")
