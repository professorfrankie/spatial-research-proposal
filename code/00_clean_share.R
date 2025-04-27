library(tidyverse)
library(sf)
library(geobr)
library(ggtext)
library(readxl)
library(datazoom.amazonia)

mining <- read_csv("raw_data/mining_munis.csv", quote = "")

mining_clean <- mining |> 
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
            area_ha = as.numeric(area) * 30^2 / 10^4) |> 
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

mining_clean <- mining_clean |> 
  left_join(substances_all, by = "mining_id")

garimpo <- mining_clean |> 
  filter(year == 1985) |> 
  filter(str_detect(substance, "artisanal")) 

#garimpo with the sum of area_ha by muni_id
garimpo_sum <- garimpo |> 
  group_by(muni_id) |> 
  summarise(total_area_ha = sum(area_ha, na.rm = TRUE)) |> 
  ungroup() |> 
  select(muni_id, total_area_ha,)

#load municipalities area
muni_area <- read_excel("raw_data/AR_BR_RG_UF_RGINT_RGI_MUN_2023.xls") |> 
  select(muni_id = CD_MUN, area_km2 = AR_MUN_2023) |> 
  mutate(area_ha = area_km2*10^4) |> 
  select(muni_id, area_ha)

data("municipalities")
legal_amazon_munis <- municipalities %>%
  filter(legal_amazon == 1) %>%
  pull(code_muni)

share <- garimpo_sum |> 
  left_join(muni_area, by = "muni_id") |> 
  mutate(share = total_area_ha / area_ha) |> 
  filter(muni_id %in% legal_amazon_munis) |>
  select(muni_id, share)

#summary statistics of share
summary(share$share)

