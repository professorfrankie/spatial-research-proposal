# File to process various GEE-Mapbiomas files
library(dplyr)
library(stringr)
library(sf)
library(readxl)


### MINING DATA

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

mining_spec <- mining_clean |> 
  left_join(substances_all, by = "mining_id") |> 
  mutate(mining_type = case_when(
    str_detect(substance, "artisanal") ~ "artisanal",
    str_detect(substance, "industrial") ~ "industrial",
    TRUE ~ "other"
  ))

garimpo <- mining_spec |> 
  filter(str_detect(substance, "artisanal"))

### LANDUSE DATA

landuse <- read_csv("raw_data/lulc_munis.csv") |> 
  transmute(muni_id = CD_MUN, 
            year = as.numeric(substr(bandName, 16, 19)), 
            histogram = replace(histogram, histogram == "{}", NA), 
            histogram = str_replace_all(histogram, "[{}]", "")) |> 
  filter(!is.na(histogram)) |> 
  separate_rows(histogram, sep = ",") |> 
  separate(histogram, into = c("landuse_id", "area"), sep = "=") |>  
  mutate(landuse_id = as.numeric(landuse_id)) |> 
  transmute(muni_id, year, landuse_id, 
            area_ha_lu = as.numeric(area) * 30^2 / 10^4) |> 
  arrange(muni_id, year)

landuse_classes <- tibble(
  landuse_id = c(
    # Forest
    3, 4, 5, 6, 49,
    # Herbaceous and Shrubby Vegetation
    11, 12, 32, 29, 50,
    # Farming
    15, 39, 20, 40, 62, 41, 46, 47, 35, 48, 9, 21,
    # Non-vegetated area
    23, 24, 30, 25,
    # Water
    33, 31,
    # Not observed
    0
  ),
  
  landuse_class = c(
    # Forest
    "Forest Formation", "Savanna Formation", "Mangrove", "Floodable Forest", "Wooded Sandbank Vegetation",
    # Herbaceous and Shrubby Vegetation
    "Wetland", "Grassland", "Hypersalibe Tidal Flat", "Rocky Outcrop", "Herbaceous Sandbank Vegetation",
    # Farming
    "Pasture", "Soybean", "Sugarcane", "Rice", "Cotton", "other temporary crops", "Coffe", "Citrus", "Palm oil", "Other perennial crops", "Forest Plantation", "Mosaic of uses",
    # Non-vegetated area
    "Beach", "Urban Area", "Mining", "Non-vegetated area",
    # Water
    "Water", "Aquaculture",
    # Not observed
    "Not observed"
  )
)

# Join landuse data with landuse classes
landuse <- landuse |> 
  left_join(landuse_classes, by = "landuse_id") |> 
  mutate(landuse_class = factor(landuse_class, levels = landuse_classes$landuse_class)) |> 
  select(muni_id, year, landuse_id, area_ha_lu, landuse_class)

### LANDUSE CHANGE DATA

transition <- readRDS("raw_data/land_use_change_v9.rds")

transition <- transition |> 
  mutate(muni_id = as.character(muni_id)) |> 
  select(muni_id, year, forest_loss_all_net, mining, mining_gain_gross, 
         mining_net, f_veg_to_mining_gross, forest_to_mining_gross, 
         forest_to_mining_net)
  
## GOLD DATA

gold <- read_csv("raw_data/monthly.csv")

gold$Date <- as.Date(paste(gold$Date, "01", sep="-"), format="%Y-%m-%d")

gold_yearly <- gold %>%
  filter(between(Date, as.Date('1985-01-01'), as.Date('2023-12-01'))) %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(MeanGoldPrice = mean(Price, na.rm = TRUE))

gold_yearly <- gold_yearly |>
  rename(year = Year)
  
## MERGE DATASET

df <- garimpo %>%
  left_join(transition, by = c("muni_id", "year")) %>%
  #left_join(landuse, by = c("muni_id", "year")) |> #to understand if it is necessary
  left_join(gold_yearly, by = c("year"))


## df of amazzonas

library(datazoom.amazonia)

muni_area <- read_excel("raw_data/AR_BR_RG_UF_RGINT_RGI_MUN_2023.xls") |> 
  select(muni_id = CD_MUN, area_km2 = AR_MUN_2023) |> 
  mutate(area_ha = area_km2*10^4) |> 
  select(muni_id, area_ha)

data("municipalities")
legal_amazon_munis <- municipalities %>%
  filter(legal_amazon == 1) %>%
  pull(code_muni)

df_amazon <- df |> 
  filter(muni_id %in% legal_amazon_munis) |> 
  left_join(muni_area, by = "muni_id")


