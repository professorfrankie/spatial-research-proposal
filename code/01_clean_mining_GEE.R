# File to process various GEE-Mapbiomas files
library(dplyr)
library(stringr)
library(sf)
library(tibble)
library(tidyverse)


mining <- read_csv("raw_data/mining_munis.csv") |> 
  transmute(muni_id = CD_MUN, 
            year = as.numeric(substr(bandName, 17, 20)), 
            histogram = replace(histogram, histogram == "{}", NA), 
            histogram = str_replace_all(histogram, "[{}]", "")) |> 
  filter(!is.na(histogram)) |> 
  separate_rows(histogram, sep = ",") |> 
  separate(histogram, into = c("mining_id", "area"), sep = "=") |>  
  mutate(mining_id = as.numeric(mining_id)) |> 
  transmute(muni_id, year, mining_id, 
            area_ha = as.numeric(area) * 30^2 / 10^4) |> 
  arrange(muni_id, year)

# need to map it to mining substances names
# maybe map together certain substances to make it more feasible
# compute shares by relating it to municipality area using the shapefile?
shp_munis <- read_sf("raw_data/BR_Municipios_2021.shp")


# mapping of mining_id to substances and mining types
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
    rep("Metallic - Other", 12),     # 202–213
    "Metallic - Tin",               # 214
    "Metallic - Gold",              # 215
    "Non-metallic - Class 2 minerals", # 217
    rep("Non-metallic - Other", 5), # 218–222
    "Precious stones",              # 224
    "Ornamental stones",            # 225
    rep("Non identified", 3),       # 227–229
    
    # Industrial
    "Metallic - Iron", "Metallic - Manganese", "Metallic - Nickel", "Metallic - Asbestos",
    "Metallic - Molybdenum", "Metallic - Titanium", "Metallic - Chrome", "Metallic - Copper",
    "Metallic - Aluminum", "Metallic - Magnesium", "Metallic - Barium", "Metallic - Niobium",
    "Metallic - Tin", "Metallic - Gold",
    "Non-metallic - Class 2 minerals", "Non-metallic - Fluorine", "Non-metallic - Phosphor",
    "Non-metallic - Graphite", "Non-metallic - Silicon", "Non-metallic - Limestone",
    "Non identified", "Non identified",
    "Energetic - Mineral coal", "Energetic - Uranium", "Energetic - Natural Gas and oil",
    
    # Other
    rep("Other", 25)
  ),
  mining_type = c(
    # Artisanal
    rep("Artisanal", 25),  # total: 25
    
    # Industrial
    rep("Industrial", 25),
    
    # Other
    rep("Other", 25)
  )
)

mining <- mining |> 
  left_join(substances_all, by = "mining_id")

